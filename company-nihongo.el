;;; company-nihongo.el --- Backend for company-mode.

;; Copyright (C) 2018 whitypig

;; Author: whitypig <whitypig@gmail.com>
;; URL:
;; Version: 0.01
;; Package-Requires: ((company-mode) (cl-lib) (s))
;; Keywords: completion

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(require 'company)
(require 'cl-lib)
(require 's)

;;; Customization

(defgroup company-nihongo nil
  "Company-mode backend for completing nihongo."
  :group 'company-nihongo)

(defcustom company-nihongo-limit 100
  "The upper number of candidates to show when completing."
  :type 'number
  :group 'company-nihongo)

(defcustom company-nihongo-separator-regexp "\\W+"
  "Default separator to split string in buffers. Characters in
this regexp are excluded from candidates."
  :type 'regexp
  :group 'company-nihongo)

(defcustom company-nihongo-select-buffer-function
  #'company-nihongo-select-target-mode-buffers
  "A function that returns a list of buffers. Those buffers are
searched for candidates."
  :type 'symbol
  :group 'company-nihongo)

(defcustom company-nihongo-mode-group-list
  '((emacs-lisp-mode lisp-interaction-mode)
    (python-mode inferior-python-mode))
  ""
  :type 'list
  :group 'company-nihongo)

(defcustom company-nihongo-ascii-regexp "[0-9A-Za-z_-]"
  "Regexp used to search for candidates that are not multibyte strings."
  :type 'regexp
  :group 'company-nihongo)

;; (defcustom company-nihongo-punctuations "[。、]"
;;   "Punctuation marks that split Japanese clauses."
;;   :type 'regexp
;;   :group 'company-nihongo)

(defcustom company-nihongo-complete-katakana-by-hiragana t
  "Specifies whether to complete katakana words by hiragana prefix."
  :type 'boolean
  :group 'company-nihongo)

(defcustom company-nihongo-candidate-sorter #'string<
  "Compare function used when sorting candidates."
  :type 'function
  :group 'company-nihongo)

(defcustom company-nihongo-searching-window-size 1000
  "The size of window in which candidates in current buffer are
searched. In other words, searching for candidates in current buffer
is perfomed on [point - window-size, point + window-size]."
  :type 'integer
  :group 'company-nihongo)

;; (setq company-nihongo-searching-window-size 500)
;; (setq company-nihongo-searching-window-size 1000)
;; (setq company-nihongo-searching-window-size 2000)

;;; Variables

(defvar company-nihongo--index-cache-alist nil
  "Alist in the form of (buffer . hashtable).")

(defvar company-nihongo--hashtable-key-length 1
  "The length of key to hash table.")

(defvar company-nihongo-alpha-regexp "[A-Za-z_-]"
  "")

(defvar company-nihongo--search-in-current-buffer-state nil
  "State when searching for candidates failed, which is a list of the
form of (buffer tick prefix).")

(defvar company-nihongo--not-found-prefix nil
  "Prefix string used that was used in the last search, which failed
to find any candidate in current buffer. `nil' means that the last
search found something.")

(defvar company-nihongo--not-found-regexp nil
  "Regexp used in the last search which failed ot find candidates.")

(defvar company-nihongo--not-found-buffer nil
  "The buffer in which the last search was performed and it failed to
find candidates.")

(defvar company-nihongo--last-edit-start-pos-table (make-hash-table :test #'equal)
  "Hashtable, key of which is buffer and value of which is the last
edit position in that buffer.")

(defvar company-nihongo--last-edit-tick-table (make-hash-table :test #'eq)
  "Hashtable, key of which is buffer and value of which is the value returned by #'buffer-chars-modified-tick")

;;; Functions

(defun company-nihongo-compare-candidates (a b)
  (let ((a-ix (string-match-p "\\cC" a))
        (b-ix (string-match-p "\\cC" b)))
    (cond
     ((and a-ix b-ix)
      (if (= a-ix b-ix)
          ;; If the indices of first occurrence of kanji are the same,
          ;; we let how many kanji character a string has be a
          ;; tie-breaker.
          (> (s-count-matches "\\cC" a) (s-count-matches "\\cC" b))
        ;; word that has kanji at smaller index comes first, that is,
        ;; "あい漢字" comes before "あいう漢字"
        (< a-ix b-ix)))
     (a-ix
      t)
     (b-ix
      nil)
     (t
      (string< a b)))))

;; (defun company-nihongo-compare-candidates (a b)
;;   (let ((a-is-kanji (string-match-p "\\cC+" a))
;;         (b-is-kanji (string-match-p "\\cC+" b))
;;         (a-is-ascii (string-match-p company-nihongo-ascii-regexp a))
;;         (b-is-ascii (string-match-p company-nihongo-ascii-regexp b)))
;;     (cond
;;      ;; ascii comes first, then comes kanji.
;;      ;; ((and a-is-ascii b-is-ascii)
;;      ;;  (string< a b))
;;      ;; (a-is-ascii
;;      ;;  t)
;;      ;; (b-is-ascii
;;      ;;  nil)
;;      ((and a-is-kanji b-is-kanji)
;;       (string< a b))
;;      (a-is-kanji
;;       t)
;;      (b-is-kanji
;;       nil)
;;      (t
;;       (string< a b)))))

(defun company-nihongo-select-target-mode-buffers ()
  "Return buffers that have the same major mode as that of current
buffer."
  (let ((target-major-modes (or (cl-find-if
                                 (lambda (elt)
                                   (memq major-mode elt))
                                 company-nihongo-mode-group-list)
                                (list major-mode))))
    (cl-remove-if-not (lambda (buf)
                        (memq (buffer-local-value 'major-mode buf)
                              target-major-modes))
                      (buffer-list))))

(defun company-nihongo--get-regexp ()
  "Return regexp to be used to determine prefix depending on the type
of `char-before'."
  (cond
   ((bobp) nil)
   (t
    (let* ((ch (char-to-string (char-before)))
           ;; Bug: When at the bob, char-before above signals error.
           (anomaly-characters "ー〜")
           (anomaly-regexp (format "[%s]" anomaly-characters)))
      (cond
       ;; ascii-word constituent characters
       ((string-match-p (format "%s" company-nihongo-ascii-regexp) ch)
        company-nihongo-ascii-regexp)
       ((string-match-p anomaly-regexp ch)
        ;; Special case:
        ;; When ch is "～" or "ー", both hiraganra and katakana will be
        ;; correct as regexp, so we will look back for another
        ;; character.
        (cond
         ((save-excursion
            (and (re-search-backward (format "[^%s]" anomaly-characters) nil t)
                 (setq ch (char-to-string (char-after)))))
          ;; We found a character other than "ー" or "〜".
          (company-nihongo--get-regexp-1 ch))
         (t
          ;; Return hiragana as a fallback case.
          "\\cH")))
       (t
        (company-nihongo--get-regexp-1 ch)))))))

(defun company-nihongo--get-regexp-1 (ch)
  (cond
   ;; 2-byte hiragana
   ((string-match-p "\\cH" ch) "\\cH")
   ;; 2-byte katakana
   ((string-match-p "\\cK" ch) "\\cK")
   ;; 2-byte kanji
   ((string-match-p "\\cC" ch) "\\cC")
   (t nil)))

(defun company-nihongo--get-prefix ()
  (let ((regexp (company-nihongo--get-regexp))
        (pos (point)))
    (unless regexp
      ;; Clear the last search state.
      (company-nihongo--clear-not-found-state))
    (when regexp
      ;; Bug: When the string before the point is "う〜",
      ;; company-nihongo-get-regexp returns "\\cH" as regexp. However,
      ;; (looking-at "\\cH") returns nil since (string-match-p "\\cH"
      ;; "〜") is nil, so company-nihongo-prefix doen't move point. As a
      ;; result, ac-prefix becomes "", i.e. empty string.
      (save-excursion
        (backward-char)
        (while (and (not (bobp))
                    (looking-at-p regexp))
          (backward-char))
        (unless (looking-at-p regexp)
          (forward-char))
        (buffer-substring-no-properties (point) pos)))))

(defun company-nihongo--set-not-found-state (prefix buffer)
  (setq company-nihongo--search-in-current-buffer-state
        (list buffer (buffer-chars-modified-tick) prefix)))

(defun company-nihongo--clear-not-found-state ()
  (setq company-nihongo--search-in-current-buffer-state nil))

(defun company-nihongo--get-candidates (prefix)
  "Return a list of candidates that begin with prefix PREFIX."
  (when (and prefix (> (length prefix) 0))
    (delete-dups
     (sort (cl-loop for buf in (funcall company-nihongo-select-buffer-function)
                    with limit = (or company-nihongo-limit
                                     20)
                    nconc (company-nihongo--get-candidates-1 prefix buf) into candidates
                    when (and (integerp limit)
                              (> (length candidates) company-nihongo-limit))
                    return candidates
                    finally return candidates)
           company-nihongo-candidate-sorter))))

(defun company-nihongo--get-candidates-1 (prefix buf)
  "Return a list of candidates that begin with PREFIX in buffer BUF."
  (cond
   ((eq buf (current-buffer))
    ;; Reset hashtable for current buffer.
    ;; (and (assoc buf company-nihongo--index-cache-alist)
    ;;      (assq-delete-all buf company-nihongo--index-cache-alist))
    (company-nihongo--get-candidates-2
     :func #'company-nihongo--get-candidates-in-current-buffer
     :prefix prefix
     :buffer buf))
   (t
    (company-nihongo--get-candidates-2
     :func #'company-nihongo--get-candidates-in-buffer
     :prefix prefix
     :buffer buf))))

(cl-defun company-nihongo--get-candidates-2 (&key func prefix buffer)
  (cond
   ((and company-nihongo-complete-katakana-by-hiragana
         (string-match-p "\\cH" prefix))
    ;; When PREFIX is hiragan word, we also try to search for katakana
    ;; word candidates if
    ;; `company-nihongo-complete-katakana-by-hiragana' is t.
    (nconc (funcall func prefix buffer)
           (funcall func (japanese-katakana prefix) buffer)))
   (t
    (funcall func prefix buffer))))

(defun company-nihongo--in-searching-window-p (buffer pos)
  (let ((start-pos (gethash buffer company-nihongo--last-edit-start-pos-table nil)))
    (cond
     ((null start-pos)
      ;; This is the first time we edit this buffer.
      (puthash buffer pos company-nihongo--last-edit-start-pos-table)
      nil)
     (t
      ;; Check if (start-pos - dist) <= pos <= (start-pos + dist)
      ;; (message "DEBUG: company-nihongo--in-searching-window-p, start-pos=%d, pos=%d" start-pos pos)
      (and (<= (- start-pos company-nihongo-searching-window-size)
               pos)
           (<= pos
               (+ start-pos company-nihongo-searching-window-size)))))))

(defun company-nihongo--buffer-modified-p (buffer)
  (let ((last-tick (gethash buffer company-nihongo--last-edit-tick-table nil))
        (cur-tick (with-current-buffer buffer (buffer-chars-modified-tick))))
    (cond
     ((null last-tick)
      ;; This case means that this is the first time we visit BUFFER.
      ;; Therefore, we have to create Hashtable for this buffer if
      ;; none, so we return t.
      (puthash buffer cur-tick company-nihongo--last-edit-tick-table)
      t)
     ((= last-tick cur-tick)
      nil)
     (t
      t))))

(defun company-nihongo--update-last-edit-start-pos (buf pos)
  (puthash buf pos company-nihongo--last-edit-start-pos-table))

(defun company-nihongo--update-last-edit-tick (buf)
  (with-current-buffer buf
    (puthash buf (buffer-chars-modified-tick) company-nihongo--last-edit-tick-table)))

(cl-defun company-nihongo--build-hashtable-for-buffer (buffer &optional
                                                              (beg (point-min))
                                                              (end (point-max)))
  (cl-loop with table = (make-hash-table :test #'equal)
           with inserted-words = (make-hash-table :test #'equal)
           for word in (company-nihongo--get-word-list buffer :beg beg :end end)
           for key = (substring word 0 company-nihongo--hashtable-key-length)
           when (and (> (length word) company-nihongo--hashtable-key-length)
                     (not (gethash word inserted-words nil)))
           if (gethash key table)
           do (progn (puthash word t inserted-words)
                     ;; this word has not been inserted yet
                     (push word (gethash key table)))
           else
           do (progn (puthash key (list word) table)
                     (puthash word t inserted-words))
           ;; Unite res-table and existing table
           finally return table))

(defun company-nihongo--update-hashtable-for-buffer-region (buffer)
  (let* ((last-edit-start-pos (gethash buffer
                                       company-nihongo--last-edit-start-pos-table))
         (beg (max (point-min)
                   (- last-edit-start-pos
                      company-nihongo-searching-window-size)))
         (end (min (point-max)
                   (+ last-edit-start-pos
                      company-nihongo-searching-window-size)))
         (partial-table (company-nihongo--build-hashtable-for-buffer buffer
                                                                     beg
                                                                     end)))
    (company-nihongo--unite-hashtables buffer partial-table)))

(defun company-nihongo--unite-hashtables (buf partial-table)
  (cl-loop with orig-table = (assoc-default buf company-nihongo--index-cache-alist)
           for key being hash-keys of partial-table
           for lst = (gethash key partial-table)
           for new-lst = (sort (cl-delete-duplicates
                                (append (gethash key orig-table nil) lst)
                                :test #'string=)
                               #'string<)
           do (puthash key new-lst orig-table)))

(defun company-nihongo--prepare-for-current-buffer-completion (buf pos)
  ;; (message "DEBUG: company-nihongo--prepare-for-current-buffer-completion, buf=%s, pos=%d" (buffer-name buf) pos)
  (cond
   ((null (assoc buf company-nihongo--index-cache-alist))
    ;; There is no hashtable for this buffer BUF.
    (company-nihongo--register-hashtable buf))
   ((company-nihongo--in-searching-window-p buf
                                              pos)
    ;; Do nothing
    )
   ((and (not (company-nihongo--in-searching-window-p buf
                                                        pos))
         (not (company-nihongo--buffer-modified-p buf)))
    ;; If we are out of the last searching window and buffer has NOT
    ;; changed, this means that we performed undo and moved somewhere
    ;; in the same buffer. Therefore, we don't need to update
    ;; hashtable, but update last edit position.
    (company-nihongo--update-last-edit-start-pos buf pos))
   (t
    ;; We are out of the searching window and buffer has been modified.
    ;; (message "DEBUG: updating hash table for buffer=%s, pos=%d" (buffer-name buf) pos)
    (company-nihongo--update-last-edit-tick buf)
    ;; Here, we first have to update hashtable because last edit pos
    ;; is used in
    ;; #'company-nihongo--update-hashtable-for-buffer-region.
    (company-nihongo--update-hashtable-for-buffer-region buf)
    (company-nihongo--update-last-edit-start-pos buf pos))))

(defun company-nihongo--get-candidates-in-current-buffer (prefix &rest _ignored)
  "Return a list of candidates in current buffer that begin with
PREFIX."
  (let* ((limit (or (and (integerp company-nihongo-limit) company-nihongo-limit)
                    10))
         (cnt 0)
         (cell (company-nihongo--make-regexp prefix))
         (prefix-regexp (and (consp cell) (car cell)))
         (cand-regexp (and (consp cell) (cdr cell)))
         (cand nil)
         (table (make-hash-table :test #'equal))
         (pos (point))
         (candidates nil)
         (head-candidates nil)
         (prefix-len (length prefix))
         (lst nil)
         (top (max (point-min) (- (point) company-nihongo-searching-window-size)))
         (bottom (min (point-max) (+ (point) company-nihongo-searching-window-size))))
    (cl-assert (and prefix-regexp cand-regexp))
    (company-nihongo--prepare-for-current-buffer-completion (current-buffer) (point))
    (when (company-nihongo--go-search-p prefix (current-buffer))
      ;; (message "DEBUG: in-current-buffer, prefix=%s, company-nihongo--not-found-prefix=%s" prefix company-nihongo--not-found-prefix)
      (company-nihongo--search-candidates-in-buffer cand-regexp
                                                    prefix-len
                                                    top
                                                    (1- pos)
                                                    limit
                                                    table)
      (company-nihongo--search-candidates-in-buffer cand-regexp
                                                    prefix-len
                                                    (1+ pos)
                                                    bottom
                                                    limit
                                                    table)
      (when (< (hash-table-count table) limit)
        ;; If we haven't collected enough candidates, go looking for
        ;; more candidates that are outside of the searching window.
        (cl-loop for cand in (company-nihongo--get-candidates-in-buffer
                              prefix
                              (current-buffer))
                 while (< (hash-table-count table) limit)
                 do (puthash cand t table)))
      ;; Collect candidates in table
      (maphash
       (lambda (cand v)
         (push cand candidates)
         (when (string-match
                (format "\\(%s\\{2,\\}\\)%s+"
                        prefix-regexp
                        (company-nihongo--make-negate-regexp prefix-regexp))
                cand)
           ;; cand contains characters of different type other than
           ;; that of prefix-regexp-matching. Extract
           ;; prefix-regexp-matching string part if its length is more
           ;; than 1 and put it into hash table.
           ;; If prefix is "Vi" and cand is "Vimプロフェッショナル",
           ;; for example, then we also want to collect "Vim"
           ;; as a candidate.
           (push (match-string-no-properties 1 cand) head-candidates)))
       table)
      (if candidates
          ;; Found candidates and we clear not-found-state.
          (company-nihongo--clear-not-found-state)
        ;; Found nothing and we set not-found-state
        (company-nihongo--set-not-found-state prefix (current-buffer)))
      (append candidates head-candidates))))

(defun company-nihongo--go-search-p (prefix buffer)
  "Return t if we should go searching for candidates in buffer
BUFFER."
  (cond
   ((null company-nihongo--search-in-current-buffer-state)
    t)
   (t
    (let ((not-found-buffer (nth 0 company-nihongo--search-in-current-buffer-state))
          (tick (nth 1 company-nihongo--search-in-current-buffer-state))
          (not-found-prefix (nth 2 company-nihongo--search-in-current-buffer-state)))
      (cond
       ((null not-found-buffer)
        t)
       ((not (eq buffer not-found-buffer))
        t)
       ((and (stringp not-found-prefix)
             (not (string-prefix-p not-found-prefix prefix)))
        ;; If prefix has completely changed, we need to search for candidates.
        t)
       ((and (eq buffer not-found-buffer) (eq (buffer-chars-modified-tick) tick))
        nil)
       ((and (stringp not-found-prefix) (string-prefix-p not-found-prefix prefix))
        ;; If buffer hasn't changed and not-found-prefix is prefix of
        ;; PREFIX, we don't have to do search.
        nil)
       (t
        t))))))

(defun company-nihongo--make-negate-regexp (regexp)
  "Return regexp that does not match regexp REGEXP."
  (cond
   ((string-match-p "^\\\\c" regexp)
    (replace-regexp-in-string "^\\\\c" "\\\\C" regexp))
   (t
    (format "[^%s]" (replace-regexp-in-string "\\[\\|\\]" "" regexp)))))

(defun company-nihongo--make-regexp (prefix)
  "Make regexp to be used in
`company-nihongo--get-candidates-in-current-buffer' and returns a cons
cell, whose car is the type of character that represents prefix, and
cdr is also a regexp used to search for candidates. The first group in
regexp in this cdr is colleted as a candidate."
  (let ((non-prefix "\\(?:%s\\|\\b\\|\\B\\)")
        ;; (non-prefix "\\(?:%s\\|\\b\\)")
        )
    (cond
     ((string-match-p (format "^%s+$" company-nihongo-ascii-regexp) prefix)
      ;; (posix-search-forward "\\(あいう[あ-ん]*\\(?:[a-z]*\\|\\cC*\\)\\)")
      ;; matches with...
      ;; あいうえお
      ;; あいう漢字
      ;; あいうascii
      ;; あいうえお漢字
      ;; "ascii" or "ascii + katakana" or "ascii + kanji".
      (cons company-nihongo-ascii-regexp
            (format "%s\\(%s%s*\\(?:\\cK*\\|\\cC*\\)\\)"
                    ;; non-prefix
                    (format non-prefix
                            (company-nihongo--make-negate-regexp
                             company-nihongo-ascii-regexp))
                    prefix
                    company-nihongo-ascii-regexp)))
     ((string-match-p "^\\cH+$" prefix)
      ;; prefix is Hiragana.
      ;; "hiragana" or "hiragan + kanji" or "hiragana + alphabet"
      (cons "\\cH"
            (format "%s\\(%s\\cH*\\(?:\\cC*\\|%s*\\)\\)"
                    (format non-prefix
                            (company-nihongo--make-negate-regexp "\\cH"))
                    prefix
                    company-nihongo-alpha-regexp)))
     ((string-match-p "^\\cK+$" prefix)
      ;; prefix is Katakana.
      ;; "katakana" or "katakana + hiragana" or "katakana + kanji"
      (cons "\\cK" (format "%s\\(%s\\cK*\\(?:\\cH*\\|\\cC*\\)\\)"
                           (format non-prefix
                                   (company-nihongo--make-negate-regexp "\\cK"))
                           prefix)))
     ((string-match-p "^\\cC+$" prefix)
      ;; prefix is Kanji.
      ;; "kanji" or "kanji + hiragana or "kanji + katakana"
      (cons "\\cC" (format "%s\\(%s\\cC*\\(?:\\cH*\\|\\cK*\\)\\)"
                           (format non-prefix
                                   (company-nihongo--make-negate-regexp "\\cC"))
                           prefix)))
     (t
      nil))))

(defun company-nihongo--search-candidates-in-buffer (regexp min-len beg end limit table)
  "Search for regexp in the region [BEG, END] until it reaches END or
the number of candidates found equals LIMIT."
  (let ((cand nil))
    (save-excursion
      (goto-char beg)
      (while (and (< (hash-table-count table) limit)
                  ;; Note: I don't know why but, we have to do (1+
                  ;; end) here to collect a candidate that just ends
                  ;; at (point-max)
                  (posix-search-forward regexp (1+ end) t))
        (setq cand (match-string-no-properties 1))
        (when (< min-len (length cand))
          (puthash cand t table))))))

(defun company-nihongo--get-possible-candidates (prefix buf)
  (gethash (substring-no-properties prefix
                                    0
                                    company-nihongo--hashtable-key-length)
           (company-nihongo--get-hashtable buf)))

(defun company-nihongo--get-candidates-in-buffer (prefix buf)
  (cl-loop with possible-candidates = (company-nihongo--get-possible-candidates
                                       prefix
                                       buf)
           for cand in possible-candidates
           ;; gethash above returns a sorted list of candidates.
           ;; So, if (string> prefix cand) returns t, this means that
           ;; we have past over possible candidates.
           ;; (b bb bba bbb bbc bbca bbcb ...)
           ;;    ^           ^
           ;;    |           |
           ;; prefix = "bb"  last="bbc", prefix > last
           with candidates = nil
           with already-found = nil
           with prefix-len = (length prefix)
           initially (when (string> prefix (car (last possible-candidates)))
                       (return nil))
           if (and (< prefix-len (length cand)) (string-prefix-p prefix cand))
           do (progn (or already-found (setq already-found t))
                     (push cand candidates))
           else if already-found
           return candidates
           finally return candidates))

(defun company-nihongo--hashtable-need-update-p (buffer)
  "Return t if hashtable for buffer BUFFER needs to be update, nil
otherwise."
  (let ((last-tick (gethash buffer company-nihongo--last-edit-tick-table nil))
        (last-pos (gethash buffer company-nihongo--last-edit-start-pos-table nil)))
    (cond
     ((null last-tick)
      ;; This becomes true if there IS a hashtable for this buffer AND
      ;; tick is NOT in last-edit-tick-table.
      ;; This means that we haven't edited this buffer, so we don't
      ;; need to update hashtable.
      nil)
     ((= last-tick
         (with-current-buffer buffer (buffer-chars-modified-tick)))
      nil)
     (t
      (integer-or-marker-p last-pos)))))

(defun company-nihongo--get-hashtable (buf &optional new)
  "Return a hashtable that holds words in buffer BUF.

If a hashtable has not been created for buffer BUF, or argument NEW is
non-nil, create a new one, and put words in buffer BUF into this
table, and store it in `company-nihongo--index-cache-alist'."
  (cond
   ((or (null (assoc buf company-nihongo--index-cache-alist))
        new)
    ;; Make a new hashtable for this buffer. Key is a string of length
    ;; company-nihongo--hashtable-key-length and its value is a list of
    ;; strings, sorted.
    ;; i.e. "あ" => '("あい" "あお" "あほ" "あんこ" ...)
    ;;      "p"  => '("pop" "prin1" "prin1-to-string" "push" ...)
    (company-nihongo--register-hashtable buf))
   ((company-nihongo--hashtable-need-update-p buf)
    (cond
     ((and (eq buf (current-buffer))
           (not (company-nihongo--in-searching-window-p buf (point))))
      ;; We are editing current buffer and are out of searching
      ;; window, so we perform partial update.
      (message "DEBUG: company-nihongo--get-hashtable, partial updating for %s" (buffer-name buf))
      (company-nihongo--update-hashtable-for-buffer-region buf)
      (company-nihongo--update-last-edit-tick buf))
     ((not (eq buf (current-buffer)))
      ;; We are collecting candidates from buffers other than current
      ;; buffer. If a buffer to be searched has been modified since
      ;; the last time the hash table for this buffer was created, we
      ;; clear this hash table and create a new one.
      (message "DEBUG: company-nihongo--get-hashtable, recreating table for %s" (buffer-name buf))
      (company-nihongo--register-hashtable buf)
      (company-nihongo--update-last-edit-tick buf)))
    (assoc-default buf company-nihongo--index-cache-alist))
   (t
    ;; In other cases, just return existing hash table.
    (assoc-default buf company-nihongo--index-cache-alist))))

;; (defun company-nihongo--get-hashtable (buf &optional new)
;;   "Return a hashtable that holds words in buffer BUF.

;; If a hashtable has not been created for buffer BUF, or argument NEW is
;; non-nil, create a new one, and put words in buffer BUF into this
;; table, and store it in `company-nihongo--index-cache-alist'."
;;   (when (or (null (assoc buf company-nihongo--index-cache-alist))
;;             new
;;             ;; (company-nihongo--hashtable-need-update-p buf)
;;             )
;;     ;; Make a new hashtable for this buffer. Key is a string of length
;;     ;; company-nihongo--hashtable-key-length and its value is a list of
;;     ;; strings, sorted.
;;     ;; i.e. "あ" => '("あい" "あお" "あほ" "あんこ" ...)
;;     ;;      "p"  => '("pop" "prin1" "prin1-to-string" "push" ...)
;;     (company-nihongo--register-hashtable buf))
;;   (assoc-default buf company-nihongo--index-cache-alist))

(defun company-nihongo--register-hashtable (buffer)
  (let ((table (make-hash-table :test #'equal)))
    (assq-delete-all buffer company-nihongo--index-cache-alist)
    (maphash (lambda (k v)
               ;; sort each list
               (puthash k (sort v #'string<) table))
             (company-nihongo--build-hashtable-for-buffer buffer))
    (push (cons buffer table) company-nihongo--index-cache-alist)
    (assoc-default buffer company-nihongo--index-cache-alist)))

;; (defun company-nihongo--register-hashtable (buffer)
;;   (cl-loop with table = (make-hash-table :test #'equal)
;;            with inserted-words = (make-hash-table :test #'equal)
;;            with res-table = (make-hash-table :test #'equal)
;;            for word in (company-nihongo--get-word-list buffer)
;;            for key = (substring word 0 1)
;;            when (and (> (length word) 1) (not (gethash word inserted-words)))
;;            if (gethash key table)
;;            do (progn (puthash word t inserted-words)
;;                      ;; this word has not been inserted yet
;;                      (push word (gethash key table)))
;;            else
;;            do (progn (puthash key (list word) table)
;;                      (puthash word t inserted-words))
;;            finally (progn (maphash (lambda (k v)
;;                                      ;; sort each list
;;                                      (puthash k (sort v #'string<) res-table))
;;                                    table)
;;                           (push (cons buffer res-table)
;;                                 company-nihongo--index-cache-alist))))

(cl-defun company-nihongo--get-word-list (buffer &key
                                                 (beg (point-min))
                                                 (end (point-max)))
  "Split buffer string by the type of character and return a list of
would-be candidates."
  (cl-loop with lst = (company-nihongo--split-buffer-string buffer :beg beg :end end)
           with ret = nil
           with sep-regexp = (format "^%s$" company-nihongo-separator-regexp)
           for curr in lst
           for next in (cdr lst)
           unless (string-match-p sep-regexp
                                  (regexp-quote curr))
           ;; If curr is not a separator
           do (progn (push curr ret)
                     (when (company-nihongo--is-connected-p curr next sep-regexp)
                       (push (concat curr next) ret)))
           finally return ret))

(defun company-nihongo--is-connected-p (curr next separator-regexp)
  (when (and (stringp curr)
             (stringp next)
             (not (string-match-p separator-regexp next)))
    (cond
     ((string-match-p (format "%s+" company-nihongo-ascii-regexp) curr)
      ;; "ascii" + X
      (or (string-match-p "\\cK+" next)
          (string-match-p "\\cC+" next)))
     ((string-match-p "\\cH+" curr)
      ;; "hiragana" + X
      (or (string-match-p "\\cC+" next)
          (string-match-p (format "%s+" company-nihongo-alpha-regexp) next)))
     ((string-match-p "\\cK+" curr)
      ;; "katakana" + X
      (or (string-match-p "\\cH+" next)
          (string-match-p "\\cC+" next)))
     ((string-match-p "\\cC+" curr)
      ;; "kanji" + X
      (or (string-match-p "\\cH+" next)
          (string-match-p "\\cK+" next))))))

(cl-defun company-nihongo--split-buffer-string (buffer &key
                                                       (beg (point-min))
                                                       (end (point-max)))
  "Return a list of strings in buffer BUFFER, split by its character
type."
  (let ((ret nil)
        (regexp (format "\\cH+\\|\\cK+\\|\\cC+\\|%s+\\|%s"
                        company-nihongo-ascii-regexp
                        company-nihongo-separator-regexp))
        (word nil)
        (end (save-excursion (goto-char end)
                             ;; avoid situations where we are in the
                             ;; middle of some word.
                             (forward-word-strictly)
                             (if (= (point) (point-max))
                                 nil
                               (1+ (point))))))
    (with-current-buffer buffer
      (save-excursion (goto-char beg)
                      ;; avoid situations where we are in the middle
                      ;; of some word, i.e.
                      ;; match-string-no-properties
                      ;;        ^
                      ;;        |
                      ;;      (point)
                      (backward-word-strictly)
                      (while (re-search-forward regexp end t)
                        (setq word (match-string-no-properties 0))
                        (push word ret)
                        (cond
                         ((string-match-p
                               (format "^%s+$" company-nihongo-ascii-regexp)
                               word)
                          ;; If word is like "abc-def", then we push
                          ;; abc and def into ret as well.
                          (mapc (lambda (elt) (push elt ret))
                                (split-string word "[_-]" t)))
                         ((string-match-p "・" word)
                          ;; If word is "クーリング・オフ", for
                          ;; example, this whole string matches
                          ;; "\\cK+".
                          ;; In this case, we put both "クーリング"
                          ;; and "オフ" into ret as well as "クーリン
                          ;; グ・オフ".
                          (mapc (lambda (elt) (push elt ret))
                                (split-string word "[・]" t)))))))
    (nreverse ret)))

(defun company-nihongo--check-index-cache-alist-idle-timer-func ()
  "Iterate through `company-nihongo--index-cache-alist' to see if
there is an entry for a killed bufer, and delete it if any."
  (cl-loop for killed-buffer in
           (cl-loop for (buffer . table) in
                    company-nihongo--index-cache-alist
                    unless (buffer-live-p buffer)
                    collect buffer)
           do (assq-delete-all killed-buffer
                               company-nihongo--index-cache-alist)))

(defun company-nihongo--check-index-cache-alist ()
  (run-with-idle-timer 10
                       nil
                       #'company-nihongo--check-index-cache-alist-idle-timer-func))

(defcustom company-nihongo-check-index-cache-interval 3600
  "Every this value seconds, `company-nihongo--index-cache-alist' is
checked if there is an entry for killed buffer.")

(defvar company-nihongo--check-index-cache-alist-timer
  (run-with-timer company-nihongo-check-index-cache-interval
                  company-nihongo-check-index-cache-interval
                  #'company-nihongo--check-index-cache-alist))

;; (setq company-nihongo--check-index-cache-alist-timer
;;       (run-with-timer company-nihongo-check-index-cache-interval
;;                       company-nihongo-check-index-cache-interval
;;                       #'company-nihongo--check-index-cache-alist))

;; (cancel-timer company-nihongo--check-index-cache-alist-timer)

(defun company-nihongo--clear-tables-for-buffer (buffer)
  (remhash buffer company-nihongo--last-edit-tick-table)
  (remhash buffer company-nihongo--last-edit-start-pos-table)
  (assq-delete-all buffer company-nihongo--index-cache-alist))

(defun company-nihongo (command &optional arg &rest _ignores)
  (interactive (list 'interactive))
  (cl-case command
      (interactive
       (company-begin-backend 'company-nihongo-backend))
      (prefix
       (company-nihongo--get-prefix))
      (candidates
       (company-nihongo--get-candidates arg))
      (sorted t)))

(provide 'company-nihongo)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;; company-nihongo.el ends here.
