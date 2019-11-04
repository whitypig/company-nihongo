;;; company-nihongo.el --- Backend for company-mode.

;; Copyright (C) 2018 whitypig

;; Author: whitypig <whitypig@gmail.com>
;; URL:
;; Version: 0.01
;; Package-Requires: ((company-mode) (cl-lib) (s) (helm))
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
(require 'helm)

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

(defcustom company-nihongo-ascii-regexp "[0-9A-Za-z_:-]"
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

(defcustom company-nihongo-use-3-words-completion t
  "By default, candidates returned by `company-nihongo' contains at
most two types of strings. With this variable being t, however, some
candidates could contain three types of strings depending on
context."
  :type 'boolean
  :group 'company-nihongo)

(defcustom company-nihongo-searching-window-size 1000
  "The size of window in which candidates in current buffer are
searched. In other words, searching for candidates in current buffer
is perfomed on [point - window-size, point + window-size]."
  :type 'integer
  :group 'company-nihongo)

(defcustom company-nihongo-check-index-cache-interval 3600
  "Every this value seconds, `company-nihongo--index-cache-alist' is
checked if there is an entry for killed buffer.")

(defcustom company-nihongo-use-consective-completion t
  "When this variable is t, after you choose a candidte, another
completion automatically fires."
  :type 'boolean
  :group 'company-nihongo)

;;; Variables

(defvar company-nihongo--index-cache-alist nil
  "Alist in the form of (buffer . hashtable).")

(defvar company-nihongo--hashtable-key-length 1
  "The length of key to hash table.")

(defvar company-nihongo--alpha-regexp "[A-Za-z]"
  "")

(defvar company-nihongo--ascii-non-alpha "[:_-]"
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

(defvar company-nihongo--group-name-to-buffers-table (make-hash-table :test #'equal)
  "Hashtable that indicates which group holds which buffers.
The key is a group name and the value is a list of buffers.")

(defvar company-nihongo--buffer-to-group-table (make-hash-table :test #'equal)
  "Hashtable that indicates which buffer belongs to which group.
The key is a buffer and the value is a list of group names to which
that buffer belongs.")

(defvar company-nihongo--black-dot "・"
  "Black dot, which is called \"Kokuten\" in Japanese.")

(defvar company-nihongo--two-black-dots "・・"
  "Two consective black dots.")

(defvar company-nihongo--friend-buffers-table (make-hash-table :test #'eq)
  "")

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

(defun company-nihongo-select-target-mode-buffers-plus-friends ()
  "Return friend buffers and ones which are the same major-mode as
that of current buffer."
  (append (company-nihongo--get-friend-buffers (current-buffer))
          (company-nihongo-select-target-mode-buffers)))

(defun company-nihongo-select-friend-buffers ()
  "Return a list of buffers which are friend with current buffer."
  (company-nihongo--get-friend-buffers (current-buffer)))

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
      ;; company-nihongo--get-regexp returns "\\cH" as regexp. However,
      ;; (looking-at "\\cH") returns nil since (string-match-p "\\cH"
      ;; "〜") returns nil, so company-nihongo-prefix doen't move point. As a
      ;; result, prefix becomes "", i.e. empty string.
      (save-excursion
        (backward-char)
        (while (and (not (bobp))
                    (looking-at-p regexp))
          (backward-char))
        (unless (looking-at-p regexp)
          (forward-char))
        (cond
         ((and (string= regexp company-nihongo-ascii-regexp)
               (looking-at-p company-nihongo--ascii-non-alpha))
          (while (and (not (bobp))
                      (looking-at-p company-nihongo--ascii-non-alpha))
            (forward-char)))
         ((and (string= regexp "\\cK")
               (looking-at-p company-nihongo--black-dot))
          ;; remove leading "・" when regexp is for katakana words.
          (while (and (not (bobp))
                      (looking-at-p company-nihongo--black-dot))
            (forward-char))))
        (buffer-substring-no-properties (point) pos)))))

(defun company-nihongo--set-not-found-state (prefix buffer)
  (setq company-nihongo--search-in-current-buffer-state
        (list buffer (buffer-chars-modified-tick) prefix)))

(defun company-nihongo--clear-not-found-state ()
  (setq company-nihongo--search-in-current-buffer-state nil))

(defun company-nihongo--get-source-buffers (buffer others)
  (cond
   ((null others)
    ;; OTHERS being nil has the highest priority.
    (list buffer))
   (t
    ;; If BUFFER belongs to any groups, collect all the buffers in
    ;; those groups. Otherwise, just call
    ;; 'company-nihongo-select-buffer-function.
    (or (company-nihongo--get-member-buffers buffer)
        (funcall company-nihongo-select-buffer-function)))))

(cl-defun company-nihongo--get-candidates (prefix &optional (others t))
  "Return a list of candidates that begin with prefix PREFIX."
  (when (and prefix (> (length prefix) 0))
    (delete-dups
     (sort (cl-loop for buf in (company-nihongo--get-source-buffers (current-buffer)
                                                                    others)
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
           for key = (substring word 0 (min
                                        (length word)
                                        company-nihongo--hashtable-key-length))
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
    (when (and prefix-regexp cand-regexp)
      ;; Sometimes input character cannot be prefix and in those
      ;; cases, prefix-regexp becomes nil and we complete nothing for
      ;; those cases.
      (company-nihongo--prepare-for-current-buffer-completion (current-buffer) (point))
      (when (company-nihongo--go-search-p prefix (current-buffer))
        ;; (message "DEBUG: in-current-buffer, prefix=%s, company-nihongo--not-found-prefix=%s" prefix company-nihongo--not-found-prefix)
        (company-nihongo--search-candidates-in-buffer prefix
                                                      cand-regexp
                                                      prefix-len
                                                      top
                                                      (1- pos)
                                                      limit
                                                      table)
        (company-nihongo--search-candidates-in-buffer prefix
                                                      cand-regexp
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
        (append candidates head-candidates)))))

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
   ((and (not (string-match-p "\\[\\^.*\\]" regexp))
         ;; not like ones such as "[^a]+"
         (string-match "\\[\\([^]]+\\)\\]\\(.*\\)" regexp))
    (format "[^%s]%s"
            (match-string-no-properties 1 regexp)
            (match-string-no-properties 2 regexp)))
   (t
    ;; strip square brackets and prepend "^", then enclose square
    ;; brackets.
    (format "[^%s]" (replace-regexp-in-string "\\[\\|\\]" "" regexp)))))

(defun company-nihongo--make-regexp (prefix)
  "Make regexp to be used in
`company-nihongo--get-candidates-in-current-buffer' and returns a cons
cell, whose car is the type of character that represents prefix, and
cdr is also a regexp used to search for candidates. The first group in
regexp in this cdr is colleted as a candidate."
  (let (
        ;; (non-prefix "\\(?:%s\\|\\b\\|\\B\\)")
        (non-prefix "\\(?:%s\\|\\b\\)")
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
                    company-nihongo--alpha-regexp)))
     ((and (string-match-p "^\\cK+$" prefix)
           (not (string-match-p "^[・]" prefix)))
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

(defun company-nihongo--search-candidates-in-buffer (prefix regexp min-len
                                                            beg end limit table)
  "Search for strings that matches REGEXP in the region [BEG, END] in
current buffer until it reaches END or the number of candidates found
equals LIMIT."
  (let ((cand nil)
        (sep nil))
    ;; (message "DEBUG: prefix=%s regexp=%s min-len=%d beg=%d end=%d"
    ;;          prefix regexp min-len beg end)
    (save-excursion
      (goto-char beg)
      (while (and (< (hash-table-count table) limit)
                  ;; Note: I don't know why but, we have to do (1+
                  ;; end) here to collect a candidate that just ends
                  ;; at (point-max)
                  (posix-search-forward regexp (1+ end) t))
        ;; We have decided to consider words like "アイ・ウエ・・・オ"
        ;; as a proper word, so we push those words into table. Also,
        ;; we push substrings of those words into table.
        (setq cand (match-string-no-properties 1))
        (when (> (length cand) min-len)
          (puthash cand t table)
          (cl-loop with sep = (cond ((string-match-p "[・]" cand)
                                     (setq sep "[・]+"))
                                    ((string-match-p company-nihongo--ascii-non-alpha cand)
                                     (setq sep (format "%s+"
                                                       company-nihongo--ascii-non-alpha)))
                                    (t
                                     nil))
                   for s in (and sep (company-nihongo--get-substrings-by-separators cand sep))
                   for len = (length s)
                   when (and (string-prefix-p prefix s)
                             (> len min-len))
                   do (puthash s t table)))))))

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
           ;; (b bb bba bbb bbc bbca bbcb bcaa ...   bye)
           ;;    ^           ^             ^          ^
           ;;    |           |             |          |
           ;; prefix="bb"  last="bbc"   cand="bcaa"  last <  prefix="bz"
           ;; If prefix > last, PREFIX cannot become a prefix of any
           ;; of words in possible-candidates.
           with candidates = nil
           with already-found = nil
           with prefix-len = (length prefix)
           initially (when (string> prefix (car (last possible-candidates)))
                       (return nil))
           if (and (< prefix-len (length cand)) (string-prefix-p prefix cand))
           do (progn (or already-found (setq already-found t))
                     (push cand candidates))
           else if already-found
           ;; We have found some candidates so far, but the remaining
           ;; elements in possible-candidates do not begin with
           ;; PREFIX. Therefore we stop searching.
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
      ;; (message "DEBUG: company-nihongo--get-hashtable, partial updating for %s" (buffer-name buf))
      (company-nihongo--update-hashtable-for-buffer-region buf)
      (company-nihongo--update-last-edit-tick buf))
     ((not (eq buf (current-buffer)))
      ;; We are collecting candidates from buffers other than current
      ;; buffer. If a buffer to be searched has been modified since
      ;; the last time the hash table for this buffer was created, we
      ;; clear this hash table and create a new one.
      ;; (message "DEBUG: company-nihongo--get-hashtable, recreating table for %s" (buffer-name buf))
      (company-nihongo--register-hashtable buf)
      (company-nihongo--update-last-edit-tick buf)))
    (assoc-default buf company-nihongo--index-cache-alist))
   (t
    ;; In other cases, just return existing hash table.
    (assoc-default buf company-nihongo--index-cache-alist))))

(defun company-nihongo--register-hashtable (buffer)
  (let ((table (make-hash-table :test #'equal)))
    (assq-delete-all buffer company-nihongo--index-cache-alist)
    (maphash (lambda (k v)
               ;; sort each list
               (puthash k (sort v #'string<) table))
             (company-nihongo--build-hashtable-for-buffer buffer))
    (push (cons buffer table) company-nihongo--index-cache-alist)
    (assoc-default buffer company-nihongo--index-cache-alist)))

(cl-defun company-nihongo--get-word-list (buffer &key
                                                 (beg (point-min))
                                                 (end (point-max)))
  "Split buffer string by the type of character and return a list of
would-be candidates."
  (cl-loop with lst = (company-nihongo--split-buffer-string buffer :beg beg :end end)
           with ret = nil
           with sep-regexp = (format "^%s$" company-nihongo-separator-regexp)
           with acc = nil
           with dot-cnt = 0
           with word-cnt = 0
           with w = nil
           for curr in lst
           for next1 in (append (cdr lst) '(nil))
           for next2 in (append (cddr lst) '(nil nil))
           do (progn
                ;; take special care for katakana words
                (cond
                 ((string= curr company-nihongo--black-dot)
                  (setq acc nil
                        dot-cnt 0
                        word-cnt 0))
                 ((string= curr company-nihongo--two-black-dots)
                  (setq acc nil
                        dot-cnt 0
                        word-cnt 0))
                 ((string-match-p "^\\cK+" curr)
                  ;; cases for "イエス", "・アイテム", "アイ・ウエ・オ
                  ;; カ・キク", and "ケコ・・・サシス".
                  ;; remove leading and trailing black-dots if any.
                  (setq curr (replace-regexp-in-string
                              (format "\\(^[%s]+\\|[%s]+$\\)"
                                      company-nihongo--black-dot
                                      company-nihongo--black-dot)
                              ""
                              curr))
                  ;; Here, curr contains at most one black dot in the
                  ;; middle of itself.
                  (mapc (lambda (s)
                          (push s ret))
                        (company-nihongo--get-substrings-by-separators
                         curr (format "[%s]+" company-nihongo--black-dot))))
                 ((string-match-p (format "%s+" company-nihongo-ascii-regexp) curr)
                  (mapc (lambda (s)
                          (push s ret))
                        (company-nihongo--get-substrings-by-separators
                         curr (format "%s+" company-nihongo--ascii-non-alpha))))
                 (t
                  ;; other kinds of words
                  (setq acc nil
                        dot-cnt 0
                        word-cnt 0)
                  ;; normal processing
                  (unless (string-match-p sep-regexp curr)
                    (push curr ret))))
                (when (company-nihongo--is-connected-p curr next1 sep-regexp)
                  (push (concat curr next1) ret))
                (when (and company-nihongo-use-3-words-completion
                           (company-nihongo--is-3-consective-p curr
                                                               next1
                                                               next2
                                                               sep-regexp))
                  (push (concat curr next1 next2) ret)))
           finally return ret))

(defun company-nihongo--has-leading-black-dot-p (string)
  (string-match-p (format "^%s" company-nihongo--black-dot) string))

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
          (and (not (company-nihongo--has-leading-black-dot-p next))
               (string-match-p "\\cK+" next))
          (string-match-p (format "%s+" company-nihongo--alpha-regexp) next)))
     ((and (not (company-nihongo--has-leading-black-dot-p curr))
           (not (string-match-p company-nihongo--two-black-dots curr))
           (string-match-p "\\cK+" curr))
      ;; "katakana" + X
      (or (string-match-p "\\cH+" next)
          (string-match-p "\\cC+" next)))
     ((string-match-p "\\cC+" curr)
      ;; "kanji" + X
      (or (string-match-p "\\cH+" next)
          (and (not (company-nihongo--has-leading-black-dot-p next))
               (string-match-p "\\cK+" next)))))))

(defun company-nihongo--is-3-consective-p (cur next1 next2 sep-regexp)
  "Return t if a concatenated word CUR + NEXT1 + NEXT2 can be a
candidate."
  (when (cl-every (lambda (s)
                    (and (stringp s)
                         (not (string-match-p sep-regexp s))))
                  (list cur next1 next2))
    (cond
     ((and
       ;; カタカナ+ひらがな+英数字
       (string-match-p "^\\cK+$" cur)
       (string-match-p "^\\cH+$" next1)
       (string-match-p (format "^%s+$" company-nihongo--alpha-regexp) next2))
      t)
     (t
      nil))))

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
                        (cond
                         ((string-match-p company-nihongo--two-black-dots word)
                          (mapc (lambda (elt)
                                  (push elt ret))
                                (company-nihongo--split-string
                                 word
                                 (format "[%s]\\{2,\\}"
                                         company-nihongo--black-dot))))
                         (t
                          (push word ret))))))
    (nreverse ret)))

(defun company-nihongo--get-substrings-by-separators (string separator)
  (cl-loop with lst = (company-nihongo--split-string string separator)
           for i from 0 below (length lst)
           for l = (nthcdr i lst)
           unless (string-match-p separator (car l))
           append (cl-loop for elt in l
                           for s = elt then (concat s elt)
                           collect s)))

(defun company-nihongo--split-string (string separator)
  "Split STRING by SEPARATOR, which can be a regexp, and return a list
of split strings including the separators in STRING."
  (if (not (string-match-p separator string))
      (list string)
    (mapcar #'cdr
            (cl-merge
             'list
             ;; locate separators
             (company-nihongo--split-string-2 string separator)
             ;; locate non-separators
             (cl-loop with ret = nil
                      with lst = (split-string string separator t)
                      for elt in lst
                      for start = 0 then (string-match-p (regexp-quote elt)
                                                         string
                                                         (+ start
                                                            (length (cdar (last ret)))))
                      when start
                      collect (cons start elt) into ret
                      finally return ret)
             #'<
             :key #'car))))

(defun company-nihongo--split-string-2 (string separator)
  (cl-loop with ret = nil
           with start = 0
           while (and
                  (integerp start)
                  (setq start (string-match separator
                                            string
                                            (+ start (length (cdar (last ret)))))))
           collect (cons start (match-string-no-properties 0 string))
           into ret
           finally return ret))

(defun company-nihongo--split-kanakana-word (word)
  "Split katakana word WORD by \"・\" and return a list of split
words."
  (cl-loop with ret = nil
           with cnt = 0
           with acc = nil
           ;; Replace three or more consective "・" with two "・", and
           ;; process character by character.
           for ch in (split-string (replace-regexp-in-string "[・]\\{3,\\}"
                                                             company-nihongo--two-black-dots
                                                             word)
                                   ""
                                   t)
           do (cond
               ((string= ch company-nihongo--black-dot)
                (when acc
                  (push (mapconcat #'identity (nreverse acc) "") ret)
                  (setq acc nil))
                (cl-incf cnt))
               (t
                (when (> cnt 0)
                  (push (make-string cnt ?・) ret)
                  (setq cnt 0))
                (push ch acc)))
           finally return
           (progn (when acc
                    (push (mapconcat #'identity (nreverse acc) "") ret))
                  (when (> cnt 0)
                    (push (make-string cnt ?・) ret))
                  (nreverse ret))))

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

(defvar company-nihongo--check-index-cache-alist-timer
  (run-with-timer company-nihongo-check-index-cache-interval
                  company-nihongo-check-index-cache-interval
                  #'company-nihongo--check-index-cache-alist))

;; (setq company-nihongo--check-index-cache-alist-timer
;;       (run-with-timer company-nihongo-check-index-cache-interval
;;                       company-nihongo-check-index-cache-interval
;;                       #'company-nihongo--check-index-cache-alist))

;; (cancel-timer company-nihongo--check-index-cache-alist-timer)


;;; Group management

(defun company-nihongo--get-existing-group-names ()
  (cl-loop for name being the hash-key of company-nihongo--group-name-to-buffers-table
           collect name))

(defun company-nihongo--read-group-name-from-minibuffer ()
  (let ((names (company-nihongo--get-existing-group-names))
        (name nil))
    (while (or (not (stringp name))
               (<= (length name) 1)
               (member name names))
      (setq name (read-from-minibuffer "Group name: "))
      (cond
       ((<= (length name) 1)
        (message "The length of a group name must be at least 2")
        (sit-for 1.5))
       ((member name names)
        (message "That group exists")
        (sit-for 1.5))))
    name))

(defun company-nihongo--substring-by-width (s width)
  (cl-loop for ret = s then (substring ret 0 (1- (length ret)))
           when (<= (string-width ret) width)
           return ret))

(defun company-nihongo--helm-format-buffer-title (name dirname)
  (let* ((max-name-width 50)
         (fmtstring (format "%%-%ds    (%%s)" max-name-width)))
    (format fmtstring
            (if (<= (string-width name) max-name-width)
                name
              (concat (company-nihongo--substring-by-width name (- max-name-width 3))
                      "..."))
            dirname)))

(defun company-nihongo--helm-get-buffer-candidates (&optional lst)
  (cl-loop for elt in (cl-loop for b in (or lst (buffer-list))
                               ;; collect (buffer buffer-file-name)
                               collect (list b
                                             (buffer-file-name b))
                               into lst
                               finally return
                               (sort lst
                                     (lambda (a b)
                                       ;; sort by location
                                       (string< (nth 1 a) (nth 1 b)))))
           collect (cons
                    ;; display is buffer name and its location.
                    (company-nihongo--helm-format-buffer-title (buffer-name (nth 0 elt))
                                                               (nth 1 elt))
                    ;; value is buffer object.
                    (nth 0 elt))))

(defun company-nihongo--helm-select-buffers (&optional lst)
  (helm :buffer "Buffers"
        :sources (helm-build-sync-source "Mark buffers and hit enter."
                   :candidates (company-nihongo--helm-get-buffer-candidates lst)
                   :action (helm-make-actions "Default" (lambda (_)
                                                          (helm-marked-candidates)))
                   :migemo t
                   :volatile t)
        :preselect (company-nihongo--helm-format-buffer-title
                    (buffer-name (current-buffer))
                    (buffer-file-name (current-buffer)))))

(defun company-nihongo-group--join-group (buffer name)
  "Add group named NAME to BUFFER's group list."
  (let ((lst (gethash buffer company-nihongo--buffer-to-group-table))
        (buffers (gethash name
                          company-nihongo--group-name-to-buffers-table)))
    (unless (member buffer buffers)
      (puthash name (cons buffer buffers) company-nihongo--group-name-to-buffers-table))
    (unless (member name lst)
      (puthash buffer (cons name lst) company-nihongo--buffer-to-group-table))))

(defun company-nihongo-group-create-new-group (name this-buffer buffers)
  "Create a new buffer group and associate THIS-BUFFER with BUFFERS in
this group. Be careful that all buffers but THIS-BUFFER are NOT
associated with the others.

First, enter a new group's name. Group names must be unique. Also,
empty string is not allowed. Then mark one or more buffers and hit enter.

In helm buffer, buffers are sorted by its path name and the cursor is
on the buffer from which this command is invoked, i.e. current
buffer."
  (interactive (list (company-nihongo--read-group-name-from-minibuffer)
                     (current-buffer)
                     (company-nihongo--helm-select-buffers)))
  ;; Make sure that there is no groups named NAME.
  (when (and (not (member name (company-nihongo--get-existing-group-names)))
             buffers)
    (puthash name (cl-remove-duplicates (cons this-buffer buffers))
             company-nihongo--group-name-to-buffers-table)
    (puthash this-buffer (cons name
                               (gethash this-buffer
                                        company-nihongo--buffer-to-group-table))
             company-nihongo--buffer-to-group-table)
    (message "Created group %s with %s"
             name
             (mapconcat (lambda (b) (buffer-name b))
                        (cl-remove-duplicates (cons this-buffer buffers))
                        ", "))))

(defun company-nihongo-group-create-new-group-and-associate-all (name this-buffer buffers)
  (interactive (list (company-nihongo--read-group-name-from-minibuffer)
                     (current-buffer)
                     (company-nihongo--helm-select-buffers)))
  (when (not (member name (company-nihongo--get-existing-group-names)))
    ;; associate each of buffers with the others in the same group.
    (mapc (lambda (buffer)
            (company-nihongo-group--join-group buffer name))
          (cons this-buffer buffers))
    (message "Associated %s in group %s"
             (mapconcat #'buffer-name (cons this-buffer buffers) ", ") name)))

(defun company-nihongo--helm-format-group-name (name buffers)
  (format "%-20s    %s"
          (company-nihongo--substring-by-width name 20)
          (mapconcat #'buffer-name buffers ", ")))

(defun company-nihongo--helm-get-group-candidates ()
  "Return a list of candidtes used with helm."
  (cl-loop for name being the hash-keys of company-nihongo--group-name-to-buffers-table
           for buffers = (gethash name company-nihongo--group-name-to-buffers-table)
           collect
           ;; display is a group name and the names of member buffers.
           ;; value is the group name.
           (cons (company-nihongo--helm-format-group-name name buffers)
                 name)))

(defun company-nihongo--helm-select-group (caption)
  (helm :buffer "company-nihongo groups"
        :sources (helm-build-sync-source caption
                   :candidates (company-nihongo--helm-get-group-candidates)
                   :migemo t
                   :volatile t)))

(defun company-nihongo-group-add-buffers-to-group ()
  "Add buffers into a company-nihongo buffer group."
  (interactive)
  (cl-loop with grp-name = (company-nihongo--helm-select-group "To which group?")
           with lst = (gethash grp-name company-nihongo--group-name-to-buffers-table)
           for buffer in (and grp-name
                              (company-nihongo--helm-select-buffers))
           do (company-nihongo-group--join-group buffer grp-name)
           finally (message "Now, group %s is (%s)"
                            grp-name
                            (mapconcat
                             #'buffer-name
                             (gethash grp-name
                                      company-nihongo--group-name-to-buffers-table)
                             ", "))))

(defun company-nihongo-group--leave-group (buffer group-name)
  (puthash buffer (remove group-name
                          (gethash buffer
                                   company-nihongo--buffer-to-group-table))
           company-nihongo--buffer-to-group-table)
  (puthash group-name
           (remove buffer
                   (gethash group-name
                            company-nihongo--group-name-to-buffers-table))
           company-nihongo--group-name-to-buffers-table))

(defun company-nihongo-group-delete-buffers-in-group (group-name &optional del-buffers)
  "Delete BUFFERS from group named GROUP-NAME."
  (interactive (list (company-nihongo--helm-select-group "From which group")))
  (cl-loop with del-buffers = (or del-buffers
                                  (company-nihongo--helm-select-buffers
                                   (gethash group-name
                                            company-nihongo--group-name-to-buffers-table)))
           with buffers = (gethash group-name company-nihongo--group-name-to-buffers-table)
           with new-buffers = (and buffers
                                   (cl-set-difference buffers del-buffers))
           with cnt = 0
           initially (and del-buffers
                          (puthash group-name
                                   new-buffers
                                   company-nihongo--group-name-to-buffers-table))
           for b in del-buffers
           do (progn
                (company-nihongo-group--leave-group b group-name)
                (cl-incf cnt))
           finally (and (> cnt 0)
                        (message "Deleted %s from %s"
                                 (mapconcat #'buffer-name del-buffers ", ")
                                 group-name))))

(defun company-nihongo-group-delete-group (names)
  "Interactively delete company-nihongo groups."
  (interactive (list
                (helm :buffer "Delete groups"
                      :sources
                      (helm-build-sync-source "Chosen groups will be deleted"
                        :migemo t
                        :volatile t
                        :candidates (company-nihongo--helm-get-group-candidates)
                        :action (helm-make-actions
                                 "Default" (lambda ($_) (helm-marked-candidates)))))))
  (cl-loop with deleted-groups = nil
           for grp-name in names
           for buffers = (gethash grp-name company-nihongo--group-name-to-buffers-table)
           do (progn
                ;; delete name from group table
                (remhash grp-name company-nihongo--group-name-to-buffers-table)
                (push grp-name deleted-groups)
                (mapc (lambda (b)
                        (company-nihongo-group--leave-group b grp-name))
                      buffers)
                (ignore-errors
                  (remhash grp-name
                           company-nihongo--group-name-to-buffers-table)))
           finally (and deleted-groups
                        (message "Deleted %d group%s (%s)"
                                 (length deleted-groups)
                                 (if (= 1 (length deleted-groups)) "" "s")
                                 (mapconcat #'identity deleted-groups ", ")))))

(defun company-nihongo--get-groups-by-buffer (buffer)
  (gethash buffer company-nihongo--buffer-to-group-table))

(defun company-nihongo--get-member-buffers (buffer)
  "If BUFFER belongs to any group, return a list of buffers that are
in the same group as that of BUFFER, including BUFFER
itself. Otherwise return nil."
  (cl-loop with ret = nil
           for grp-name in (company-nihongo--get-groups-by-buffer buffer)
           append (gethash grp-name company-nihongo--group-name-to-buffers-table)
           into ret
           ;; Make BUFFER come first in ret if ret is no-nil.
           ;; Otherwise, return nil.
           finally return (and ret
                               (cons buffer
                                     (remove buffer
                                             (cl-remove-duplicates ret))))))

(defun company-nihongo--clear-group-tables ()
  (clrhash company-nihongo--group-name-to-buffers-table)
  (clrhash company-nihongo--buffer-to-group-table))

;;; Friend buffers management

;; Assume that bufferA has friend buffers (bufferB bufferC bufferD).
;; Then, candidates for completion while editing bufferA will also be
;; collected from bufferB, bufferC, and bufferD.
;;
;; Friend buffers can be helpful if you need candidates from buffers
;; other than those returned by calling
;; 'company-nihongo-select-buffer-function.
;;
;; Note that this relation is unilateral, NOT bilateral. This means
;; that, in the above example, bufferB, bufferC, and bufferD are
;; source buffers for completion in bufferA, but does not necessarily
;; mean that bufferA is a source buffer for bufferB, buferC, or
;; bufferD.

(defun company-nihongo--get-friend-buffers (buffer)
  (gethash buffer company-nihongo--friend-buffers-table nil))

(defun company-nihongo-make-friend-buffers (buffers)
  "Choose buffers through `helm' and become friends with them.

Chosen buffers are added to the list of friend buffers of the buffer
from which this command has been invoked."
  (interactive (list (company-nihongo--helm-select-buffers)))
  ;; choose buffers.
  (cl-loop with key-buffer = (current-buffer)
           with lst = nil
           for buffer in buffers
           do (push buffer
                    (gethash key-buffer
                             company-nihongo--friend-buffers-table))
           ;; remove duplicates if any.
           finally (progn
                     (puthash key-buffer
                              (cl-remove-duplicates
                               (gethash key-buffer
                                        company-nihongo--friend-buffers-table))
                              company-nihongo--friend-buffers-table)
                     (message "Current friends: %s"
                              (mapconcat #'buffer-name
                                         (company-nihongo--get-friend-buffers
                                          (current-buffer))
                                         ", ")))))

(defun company-nihongo-delete-friend-buffers (buffers)
  "Break up with BUFFERS."
  (interactive (list
                (and (company-nihongo--get-friend-buffers (current-buffer))
                     (company-nihongo--helm-select-buffers
                      (company-nihongo--get-friend-buffers (current-buffer))))))
  (if (null buffers)
      (message "There is no friend buffer to break up with")
    (let ((new-friends (cl-set-difference
                        (company-nihongo--get-friend-buffers (current-buffer))
                        buffers)))
      (puthash (current-buffer)
               new-friends
               company-nihongo--friend-buffers-table)
      (message "%s"
               (if (null (company-nihongo--get-friend-buffers (current-buffer)))
                   "Now, we have become alone"
                 (format "Current friends: %s"
                         (mapconcat #'buffer-name
                                    (company-nihongo--get-friend-buffers
                                     (current-buffer))
                                    ", ")))))))

(defun company-nihongo--reset-friend-buffers-table ()
  (interactive)
  (clrhash company-nihongo--friend-buffers-table))

(defun company-nihongo--clear-tables-for-buffer (buffer)
  (remhash buffer company-nihongo--last-edit-tick-table)
  (remhash buffer company-nihongo--last-edit-start-pos-table)
  (assq-delete-all buffer company-nihongo--index-cache-alist))

(defun company-nihongo--try-consecutive-completion (candidate)
  ;; If some conditions are met, call #'company-manual-begin
  ;; interactively.
  (when (and company-nihongo-use-consective-completion
             (stringp candidate)
             (not (string-match-p (format "^%s+$"
                                          company-nihongo-ascii-regexp)
                                  candidate)))
    (ignore-errors (company-begin-backend 'company-nihongo))))

(defun company-nihongo (command &optional arg &rest _ignores)
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (company-begin-backend 'company-nihongo-backend))
    (prefix
     (company-nihongo--get-prefix))
    (candidates
     (company-nihongo--get-candidates arg))
    (sorted t)
    (post-completion
     (company-nihongo--try-consecutive-completion arg))))

(defun company-nihongo-current-buffer (command &optional arg &rest _ignores)
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (company-begin-backend 'company-nihongo-current-buffer-backend))
    (prefix
     (company-nihongo--get-prefix))
    (candidates
     (company-nihongo--get-candidates arg nil))
    (sorted t)))

(defun company-nihongo-current-buffer-activate ()
  (interactive)
  (cond
   (company-mode
    (add-to-list company-backends 'company-nihongo))
   (t
    ;; activate company-mode
    (company-mode-on)
    (setq company-backends '(company-nihongo-current-buffer)))))

;; Do we need this command?
(defun company-nihongo-current-buffer-deactivate ()
  (interactive)
  (setq company-backends
        (cl-labels ((rec (item seq)
                         (cond
                          ((null seq)
                           nil)
                          ((listp seq)
                           (cond
                            ((and (atom (car seq)) (eq item (car seq)))
                             (rec item (cdr seq)))
                            (t
                             (cons (rec item (car seq)) (rec item (cdr seq))))))
                          ((eq item seq)
                           nil)
                          (t
                           seq))))
          ;; The depth of seq is at most 2 and returned value could be
          ;; somethig like '(nil (backendA backendB)).
          ;; Tha'ts why we delete nil here.
          (delete nil
                  (rec 'company-nihongo-current-buffer company-backends))))
  (message "Now company-backends is %s" company-backends))

(provide 'company-nihongo)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;; company-nihongo.el ends here.
