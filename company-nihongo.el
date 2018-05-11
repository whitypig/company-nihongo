;;; company-nihongo.el --- Backend for company-mode.

;; Copyright (C) 2018 whitypig

;; Author: whitypig <whitypig@gmail.com>
;; URL:
;; Version: 0.01
;; Package-Requires: ((company-mode) (cl-lib))
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

;;; Customization

(defgroup company-nihongo nil
  "Company-mode backend for completing nihongo."
  :group 'company-nihongo)

(defcustom company-nihongo-limit 50
  "The upper number of candidates to show when completing."
  :type 'number
  :group 'company-nihongo)

(defcustom company-nihongo-separator-regexp "[[:ascii:]、。・「」：？…（）\n\t]"
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

(defcustom company-nihongo-complete-katakana-by-hiragana t
  "Specifies whether to complete katakana words by hiragana prefix."
  :type 'boolean
  :group 'company-nihongo)


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


;;; Functions

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
      (company-nihongo--get-regexp-1 ch)))))

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
           #'string<))))

(defun company-nihongo--get-candidates-1 (prefix buf)
  "Return a list of candidates that begin with PREFIX in buffer BUF."
  (cond
   ((eq buf (current-buffer))
    ;; Reset hashtable for current buffer.
    (and (assoc buf company-nihongo--index-cache-alist)
         (assq-delete-all buf company-nihongo--index-cache-alist))
    (company-nihongo--get-candidates-2
     :func #'company-nihongo--get-candidates-in-current-buffer
     :prefix prefix))
   (t
    (company-nihongo--get-candidates-2
     :func #'company-nihongo--get-candidates-in-other-buffer
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
         (tail-candidates nil)
         (prefix-len (length prefix))
         (lst nil))
    (cl-assert (and prefix-regexp cand-regexp))
    (when (company-nihongo--go-search-p prefix (current-buffer))
      ;; Note: Would it better to use hashtable for collecting candidates
      ;; in current buffer?
      ;; (message "DEBUG: in-current-buffer, prefix=%s, company-nihongo--not-found-prefix=%s" prefix company-nihongo--not-found-prefix)
      (company-nihongo--search-candidates-in-buffer cand-regexp
                                                    prefix-len
                                                    (1- pos)
                                                    limit
                                                    table
                                                    #'posix-search-backward)
      (company-nihongo--search-candidates-in-buffer cand-regexp
                                                    prefix-len
                                                    (1+ pos)
                                                    limit
                                                    table
                                                    #'posix-search-forward)
      ;; Collect candidates in table
      (maphash
       (lambda (cand v)
         (push cand candidates)
         (when (string-match
                (format "\\(%s+\\)%s+"
                        prefix-regexp
                        (company-nihongo--make-negate-regexp prefix-regexp))
                cand)
           ;; cand contains different characters other than
           ;; prefix-regexp-matching ones. Extract
           ;; prefix-regexp-matching string part and put it
           ;; into hash table.
           ;; If prefix is "Vi" and cand is "Vimプロフェッショナル",
           ;; for example, then we also want to collect "Vim"
           ;; as a candidate.
           (push (match-string-no-properties 1 cand) tail-candidates)))
       table)
      (if candidates
          ;; Found candidates and we clear not-found-state.
          (company-nihongo--clear-not-found-state)
        ;; Found nothing and we set not-found-state
        (company-nihongo--set-not-found-state prefix (current-buffer)))
      (append candidates tail-candidates))))

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
  (let ((non-prefix "\\(?:%s\\|\\b\\)"))
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
      ;; "hiragana" or "hiragan + kanji" or "hiragana + alphabet"
      (cons "\\cH" (format "%s\\(%s\\cH*\\(?:\\cC*\\|%s*\\)\\)"
                           (format non-prefix
                                   (company-nihongo--make-negate-regexp "\\cH"))
                           prefix
                           company-nihongo-alpha-regexp)))
     ((string-match-p "^\\cK+$" prefix)
      ;; "katakana" or "katakana + hiragana" or "katakana + kanji"
      (cons "\\cK" (format "%s\\(%s\\cK*\\(?:\\cH*\\|\\cC*\\)\\)"
                           (format non-prefix
                                   (company-nihongo--make-negate-regexp "\\cK"))
                           prefix)))
     ((string-match-p "^\\cC+$" prefix)
      ;; "kanji" or "kanji + hiragana or "kanji + katakana"
      (cons "\\cC" (format "%s\\(%s\\cC*\\(?:\\cH*\\|\\cK*\\)\\)"
                           (format non-prefix
                                   (company-nihongo--make-negate-regexp "\\cC"))
                           prefix)))
     (t
      nil))))

(defun company-nihongo--search-candidates-in-buffer (regexp min-len pos
                                                     limit table search-func)
  (let ((cand nil))
    (save-excursion
      (ignore-errors (goto-char pos))
      (while (and (< (hash-table-count table) limit)
                  (funcall search-func regexp nil t))
        (setq cand (match-string-no-properties 1))
        (when (< min-len (length cand))
          (puthash cand t table))))))

(defun company-nihongo--get-candidates-in-other-buffer (prefix buf)
  (cl-loop with possible-candidates = (gethash
                                       (substring-no-properties
                                        prefix
                                        0
                                        company-nihongo--hashtable-key-length)
                                       (company-nihongo--get-hashtable buf))
           for cand in possible-candidates
           ;; gethash above returns a sorted list of candidates.
           ;; So, if (string> prefix cand) returns t, it means that
           ;; we have past over possible candidates.
           ;; (aabc abc b bb bba bbb bbc c ca ...)
           ;;             ^           ^
           ;;             |           |
           ;;         prefix = "bd"  last="bbc", prefix > last
           with candidates = nil
           with already-found = nil
           initially (when (string> prefix (car (last possible-candidates)))
                       (return nil))
           if (string-prefix-p prefix cand)
           do (progn (or already-found (setq already-found t))
                     (push cand candidates))
           else if already-found
           return candidates
           finally return candidates))

(defun company-nihongo--get-hashtable (buf &optional new)
  "Return a hashtable that holds words in buffer BUF.

If a hashtable has not been created for buffer BUF, or argument NEW is
non-nil, create a new one, and put words in buffer BUF into this
table, and store it in `company-nihongo--index-cache-alist'."
  (when (or (null (assoc buf company-nihongo--index-cache-alist)) new)
    ;; Make a new hashtable for this buffer. Key is a string of length
    ;; company-nihongo--hashtable-key-length and its value is a list of
    ;; strings, sorted.
    ;; i.e. "あ" => '("あい" "あお" "あほ" "あんこ" ...)
    ;;      "p"  => '("pop" "prin1" "prin1-to-string" "push" ...)
    (company-nihongo--register-hashtable buf))
  (assoc-default buf company-nihongo--index-cache-alist))

(defun company-nihongo--register-hashtable (buffer)
  (cl-loop with table = (make-hash-table :test #'equal)
           with inserted-words = (make-hash-table :test #'equal)
           with res-table = (make-hash-table :test #'equal)
           for word in (company-nihongo--get-word-list buffer)
           for key = (substring word 0 1)
           when (and (> (length word) 1) (not (gethash word inserted-words)))
           if (gethash key table)
           do (progn (puthash word t inserted-words)
                     ;; this word has not been inserted yet
                     (push word (gethash key table)))
           else
           do (progn (puthash key (list word) table)
                     (puthash word t inserted-words))
           finally (progn (maphash (lambda (k v)
                                     ;; sort each list
                                     (puthash k (sort v #'string<) res-table))
                                   table)
                          (push (cons buffer res-table)
                                company-nihongo--index-cache-alist))))

(defun company-nihongo--get-word-list (buffer)
  "Split buffer string by the type of character and return a list of
would-be candidates."
  (cl-loop with lst = (company-nihongo--split-buffer-string buffer)
           with ret = nil
           for curr in lst
           for next in (cdr lst)
           do (progn (push curr ret)
                     (when (company-nihongo--is-target-word curr next)
                       (push (concat curr next) ret)))
           finally return ret))

(defun company-nihongo--is-target-word (curr next)
  (when (and (stringp curr) (stringp next))
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

(defun company-nihongo--split-buffer-string (buffer)
  "Return a list of hiragana words, katakana words and kanji words in
current buffer."
  (let ((ret nil)
        (regexp (format "\\cH+\\|\\cK+\\|\\cC+\\|%s+" company-nihongo-ascii-regexp))
        (word nil))
    (with-current-buffer buffer
      (save-excursion (goto-char (point-min))
                      (while (re-search-forward regexp nil t)
                        (setq word (match-string-no-properties 0))
                        (push word ret)
                        (when (string-match-p company-nihongo-ascii-regexp word)
                          ;; If word is like "abc-def", then we push
                          ;; abc and def into ret as well.
                          (mapc (lambda (elt) (push elt ret))
                                (split-string word "[_-]" t))))))
    (nreverse ret)))

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
