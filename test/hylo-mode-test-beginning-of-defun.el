;;; hylo-mode-test-beginning-of-defun.el --- Test for hylo-mode: beginning-of-defun -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 taku0

;; Author: taku0 (http://github.com/taku0)

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Test for hylo-mode: beginning-of-defun.
;; Execute hylo-mode:run-test:beginning-of-defun interactively or in batch
;; mode.

;;; Code:

(require 'hylo-mode)
(require 'hylo-mode-test)
(require 'hylo-mode-beginning-of-defun)
(require 'seq)

(defun hylo-mode:run-test:beginning-of-defun
    (&optional error-buffer error-counts progress-reporter)
  "Run `beginning-of-defun' test for `hylo-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors.  Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)
  (if (not hylo-mode:test:running)
      (hylo-mode:run-test '(hylo-mode:run-test:beginning-of-defun))
    (let ((current-line 0)
          expected-positions
          expected-positions-desc
          expected-positions-asc
          test-parameters)
      (setq default-directory
            (concat (file-name-as-directory hylo-mode:test:basedir)
                    (file-name-as-directory "hylo-files")
                    "beginning-of-defun"))
      (dolist (hylo-file (file-expand-wildcards "*.hylo"))
        (redisplay)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (insert-file-contents-literally hylo-file)
          (hylo-mode)
          (setq expected-positions
                (hylo-mode:parse-beginning-of-defun-test-file))
          (setq expected-positions-desc
                (mapcar (lambda (p)
                          (list
                           (nth 0 p)
                           (nth 2 p)
                           (nth 4 p)))
                        expected-positions))
          (setq expected-positions-asc
                (mapcar (lambda (p)
                          (list
                           (nth 0 p)
                           (nth 1 p)
                           (nth 3 p)))
                        (reverse expected-positions)))
          (setq test-parameters
                (list
                 (list
                  expected-positions-desc
                  #'<
                  (lambda ()
                    (hylo-mode:beginning-of-defun)
                    (skip-syntax-forward " "))
                  'beginning-of-defun)
                 (list
                  expected-positions-asc
                  #'>
                  #'hylo-mode:end-of-defun
                  'end-of-defun)
                 (list
                  expected-positions-desc
                  #'<
                  (lambda ()
                    (hylo-mode:backward-sentence)
                    (skip-syntax-forward " "))
                  '(beginning-of-sentence beginning-of-defun))
                 (list
                  expected-positions-asc
                  #'>
                  #'hylo-mode:forward-sentence
                  '(end-of-sentence end-of-defun))))
          (setq current-line 0)
          (while (not (eobp))
            (when (not noninteractive)
              (progress-reporter-update progress-reporter))
            (setq current-line (1+ current-line))
            (when (looking-at ".*//.*hylo-mode:test:eval\\(.*\\)")
              (eval-region (match-beginning 1) (match-end 1)))

            (dolist (test-parameter test-parameters)
              (let* ((status (apply
                              #'hylo-mode:test-current-line-beginning-of-defun
                              hylo-file
                              current-line
                              error-buffer
                              test-parameter))
                     (count-assoc (assq status error-counts)))
                (setcdr count-assoc (1+ (cdr count-assoc)))))
            (forward-line)))))))

(defun hylo-mode:parse-beginning-of-defun-test-file ()
  "Parse the current buffer as a test file and return its structure.

The result is a list of remarkable tokens in descendant order.  A remarkable
token is a list with the follwing elements:

1. Type; one of `beginning-of-defun', `end-of-defun', `beginning-of-sentence',
`end-of-sentence', `{', or `}'
2. Start position
3. End position
4. Nesting level at the start position
5. Nesting level at the end position

`beginning-of-defun', `end-of-defun', `beginning-of-sentence', and
`end-of-sentence' are represented as /*{*/, /*}*/, /*[*/, and /*]*/,
respectively, in the test file, and removed from the buffer.

`{' and `}' includes square brackets and parentheses."
  (save-excursion
    (goto-char (point-min))
    (let ((expected-positions
           (list (list 'beginning-of-defun (point) (point) 0 0)))
          (depth 0)
          (pattern (mapconcat #'regexp-quote
                              '("/*{*/" "/*}*/" "/*[*/" "/*]*/"
                                "{" "}" "[" "]" "(" ")"
                                "/*" "*/"
                                "//" "\n"
                                "\"\"\""
                                "\""
                                )
                              "\\|"))
          match-string
          match-beginning
          match-end)
      (while (search-forward-regexp pattern nil t)
        (setq match-string (match-string-no-properties 0))
        (setq match-beginning (match-beginning 0))
        (setq match-end (match-end 0))
        (cond
         ((equal match-string "/*{*/")
          (push (list 'beginning-of-defun
                      match-beginning match-beginning
                      depth depth)
                expected-positions)
          (replace-match ""))
         ((equal match-string "/*}*/")
          (push (list 'end-of-defun
                      match-beginning match-beginning
                      depth depth)
                expected-positions)
          (replace-match ""))
         ((equal match-string "/*[*/")
          (push (list 'beginning-of-sentence
                      match-beginning match-beginning
                      depth depth)
                expected-positions)
          (replace-match ""))
         ((equal match-string "/*]*/")
          (push (list 'end-of-sentence
                      match-beginning match-beginning
                      depth depth)
                expected-positions)
          (replace-match ""))
         ((and (member match-string '("{" "[" "(" "/*"))
               (not (hylo-mode:chunk-after match-beginning)))
          (setq depth (1+ depth))
          (push (list '{ match-beginning match-end (1- depth) depth)
                expected-positions))
         ((and (member match-string '("}" "]" ")" "*/"))
               (not (hylo-mode:chunk-after match-end)))
          (setq depth (1- depth))
          (push (list '} match-beginning match-end (1+ depth) depth)
                expected-positions))

         ((and (equal match-string "//")
               (not (hylo-mode:chunk-after match-beginning)))
          (setq depth (1+ depth))
          (push (list '{ match-beginning match-end (1- depth) depth)
                expected-positions))
         ((and (equal match-string "\n")
               (eq (hylo-mode:chunk:type
                    (hylo-mode:chunk-after match-beginning))
                   'single-line-comment))
          (if (looking-at "\\s *//")
              ;; Fuses with next line.
              (goto-char (match-end 0))
            (setq depth (1- depth))
            (push (list '} match-beginning match-end (1+ depth) depth)
                  expected-positions)))
         ((and (equal match-string "\"\"\"")
               (not (eq (char-before match-beginning) ?\\))
               (not (hylo-mode:chunk:comment-p
                     (hylo-mode:chunk-after match-beginning))))
          (if (hylo-mode:chunk:multiline-string-p
               (hylo-mode:chunk-after match-end))
              (progn (setq depth (1+ depth))
                     (push (list '{ match-beginning match-end (1- depth) depth)
                           expected-positions))
            (setq depth (1- depth))
            (push (list '} match-beginning match-end (1+ depth) depth)
                  expected-positions)))
         ((and (equal match-string "\"")
               (not (eq (char-before match-beginning) ?\\))
               (not (hylo-mode:chunk:comment-p
                     (hylo-mode:chunk-after match-beginning)))
               (not (hylo-mode:chunk:multiline-string-p
                     (hylo-mode:chunk-after match-beginning))))
          (if (hylo-mode:chunk:single-line-string-p
               (hylo-mode:chunk-after match-end))
              (progn (setq depth (1+ depth))
                     (push (list '{ match-beginning match-end (1- depth) depth)
                           expected-positions))
            (setq depth (1- depth))
            (push (list '} match-beginning match-end (1+ depth) depth)
                  expected-positions)))))
      (goto-char (point-max))
      (push (list 'end-of-defun (point) (point) depth depth)
            expected-positions)
      expected-positions)))

(defun hylo-mode:test-current-line-beginning-of-defun
    (hylo-file
     current-line
     error-buffer
     expected-positions
     less-than-function
     beginning-of-thing-function
     boundary-symbols)
  "Run `beginning-of-defun' test for `hylo-mode' on current line.

HYLO-FILE is the filename of the current test case.
CURRENT-LINE is the current line number.
ERROR-BUFFER is the buffer to output errors.
EXPECTED-POSITIONS is a list of remarkable tokens
\(see `hylo-mode:parse-beginning-of-defun-test-file').
LESS-THAN-FUNCTION is a function returns non-nil iff the firt argument is
before (or after for `end-of-defun' test) the second argument.
BEGINNING-OF-THING-FUNCTION is a function goes to the boundary, that is the
beginning of a defun or the end of the defun..
BOUNDARY-SYMBOLS is the type or the list of types of expected remarkable token,
like `beginning-of-defun' or `end-of-defun'"
  (when (symbolp boundary-symbols)
    (setq boundary-symbols (list boundary-symbols)))
  (forward-line 0)
  (let ((status 'ok)
        depth
        expected-positions-before-point
        expected-position
        actual-position)
    (while (eq status 'ok)
      (setq expected-positions-before-point
            (seq-drop-while
             (lambda (position)
               (funcall less-than-function (point) (nth 1 position)))
             expected-positions))
      (setq depth (or (nth 2 (car expected-positions-before-point)) 0))
      (setq expected-position
            (nth 1 (seq-find
                    (lambda (position)
                      (setq depth (min depth (nth 2 position)))
                      (and
                       (memq (nth 0 position) boundary-symbols)
                       (funcall less-than-function (nth 1 position) (point))
                       (<= (nth 2 position) depth)))
                    expected-positions-before-point
                    (list nil (point-min) nil))))
      (setq actual-position (save-excursion
                              (funcall beginning-of-thing-function)
                              (point)))
      (when (/= expected-position actual-position)
        (setq status 'error)
        (hylo-mode:show-error
         error-buffer hylo-file current-line
         "error"
         (concat
          (symbol-name (car boundary-symbols))
          ": at "
          (prin1-to-string (point))
          ", expected "
          (prin1-to-string expected-position)
          " but "
          (prin1-to-string actual-position))))
      (if (eolp)
          (setq status 'done)
        (forward-char)))
    (when (eq status 'done)
      (setq status 'ok))
    status))

(provide 'hylo-mode-test-beginning-of-defun)

;;; hylo-mode-test-beginning-of-defun.el ends here
