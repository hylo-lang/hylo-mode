;;; hylo-mode-test-font-lock.el --- Test for hylo-mode: font-lock  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Daniel Martín

;; Author: Daniel Martín (http://github.com/taku0)

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

;; Test for hylo-mode: font-lock.
;; Execute hylo-mode:run-test:font-lock interactively or in batch mode.

;;; Code:

(require 'hylo-mode)
(require 'hylo-mode-test)
(require 'hylo-mode-font-lock)

(defun hylo-mode:run-test:font-lock
    (&optional error-buffer error-counts progress-reporter)
  "Run font-lock test for `hylo-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors.  Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)
  (if (not hylo-mode:test:running)
      (hylo-mode:run-test '(hylo-mode:run-test:font-lock))
    (let ((current-line 0))
      (setq default-directory
            (concat (file-name-as-directory hylo-mode:test:basedir)
                    (file-name-as-directory "hylo-files")
                    "font-lock"))

      (dolist (hylo-file (file-expand-wildcards "*.hylo"))
        (redisplay)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (insert-file-contents-literally hylo-file)
          (hylo-mode)
          (funcall (if (fboundp 'font-lock-ensure)
                       #'font-lock-ensure
                     #'font-lock-fontify-buffer))
          (setq current-line 0)
          (while (not (eobp))
            (when (not noninteractive)
              (progress-reporter-update progress-reporter))
            (setq current-line (1+ current-line))
            (cond
             ((= (line-beginning-position) (line-end-position))
              ;; Empty line
              nil)
             ((looking-at-p "//.*")
              ;; Ignore comments
              nil)
             (t
              (let*
                  ((status (hylo-mode:test-current-line-font-lock
                            hylo-file current-line error-buffer))
                   (count-assoc (assq status error-counts)))
                (setcdr count-assoc (1+ (cdr count-assoc))))))
            (forward-line)))))))

(defun hylo-mode:test-current-line-font-lock
    (hylo-file current-line error-buffer)
  "Compute the font-lock properties applied by hylo-mode on current line.

HYLO-FILE is the filename of the current test case.
CURRENT-LINE is the current line number.
ERROR-BUFFER is the buffer to output errors."
  (let ((status 'ok))
    (when (looking-at "\\(.*\\)[ /t]+//[ /t]+\\(.*\\)")
      (let ((actual-props (format "%S" (buffer-substring (match-beginning 1) (match-end 1))))
            (expected-props (buffer-substring-no-properties (match-beginning 2)
                                                            (match-end 2))))
        (when (not (string-equal expected-props actual-props))
          (setq status 'error)
          (hylo-mode:show-error
           error-buffer hylo-file current-line
           "error"
           (concat
            "font-lock: expected "
            (prin1-to-string expected-props)
            " but "
            (prin1-to-string actual-props))))))
    status))

(provide 'hylo-mode-test-font-lock)

;;; hylo-mode-test-font-lock.el ends here
