;;; hylo-mode-test-imenu.el --- Test for hylo-mode: Imenu -*- lexical-binding: t -*-

;; Copyright (C) 2019 taku0

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

;; Test for hylo-mode: Imenu.
;; Execute hylo-mode:run-test:imenu interactively or in batch mode.

;;; Code:

(require 'hylo-mode)
(require 'hylo-mode-test)
(require 'hylo-mode-imenu)

(defun hylo-mode:run-test:imenu
    (&optional error-buffer error-counts progress-reporter)
  "Run `imenu' test for `hylo-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors.  Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)
  (if (not hylo-mode:test:running)
      (hylo-mode:run-test '(hylo-mode:run-test:imenu))
    (setq default-directory
          (concat (file-name-as-directory hylo-mode:test:basedir)
                  (file-name-as-directory "hylo-files")
                  "imenu"))
    (dolist (hylo-file (file-expand-wildcards "*.hylo"))
      (redisplay)
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert-file-contents-literally hylo-file)
        (hylo-mode)
        (let* ((actual (hylo-mode:scan-declarations))
               (expected-file-name (concat
                                    (file-name-sans-extension hylo-file)
                                    "-expected.eld"))
               (expected
                (with-temp-buffer
                  (insert-file-contents-literally expected-file-name)
                  (read (current-buffer))))
               (status (if (equal actual expected) 'ok 'error))
               (count-assoc (assq status error-counts)))
          (when (eq status 'error)
            (hylo-mode:show-error
             error-buffer hylo-file 0
             "error"
             (concat
              "expected: "
              (prin1-to-string expected)
              " but: "
              (prin1-to-string actual))))
          (setcdr count-assoc (1+ (cdr count-assoc)))))
      (when (not noninteractive)
        (progress-reporter-update progress-reporter)))))

(provide 'hylo-mode-test-imenu)

;;; hylo-mode-test-imenu.el ends here
