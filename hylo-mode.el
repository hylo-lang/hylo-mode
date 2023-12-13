;;; hylo-mode.el --- Major-mode for the Hylo programming language -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021 taku0, Chris Barrett, Bozhidar Batsov,
;;                         Arthur Evstifeev

;; Author: taku0 <mxxouy6x3m_github@tatapa.org>
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;; Maintainer: taku0 <mxxouy6x3m_github@tatapa.org>
;;
;; Version: 9.1.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
;; Keywords: languages hylo
;; URL: https://github.com/hylo-emacs/hylo-mode

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

;; Major-mode for the Hylo programming language.

;;; Code:

(require 'hylo-mode-lexer)
(require 'hylo-mode-indent)
(require 'hylo-mode-fill)
(require 'hylo-mode-font-lock)
(require 'hylo-mode-beginning-of-defun)
(require 'hylo-mode-repl)
(require 'hylo-mode-imenu)

;;;###autoload
(defgroup hylo nil
  "Major-mode for the Hylo programming language."
  :group 'languages
  :prefix "hylo-mode:")

;; WORKAROUND: `update-directory-autoloads' does not handle `:group'.
;; Fixed in 29.1 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58015.
;; commit: 212e94c3f445ebe1388f6fab134133ebad9316d0
;;;###autoload (custom-add-load 'languages 'hylo-mode)

;; WORKAROUND: `cus-load' overrides `custom-loads'
;; Fixed in 29.1 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58015.
;; commit: 75b3f4d0ac00bf47459629615ab2246c8a34b4c6
;;;###autoload (with-eval-after-load 'cus-load
;;;###autoload   (custom-add-load 'languages 'hylo-mode))

;;; Keymap

(defvar hylo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map [remap indent-new-comment-line]
                #'hylo-mode:indent-new-comment-line)
    (define-key map (kbd "C-c C-z") #'hylo-mode:run-repl)
    (define-key map (kbd "C-c C-f") #'hylo-mode:send-buffer)
    (define-key map (kbd "C-c C-r") #'hylo-mode:send-region)
    (define-key map [remap beginning-of-defun] #'hylo-mode:beginning-of-defun)
    (define-key map [remap end-of-defun] #'hylo-mode:end-of-defun)
    (define-key map [remap mark-defun] #'hylo-mode:mark-defun)
    (define-key map [remap narrow-to-defun] #'hylo-mode:narrow-to-defun)
    (define-key map [remap backward-sentence] #'hylo-mode:backward-sentence)
    (define-key map [remap forward-sentence] #'hylo-mode:forward-sentence)
    (define-key map [remap kill-sentence] #'hylo-mode:kill-sentence)
    (define-key map [remap backward-kill-sentence]
                #'hylo-mode:backward-kill-sentence)
    ;; (define-key map (kbd "???") #'hylo-mode:mark-sentence)
    (define-key map [remap narrow-to-sentence] #'hylo-mode:narrow-to-sentence)

    (easy-menu-define hylo-menu map "Hylo Mode menu"
      `("Hylo"
        :help "Hylo-specific Features"
        ["Run REPL" hylo-mode:run-repl
         :help "Run Hylo REPL"]
        ["Send buffer to REPL" hylo-mode:send-buffer
         :help "Send the current buffer's contents to the REPL"]
        ["Send region to REPL" hylo-mode:send-region
         :help "Send currently selected region to the REPL"]
        ["Build Hylo module" hylo-mode:build-hylo-module
         :help "Build current Hylo module"]
        ["Build iOS app" hylo-mode:build-ios-app
         :help "Build current iOS app"]
        ["Debug Hylo module" hylo-mode:debug-hylo-module
         :help "Debug current Hylo module"]
        ["Debug iOS app" hylo-mode:debug-ios-app
         :help "Debug current iOS app with simulator"]))
    map)
  "Hylo mode key map.")

;;; `forward-sexp-function'

(defun hylo-mode:forward-sexp (&optional arg)
  "Move forward/backward a token or list.

See `forward-sexp for ARG."
  (setq arg (or arg 1))
  (when (hylo-mode:chunk-after)
    (goto-char (hylo-mode:chunk:start (hylo-mode:chunk-after))))
  (if (< 0 arg)
      (while (< 0 arg)
        (while (eq (hylo-mode:token:type (hylo-mode:forward-sexp-1))
                   'implicit-\;))
        (setq arg (1- arg))))
  (while (< arg 0)
    (while (eq (hylo-mode:token:type (hylo-mode:backward-sexp-1))
               'implicit-\;))
    (setq arg (1+ arg))))

(defun hylo-mode:forward-sexp-1 ()
  "Move forward a token or list.

Signal `scan-error' if it hits closing parentheses."
  (let ((token (hylo-mode:forward-token-or-list))
        (pos (point)))
    (when (memq (hylo-mode:token:type token) '(\] \) }))
      (goto-char pos)
      (signal 'scan-error
              (list "Unbalanced parentheses"
                    (hylo-mode:token:start token)
                    (hylo-mode:token:end token))))
    token))

(defun hylo-mode:backward-sexp-1 ()
  "Move backward a token or list.

Signal `scan-error' if it hits opening parentheses."
  (let ((token (hylo-mode:backward-token-or-list))
        (pos (point)))
    (when (memq (hylo-mode:token:type token) '(\[ \( {))
      (goto-char pos)
      (signal 'scan-error
              (list "Unbalanced parentheses"
                    (hylo-mode:token:start token)
                    (hylo-mode:token:end token))))
    token))

(declare-function speedbar-add-supported-extension "speedbar" (extension))

;;;###autoload
(defsubst hylo-mode:add-supported-extension-for-speedbar ()
  "Register .hylo to speedbar."
  ;; FIXME: Use `with-eval-after-load' when `package-lint' allows it.
  ;; See also https://github.com/hylo-emacs/hylo-mode/pull/179
  (if (fboundp 'speedbar-add-supported-extension)
      (speedbar-add-supported-extension ".hylo")
    (add-hook 'speedbar-load-hook
              (lambda ()
                (speedbar-add-supported-extension ".hylo")))))

;;;###autoload
(define-derived-mode hylo-mode prog-mode "Hylo"
  "Major mode for editing Hylo code.

\\{hylo-mode-map}"
  :syntax-table hylo-mode:syntax-table
  :group 'hylo

  (setq font-lock-defaults '(hylo-mode:font-lock-keywords))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  ;; ":" is for Playground Rich Comments Markup Syntax:
  ;; https://developer.apple.com/library/prerelease/ios/documentation/Xcode/Reference/xcode_markup_formatting_ref/PlaygroundRichComments.html
  (setq-local comment-start-skip
              (concat
               "\\s *"
               "\\(?:"
               ;; Single-line comment
               "//+" ":?" "\\|"
               ;; Multi-line comment
               "/\\*+" ":?" "\\|"
               ;; Middle of multi-line-comment
               "\\*+ "
               "\\)"
               "\\s *"))
  (setq-local adaptive-fill-regexp
              (concat
               "\\s *"
               "\\(?:"
               ;; Single-line comment
               "//+" ":?" "\\|"
               ;; Middle of multi-line-comment
               "\\*+ "
               "\\)"
               "\\s *"))
  (setq-local fill-indent-according-to-mode t)
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function #'hylo-mode:indent-new-comment-line)
  (setq-local fill-paragraph-function #'hylo-mode:fill-paragraph)
  (setq-local fill-forward-paragraph-function
              #'hylo-mode:fill-forward-paragraph)
  (setq-local normal-auto-fill-function #'hylo-mode:do-auto-fill)
  (hylo-mode:install-fill-region-as-paragraph-advice)
  (hylo-mode:install-current-fill-column-advice)

  (setq-local parse-sexp-lookup-properties t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'hylo-mode:syntax-propertize-extend-region
            nil t)
  (setq-local syntax-propertize-function #'hylo-mode:syntax-propertize)

  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'hylo-mode:indent-line)

  (setq-local forward-sexp-function #'hylo-mode:forward-sexp)

  (setq-local electric-indent-chars
              (append "{}()[]:;,." electric-indent-chars))

  (add-hook 'post-self-insert-hook #'hylo-mode:post-self-insert nil t)

  (setq-local imenu-create-index-function #'hylo-mode:imenu-create-index)

  (setq-local beginning-of-defun-function #'hylo-mode:beginning-of-defun)
  (setq-local end-of-defun-function #'hylo-mode:end-of-defun)

  (setq-local hylo-mode:anchor-overlay
              (make-overlay (point-min) (point-min) nil t))

  (delete-overlay hylo-mode:anchor-overlay)

  (add-hook 'which-fun-functions
            (lambda ()
              (when (equal (with-current-buffer (current-buffer) major-mode)
                           'hylo-mode)
                (hylo-mode:current-defun-name))))
  (setq-local add-log-current-defun-function #'hylo-mode:current-defun-name))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.hylo\\(interface\\)?\\'" . hylo-mode))

;;;###autoload (hylo-mode:add-supported-extension-for-speedbar)

(provide 'hylo-mode)

;;; hylo-mode.el ends here
