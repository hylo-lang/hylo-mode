;;; hylo-mode-repl.el --- Run the Hylo processes in Emacs buffers -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021 taku0, Chris Barrett, Bozhidar Batsov,
;;                         Arthur Evstifeev, Michael Sanders

;; Author: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;       Michael Sanders <https://github.com/msanders>

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

;; Run the Hylo processes in Emacs buffers.

;;; Code:

(require 'comint)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)

;;;###autoload
(defgroup hylo-mode:repl nil
  "REPL."
  :tag "Hylo Mode REPL"
  :group 'hylo)

(defcustom hylo-mode:repl-executable
  (concat (when (executable-find "xcrun") "xcrun ") "hylo repl")
  "Path to the Hylo CLI.  The string is split by spaces, then unquoted."
  :tag "Hylo Mode REPL Executable"
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:hylo-package-executable
  (concat (when (executable-find "xcrun") "xcrun ") "hylo package")
  "Path to the Hylo command for package manipulation.
The string is split by spaces, then unquoted."
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:hylo-build-executable
  (concat (when (executable-find "xcrun") "xcrun ") "hylo build")
  "Path to the Hylo command for building.
The string is split by spaces, then unquoted."
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:debugger-executable
  (concat (when (executable-find "xcrun") "xcrun ") "lldb")
  "Path to the debugger command.
The string is split by spaces, then unquoted."
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:ios-deploy-executable
  "ios-deploy"
  "Path to ios-deploy command.
The string is split by spaces, then unquoted."
  :tag "Hylo Mode iOS Deploy Executable"
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:simulator-controller-executable
  (concat (when (executable-find "xcrun") "xcrun ") "simctl")
  "Path to the simulator controller command.
The string is split by spaces, then unquoted."
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:xcodebuild-executable
  (concat (when (executable-find "xcrun") "xcrun ") "xcodebuild")
  "Path to the Xcode builder.
The string is split by spaces, then unquoted."
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:xcode-select-executable
  "xcode-select"
  "Path to the Xcode selector.
The string is split by spaces, then unquoted."
  :type '(choice string (list string))
  :safe #'stringp)

(defcustom hylo-mode:debugger-prompt-regexp "^(lldb) +\\|^[0-9]+> +"
  "Regexp to search a debugger prompt."
  :type 'string
  :safe #'stringp)

(defvar hylo-mode:repl-buffer nil
  "Stores the name of the current hylo REPL buffer, or nil.")

(defvar hylo-mode:repl-command-queue nil
  "List of strings to be executed on REPL.

Use `hylo-mode:enqueue-repl-commands' to enqueue commands.
If an element is a cons cell, its car is used as a regexp for prompt and
cdr is used as a command.  If its car is a function, it is called to search
prompt.  It should return non-nil when a prompt is found and return nil
otherwise.")

(defvar hylo-mode:ios-device-identifier nil
  "Identifier of iOS device used for building/debugging.")

(defconst hylo-mode:ios-local-device-identifier
  "00000000-0000-0000-0000-000000000000"
  "Identifier of local iOS device.")

(defvar hylo-mode:ios-project-scheme nil
  "Scheme to use in Xcode project for building/debugging.")

(defun hylo-mode:command-list-to-string (cmd)
  "Concatenate the CMD unless it is a string.

This function quotes elements appropriately."
  (if (stringp cmd) cmd (combine-and-quote-strings cmd)))

(defun hylo-mode:command-string-to-list (cmd)
  "Split the CMD unless it is a list.

This function respects quotes."
  (if (listp cmd) cmd (split-string-and-unquote cmd)))

;;;###autoload
(defun hylo-mode:run-repl (cmd &optional dont-switch keep-default)
  "Run a Hylo REPL process.

This function input and output via buffer `*CMD*' where CMD is replaced with
the CMD given.
If there is a process already running in `*CMD*', and DONT-SWITCH is nil,
switch to that buffer.
CMD is a string or a list, interpreted as a command line.  The default value is
`hylo-mode:repl-executable'.  This function updates the buffer local variable
`hylo-mode:repl-executable' with the given CMD if KEEP-DEFAULT is nil,
so it will be used as the default value for the next invocation in the current
buffer.
If KEEP-DEFAULT is non-nil, the `hylo-mode:repl-executable' and the global
variable `hylo-mode:repl-buffer' are not updated.  The buffer local variable
`hylo-mode:repl-buffer' is always updated.
Runs the hook `hylo-repl-mode-hook' \(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive
   (list (if current-prefix-arg
             (read-string
              "Run hylo REPL: "
              (hylo-mode:command-list-to-string hylo-mode:repl-executable))
           hylo-mode:repl-executable)))
  (let* ((original-buffer (current-buffer))
         (cmd-string (hylo-mode:command-list-to-string cmd))
         (cmd-list (hylo-mode:command-string-to-list cmd))
         (buffer-name (concat "*Hylo REPL [" cmd-string "]*"))
         (buffer (get-buffer-create buffer-name))
         old-size)
    (with-current-buffer original-buffer
      (setq-local hylo-mode:repl-buffer buffer)
      (unless keep-default
        (setq-local hylo-mode:repl-executable cmd)
        (setq-default hylo-mode:repl-buffer hylo-mode:repl-buffer)))
    (with-current-buffer buffer
      (setq old-size (buffer-size))
      (hylo-repl-mode)
      (setq-local hylo-mode:repl-buffer buffer))
    (unless (comint-check-proc buffer)
      (apply #'make-comint-in-buffer
             cmd-string buffer (car cmd-list) nil (cdr cmd-list))
      (with-current-buffer buffer
        (while (= old-size (buffer-size))
          (sleep-for .1))))
    (unless dont-switch
      (pop-to-buffer buffer))))

;;;###autoload
(defalias 'run-hylo #'hylo-mode:run-repl)

;;;###autoload
(defun hylo-mode:send-region (start end)
  "Send the current region to the inferior hylo process.

START and END define region within current buffer"
  (interactive "r")
  (hylo-mode:run-repl hylo-mode:repl-executable t t)
  (comint-send-region hylo-mode:repl-buffer start end)
  (comint-send-string hylo-mode:repl-buffer "\n"))

;;;###autoload
(defun hylo-mode:send-buffer ()
  "Send the buffer to the Hylo REPL process."
  (interactive)
  (hylo-mode:send-region (point-min) (point-max)))

(define-derived-mode hylo-repl-mode comint-mode "Hylo REPL"
  "Major mode for interacting with Hylo REPL.

A REPL can be fired up with \\<hylo-mode-map>\\[hylo-mode:run-repl] or \
\\<hylo-mode-map>\\[run-hylo].

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`hylo-repl-mode-hook' (in that order).

You can send text to the REPL process from other buffers containing source.
`hylo-mode:send-region' sends the current region to the REPL process,
`hylo-mode:send-buffer' sends the current buffer to the REPL process.")

(defun hylo-mode:call-process (executable &rest args)
  "Call EXECUTABLE synchronously in separate process.

EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
ARGS are rest arguments, appended to the argument list.
Returns the exit status."
  (hylo-mode:do-call-process executable nil t nil args))

(defun hylo-mode:call-process-async (executable &rest args)
  "Call EXECUTABLE asynchronously in separate process.

EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
ARGS are rest arguments, appended to the argument list."
  (hylo-mode:do-call-process executable nil 0 nil args))

(defun hylo-mode:do-call-process (executable infile destination display args)
  "Wrapper for `call-process'.

EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
For INFILE, DESTINATION, DISPLAY, see `call-process'.
ARGS are rest arguments, appended to the argument list.
Returns the exit status."
  (let ((command-list
         (append (hylo-mode:command-string-to-list executable) args)))
    (apply #'call-process
           (append
            (list (car command-list))
            (list infile destination display)
            (cdr command-list)))))

(defun hylo-mode:call-process-to-json (executable &rest args)
  "Call EXECUTABLE synchronously in separate process.

The output is parsed as a JSON document.
EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
ARGS are rest arguments, appended to the argument list."
  (with-temp-buffer
    (unless (zerop
             (hylo-mode:do-call-process executable
                                         nil
                                         ;; Disregard stderr output, as it
                                         ;; corrupts JSON.
                                         (list t nil)
                                         nil
                                         args))
      (error "%s: %s" "Cannot invoke executable" (buffer-string)))
    (goto-char (point-min))
    (json-read)))

(defun hylo-mode:describe-package (project-directory)
  "Read the package definition from the manifest file Package.hylo.

The manifest file is searched from the PROJECT-DIRECTORY, defaults to
`default-directory', or its ancestors.
Return a JSON object."
  (unless project-directory (setq project-directory default-directory))
  (hylo-mode:call-process-to-json
   hylo-mode:hylo-package-executable
   "--package-path" project-directory "describe" "--type" "json"))

(defun hylo-mode:read-main-module (project-directory)
  "Read the main module description from the manifest file Package.hylo.

The manifest file is searched from the PROJECT-DIRECTORY, defaults to
`default-directory', or its ancestors."
  (let* ((description (hylo-mode:describe-package project-directory))
         (modules (cdr (assoc 'targets description))))
    (seq-find
     (lambda (module) (not (equal "test" (cdr (assoc 'type module)))))
     modules)))

(defun hylo-mode:read-package-name (project-directory)
  "Read the package name from the manifest file Package.hylo.

The manifest file is searched from the PROJECT-DIRECTORY, defaults to
`default-directory', or its ancestors."
  (cdr (assoc 'name (hylo-mode:read-main-module project-directory))))

(defun hylo-mode:read-c99-name (project-directory)
  "Read the C99 name from the manifest file Package.hylo.

The manifest file is searched from the PROJECT-DIRECTORY, defaults to
`default-directory', or its ancestors."
  (cdr (assoc 'c99name (hylo-mode:read-main-module project-directory))))

(defun hylo-mode:read-module-type (project-directory)
  "Read the module type from the manifest file Package.hylo.

The manifest file is searched from the PROJECT-DIRECTORY, defaults to
`default-directory', or its ancestors."
  (cdr (assoc 'type (hylo-mode:read-main-module project-directory))))

(defun hylo-mode:join-path (directory &rest components)
  "Make path string for DIRECTORY followed by COMPONENTS."
  (seq-reduce
   (lambda (directory component) (expand-file-name component directory))
   components
   directory))

(defun hylo-mode:find-ancestor-or-self-directory (predicate
                                                   &optional
                                                   directory)
  "Find the nearest ancestor-or-self directory satisfying a PREDICATE.

Traverse up from DIRECTORY up to the root directory.
Return a directory satisfying the PREDICATE if exists.  Otherwise, return nil."
  (unless directory (setq directory default-directory))
  (if (funcall predicate directory)
      directory
    (let ((parent (file-name-directory (directory-file-name directory))))
      (if (or (null parent) (string-equal parent directory))
          nil
        (hylo-mode:find-ancestor-or-self-directory predicate parent)))))

(defun hylo-mode:hylo-project-directory-p (directory)
  ;; supress warnings:
  ;;   (checkdoc) Probably "contains" should be imperative "contain"
  "Return t if the DIRECTORY contain\u0073 a file Package.hylo."
  (file-exists-p (expand-file-name "Package.hylo" directory)))

(defun hylo-mode:find-hylo-project-directory (&optional directory)
  "Find a file Package.hylo in the DIRECTORY or its ancestors.

Return a directory path if found.  Return nil otherwise."
  (hylo-mode:find-ancestor-or-self-directory
   'hylo-mode:hylo-project-directory-p directory))

(defun hylo-mode:read-project-directory (default)
  "Read a project directory from the minibuffer with DEFAULT directory."
  (expand-file-name (read-directory-name "Project directory: " default nil t)))

(defun hylo-mode:ensure-hylo-project-directory (project-directory)
  ;; supress warnings:
  ;;   (checkdoc) Probably "contains" should be imperative "contain"
  "Check PROJECT-DIRECTORY contain\u0073 the manifest file Package.hylo.

If PROJECT-DIRECTORY is nil, this function searches it from `default-directory'
or its ancestors."
  (unless project-directory
    (setq project-directory (hylo-mode:find-hylo-project-directory)))
  (unless project-directory
    (error "Project directory not found"))
  (unless (hylo-mode:hylo-project-directory-p project-directory)
    (error "Not a project directory"))
  project-directory)

(defun hylo-mode:xcode-project-directory-p (directory)
  ;; supress warnings:
  ;;   (checkdoc) Probably "contains" should be imperative "contain"
  "Return t if the DIRECTORY contain\u0073 a file *.xcodeproj."
  (consp (directory-files directory nil ".*\\.xcodeproj")))

(defun hylo-mode:xcode-workspace-directory-p (directory)
  ;; supress warnings:
  ;;   (checkdoc) Probably "contains" should be imperative "contain"
  "Return t if the DIRECTORY contain\u0073 a file *.xcworkspace."
  (consp (directory-files directory nil ".*\\.xcworkspace")))

(defun hylo-mode:find-xcode-project-directory (&optional directory)
  "Find a file *.xcodeproj in the DIRECTORY or its ancestors.

Return a directory path if found.  Return nil otherwise."
  (hylo-mode:find-ancestor-or-self-directory
   'hylo-mode:xcode-project-directory-p directory))

(defun hylo-mode:find-xcode-workspace-directory (&optional directory)
  "Find a file *.xcworkspace in the DIRECTORY or its ancestors.

Return a directory path if found.  Return nil otherwise."
  (hylo-mode:find-ancestor-or-self-directory
   'hylo-mode:xcode-workspace-directory-p directory))

(defun hylo-mode:ensure-xcode-project-directory (project-directory)
  ;; supress warnings:
  ;;   (checkdoc) Probably "contains" should be imperative "contain"
  "Check PROJECT-DIRECTORY contain\u0073 *.xcworkspace or *.xcodeproj.

If PROJECT-DIRECTORY is nil, this function searches it from `default-directory'
or its ancestors."
  (unless project-directory
    (setq project-directory
          (or
           (hylo-mode:find-xcode-workspace-directory)
           (hylo-mode:find-xcode-project-directory))))
  (unless project-directory
    (error "Project directory not found"))
  (unless (or (hylo-mode:xcode-project-directory-p project-directory)
              (hylo-mode:xcode-workspace-directory-p project-directory))
    (error "Not a project directory"))
  project-directory)

(defun hylo-mode:list-ios-simulators ()
  "List iOS simulator devices, device types, runtimes, or device pairs."
  (hylo-mode:call-process-to-json
   hylo-mode:simulator-controller-executable
   "list" "--json"))

(defun hylo-mode:list-ios-simulator-devices ()
  "List available iOS simulator devices."
  (let* ((json (hylo-mode:list-ios-simulators))
         (devices (cdr (assoc 'devices json)))
         (flattened (apply #'seq-concatenate 'list (seq-map #'cdr devices)))
         (available-devices
          (seq-filter
           (lambda (device) (cdr (assoc 'isAvailable device)))
           flattened)))
    available-devices))

(defun hylo-mode:read-ios-device-identifier ()
  "Read a iOS simulator device identifier from the minibuffer."
  (let* ((devices (hylo-mode:list-ios-simulator-devices))
         (items (append (list (cons "Local device"
                                    hylo-mode:ios-local-device-identifier))
                        (seq-map
                         (lambda (device)
                           (cons (cdr (assoc 'name device))
                                 (cdr (assoc 'udid device))))
                         devices))))
    (widget-choose "Choose a device" items)))

(defun hylo-mode:read-xcode-build-settings (project-directory
                                             scheme
                                             sdk
                                             device-identifier)
  "Read Xcode build settings in PROJECT-DIRECTORY.

SCHEME is the name of the project scheme in Xcode.
SDK is the name of the SDK build against.
DEVICE-IDENTIFIER is used as the destination parameter for xcodebuild.  If
identifier is equal to `hylo-mode:ios-local-device-identifier', it is not
passed as a destination to xcodebuild."
  (with-temp-buffer
    (let ((default-directory project-directory)
          (arglist `(,hylo-mode:xcodebuild-executable
                     "-configuration"
                     "Debug"
                     "-sdk"
                     ,sdk
                     "-scheme"
                     ,scheme
                     "-showBuildSettings")))
      (when (and device-identifier
                 (not (equal device-identifier
                             hylo-mode:ios-local-device-identifier)))
        (setq arglist
              (append
               arglist
               `("-destination"
                 ,(concat "platform=iOS Simulator,id=" device-identifier)))))
      (unless (zerop (apply #'hylo-mode:call-process arglist))
        (error "%s %s" "Cannot read Xcode build settings" (buffer-string))))
    (goto-char (point-min))
    (let ((settings nil))
      (while (search-forward-regexp " *\\([_a-zA-Z0-9]+\\) *= *\\(.*\\)" nil t)
        (push (cons (match-string 1) (match-string 2)) settings))
      settings)))

(defun hylo-mode:xcodebuild-list (project-directory)
  "Return the contents of `xcodebuild -list' in PROJECT-DIRECTORY as JSON."
  (let ((default-directory project-directory)
        (json-array-type 'list))
    (hylo-mode:call-process-to-json
     hylo-mode:xcodebuild-executable
     "-list"
     "-json")))

(defun hylo-mode:read-project-scheme (project-directory)
  "Read and prompt for a project's scheme in the minibuffer.

xcodebuild is executed in PROJECT-DIRECTORY."
  (let* ((json (hylo-mode:xcodebuild-list project-directory))
         (project (or (cdr (assoc 'project json))
                      (cdr (assoc 'workspace json))))
         (schemes (cdr (assoc 'schemes project)))
         (choices (seq-map
                   (lambda (scheme) (cons scheme scheme))
                   schemes)))
    (pcase (length schemes)
      (1 (car schemes))
      (0 nil)
      (_ (widget-choose "Choose a scheme" choices)))))

(defun hylo-mode:locate-xcode ()
  "Return the developer path in Xcode.app.

Typically, it is /Applications/Xcode.app/Contents/Developer."
  (string-trim-right
   (with-output-to-string
     (with-current-buffer
         standard-output
       (unless (zerop (hylo-mode:call-process
                       hylo-mode:xcode-select-executable
                       "--print-path"))
         (error "%s: %s" "Cannot locate Xcode" (buffer-string)))))))

;;;###autoload
(defun hylo-mode:build-hylo-module (&optional project-directory args)
  "Build a Hylo module in the PROJECT-DIRECTORY.

If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors.
An list ARGS are appended for builder command line arguments."
  (interactive
   (let* ((default-project-directory (hylo-mode:find-hylo-project-directory))
          (project-directory
           (if current-prefix-arg
               (hylo-mode:read-project-directory default-project-directory)
             default-project-directory)))
     (list
      project-directory
      (if (string-equal (hylo-mode:read-module-type project-directory)
                        "library")
          '("-Xhyloc" "-emit-library")
        '()))))
  (setq project-directory
        (hylo-mode:ensure-hylo-project-directory project-directory))
  (with-current-buffer (get-buffer-create "*hylo-mode:compilation*")
    (fundamental-mode)
    (setq buffer-read-only nil)
    (let ((progress-reporter (make-progress-reporter "Building...")))
      (unless
          (zerop
           (apply #'hylo-mode:call-process
                  hylo-mode:hylo-build-executable
                  "--package-path" project-directory
                  args))
        (compilation-mode)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))
        (error "Build error"))
      (kill-buffer)
      (progress-reporter-done progress-reporter))))

;;;###autoload
(defun hylo-mode:build-ios-app (&optional project-directory
                                           device-identifier
                                           scheme)
  "Build an iOS app in the PROJECT-DIRECTORY.
Build it for iOS device DEVICE-IDENTIFIER for the given SCHEME.
If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors.
DEVICE-IDENTIFIER is the device identifier of the iOS simulator.  If it is nil
or omitted, the value of `hylo-mode:ios-device-identifier' is used.  If it is
equal to `hylo-mode:ios-local-device-identifier', a local device is used via
`ios-deploy' instead.
SCHEME is the name of the project scheme in Xcode.  If it is nil or omitted,
the value of `hylo-mode:ios-project-scheme' is used."
  (interactive
   (let* ((default-project-directory
           (or
            (hylo-mode:find-xcode-workspace-directory)
            (hylo-mode:find-xcode-project-directory)))
          (project-directory
           (if current-prefix-arg
               (hylo-mode:read-project-directory default-project-directory)
             default-project-directory)))
     (list
      project-directory
      (if current-prefix-arg
          (hylo-mode:read-ios-device-identifier)
        hylo-mode:ios-device-identifier)
      (if current-prefix-arg
          (hylo-mode:read-project-scheme project-directory)
        hylo-mode:ios-project-scheme))))
  (setq project-directory
        (hylo-mode:ensure-xcode-project-directory project-directory))
  (unless device-identifier
    (setq device-identifier
          (or
           hylo-mode:ios-device-identifier
           (hylo-mode:read-ios-device-identifier))))
  (setq hylo-mode:ios-device-identifier device-identifier)
  (unless scheme
    (setq scheme
          (or
           hylo-mode:ios-project-scheme
           (hylo-mode:read-project-scheme project-directory))))
  (setq hylo-mode:ios-project-scheme scheme)

  (with-current-buffer (get-buffer-create "*hylo-mode:compilation*")
    (fundamental-mode)
    (setq buffer-read-only nil)
    (let ((progress-reporter (make-progress-reporter "Building..."))
          (xcodebuild-args `(,hylo-mode:xcodebuild-executable
                             "-configuration" "Debug"
                             "-scheme" ,scheme)))
      (if (equal device-identifier hylo-mode:ios-local-device-identifier)
          (setq xcodebuild-args (append xcodebuild-args '("-sdk" "iphoneos")))
        (setq xcodebuild-args
              (append xcodebuild-args
                      `("-destination"
                        ,(concat "platform=iOS Simulator,id=" device-identifier)
                        "-sdk" "iphonesimulator"))))
      (unless
          (zerop
           (let ((default-directory project-directory))
             (apply #'hylo-mode:call-process xcodebuild-args)))
        (compilation-mode)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))
        (error "Build error"))
      (kill-buffer)
      (progress-reporter-done progress-reporter))))

(defun hylo-mode:wait-for-prompt-then-execute-commands (string)
  "Execute the next command from the queue if the point is on a prompt.

Intended for used as a `comint-output-filter-functions'.
STRING is passed to the command."
  (let ((command (car hylo-mode:repl-command-queue)))
    (when (and
           ;; The point is on an input field of comint.
           (null (field-at-pos (point)))
           ;; It is a LLDB prompt rather than that of the target executable.
           (save-excursion
             (if (and (consp command) (functionp (car command)))
                 ;; Calls custom function to search expected output
                 (funcall (car command) string)
               (forward-line 0)
               (looking-at
                (if (consp command)
                    ;; Using custom regexp
                    (car command)
                  ;; Using standard regexp
                  hylo-mode:debugger-prompt-regexp)))))
      (when hylo-mode:repl-command-queue
        (pop hylo-mode:repl-command-queue)
        (insert (if (consp command) (cdr command) command))
        (comint-send-input))
      (unless hylo-mode:repl-command-queue
        (remove-hook 'comint-output-filter-functions
                     #'hylo-mode:wait-for-prompt-then-execute-commands t)))))

(defun hylo-mode:enqueue-repl-commands (&rest commands)
  "Enqueue COMMANDS to be executed on REPL."
  (with-current-buffer hylo-mode:repl-buffer
    (setq-local hylo-mode:repl-command-queue
                (append hylo-mode:repl-command-queue commands))
    (add-hook 'comint-output-filter-functions
              #'hylo-mode:wait-for-prompt-then-execute-commands
              nil t)))

(defun hylo-mode:debug-hylo-module-library (project-directory)
  "Run debugger on a Hylo library module in the PROJECT-DIRECTORY."
  (let* ((c99name (hylo-mode:read-c99-name project-directory))
         (import-statement (concat "import " c99name))
         (build-debug-directory
          (hylo-mode:join-path project-directory ".build" "debug")))
    (unless c99name (error "Cannot get module name"))
    (hylo-mode:build-hylo-module project-directory)
    (hylo-mode:run-repl
     (append
      (hylo-mode:command-string-to-list hylo-mode:repl-executable)
      (list
       "-I" build-debug-directory
       "-L" build-debug-directory
       (concat "-l" c99name)))
     nil t)
    (hylo-mode:enqueue-repl-commands import-statement)))

(defun hylo-mode:debug-hylo-module-executable (project-directory)
  "Run debugger on a Hylo executable module in the PROJECT-DIRECTORY."
  (let ((package-name (hylo-mode:read-package-name project-directory)))
    (unless package-name (error "Cannot get module name"))
    (hylo-mode:build-hylo-module project-directory)
    (hylo-mode:run-repl
     (append
      (hylo-mode:command-string-to-list hylo-mode:debugger-executable)
      (list
       (hylo-mode:join-path project-directory ".build" "debug" package-name)))
     nil t)
    (hylo-mode:enqueue-repl-commands
     "breakpoint set --one-shot true --file main.hylo --name main"
     "run"
     "repl")))

;;;###autoload
(defun hylo-mode:debug-hylo-module (&optional project-directory)
  "Run debugger on a Hylo module in the PROJECT-DIRECTORY.

If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors."
  (interactive
   (let ((default-project-directory (hylo-mode:find-hylo-project-directory)))
     (list
      (if current-prefix-arg
          (hylo-mode:read-project-directory default-project-directory)
        default-project-directory))))
  (setq project-directory
        (hylo-mode:ensure-hylo-project-directory project-directory))
  (if (string-equal (hylo-mode:read-module-type project-directory) "library")
      (hylo-mode:debug-hylo-module-library project-directory)
    (hylo-mode:debug-hylo-module-executable project-directory)))

(defun hylo-mode:find-ios-simulator-process ()
  "Return the process ID of an iOS simulator process if exists.

Return nil otherwise."
  (with-temp-buffer
    (hylo-mode:call-process "ps" "-x" "-o" "pid,comm")
    (goto-char (point-min))
    (if (search-forward-regexp
         " *\\([0-9]*\\) .*/Applications/Simulator.app/Contents/MacOS/Simulator"
         nil t)
        (string-to-number (match-string 1))
      nil)))

(defun hylo-mode:kill-ios-simulator ()
  "Kill an iOS simulator process if exists."
  (let ((process-identifier (hylo-mode:find-ios-simulator-process)))
    (when process-identifier
      (signal-process
       process-identifier
       'SIGTERM))))

(defun hylo-mode:open-ios-simulator (device-identifier)
  "Open an iOS simulator asynchronously with DEVICE-IDENTIFIER."
  (hylo-mode:call-process-async
   (hylo-mode:join-path
    (hylo-mode:locate-xcode)
    "Applications" "Simulator.app" "Contents" "MacOS" "Simulator")
   "-CurrentDeviceUDID" device-identifier))

(defun hylo-mode:wait-for-ios-simulator (device-identifier)
  "Wait until an iOS simulator with DEVICE-IDENTIFIER booted."
  (while (null (seq-find
                (lambda (device)
                  (and
                   (string-equal (cdr (assoc 'udid device)) device-identifier)
                   (string-equal (cdr (assoc 'state device)) "Booted")))
                (hylo-mode:list-ios-simulator-devices)))
    (sit-for 0.5)))

(defun hylo-mode:install-ios-app (device-identifier codesigning-folder-path)
  "Install an iOS app to an iOS simulator with DEVICE-IDENTIFIER.

CODESIGNING-FOLDER-PATH is the path of the app."
  (with-temp-buffer
    (unless (zerop (hylo-mode:call-process
                    hylo-mode:simulator-controller-executable
                    "install"
                    device-identifier
                    codesigning-folder-path))
      (error "%s: %s" "Cannot install app" (buffer-string)))))

(defun hylo-mode:launch-ios-app (device-identifier
                                  product-bundle-identifier
                                  &optional
                                  wait-for-debugger)
  "Launch an iOS app in DEVICE-IDENTIFIER.

PRODUCT-BUNDLE-IDENTIFIER is the product bundle identifier of the app.
If WAIT-FOR-DEBUGGER is non-nil, the new process is suspended until a debugger
attaches to it."
  (with-temp-buffer
    (unless (zerop (apply
                    #'hylo-mode:call-process
                    hylo-mode:simulator-controller-executable
                    (append
                     '("launch")
                     (if wait-for-debugger '("--wait-for-debugger") nil)
                     (list device-identifier product-bundle-identifier))))
      (error "%s: %s" "Cannot launch app" (buffer-string)))
    (goto-char (point-min))
    (search-forward-regexp ": \\([0-9]*\\)$")
    (string-to-number (match-string 1))))

(defun hylo-mode:search-process-stopped-message (process-identifier)
  "Find a message of process suspension in the comint output.

PROCESS-IDENTIFIER is the process ID."
  (let ((expected-output
         (concat "Process "
                 (number-to-string process-identifier)
                 " stopped")))
    (goto-char comint-last-input-end)
    (search-forward expected-output nil t)))

;;;###autoload
(defun hylo-mode:debug-ios-app-on-device (project-directory
                                           scheme
                                           codesigning-folder-path)
  "Run debugger on an iOS app in the PROJECT-DIRECTORY.
Run it for the iOS local device DEVICE-IDENTIFIER for the given SCHEME.
CODESIGNING-FOLDER-PATH is the path of the codesigning folder in Xcode
build settings."
  (hylo-mode:build-ios-app project-directory
                            hylo-mode:ios-local-device-identifier
                            scheme)
  (hylo-mode:run-repl
   (append
    (hylo-mode:command-string-to-list hylo-mode:ios-deploy-executable)
    (list "--debug" "--no-wifi" "--bundle" codesigning-folder-path))
   nil t))

;;;###autoload
(defun hylo-mode:debug-ios-app-on-simulator (project-directory
                                              device-identifier
                                              scheme
                                              codesigning-folder-path
                                              product-bundle-identifier)
  "Run debugger on an iOS app in the PROJECT-DIRECTORY.
Run it for the iOS simulator DEVICE-IDENTIFIER for the given SCHEME.
DEVICE-IDENTIFIER is the device identifier of the iOS simulator.
SCHEME is the name of the project scheme in Xcode.
CODESIGNING-FOLDER-PATH is the path of the codesigning folder used in Xcode
build settings.
PRODUCT-BUNDLE-IDENTIFIER is the name of the product bundle identifier used
in Xcode build settings."
  (hylo-mode:build-ios-app project-directory device-identifier scheme)
  (let* ((devices (hylo-mode:list-ios-simulator-devices))
         (target-device
          (seq-find
           (lambda (device)
             (string-equal (cdr (assoc 'udid device)) device-identifier))
           devices))
         (active-devices
          (seq-filter
           (lambda (device)
             (string-equal (cdr (assoc 'state device)) "Booted"))
           devices))
         (target-booted
          (string-equal (cdr (assoc 'state target-device)) "Booted"))
         (simulator-running (consp active-devices))
         (progress-reporter
          (make-progress-reporter "Waiting for simulator...")))
    (cond
     (target-booted
      ;; The target device is already booted. Does nothing.
      t)
     (simulator-running
      (hylo-mode:kill-ios-simulator)
      (hylo-mode:open-ios-simulator device-identifier))
     (t (hylo-mode:open-ios-simulator device-identifier)))

    (hylo-mode:wait-for-ios-simulator device-identifier)

    (progress-reporter-done progress-reporter)

    (let ((progress-reporter (make-progress-reporter "Installing app...")))
      (hylo-mode:install-ios-app device-identifier codesigning-folder-path)
      (progress-reporter-done progress-reporter))

    (let ((progress-reporter (make-progress-reporter "Launching app..."))
          (process-identifier
           (hylo-mode:launch-ios-app
            device-identifier product-bundle-identifier t)))
      (progress-reporter-done progress-reporter)
      (hylo-mode:run-repl
       (append
        (hylo-mode:command-string-to-list hylo-mode:debugger-executable)
        (list "--" codesigning-folder-path))
       nil t)
      (hylo-mode:enqueue-repl-commands
       "platform select ios-simulator"
       (concat "platform connect " device-identifier)
       (concat "process attach --pid " (number-to-string process-identifier))
       "breakpoint set --one-shot true --name UIApplicationMain"
       "cont"
       (cons
        (lambda (_string)
          (hylo-mode:search-process-stopped-message process-identifier))
        "repl")))))

;;;###autoload
(defun hylo-mode:debug-ios-app (&optional project-directory
                                           device-identifier
                                           scheme)
  "Run debugger on an iOS app in the PROJECT-DIRECTORY.
Run it for the iOS simulator device DEVICE-IDENTIFIER for the given SCHEME.
If PROJECT-DIRECTORY is nil or omitted, it is searched from `default-directory'
or its ancestors.
DEVICE-IDENTIFIER is the device identifier of the iOS simulator.  If it is
nil or omitted, the value of `hylo-mode:ios-device-identifier' is used.  If
it is equal to `hylo-mode:ios-local-device-identifier', a local build via
`ios-deploy' is generated instead.
SCHEME is the name of the project scheme in Xcode.  If it is nil or omitted,
the value of `hylo-mode:ios-project-scheme' is used."
  (interactive
   (let* ((default-project-directory
           (or
            (hylo-mode:find-xcode-workspace-directory)
            (hylo-mode:find-xcode-project-directory)))
          (project-directory
           (if current-prefix-arg
               (hylo-mode:read-project-directory default-project-directory)
             default-project-directory)))
     (list
      project-directory
      (if current-prefix-arg
          (hylo-mode:read-ios-device-identifier)
        hylo-mode:ios-device-identifier)
      (if current-prefix-arg
          (hylo-mode:read-project-scheme project-directory)
        hylo-mode:ios-project-scheme))))
  (setq project-directory
        (hylo-mode:ensure-xcode-project-directory project-directory))
  (unless device-identifier
    (setq device-identifier
          (or
           hylo-mode:ios-device-identifier
           (hylo-mode:read-ios-device-identifier))))
  (setq hylo-mode:ios-device-identifier device-identifier)
  (unless scheme
    (setq scheme
          (or
           hylo-mode:ios-project-scheme
           (hylo-mode:read-project-scheme project-directory))))
  (setq hylo-mode:ios-project-scheme scheme)
  (let* ((local-device-build (equal device-identifier
                                    hylo-mode:ios-local-device-identifier))
         (sdk (if local-device-build "iphoneos" "iphonesimulator"))
         (build-settings
          (hylo-mode:read-xcode-build-settings
           project-directory
           scheme
           sdk
           device-identifier))
         (codesigning-folder-path
          (cdr (assoc "CODESIGNING_FOLDER_PATH" build-settings)))
         (product-bundle-identifier
          (cdr (assoc "PRODUCT_BUNDLE_IDENTIFIER" build-settings))))
    (unless codesigning-folder-path
      (error "Cannot get codesigning folder path"))
    (unless product-bundle-identifier
      (error "Cannot get product bundle identifier"))

    (if local-device-build
        (hylo-mode:debug-ios-app-on-device project-directory
                                            scheme
                                            codesigning-folder-path)
      (hylo-mode:debug-ios-app-on-simulator project-directory
                                             device-identifier
                                             scheme
                                             codesigning-folder-path
                                             product-bundle-identifier))))

(provide 'hylo-mode-repl)

;;; hylo-mode-repl.el ends here
