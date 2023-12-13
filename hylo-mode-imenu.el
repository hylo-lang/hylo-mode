;;; hylo-mode-imenu.el --- Major-mode for the Hylo programming language, , Imenu -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 taku0

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

;; List declarations for Imenu

;;; Code:

(require 'hylo-mode-lexer)
(require 'hylo-mode-indent)
(require 'seq)

;;;###autoload
(defgroup hylo-mode:imenu nil
  "Imenu."
  :group 'hylo)

(defcustom hylo-mode:imenu-style
  'nested
  "Style of Imenu hierarchy.

Values:

- `nested': Class and its members are organized as trees.
- `flat': Organized into a flat list of fully qualified names."
  :type '(choice (const :tag "Nested" nested)
                 (const :tag "Flat" flat))
  :safe #'symbolp)

(defun hylo-mode:declaration (type name-token children)
  "Construct and return a declaration.

TYPE is the type of the declaration such as `class' or `struct'.
NAME-TOKEN is the name token of the declaration.  For declarations like `init',
it is the keyword token itself.
CHILDREN is the child declarations if exists."
  (list type name-token children))

(defun hylo-mode:declaration:type (declaration)
  "Return the type of DECLARATION."
  (nth 0 declaration))

(defun hylo-mode:declaration:name-token (declaration)
  "Return the name token of DECLARATION."
  (nth 1 declaration))

(defun hylo-mode:declaration:children (declaration)
  "Return the children of DECLARATION."
  (nth 2 declaration))


(defun hylo-mode:imenu-create-index (&optional style)
  "Create an index alist of the current buffer for Imenu.

STYLE is either `nested' or `flat', defaults to `nested'.
If it is `nested', class and its members are organized as trees.
If it is `flat', declarations are organized into a flat list of fully qualified
names."
  (unless style (setq style hylo-mode:imenu-style))
  (save-excursion
    (goto-char (point-min))

    (let ((declarations '())
          (customization-item (list
                               "*Customize*"
                               0
                               (lambda (_name _position)
                                 (customize-group 'hylo-mode:imenu)))))
      (while (not (eq (hylo-mode:token:type
                       (save-excursion (hylo-mode:forward-token)))
                      'outside-of-buffer))
        (setq declarations
              (append (hylo-mode:scan-declarations) declarations)))
      (append (if (eq style 'flat)
                  (hylo-mode:format-for-imenu:flat (nreverse declarations))
                (hylo-mode:format-for-imenu:nested (nreverse declarations)))
              (list customization-item)))))

(defun hylo-mode:scan-declarations ()
  "Scan declarations from current point.

Return found declarations in reverse order."
  (let (next-token
        next-type
        next-text
        name-token
        last-class-token
        (done nil)
        (declarations '()))
    (while (not done)
      (setq next-token
            (hylo-mode:forward-token-or-list-except-curly-bracket))
      (when (and (eq (hylo-mode:token:type next-token) 'implicit-\;)
                 (save-excursion
                   (hylo-mode:skip-whitespaces)
                   (eq (char-after) ?\{)))
        (setq next-token (hylo-mode:forward-token)))
      (setq next-type (hylo-mode:token:type next-token))
      (setq next-text (hylo-mode:token:text next-token))
      (cond
       ((equal next-text "import")
        ;; Skips an import kind, for example, "class" token below:
        ;;
        ;; import class Foo.Bar
        (hylo-mode:forward-token-or-list-except-curly-bracket))

       ((equal next-text "class")
        ;; "class" token may be either a class declaration keyword or a
        ;; modifier:
        ;;
        ;; // Nested class named "final"
        ;; class Foo { class final {} }
        ;;
        ;; // Non-overridable class method named "foo"
        ;; class Foo { class final func foo() {} }
        ;;
        ;; So delays until "{" token.
        (setq last-class-token next-token))

       ((memq next-type '(\; implicit-\; { } outside-of-buffer))
        (when (memq next-type '(} outside-of-buffer))
          (setq done t))
        (cond
         ;; Having pending "class" token
         (last-class-token
          (save-excursion
            (goto-char (hylo-mode:token:end last-class-token))
            (setq name-token (hylo-mode:forward-token)))
          (setq last-class-token nil)
          (when (eq (hylo-mode:token:type name-token) 'identifier)
            (push
             (hylo-mode:declaration
              'class
              name-token
              (when (eq next-type '{)
                (nreverse (hylo-mode:scan-declarations))))
             declarations)))

         ;; Closure or other unknown block
         ((eq next-type '{)
          (goto-char (hylo-mode:token:start next-token))
          (hylo-mode:forward-token-or-list))

         ;; Ignores the token otherwise.
         ))

       ((member next-text '("struct" "protocol" "extension" "enum" "actor"))
        (setq last-class-token nil)
        (let ((declaration
               (hylo-mode:scan-declarations:handle-struct-like next-token)))
          (when declaration
            (push declaration declarations))))

       ((equal next-text "case")
        (setq last-class-token nil)
        (let ((case-declarations
               (hylo-mode:scan-declarations:handle-case-or-variable 'case)))
          (setq declarations (append case-declarations declarations))))

       ((member next-text '("typealias" "associatedtype"))
        (setq last-class-token nil)
        (setq name-token
              (hylo-mode:forward-token-or-list-except-curly-bracket))
        (when (eq (hylo-mode:token:type name-token) 'identifier)
          (push
           (hylo-mode:declaration (intern next-text) name-token nil)
           declarations)))

       ((member next-text '("func" "init" "subscript" "macro"))
        (setq last-class-token nil)
        (unless (member next-text '("func" "macro"))
          (goto-char (hylo-mode:token:start next-token)))
        (let ((declaration-type (intern next-text))
              (names (hylo-mode:scan-function-name-and-parameter-names)))
          (when names
            (setq name-token (car names))
            (push
             (hylo-mode:declaration
              declaration-type
              (hylo-mode:token
               (hylo-mode:token:type name-token)
               (concat (hylo-mode:token:text name-token)
                       "("
                       (mapconcat
                        (lambda (token)
                          (concat (hylo-mode:token:text token) ":"))
                        (cdr names)
                        "")
                       ")")
               (hylo-mode:token:start name-token)
               (hylo-mode:token:end name-token))
              nil)
             declarations))))

       ((equal next-text "deinit")
        (setq last-class-token nil)
        (push (hylo-mode:declaration 'deinit next-token nil) declarations))

       ((member next-text '("let" "var"))
        (setq last-class-token nil)
        (let ((variable-declarations
               (hylo-mode:scan-declarations:handle-case-or-variable
                (intern next-text))))
          (setq declarations (append variable-declarations declarations))))

       ((member next-text '("prefix" "postfix" "infix"))
        (setq last-class-token nil)
        (setq next-token
              (hylo-mode:forward-token-or-list-except-curly-bracket))
        (when (equal (hylo-mode:token:text next-token) "operator")
          (setq name-token
                (hylo-mode:forward-token-or-list-except-curly-bracket))
          (when (eq (hylo-mode:token:type name-token) 'identifier)
            (push
             (hylo-mode:declaration 'operator name-token nil)
             declarations))))

       ((equal next-text "precedencegroup")
        (setq last-class-token nil)
        (setq name-token
              (hylo-mode:forward-token-or-list-except-curly-bracket))
        (when (eq (hylo-mode:token:type name-token) 'identifier)
          (push
           (hylo-mode:declaration 'precedencegroup name-token nil)
           declarations)))))
    declarations))

(defun hylo-mode:forward-token-or-list-except-curly-bracket ()
  "Move point to the end position of the next token or list.

Curly brackets are not regarded as a list.
Return the token skipped."
  (let ((next-token (hylo-mode:forward-token)))
    (if (or (memq (hylo-mode:token:type next-token) '(\( \[))
            (equal (hylo-mode:token:text next-token) "<"))
        (progn
          (goto-char (hylo-mode:token:start next-token))
          (hylo-mode:forward-token-or-list))
      next-token)))

(defun hylo-mode:scan-declarations:handle-struct-like (keyword-token)
  "Parse struct-like declaration.

Return a declaration if it have a name.  Return nil otherwise.
KEYWORD-TOKEN is the keyword beginning the declaration like \"struct\" or
\"enum\"."
  (let (next-token
        (name-token (hylo-mode:forward-token)))
    (when (eq (hylo-mode:token:type name-token) 'identifier)
      (while (progn
               (setq next-token
                     (hylo-mode:forward-token-or-list-except-curly-bracket))
               (not (memq (hylo-mode:token:type next-token)
                          '(\; implicit-\; { } outside-of-buffer)))))
      (when (and (eq (hylo-mode:token:type next-token) 'implicit-\;)
                 (save-excursion
                   (hylo-mode:skip-whitespaces)
                   (eq (char-after) ?\{)))
        (setq next-token (hylo-mode:forward-token)))
      (hylo-mode:declaration
       (intern (hylo-mode:token:text keyword-token))
       name-token
       (when (eq (hylo-mode:token:type next-token) '{)
         (nreverse (hylo-mode:scan-declarations)))))))

(defun hylo-mode:scan-declarations:handle-case-or-variable (type)
  "Parse enum-case, let, or var.

Return a list of declarations.
TYPE is one of `case', `let', or `var'."
  ;; case A, B(String), C
  ;; case A, B = 2, C
  ;;
  ;; let x = 1,
  ;;     y = 2,
  ;;     z = 3
  ;;
  ;; var x {
  ;;   get {
  ;;     return 1
  ;;   }
  ;; }
  ;;
  ;; var x {
  ;;   willSet {
  ;;   }
  ;; }
  ;;
  ;; let (x, y) = (1, 2) // not supported yet
  (let (next-token
        (items '()))
    (while
        (progn
          (setq next-token (hylo-mode:forward-token-or-list))
          (when (eq (hylo-mode:token:type next-token) 'identifier)
            (push (hylo-mode:declaration type next-token nil) items))
          (while
              (progn
                (setq next-token (hylo-mode:forward-token-or-list))
                (not (memq (hylo-mode:token:type next-token)
                           '(\, \; implicit-\; } outside-of-buffer)))))
          (eq (hylo-mode:token:type next-token) '\,)))
    (when (eq (hylo-mode:token:type next-token) '})
      (goto-char (hylo-mode:token:start next-token)))
    items))

(defun hylo-mode:scan-function-name-and-parameter-names ()
  "Parse function/macro name and parameter names.

The point is assumed to be before a function/macro name.

Return tokens of function/macro names and parameter names.

For example, given the following code, this return tokens \"foo\", \"a\",
and \"c\".

  func foo(a b: Int, c: Int)"
  (let* ((name-token
          (hylo-mode:forward-token-or-list-except-curly-bracket))
         next-token
         parameter-end
         (parameter-names '())
         (seq-contains-p (if (fboundp 'seq-contains-p)
                             'seq-contains-p
                           'seq-contains))
         (is-operator
          (funcall seq-contains-p
                   "/=-+!*%<>&|^~?."
                   (elt (hylo-mode:token:text name-token) 0))))
    (cond
     ((eq (hylo-mode:token:type name-token) 'identifier)
      (while (progn
               (setq next-token
                     (hylo-mode:forward-token-or-list-except-curly-bracket))
               (not (memq (hylo-mode:token:type next-token)
                          '(\(\) \( { \; implicit-\; outside-of-buffer)))))
      (if (eq (hylo-mode:token:type next-token) '\(\))
          (progn
            (setq parameter-end (hylo-mode:token:end next-token))
            (goto-char (hylo-mode:token:start next-token))
            (hylo-mode:forward-token)

            (while (< (point) parameter-end)
              (setq next-token (hylo-mode:forward-token))

              (when (eq (hylo-mode:token:type next-token) 'identifier)
                (when (or is-operator
                          (and (equal (hylo-mode:token:text name-token)
                                      "subscript")
                               (eq (hylo-mode:token:type
                                    (hylo-mode:forward-token-or-list))
                                   ':)))
                  (setq next-token (hylo-mode:token
                                    'identifier
                                    "_"
                                    (hylo-mode:token:start next-token)
                                    (hylo-mode:token:end next-token))))
                (push next-token parameter-names))

              (while (and (< (point) parameter-end)
                          (not (eq (hylo-mode:token:type next-token) '\,)))
                (setq next-token (hylo-mode:forward-token-or-list))))
            (cons name-token (reverse parameter-names)))
        (list name-token)))
     (t nil))))

(defun hylo-mode:format-for-imenu:flat (declarations)
  "Convert list of DECLARATIONS to alist for `imenu--index-alist'.

Declarations are organized as trees."
  (seq-mapcat
   (lambda (declaration)
     (let* ((name-token (hylo-mode:declaration:name-token declaration))
            (name (hylo-mode:token:text name-token))
            (position (hylo-mode:token:start name-token))
            (children (hylo-mode:declaration:children declaration)))
       (cons
        (cons name position)
        (mapcar
         (lambda (pair)
           (cons (concat name "." (car pair)) (cdr pair)))
         (hylo-mode:format-for-imenu:flat children)))))
   declarations))

(defun hylo-mode:format-for-imenu:nested (declarations)
  "Convert list of DECLARATIONS to alist for `imenu--index-alist'.

Declarations are organized as a flat list of fully qualified names."
  (mapcar
   (lambda (declaration)
     (let* ((name-token (hylo-mode:declaration:name-token declaration))
            (name (hylo-mode:token:text name-token))
            (position (hylo-mode:token:start name-token))
            (children (hylo-mode:declaration:children declaration)))
       (if children
           (cons name (cons (cons "self"  position)
                            (hylo-mode:format-for-imenu:nested children)))
         (cons name position))))
   declarations))

(provide 'hylo-mode-imenu)

;;; hylo-mode-imenu.el ends here
