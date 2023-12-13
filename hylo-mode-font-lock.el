;;; hylo-mode-font-lock.el --- Major-mode for the Hylo programming language, Font Locks. -*- lexical-binding: t -*-

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

;; Routines for Font Locks

;;; Code:

(require 'hylo-mode-standard-types)
(require 'seq)
(require 'subr-x)

;;; Customizations

;;;###autoload
(defgroup hylo-mode:faces nil
  "Font faces."
  :group 'hylo)

(defcustom hylo-mode:highlight-symbols-in-standard-library
  t
  "Highlight symbols in the standard library."
  :type 'boolean
  :safe #'booleanp)

(defcustom hylo-mode:highlight-symbols-in-foundation-framework
  t
  "Highlight symbols in the Foundation framework."
  :type 'boolean
  :safe #'booleanp)

(defface hylo-mode:constant-keyword-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for highlighting constant keywords.

That is, true, false, and nil.")

(defface hylo-mode:preprocessor-keyword-face
  '((t . (:inherit font-lock-preprocessor-face)))
  "Face for highlighting preprocessor keywords.

Example: #if, #endif, and #selector.")

(defface hylo-mode:keyword-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for highlighting keywords.")

(defface hylo-mode:builtin-method-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods with trailing closure.")

(defface hylo-mode:builtin-method-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods.")

(defface hylo-mode:builtin-function-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions with trailing closure.")

(defface hylo-mode:builtin-function-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions.")

(defface hylo-mode:builtin-property-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin properties.")

(defface hylo-mode:builtin-constant-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin constants.")

(defface hylo-mode:builtin-enum-case-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin enum cases.")

(defface hylo-mode:build-config-keyword-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting build configuration keywords.")

(defface hylo-mode:builtin-type-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin types.")

(defface hylo-mode:builtin-precedence-group-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin precedence groups.")

(defface hylo-mode:function-call-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for highlighting function calls.")

(defface hylo-mode:function-name-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for highlighting function names.")

(defface hylo-mode:property-access-face
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for highlighting property accesses.")

(defface hylo-mode:negation-char-face
  '((t . (:inherit font-lock-negation-char-face)))
  "Face for highlighting the negation char.")

(defun hylo-mode:make-set (list)
  "Return a hash where its keys are elements of the LIST.

All values are t."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (value list)
      (puthash value t hash))
    hash))

(defvar hylo-mode:standard-types-hash
  (hylo-mode:make-set hylo-mode:standard-types)
  "Set of standard type names.  All values are t.")

(defvar hylo-mode:standard-enum-cases-hash
  (hylo-mode:make-set hylo-mode:standard-enum-cases)
  "Set of standard enum case names.  All values are t.")

(defvar hylo-mode:standard-methods-hash
  (hylo-mode:make-set hylo-mode:standard-methods)
  "Set of standard method names.  All values are t.")

(defvar hylo-mode:standard-properties-hash
  (hylo-mode:make-set hylo-mode:standard-properties)
  "Set of standard property names.  All values are t.")

(defvar hylo-mode:standard-functions-hash
  (hylo-mode:make-set hylo-mode:standard-functions)
  "Set of standard function names.  All values are t.")

(defvar hylo-mode:standard-constants-hash
  (hylo-mode:make-set hylo-mode:standard-constants)
  "Set of standard constant names.  All values are t.")

(defvar hylo-mode:foundation-types-hash
  (hylo-mode:make-set hylo-mode:foundation-types)
  "Set of Foundation type names.  All values are t.")

(defvar hylo-mode:foundation-enum-cases-hash
  (hylo-mode:make-set hylo-mode:foundation-enum-cases)
  "Set of Foundation enum case names.  All values are t.")

(defvar hylo-mode:foundation-methods-hash
  (hylo-mode:make-set hylo-mode:foundation-methods)
  "Set of Foundation method names.  All values are t.")

(defvar hylo-mode:foundation-properties-hash
  (hylo-mode:make-set hylo-mode:foundation-properties)
  "Set of Foundation property names.  All values are t.")

(defvar hylo-mode:foundation-functions-hash
  (hylo-mode:make-set hylo-mode:foundation-functions)
  "Set of Foundation function names.  All values are t.")

(defvar hylo-mode:foundation-constants-hash
  (hylo-mode:make-set hylo-mode:foundation-constants)
  "Set of Foundation constant names.  All values are t.")


;;; Supporting functions

(defun hylo-mode:declared-function-name-pos-p (pos limit)
  "Return t if POS is just before the name of a function declaration.

This function does not search beyond LIMIT."
  (goto-char pos)
  (forward-comment (- (point)))
  (skip-syntax-backward "w_")
  (and
   (< (point) limit)
   (looking-at
    (concat
     "\\<\\("
     (string-join
      '("fun" "enum" "struct" "class" "protocol" "extension" "actor" "macro")
      "\\|")
     "\\)\\>"))))

(defun hylo-mode:property-access-pos-p (pos limit)
  "Return t if POS is just before the property name of a member expression.

This function does not search beyond LIMIT."
  ;; foo.bar    // property access
  ;; foo .bar   // property access
  ;; foo . bar  // INVALID
  ;; foo. bar   // INVALID
  ;; foo?.bar   // property access
  ;; foo?. bar  // INVALID
  ;; foo ?.bar  // INVALID, but highlight as a property access anyway
  ;; foo? .bar  // property access
  ;; foo.bar()  // NOT property access
  ;; foo.bar () // NOT property access
  ;; foo.1      // property access
  ;; foo1.1     // property access
  ;; 1.1        // NOT property access
  ;; .1         // NOT property access
  ;; $1.1       // property access
  ;; .x         // property access
  (and
   ;; Just after dot
   (progn
     (goto-char pos)
     (eq (char-before) ?.))

   ;; Not floating-point literal
   (progn
     (goto-char pos)
     (backward-char)
     (skip-syntax-backward "w_")
     (not (looking-at "[0-9]*\\.[0-9]+\\>")))

   ;; Not method/function call
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     ;; I don't sure we can use `forward-comment' beyond limit, so assuming
     ;; no comments here.
     (skip-syntax-forward " " limit)
     (not (eq (char-after) ?\()))))

(defun hylo-mode:builtin-name-pos-p (names pos limit)
  "Return t if an identifier in the hash NAMES appears at POS.

This function does not search beyond LIMIT."
  (goto-char pos)
  (skip-syntax-forward "w_" limit)
  (gethash (buffer-substring-no-properties pos (point)) names))

(defun hylo-mode:builtin-type-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin type name in NAMES.

This function does not search beyond LIMIT."
  (hylo-mode:builtin-name-pos-p names pos limit))

(defun hylo-mode:builtin-enum-case-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin enum case name in NAMES.

This function does not search beyond LIMIT."
  (and
   (eq (char-before pos) ?.)
   (hylo-mode:builtin-name-pos-p names pos limit)))

(defun hylo-mode:builtin-method-trailing-closure-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin method name in NAMES.

It must followed by open curly bracket.
This function does not search beyond LIMIT."
  (and
   (eq (char-before pos) ?.)
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?{))
   (hylo-mode:builtin-name-pos-p names pos limit)))

(defun hylo-mode:builtin-method-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin method name in NAMES.

This function does not search beyond LIMIT."
  (and
   (eq (char-before pos) ?.)
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?\())
   (hylo-mode:builtin-name-pos-p names pos limit)))

(defun hylo-mode:builtin-property-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin property name in NAMES.

This function does not search beyond LIMIT."
  (and
   (hylo-mode:property-access-pos-p pos limit)
   (hylo-mode:builtin-name-pos-p names pos limit)))

(defun hylo-mode:builtin-function-trailing-closure-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin function name in NAMES.

It must followed by open curly bracket.
This function does not search beyond LIMIT."
  (and
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?{))
   (hylo-mode:builtin-name-pos-p names pos limit)))

(defun hylo-mode:builtin-function-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin function name in NAMES.

This function does not search beyond LIMIT."
  (and
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?\())
   (hylo-mode:builtin-name-pos-p names pos limit)))

(defun hylo-mode:builtin-constant-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin constant name in NAMES.

This function does not search beyond LIMIT."
  (hylo-mode:builtin-name-pos-p names pos limit))

(defun hylo-mode:font-lock-match-expr (limit match-p)
  "Move the cursor just after an identifier that satisfy given predicate.

Set `match-data', and return t if the identifier found before position LIMIT.
Return nil otherwise.

The predicate MATCH-P is called with two arguments:
- the position of the identifier, and
- the limit of search functions."
  (let ((result nil))
    (while (and
            (< (point) limit)
            (not result)
            (re-search-forward "\\<\\(\\sw\\|\\s_\\)+\\>" limit t))
      (when (save-excursion
              (save-match-data
                (funcall match-p (match-beginning 0) limit)))
        (setq result t)))
    result))

(defun hylo-mode:font-lock-match-declared-function-names (limit)
  "Move the cursor just after a function name or others.

Others includes enum, struct, class, protocol, and extension name.
Set `match-data', and return t if a function name or others found before
position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-expr
   limit #'hylo-mode:declared-function-name-pos-p))

(defun hylo-mode:font-lock-match-property-access (limit)
  "Move the cursor just after a property access.
Set `match-data', and return t if a property access found before position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-expr limit #'hylo-mode:property-access-pos-p))

(defmacro hylo-mode:font-lock-match-builtin-names (f limit &rest list-of-sets)
  "Move the cursor just after a builtin name.

Function F takes set of names, position, and limit.

Set `match-data', and return t if a builtin name found before position LIMIT.
Return nil otherwise.

LIST-OF-SETS is a list of set of names."
  (let ((pos (make-symbol "pos"))
        (limit2 (make-symbol "limit"))
        (matched (make-symbol "matched"))
        (names (make-symbol "names")))
    `(hylo-mode:font-lock-match-expr
      ,limit
      (lambda (,pos ,limit2)
        (seq-reduce
         (lambda (,matched ,names)
           (or ,matched
               (and ,names
                    (funcall ,f ,names ,pos ,limit2))))
         (list ,@list-of-sets)
         nil)))))

(defun hylo-mode:font-lock-match-builtin-type-names (limit)
  "Move the cursor just after a builtin type name.

Set `match-data', and return t if a builtin type name found before position
LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-type-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-types-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-types-hash)))

(defun hylo-mode:font-lock-match-builtin-enum-case-names (limit)
  "Move the cursor just after a builtin enum case name.

Set `match-data', and return t if a builtin enum case name found before
position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-enum-case-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-enum-cases-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-enum-cases-hash)))

(defun hylo-mode:font-lock-match-builtin-method-trailing-closure-names (limit)
  "Move the cursor just after a builtin method name with trailing closure.

Set `match-data', and return t if a builtin method name found before position
LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-method-trailing-closure-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-methods-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-methods-hash)))

(defun hylo-mode:font-lock-match-builtin-method-names (limit)
  "Move the cursor just after a builtin method name.

Set `match-data', and return t if a builtin method name found before
position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-method-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-methods-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-methods-hash)))

(defun hylo-mode:font-lock-match-builtin-property-names (limit)
  "Move the cursor just after a builtin property name.

Set `match-data', and return t if a builtin property name found before
position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-property-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-properties-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-properties-hash)))

(defun hylo-mode:font-lock-match-builtin-function-trailing-closure-names
    (limit)
  "Move the cursor just after a builtin function name with trailing closure.

Set `match-data', and return t if a builtin function name found before
position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-function-trailing-closure-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-functions-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-functions-hash)))

(defun hylo-mode:font-lock-match-builtin-function-names (limit)
  "Move the cursor just after a builtin function name.

Set `match-data', and return t if a builtin function name found before
position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-function-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-functions-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-functions-hash)))

(defun hylo-mode:font-lock-match-builtin-constant-names (limit)
  "Move the cursor just after a builtin constant name.

Set `match-data', and return t if a builtin constant name found before
position LIMIT.
Return nil otherwise."
  (hylo-mode:font-lock-match-builtin-names
   #'hylo-mode:builtin-constant-name-pos-p
   limit
   (and hylo-mode:highlight-symbols-in-standard-library
        hylo-mode:standard-constants-hash)
   (and hylo-mode:highlight-symbols-in-foundation-framework
        hylo-mode:foundation-constants-hash)))

;;; Keywords and standard identifiers

;; Keywords
;; https://developer.apple.com/library/ios/documentation/Hylo/Conceptual/Hylo_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410

(defconst hylo-mode:constant-keywords
  '("true" "false" "nil")
  "Keywords used as constants.")

(defconst hylo-mode:declaration-keywords
  '("associatedtype" "class" "deinit" "enum" "extension" "fileprivate" "fun"
    "import" "init" "inout" "internal" "let" "open" "operator" "package"
    "private" "protocol" "public" "any" "some" "static" "struct" "subscript"
    "typealias" "var" "actor" "nonisolated" "isolated" "distributed"
    "borrowing" "consuming" "macro")
  "Keywords used in declarations.")

(defconst hylo-mode:statement-keywords
  '("break" "case" "continue" "default" "defer" "do" "else" "fallthrough" "for"
    "guard" "if" "in" "repeat" "return" "switch" "where" "while")
  "Keywords used in statements.")

(defconst hylo-mode:expression-keywords
  '("as" "catch" "dynamicType" "is" "rethrows" "super" "self" "Self" "throws"
    "throw" "try" "async" "await" "consume" "copy" "discard" "each")
  "Keywords used in expressions and types.

Excludes true, false, and keywords begin with a number sign.")

(defconst hylo-mode:context-keywords
  '("Protocol" "Type" "and" "assignment" "associativity" "convenience" "didSet"
    "dynamic" "final" "get" "higherThan" "indirect" "infix" "lazy" "left"
    "lowerThan" "mutating" "none" "nonmutating" "optional" "override" "postfix"
    "precedence" "precedencegroup" "prefix" "required" "right" "set" "unowned"
    "weak" "willSet")
  "Keywords reserved in particular contexts.")

(defconst hylo-mode:build-config-keywords
  '("os" "arch" "hylo" "compiler" "canImport" "targetEnvironment"
    "i386" "x86_64" "arm" "arm64"
    "OSX" "OSXApplicationExtension"
    "macOS" "macOSApplicationExtension"
    "iOS" "iOSApplicationExtension"
    "watchOS" "watchOSApplicationExtension"
    "tvOS" "tvOSApplicationExtension"
    "macCatalyst" "macCatalystApplicationExtension"
    "Linux" "Windows"
    "simulator" "unavailable" "noasync"
    "hasFeature" "hasAttribute" "before" "introduced" "deprecated" "obsoleted"
    "message" "renamed")
  "Keywords for build configuration statements.")

(defconst hylo-mode:standard-precedence-groups
  '("AssignmentPrecedence"
    "FunctionArrowPrecedence"
    "TernaryPrecedence"
    "DefaultPrecedence"
    "LogicalDisjunctionPrecedence"
    "LogicalConjunctionPrecedence"
    "ComparisonPrecedence"
    "NilCoalescingPrecedence"
    "CastingPrecedence"
    "RangeFormationPrecedence"
    "AdditionPrecedence"
    "MultiplicationPrecedence"
    "BitwiseShiftPrecedence")
  "Precedence groups in the standard library.")

;;; font-lock definition

(defconst hylo-mode:font-lock-keywords
  `(
    ;; Attributes
    "@\\(\\sw\\|\\s_\\)*"

    (,(regexp-opt hylo-mode:constant-keywords 'words)
     .
     'hylo-mode:constant-keyword-face)

    ;; Preprocessor keywords
    (,"#\\(\\sw\\|\\s_\\)*"
     .
     'hylo-mode:preprocessor-keyword-face)

    (,(regexp-opt (append hylo-mode:declaration-keywords
                          hylo-mode:statement-keywords
                          hylo-mode:expression-keywords
                          hylo-mode:context-keywords)
                  'words)
     .
     'hylo-mode:keyword-face)

    (hylo-mode:font-lock-match-builtin-type-names
     .
     'hylo-mode:builtin-type-face)

    (hylo-mode:font-lock-match-builtin-enum-case-names
     .
     'hylo-mode:builtin-enum-case-face)

    (hylo-mode:font-lock-match-builtin-method-trailing-closure-names
     .
     'hylo-mode:builtin-method-trailing-closure-face)

    (hylo-mode:font-lock-match-builtin-method-names
     .
     'hylo-mode:builtin-method-face)

    (hylo-mode:font-lock-match-builtin-property-names
     .
     'hylo-mode:builtin-property-face)

    (hylo-mode:font-lock-match-builtin-function-trailing-closure-names
     .
     'hylo-mode:builtin-function-trailing-closure-face)

    (hylo-mode:font-lock-match-builtin-function-names
     .
     'hylo-mode:builtin-function-face)

    (hylo-mode:font-lock-match-builtin-constant-names
     .
     'hylo-mode:builtin-constant-face)

    (,(regexp-opt hylo-mode:build-config-keywords 'words)
     .
     'hylo-mode:build-config-keyword-face)

    (,(concat "\\<"
              (regexp-opt hylo-mode:standard-precedence-groups 'non-nil)
              "\\>")
     .
     'hylo-mode:builtin-precedence-group-face)

    ;; Function and type declarations
    (hylo-mode:font-lock-match-declared-function-names
     .
     'hylo-mode:function-name-face)

    ;; Method/function calls
    ("\\<\\(\\(\\sw\\|\\s_\\)+\\)\\>\\??\\s-*("
     1
     'hylo-mode:function-call-face)

    ;; Property accesses
    (hylo-mode:font-lock-match-property-access
     .
     'hylo-mode:property-access-face)

    ;; Make negation chars easier to see
    ("\\(?:^\\|\\s-\\|\\s(\\|\\s>\\|[,:;]\\)\\(!+\\)[^=]"
     1
     'hylo-mode:negation-char-face))
  "Hylo mode keywords for Font Lock.")


(provide 'hylo-mode-font-lock)

;;; hylo-mode-font-lock.el ends here
