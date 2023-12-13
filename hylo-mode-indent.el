;;; hylo-mode-indent.el --- Major-mode for the Hylo programming language, indentation. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021 taku0, Chris Barrett, Bozhidar Batsov,
;;                         Arthur Evstifeev

;; Author: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>

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

;; Routines for Indentation

;;; Code:

(require 'hylo-mode-lexer)

;;; Customizations

(defcustom hylo-mode:basic-offset 4
  "Amount of indentation for block contents."
  :type 'integer
  :group 'hylo
  :safe #'integerp)

(defcustom hylo-mode:parenthesized-expression-offset 2
  "Amount of indentation inside parentheses and square brackets."
  :type 'integer
  :group 'hylo
  :safe #'integerp)

(defcustom hylo-mode:multiline-statement-offset 2
  "Amount of indentation for continuations of expressions."
  :type 'integer
  :group 'hylo
  :safe #'integerp)

(defcustom hylo-mode:switch-case-offset 0
  "Amount of indentation for case labels in switch statements."
  :type 'integer
  :group 'hylo
  :safe #'integerp)

(defcustom hylo-mode:prepend-asterisk-to-comment-line nil
  "Automatically insert a asterisk to each comment line if non-nil."
  :type 'boolean
  :group 'hylo
  :safe #'booleanp)

(defcustom hylo-mode:insert-space-after-asterisk-in-comment t
  "Automatically insert a space after asterisk in comment if non-nil."
  :type 'boolean
  :group 'hylo
  :safe #'booleanp)

(defcustom hylo-mode:auto-close-multiline-comment t
  "If non-nil, `indent-new-comment-line' automatically close multiline comment."
  :type 'boolean
  :group 'hylo
  :safe #'booleanp)

(defcustom hylo-mode:fix-comment-close t
  "Fix \"* /\" in incomplete multiline comment to \"*/\" if non-nil."
  :type 'boolean
  :group 'hylo
  :safe #'booleanp)

(defcustom hylo-mode:break-line-before-comment-close t
  "If non-nil, break line before the closing delimiter of multiline comments."
  :type 'boolean
  :group 'hylo
  :safe #'booleanp)

(defcustom hylo-mode:highlight-anchor nil
  "Highlight anchor point for indentation if non-nil.

Intended for debugging."
  :type 'boolean
  :group 'hylo
  :safe #'booleanp)

;;; Constants and variables

(defconst hylo-mode:statement-parent-tokens
  '(implicit-\; \; case-: { anonymous-function-parameter-in)
  "Parent tokens for statements.")

(defconst hylo-mode:expression-parent-tokens
  (append hylo-mode:statement-parent-tokens
          '(\, < \( \[ supertype-: "where" "if" "guard" "while" "for" "catch"
            string-chunk-before-interpolated-expression))
  "Parent tokens for expressions.")

(defvar-local hylo-mode:anchor-overlay nil)
(defvar-local hylo-mode:anchor-overlay-timer nil)

;;; Indentation type

(defun hylo-mode:indentation (point offset)
  "Construct and return a indentation.

POINT is the position of the anchor point, such as the start of the previous
line or the start of the type declaration.
OFFSET is the offset from the anchor point.  For example, when indenting the
first line of a type body, its anchor point is the start of the type
declaration and its offset is `hylo-mode:basic-offset'."
  (list point offset))

(defun hylo-mode:indentation:point (indentation)
  "Return the point of INDENTATION."
  (nth 0 indentation))

(defun hylo-mode:indentation:offset (indentation)
  "Return the offset of INDENTATION."
  (nth 1 indentation))

;;; Indentation logics

(defun hylo-mode:indent-line ()
  "Indent the current line."
  (let* ((indentation (save-excursion (hylo-mode:calculate-indent)))
         (indentation-column
          (save-excursion
            (goto-char (hylo-mode:indentation:point indentation))
            (+ (current-column) (hylo-mode:indentation:offset indentation))))
         (current-indent
          (save-excursion (back-to-indentation) (current-column))))
    (if (<= (current-column) current-indent)
        ;; The cursor is on the left margin. Moving to the new indent.
        (indent-line-to indentation-column)
      ;; Keeps current relative position.
      (save-excursion (indent-line-to indentation-column)))
    (when hylo-mode:highlight-anchor
      (hylo-mode:highlight-anchor indentation))))

(defun hylo-mode:calculate-indent ()
  "Return the indentation of the current line."
  (back-to-indentation)
  (let ((parser-state (syntax-ppss)))
    (cond
     ((nth 4 parser-state)
      ;; If the 4th element of `(syntax-ppss)' is non-nil, the cursor is on
      ;; the 2nd or following lines of a multiline comment, because:
      ;;
      ;; - The 4th element of `(syntax-ppss)' is nil on the comment starter.
      ;; - We have called `back-to-indentation`.
      (hylo-mode:calculate-indent-of-multiline-comment))

     ((eq (nth 3 parser-state) t)
      (hylo-mode:calculate-indent-of-multiline-string))

     ((looking-at "//")
      (hylo-mode:calculate-indent-of-single-line-comment))

     (t
      (hylo-mode:calculate-indent-of-code)))))

(defun hylo-mode:calculate-indent-of-multiline-comment ()
  "Return the indentation of the current line inside a multiline comment."
  (back-to-indentation)
  (let ((comment-beginning-position (nth 8 (syntax-ppss)))
        (starts-with-asterisk (eq (char-after) ?*)))
    (forward-line -1)
    (back-to-indentation)
    (cond
     ((<= (point) comment-beginning-position)
      ;; The cursor was on the 2nd line of the comment.
      (goto-char comment-beginning-position)
      (forward-char)
      ;; If there are extra characters or spaces after asterisks, aligns with
      ;; the first non-space character or end of line.  Otherwise, aligns with
      ;; the first asterisk.
      (when (and
             (looking-at "\\**[^*\n]+")
             (not (and hylo-mode:prepend-asterisk-to-comment-line
                       starts-with-asterisk)))
        (skip-chars-forward "*")
        (skip-syntax-forward " "))
      (hylo-mode:indentation (point) 0))

     ;; The cursor was on the 3rd or following lines of the comment.

     ((= (save-excursion
           (forward-line)
           (back-to-indentation)
           (point))
         (save-excursion
           (goto-char comment-beginning-position)
           (if (forward-comment 1)
               (progn
                 (backward-char)
                 (skip-chars-backward "*")
                 (point))
             -1)))
      ;; Before the closing delimiter.  Aligns with the first asterisk of the
      ;; opening delimiter.
      (goto-char comment-beginning-position)
      (forward-char)
      (hylo-mode:indentation (point) 0))

     ;; Otherwise, aligns with a non-empty preceding line.

     ((and (bolp) (eolp))
      ;; The previous line is empty, so seeks a non-empty-line.
      (hylo-mode:calculate-indent-of-multiline-comment))

     (t
      ;; The previous line is not empty, so aligns to this line.
      (hylo-mode:indentation (point) 0)))))

(defun hylo-mode:calculate-indent-of-multiline-string ()
  "Return the indentation of the current line inside a multiline string.

Also used for regexes."
  (back-to-indentation)
  (let ((string-beginning-position
         (save-excursion (hylo-mode:beginning-of-string))))
    (if (and (looking-at "\\(\"\"\"\\|/\\)#*")
             (equal (get-text-property (1- (match-end 0)) 'syntax-table)
                    (string-to-syntax "|")))
        ;; The last line.
        (progn
          (goto-char string-beginning-position)
          (hylo-mode:calculate-indent-of-expression
           hylo-mode:multiline-statement-offset))
      (forward-line 0)
      (backward-char)
      (hylo-mode:goto-non-interpolated-expression-bol)
      (back-to-indentation)
      (if (<= (point) string-beginning-position)
          ;; The cursor was on the 2nd line of the string, so aligns with
          ;; that line with offset.
          (progn
            (goto-char string-beginning-position)
            (hylo-mode:calculate-indent-of-expression
             hylo-mode:multiline-statement-offset))
        ;; The cursor was on the 3rd or following lines of the string, so
        ;; aligns with a non-empty preceding line.
        (if (and (bolp) (eolp))
            ;; The cursor is on an empty line, so seeks a non-empty-line.
            (hylo-mode:calculate-indent-of-multiline-string)
          (hylo-mode:indentation (point) 0))))))

(defun hylo-mode:goto-non-interpolated-expression-bol ()
  "Back to the beginning of line that is not inside a interpolated expression."
  (let ((string-beginning-position (nth 8 (syntax-ppss)))
        (matching-parenthesis t))
    (while (and matching-parenthesis
                (< (line-beginning-position) string-beginning-position))
      (setq matching-parenthesis
            (get-text-property
             string-beginning-position 'hylo-mode:matching-parenthesis))
      (when matching-parenthesis
        (goto-char matching-parenthesis)
        (setq string-beginning-position (nth 8 (syntax-ppss)))))
    (forward-line 0)))

(defun hylo-mode:calculate-indent-of-single-line-comment ()
  "Return the indentation of the current line inside a single-line comment."
  (cond
   ((save-excursion
      (forward-line 0)
      (bobp))
    (hylo-mode:indentation (point-min) 0))
   ((save-excursion
      (forward-line -1)
      (skip-syntax-forward " ")
      (looking-at "//"))
    (forward-line -1)
    (skip-syntax-forward " ")
    (hylo-mode:indentation (point) 0))
   (t
    (hylo-mode:calculate-indent-of-code))))

(defun hylo-mode:calculate-indent-of-code ()
  "Return the indentation of the current line outside multiline comments."
  (back-to-indentation)
  (let* ((previous-token (save-excursion (hylo-mode:backward-token)))
         (previous-type (hylo-mode:token:type previous-token))
         (previous-text (hylo-mode:token:text previous-token))
         (next-token (save-excursion (hylo-mode:forward-token)))
         (next-type (hylo-mode:token:type next-token))
         (next-text (hylo-mode:token:text next-token))
         (next-is-on-current-line
          (<= (hylo-mode:token:start next-token) (line-end-position))))
    (cond
     ;; Beginning of the buffer
     ((eq previous-type 'outside-of-buffer)
      (hylo-mode:indentation (point-min) 0))

     ;; Before } on the current line
     ((and next-is-on-current-line (eq next-type '}))
      (goto-char (hylo-mode:token:end next-token))
      (backward-list)
      (hylo-mode:calculate-indent-for-curly-bracket 0))

     ;; Before ) or ] on the current line
     ((and next-is-on-current-line (memq next-type '(\) \])))
      (goto-char (hylo-mode:token:end next-token))
      (backward-list)
      (hylo-mode:calculate-indent-of-expression 0))

     ;; Before > as a close angle bracket on the current line
     ((and next-is-on-current-line
           (save-excursion
             (goto-char (hylo-mode:token:start next-token))
             (and (eq (char-after) ?>)
                  (progn (hylo-mode:try-backward-generic-parameters)
                         (< (point) (hylo-mode:token:start next-token))))))
      (hylo-mode:try-backward-generic-parameters)
      (hylo-mode:calculate-indent-of-expression 0))

     ;; Before end of a interpolated expression on the current line
     ((and next-is-on-current-line
           (eq next-type 'string-chunk-after-interpolated-expression))
      (goto-char (get-text-property
                  (hylo-mode:token:start next-token)
                  'hylo-mode:matching-parenthesis))
      (forward-char 2)
      (hylo-mode:backward-string-chunk)
      (hylo-mode:calculate-indent-after-beginning-of-interpolated-expression
       0))

     ;; Before , on the current line
     ((and next-is-on-current-line (eq next-type '\,))
      (hylo-mode:calculate-indent-of-prefix-comma))

     ;; After ,
     ((eq previous-type '\,)
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:calculate-indent-after-comma))

     ;; Before "case" or "default" on the current line, for switch statement
     ((and
       next-is-on-current-line
       (member next-text '("case" "default"))
       (save-excursion
         (let ((head
                (hylo-mode:backward-sexps-until
                 '(implicit-\; \; "switch" "enum" "for" "while" "if" "guard"))))
           (or
            (equal (hylo-mode:token:text head) "switch")
            (and
             ;; If we got a semicolon, the statement is either switch or enum:
             ;;
             ;; switch foo {
             ;; case 1:
             ;;   if aaa {
             ;;   }; // implicit semicolon
             ;; case 2:
             ;; }
             (memq (hylo-mode:token:type head) '(implicit-\; \;))
             (equal (hylo-mode:token:text
                     (hylo-mode:backward-sexps-until '("switch" "enum")))
                    "switch"))))))
      ;; "case" is used for "switch", "enum", "for", "while", "if", and "guard".
      ;; Only switch statement has special indentation rule.
      ;;
      ;; switch foo {
      ;; default:
      ;;   aaa
      ;; case A:
      ;;   aaa
      ;; case B, C, D:
      ;;   aaa
      ;; case E(1, 2, 3, (4, 5)) where aaa,
      ;;      F(1, 2, 3, (4, 5)) where aaa:
      ;;   aaa
      ;; case G: print(1); case H: print(2)
      ;; case I:
      ;;   ...
      ;; }
      ;;
      ;; enum Foo {
      ;;   case A
      ;;   case B, C, D
      ;;   indirect
      ;;     case E(x: Int, y: Int)
      ;; }
      ;;
      ;; enum Foo: Int, A, B {
      ;;   case A = 1, B = 2
      ;;   case C = 3
      ;; }
      ;;
      ;; for
      ;;   case let (x, y) in tuples {
      ;; }
      ;; if
      ;;   case let (x, y) = tuple {
      ;; }
      ;; while
      ;;   case let (x, y) = tuple {
      ;; }
      ;;
      ;; Searches sibling "case" at the beginning of a line. If found, aligns
      ;; with it.
      ;;
      ;; Otherwise, searches "switch" and align with it with offset.
      (let ((parent (hylo-mode:backward-sexps-until
                     '("switch") nil '("case" "default"))))
        (if (equal (hylo-mode:token:text parent) "switch")
            ;; Inside a switch-statement. Aligns with the "switch"
            (if (hylo-mode:bol-other-than-comments-p)
                (hylo-mode:align-with-current-line
                 hylo-mode:switch-case-offset)
              (hylo-mode:find-parent-and-align-with-next
               hylo-mode:statement-parent-tokens
               hylo-mode:switch-case-offset))
          ;; Other cases. Aligns with the previous case.
          (hylo-mode:align-with-current-line))))

     ;; Before "else" on the current line
     ((and next-is-on-current-line (equal next-text "else"))
      (hylo-mode:calculate-indent-before-else))

     ;; After "else"
     ;;
     ;; let a =
     ;;   if x { 1 }
     ;;   else
     ;;     if y { 2 }
     ;;     else { 3 }
     ;;
     ;; let a =
     ;;   if x { 1 } else
     ;;     if y { 2 } else
     ;;       { 3 }
     ;;
     ;; let a = if x { 1 } else if y { 2 } else
     ;;   { 3 }
     ((equal previous-text "else")
      (goto-char (hylo-mode:token:start previous-token))
      (if (hylo-mode:bol-other-than-comments-p)
          (hylo-mode:align-with-current-line
           hylo-mode:multiline-statement-offset)
        (hylo-mode:calculate-indent-before-else
         hylo-mode:multiline-statement-offset)))

     ;; After "if"
     ;;
     ;; let a = if
     ;;           x
     ;;             .foo() {
     ;;     1
     ;; } else {
     ;;     2
     ;; }
     ((equal previous-text "if")
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:align-with-current-line
       hylo-mode:multiline-statement-offset))

     ;; After "catch"
     ((equal previous-text "catch")
      (hylo-mode:find-parent-and-align-with-next
       hylo-mode:statement-parent-tokens
       hylo-mode:multiline-statement-offset))

     ;; After {
     ((eq previous-type '{)
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:calculate-indent-for-curly-bracket
       hylo-mode:basic-offset))

     ;; After (, [,  or < as a open angle bracket
     ((memq previous-type '(\( \[ <))
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:calculate-indent-of-expression
       hylo-mode:parenthesized-expression-offset
       hylo-mode:parenthesized-expression-offset))

     ;; After beginning of a interpolated expression
     ((eq previous-type 'string-chunk-before-interpolated-expression)
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:calculate-indent-after-beginning-of-interpolated-expression
       hylo-mode:parenthesized-expression-offset))

     ;; Before "in" on the current line
     ((and next-is-on-current-line (equal next-text "in"))
      ;; When it is for-in statement, align with the token after "for":
      ;;
      ;; for
      ;;   x
      ;;   in
      ;;   foo
      ;;
      ;; for x
      ;;     in
      ;;     foo
      ;;
      ;; When it is anonymous function, align with the token after {:
      ;;
      ;; foo {
      ;;   x
      ;;   in
      ;;   ...
      ;; }
      ;;
      ;;
      ;; foo { x
      ;;       in
      ;;  ...
      ;; }
      ;;
      ;; foo { [
      ;;         weak self
      ;;       ]
      ;;       (
      ;;         x,
      ;;         y
      ;;       )
      ;;       -> Void
      ;;       in
      ;;   a
      ;; }
      (hylo-mode:find-parent-and-align-with-next '("for" {)))

     ;; Before "where" on the current line
     ((and next-is-on-current-line (equal next-text "where"))
      ;; switch {
      ;; case let P(x)
      ;;        where // Aligns with the pattern.
      ;;          a
      ;;   aaa
      ;; }
      ;; case let A
      ;;        .P(x)
      ;;          where // Aligns with the last line of the pattern.
      ;;            a
      ;;   aaa
      ;; }
      ;;
      ;; for case (x, y) in xys
      ;;     where // Aligns with the next token of the "for" token.
      ;;       aaa {
      ;; }
      ;;
      ;; for (x, y)
      ;;     in
      ;;     xys
      ;;     where // Aligns with the next token of the "for" token.
      ;;       aaa {
      ;; }
      ;;
      ;; do {
      ;; } catch let P(x)
      ;;           where // Aligns with the pattern.
      ;;             aaa
      ;;
      ;; fun foo<A: AAA,
      ;;          B: BBB
      ;;            where // Aligns with the start of the type parameters.
      ;;              ABC>() {
      ;; }
      ;;
      ;; type Foo<A,
      ;;           B,
      ;;           C>: AAA,
      ;;               BBB,
      ;;               CCC
      ;;   where // Aligns with the "type" token.
      ;;     ABC {
      ;; }
      (let* ((parent (save-excursion (hylo-mode:backward-sexps-until
                                      (append hylo-mode:statement-parent-tokens
                                              '("case" "catch" "for")))))
             (previous-of-parent (save-excursion
                                   (goto-char (hylo-mode:token:start parent))
                                   (hylo-mode:backward-token))))
        (when (and
               (equal (hylo-mode:token:text parent) "case")
               (equal (hylo-mode:token:text previous-of-parent) "for"))
          (setq parent previous-of-parent))
        (cond
         ((member (hylo-mode:token:text parent) '("case" "catch"))
          (goto-char (hylo-mode:token:end previous-token))
          (hylo-mode:backward-token-or-list)
          (hylo-mode:calculate-indent-of-expression
           hylo-mode:multiline-statement-offset
           hylo-mode:multiline-statement-offset))
         ((equal (hylo-mode:token:text parent) "for")
          (hylo-mode:find-parent-and-align-with-next '("for")))
         (t
          (hylo-mode:find-parent-and-align-with-next
           (append hylo-mode:statement-parent-tokens '(<))
           hylo-mode:multiline-statement-offset)))))

     ;; After "where"
     ((equal previous-text "where")
      ;; switch {
      ;; case let .P(x) where
      ;;        A, // Aligns with the pattern.
      ;;      let A
      ;;        .Q(x) where // Aligns with the last line of the pattern.
      ;;          A:
      ;;   aaa
      ;; case let .P(x)
      ;;        where
      ;;          A, // Aligns with the "where" token.
      ;;      let .Q(x)
      ;;        where // Aligns with the "where" token.
      ;;          A:
      ;;   aaa
      ;; case let .P(x), let .Q(x) where // Aligns with the pattern.
      ;;                   a
      ;; }
      ;;
      ;; for case let (x, y) in xys where
      ;;       aaa { // Aligns with the next token of the "for" token.
      ;; }
      ;;
      ;; for case let (x, y) in xys
      ;;     where
      ;;       aaa { // Aligns with the "where" token
      ;; }
      ;;
      ;; do {
      ;; } catch let .P(x) where
      ;;           aaa // Aligns with the pattern.
      ;; do {
      ;; } catch let .P(x)
      ;;           where
      ;;             aaa // Aligns with the "where" token.
      ;;
      ;;
      ;;
      ;; fun foo<A: AAA,
      ;;          B: BBB where
      ;;            ABC>() { // Aligns with the start of the type parameters.
      ;; }
      ;;
      ;; fun foo<A: AAA,
      ;;          B: BBB
      ;;            where
      ;;              ABC>() { // Aligns with the "where" token.
      ;; }
      ;;
      ;; type Foo<A,
      ;;           B,
      ;;           C> A,
      ;;              B,
      ;;              C where
      ;;   ABC { // Aligns with the "type" token"
      ;; }
      ;;
      ;; type Foo<A,
      ;;           B,
      ;;           C>: A,
      ;;               B,
      ;;               C
      ;;   where
      ;;     ABC { // Aligns with the "where" token"
      ;; }
      (goto-char (hylo-mode:token:start previous-token))
      (if (hylo-mode:bol-other-than-comments-p)
          (hylo-mode:align-with-current-line
           hylo-mode:multiline-statement-offset)
        (let ((parent (save-excursion
                        (hylo-mode:backward-sexps-until
                         (append hylo-mode:statement-parent-tokens
                                 '("case" "catch"))))))
          (if (member (hylo-mode:token:text parent) '("case" "catch"))
              (progn
                (hylo-mode:backward-token-or-list)
                (hylo-mode:calculate-indent-of-expression
                 hylo-mode:multiline-statement-offset
                 hylo-mode:multiline-statement-offset))
            (hylo-mode:find-parent-and-align-with-next
             (append hylo-mode:statement-parent-tokens '(< "for"))
             hylo-mode:multiline-statement-offset)))))

     ;; After implicit-\; or ;
     ((memq previous-type '(implicit-\; \;))
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:find-parent-and-align-with-next
       (remove '\; (remove 'implicit-\; hylo-mode:statement-parent-tokens))
       0
       '(implicit-\; \;)))

     ;; After "in" for anonymous function parameters
     ((eq previous-type 'anonymous-function-parameter-in)
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:backward-sexps-until-open-curly-bracket)
      (hylo-mode:calculate-indent-for-curly-bracket
       hylo-mode:basic-offset))

     ;; After "in" for "for" statements
     ((equal previous-text "in")
      ;; Aligns with "in" if it is at the start of the line:
      ;;
      ;; for
      ;;   x
      ;;   in
      ;;   foo
      ;;
      ;; for x
      ;;     in
      ;;     foo
      ;;
      ;; Otherwise, aligns with the next token of the "for".
      ;;
      ;; for x in
      ;;     foo
      ;;
      ;; for
      ;;   x in
      ;;   foo
      (goto-char (hylo-mode:token:start previous-token))
      (if (hylo-mode:bol-other-than-comments-p)
          (hylo-mode:align-with-current-line)
        (let ((parent (hylo-mode:backward-sexps-until '("for" {))))
          (hylo-mode:align-with-next-token parent))))

     ;; After case ... : or default:
     ((eq previous-type 'case-:)
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:find-parent-and-align-with-next
       hylo-mode:statement-parent-tokens
       (let ((relative-case-offset
              (- hylo-mode:basic-offset hylo-mode:switch-case-offset)))
         (if (<= relative-case-offset 0)
             hylo-mode:basic-offset
           relative-case-offset))))

     ;; Before ; on the current line
     ((and next-is-on-current-line (eq next-type '\;))
      (hylo-mode:find-parent-and-align-with-next
       (remove '\; (remove 'implicit-\; hylo-mode:statement-parent-tokens))
       0
       '(implicit-\; \;)))

     ;; After if, guard, and while
     ((member previous-text '("if" "guard" "while"))
      (hylo-mode:find-parent-and-align-with-next
       hylo-mode:statement-parent-tokens
       hylo-mode:multiline-statement-offset))

     ;; After attributes
     ((eq previous-type 'attribute)
      (goto-char (hylo-mode:token:end previous-token))
      (hylo-mode:backward-token-or-list)
      (hylo-mode:calculate-indent-of-expression
       hylo-mode:multiline-statement-offset
       0
       t))

     ;; Otherwise, it is continuation of the previous line
     (t
      (goto-char (hylo-mode:token:end previous-token))
      (hylo-mode:backward-token-or-list)
      (hylo-mode:calculate-indent-of-expression
       hylo-mode:multiline-statement-offset)))))

(defun hylo-mode:find-parent-and-align-with-next
    (parents
     &optional
     offset
     stop-at-eol-token-types
     stop-at-bol-token-types
     bol-offset)
  "Find the parent and return indentation based on it.

A parent is, for example, the open bracket of the containing block or
semicolon of the preceding statement.

PARENTS is a list of token types that precedes an expression or a statement.
OFFSET is the offset.  If it is omitted, assumed to be 0.
See `hylo-mode:backward-sexps-until' for the details of
STOP-AT-EOL-TOKEN-TYPES and STOP-AT-BOL-TOKEN-TYPES.
If scanning stops at STOP-AT-EOL-TOKEN-TYPES, align with the next token with
BOL-OFFSET.
If scanning stops at STOP-AT-BOL-TOKEN-TYPES, align with that token with
BOL-OFFSET.
If STOP-AT-BOL-TOKEN-TYPES or STOP-AT-BOL-TOKEN-TYPES is the symbol
`any', it matches all tokens.
The point is assumed to be on the previous line."
  (save-excursion
    (let* ((parent (hylo-mode:backward-sexps-until
                    parents
                    stop-at-eol-token-types
                    stop-at-bol-token-types))
           (parent-end (hylo-mode:token:end parent))
           (stopped-at-parent
            (or (memq (hylo-mode:token:type parent) parents)
                (member (hylo-mode:token:text parent) parents)
                (eq (hylo-mode:token:type parent) 'outside-of-buffer)))
           (stopped-at-eol
            (and
             (not stopped-at-parent)
             stop-at-eol-token-types
             (or
              (eq stop-at-eol-token-types 'any)
              (memq (hylo-mode:token:type parent)
                    stop-at-eol-token-types)
              (member (hylo-mode:token:text parent)
                      stop-at-eol-token-types)))))
      (if stopped-at-parent
          (hylo-mode:align-with-next-token parent offset)
        (when stopped-at-eol
          (goto-char parent-end)
          (forward-comment (point-max)))
        (hylo-mode:align-with-current-line bol-offset)))))

(defun hylo-mode:calculate-indent-of-expression
    (&optional
     offset
     bol-offset
     after-attributes)
  "Return indentation of the current expression.

the cursor is assumed to be on the previous line.

OFFSET is the offset.  If it is omitted, assumed to be 0.
If scanning stops at eol, align with the next token with BOL-OFFSET.
If AFTER-ATTRIBUTES is nil, skip lines with only attributes at the start of
the expression."
  (save-excursion
    (let* ((parent-of-previous-line
            (save-excursion
              (hylo-mode:goto-non-comment-bol-with-same-nesting-level)
              (hylo-mode:backward-token)))
           (parent (hylo-mode:find-parent-of-expression)))

      (when (not after-attributes)
        (goto-char (hylo-mode:token:end parent))
        (hylo-mode:forward-attributes)
        (hylo-mode:goto-non-comment-bol-with-same-nesting-level)
        (when (< (point) (hylo-mode:token:end parent))
          (goto-char (hylo-mode:token:end parent)))
        (setq parent (hylo-mode:backward-token)))

      ;; When indenting a token after an attribute at the start of the
      ;; expression, aligns with it.
      (when (and after-attributes
                 (save-excursion
                   (goto-char (hylo-mode:token:end parent))
                   (forward-comment (point-max))
                   (eq (hylo-mode:token:type (hylo-mode:forward-token))
                       'attribute)))
        (setq offset 0))

      (if (<= (hylo-mode:token:start parent-of-previous-line)
              (hylo-mode:token:start parent))
          ;; let x =
          ;;   1 + // here
          ;;   2 +
          ;;   3
          ;;
          ;; Aligns with the parent of the expression with offset.
          (hylo-mode:align-with-next-token parent offset)
        ;; let x =
        ;;   1 +
        ;;   2 + // here
        ;;   3   // or here
        ;;
        ;; Aligns with the previous line.
        (hylo-mode:align-with-next-token parent-of-previous-line
                                          bol-offset)))))

(defun hylo-mode:forward-attributes ()
  "Skip forward comments, whitespaces, and attributes."
  (while
      (not
       (eq (point)
           (progn
             (forward-comment (point-max))
             (when (eq (char-after) ?@)
               (hylo-mode:forward-token-simple))
             (point))))))

(defun hylo-mode:calculate-indent-before-else (&optional offset)
  "Return indentation before \"else\" token.

Assuming the cursor is before \"else\".
OFFSET is extra offset if given."
  ;; let x = if x { 1 }
  ;;   else if y { 2 }
  ;;   else { 3 }
  ;;
  ;; let x =
  ;;   if x { 1 }
  ;;   else if y { 2 }
  ;;   else { 3 }
  ;;
  ;; let a = if x { 1 }
  ;;   else
  ;;     if y { 2 }
  ;;     else { 3 }
  ;;
  ;; let a =
  ;;   if x { 1 }
  ;;   else
  ;;     if y { 2 }
  ;;     else { 3 }
  (let ((parent (hylo-mode:backward-sexps-until
                 (append
                  (remove 'implicit-\; hylo-mode:statement-parent-tokens)
                  '("if" "guard")))))
    (if (equal (hylo-mode:token:text parent) "if")
        (cond
         ;; Found "if" at the beginning of a line.  Align with it.
         ;;
         ;; let a =
         ;;   if x { 1 }
         ;;   else
         ;;     if y { 2 }
         ;;     else { 3 }
         ((hylo-mode:bol-other-than-comments-p)
          (hylo-mode:align-with-current-line offset))

         ;; Found "else if".
         ;;
         ;; let x =
         ;;   if x { 1 }
         ;;   else if y { 2 }
         ;;   else { 3 }
         ;;
         ;; let x =
         ;;   if x { 1 } else if y { 2 }
         ;;   else { 3 }
         ((equal (hylo-mode:token:text (save-excursion
                                          (hylo-mode:backward-token)))
                 "else")
          (hylo-mode:backward-token)
          (if (hylo-mode:bol-other-than-comments-p)
              (hylo-mode:align-with-current-line offset)
            (hylo-mode:calculate-indent-before-else offset)))

         ;; let x = if x { 1 }
         ;;   else { 2 }
         (t
          (hylo-mode:calculate-indent-of-expression
           (or offset hylo-mode:multiline-statement-offset))))
      (hylo-mode:align-with-current-line offset))))

(defun hylo-mode:calculate-indent-for-curly-bracket (offset)
  "Return indentation relating to curly brackets.

It is used for indentation after open curly brackets and for close brackets.

Assuming the cursor is on the open bracket.
OFFSET is the offset of the contents."
  ;; If the statement is multiline expression, aligns with the start of
  ;; the line on which the open bracket is:
  ;;
  ;; foo()
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;;   }
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;;   }
  ;;
  ;; rather than the start of the statement:
  ;;
  ;; foo()
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;; }
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;; }
  ;;
  ;; Otherwise, aligns with the start of the whole statement:
  ;;
  ;; for x in
  ;;     xs
  ;;       .foo() {
  ;; }
  ;;
  ;; rather than
  ;;
  ;; for x in
  ;;     xs
  ;;       .foo() {
  ;;       }
  ;;
  ;; Note that curly bracket after binary operator is a part of
  ;; a multiline expression:
  ;;
  ;; for x in
  ;;     xs
  ;;     +++ { x in
  ;;       // this is not the body of the for-statement.
  ;;     } {
  ;;   // The body of the for-statement.
  ;; }
  (let ((pos (point))
        previous-token
        next-token
        is-declaration-or-control-statement-body)
    (if (save-excursion
          (setq previous-token (hylo-mode:backward-token))
          (and (eq (hylo-mode:token:type previous-token) 'binary-operator)
               ;; not > as close angle bracket
               (not
                (progn
                  (goto-char (hylo-mode:token:end previous-token))
                  (and (eq (char-before) ?>)
                       (progn
                         (backward-char)
                         (hylo-mode:try-backward-generic-parameters)
                         (< (point)
                            (1- (hylo-mode:token:end previous-token)))))))))
        ;; for x in
        ;;     xs
        ;;     +++ { x in
        ;;       // this is not the body of the for statement.
        ;;     } {
        ;;   ...
        ;; }
        (setq is-declaration-or-control-statement-body nil)
      (save-excursion
        (goto-char (hylo-mode:token:end
                    (hylo-mode:backward-sexps-until
                     (append hylo-mode:statement-parent-tokens '(\( \[)))))
        (setq next-token (hylo-mode:forward-token-or-list))
        (while (<= (point) pos)
          (cond
           ((member
             (hylo-mode:token:text next-token)
             '("for" "while" "repeat" "guard" "switch" "if" "else"
               "defer" "do" "catch"
               "get" "set" "willSet" "didSet" "fun" "init" "subscript"
               "enum" "type" "actor" "extension"
               "prefix" "postfix" "infix" "precedencegroup"))
            (setq is-declaration-or-control-statement-body t)
            (goto-char (1+ pos)))

           ((and
             (equal (hylo-mode:token:text next-token) "protocol")
             (not (equal (hylo-mode:token:text
                          (save-excursion (hylo-mode:forward-token)))
                         "<")))
            (setq is-declaration-or-control-statement-body t)
            (goto-char (1+ pos)))

           ((equal (hylo-mode:token:text next-token) "var")
            ;; There are several cases:
            ;;
            ;; var foo = bar
            ;;   .then { x in
            ;;       x
            ;;   }
            ;;   .then { x in
            ;;       x
            ;;   }
            ;;
            ;; var foo = bar
            ;;   .baz {
            ;;     willSet {
            ;;         ...
            ;;     }
            ;; }
            ;;
            ;; var foo {
            ;;     get {
            ;;         ...
            ;;     }
            ;; }
            ;;
            ;; Note that the 1st and the 2nd cases cannot be distinguished
            ;; without inspecting the contents of the block.
            ;; We indent the 2nd case like the 1st case for now.
            ;; Future implementation may use more sophisticated logic.
            (goto-char pos)
            (setq is-declaration-or-control-statement-body
                  (equal (hylo-mode:token:text
                          (hylo-mode:backward-sexps-until '("var" "=")))
                         "var"))
            (goto-char (1+ pos)))

           (t
            ;; Suppose indenting the A token below.
            ;;
            ;; foo {
            ;;   A
            ;;
            ;; This function is called on the open curly bracket.
            ;; If the close curly bracket doesn't exist,
            ;; hylo-mode:forward-token-or-list results in
            ;; "Unbalanced parentheses" error.
            ;; So if the point is just before the open curly bracket,
            ;; exits immediately.
            (forward-comment (point-max))
            (if (< (point) pos)
                (setq next-token (hylo-mode:forward-token-or-list))
              (goto-char (1+ pos))))))))
    (cond
     ((equal (hylo-mode:token:text previous-token) "else")
      (goto-char (hylo-mode:token:start previous-token))
      (hylo-mode:calculate-indent-before-else offset))

     ((or (member (hylo-mode:token:text next-token) '("if" "switch")))
      (goto-char (hylo-mode:token:start next-token))
      (if (hylo-mode:bol-other-than-comments-p)
          (hylo-mode:align-with-current-line offset)
        (hylo-mode:find-parent-and-align-with-next
         hylo-mode:statement-parent-tokens
         offset)))

     (is-declaration-or-control-statement-body
      (hylo-mode:find-parent-and-align-with-next
       hylo-mode:statement-parent-tokens
       offset))

     (t
      (hylo-mode:calculate-indent-of-expression offset offset)))))

(defun hylo-mode:calculate-indent-of-prefix-comma ()
  "Return indentation for prefix comma.

Example:

let x = [ 1
        , 2
        , 3
]

type Foo: A
         , B
         , C

case A
   , B
   , C
       :

var x = 1
  , y = 2
  , z = 3

This is also known as Utrecht-style in the Haskell community."
  (let ((parent (hylo-mode:find-parent-of-list-element t)))
    (if (eq (hylo-mode:token:type parent) '\,)
        ;; The comma was the 2nd or the following commas.
        ;; Aligns with the previous comma.
        (hylo-mode:align-with-current-line)
      ;; The comma was the 1st comma.
      ;; Aligns with the end of the parent.
      (goto-char (hylo-mode:token:end parent))
      (backward-char)
      (hylo-mode:indentation (point) 0))))

(defun hylo-mode:calculate-indent-after-comma ()
  "Return indentation after comma.

Assuming the cursor is on the comma."
  (hylo-mode:align-with-next-token
   (hylo-mode:find-parent-of-list-element nil)))

(defun hylo-mode:find-parent-of-list-element (&optional utrecht-style)
  "Move point backward to the parent token of the comma under the cursor.
If UTRECHT-STYLE is non-nil, stop at a comma at bol.  Otherwise, stop at a
comma at eol."
  ;; Various examples:
  ;;
  ;; let x = ( // simple case
  ;;   1,
  ;;   2,
  ;;   3
  ;; )
  ;;
  ;; let x = [ // simple case
  ;;   1,
  ;;   2,
  ;;   3
  ;; ]
  ;;
  ;; let x: Foo<A, B> = a, // "let" is not a part of elements
  ;;     y: Foo<A, B> = b,
  ;;     z: Foo<A, B> = c
  ;;
  ;; switch foo {
  ;; case (let x, // "let" is a part of an element
  ;;       Y,
  ;;       Z):
  ;;   aaa
  ;; }
  ;;
  ;; type Foo<A where A: B, // "where" is not a part of elements
  ;;                   A: C,
  ;;                   A: D> {
  ;; }
  ;;
  ;; switch foo {
  ;; case A(x) where p(x), // "case" is not a part of elements
  ;;      B(x) where p(x), // "where" is a part of an element
  ;;      C(x) where p(x):
  ;;   aaa
  ;; }
  ;;
  ;; if // or guard or while
  ;;   let x = x, // "let" is a part of an element
  ;;   let y = y,
  ;;   let z = z,
  ;;   case .P(a, b, c) = abc, // "case" is a part of an element.
  ;;   aaa {
  ;;
  ;;   bbb
  ;; }
  ;;
  ;; See also SE-0099 and SE-0043:
  ;; https://github.com/apple/hylo-evolution/blob/master/proposals/0099-conditionclauses.md
  ;; https://github.com/apple/hylo-evolution/blob/master/proposals/0043-declare-variables-in-case-labels-with-multiple-patterns.md
  ;; SE-0099 seems precedes SE-0043.
  ;;
  ;; type Foo<T>: A,
  ;;               B,
  ;;               C
  ;;   where
  ;;     T: A,
  ;;     T: B,
  ;;     T: C {
  ;; }
  ;;
  ;; extension _ArrayType
  ;;   where
  ;;     Generator.Element: A,
  ;;     Generator.Element: B,
  ;;     Generator.Element: C {
  ;; }
  ;;
  ;; fun foo<T> -> Int
  ;;   where
  ;;     T: A,
  ;;     T: B,
  ;;     T: C {
  ;; }
  ;;
  ;; enum Foo {
  ;;   case A(x: Int),
  ;;        B(y: Int),
  ;;        C(z: Int)
  ;;   case D(x: Int)
  ;;      , E(y: Int)
  ;;      , F(z: Int)
  ;; }
  ;;
  ;; enum Foo: Int {
  ;;   case A,
  ;;        B,
  ;;        C = 2
  ;;   case D = 3
  ;;      , E
  ;;      , F
  ;; }
  ;;
  ;; https://github.com/apple/hylo-evolution/blob/master/proposals/0276-multi-pattern-catch-clauses.md
  ;; do {
  ;; } catch Foo(let a),
  ;;         Bar(let a) {
  ;;   foo(a)
  ;; }
  (let ((pos (point))
        (parent (hylo-mode:backward-sexps-until
                 ;; Includes "if" to stop at the last else-if.
                 ;; Includes "catch" to stop at the last catch.
                 (append hylo-mode:statement-parent-tokens
                         '("if" "catch" \( \[ <))
                 (if utrecht-style nil '(\,))
                 (if utrecht-style '(\,) nil))))
    (cond
     ((memq (hylo-mode:token:type parent) '(\( \[ \,))
      parent)

     ((eq (hylo-mode:token:type parent) '<)
      (goto-char pos)
      (hylo-mode:backward-sexps-until '(< "where")))

     ((member (hylo-mode:token:text parent) '("if" "catch"))
      parent)

     (t
      (goto-char (hylo-mode:token:end parent))
      (let ((next-token (hylo-mode:forward-token-or-list))
            result)
        (while (and (<= (point) pos) (not result))
          (cond
           ((member (hylo-mode:token:text next-token)
                    '("guard" "while" "let" "var" "case" "where"))
            (setq result next-token))

           ((eq (hylo-mode:token:type next-token) 'supertype-:)
            (goto-char pos)
            (setq result (hylo-mode:backward-sexps-until
                          '(supertype-: "where")))))

          (setq next-token (hylo-mode:forward-token-or-list)))
        (when (and (> (point) pos)
                   (eq (hylo-mode:token:type next-token) '<>))
          ;; The comma was inside <> but scanner misunderstood < as
          ;; a binary-operator.
          (hylo-mode:backward-token-or-list)
          (setq result (hylo-mode:forward-token)))
        (when (null result)
          (setq result parent))
        (goto-char (hylo-mode:token:start result))
        result)))))

(defun hylo-mode:find-parent-of-expression ()
  "Move point backward to the parent token of the expression under the cursor."
  ;; TODO Unify with hylo-mode:find-parent-of-list-element
  (let ((pos (point))
        (parent (hylo-mode:backward-sexps-until
                 hylo-mode:expression-parent-tokens
                 '("in") '("in"))))
    (cond
     ((memq (hylo-mode:token:type parent) '(\( \[))
      parent)

     ((equal (hylo-mode:token:text parent) "in")
      (goto-char (hylo-mode:token:end parent))
      (if (hylo-mode:eol-other-than-comments-p)
          parent
        (goto-char (hylo-mode:token:start parent))
        (hylo-mode:backward-token-or-list)))

     ((or
       (memq (hylo-mode:token:type parent)
             hylo-mode:statement-parent-tokens)
       (member (hylo-mode:token:text parent)
               hylo-mode:statement-parent-tokens)
       (eq (hylo-mode:token:type parent) 'outside-of-buffer))
      (goto-char (hylo-mode:token:end parent))
      (let ((next-token (hylo-mode:forward-token-or-list))
            result)
        (while (and (<= (point) pos) (not result))
          (cond
           ((equal (hylo-mode:token:text next-token) "case")
            (setq result next-token))

           ((member (hylo-mode:token:text next-token)
                    '("let" "var"))
            ;; Special handling for "let" and "var".
            ;;
            ;; Declaring multiple variables with single let statement doesn't
            ;; seem to be popular. Rather, we choose saving columns for the
            ;; first variable:
            ;;
            ;; let x =
            ;;   foo(),
            ;;     y =
            ;;       foo()
            ;;
            ;; rather than:
            ;;
            ;; let x =
            ;;       foo(),
            ;;     y =
            ;;       foo()
            ;;
            ;; TODO make customizable
            (setq result parent)))

          (forward-comment (point-max))
          (if (< (point) pos)
              (setq next-token (hylo-mode:forward-token-or-list))
            (setq next-token (hylo-mode:forward-token))))
        (when (and (> (point) pos)
                   (eq (hylo-mode:token:type next-token) '<>))
          ;; The expression was inside <> but scanner misunderstood < as
          ;; a binary-operator.
          (hylo-mode:backward-token-or-list)
          (setq result (hylo-mode:forward-token)))
        (when (null result)
          (setq result parent))
        (goto-char (hylo-mode:token:start result))
        result))

     (t
      parent))))

(defun hylo-mode:calculate-indent-after-beginning-of-interpolated-expression
    (offset)
  "Return indentation after the beginning of a interpolated expression.
It has offset specified with OFFSET.

Assuming the cursor is before the string chunk."
  (let ((pos (point)))
    (hylo-mode:forward-string-chunk)
    (if (< pos (line-beginning-position))
        (progn
          (back-to-indentation)
          (hylo-mode:indentation (point) offset))
      (goto-char pos)
      (hylo-mode:calculate-indent-of-expression offset offset))))

(defun hylo-mode:backward-sexps-until (token-types
                                        &optional
                                        stop-at-eol-token-types
                                        stop-at-bol-token-types)
  "Backward sexps until a token with one of given token types appears.
Return the token.
When this function returns, the cursor is at the start of the token.

TOKEN-TYPES is a list of guard token types.  This function backs to a token
with  one of those token types.
STOP-AT-EOL-TOKEN-TYPES is a list of token types that if we skipped the end of
a line just after a token with one of given token type, the function returns.
Typically, this is a list of token types that separates list elements
\(e.g.  ',', ';').  If STOP-AT-EOL-TOKEN-TYPES is the symbol `any', it matches
all tokens.
STOP-AT-BOL-TOKEN-TYPES is a list of token types that if we hit
the beginning of a line just before a token with one of given token types,
the function returns.  Typically, this is a list of token types that starts
list element (e.g. `case' of switch statement body).  If STOP-AT-BOL-TOKEN-TYPES
is the symbol `any', it matches all tokens."
  (let*
      ((parent (hylo-mode:backward-token-or-list))
       (type (hylo-mode:token:type parent))
       (text (hylo-mode:token:text parent)))
    (while (not
            ;; Stops loop when...
            (or
             ;; Hits a guard token.
             (member type token-types)
             (member text token-types)

             ;; Beginning of the buffer.
             (eq type 'outside-of-buffer)

             ;; When this function is called on "," token before position (1),
             ;; this function stops just before the "," token after "Foo".
             ;;
             ;; case Foo,
             ;;      Bar, Baz, // (1)
             ;;      AAA
             (and stop-at-eol-token-types
                  (save-excursion
                    (hylo-mode:forward-token-or-list)
                    (forward-comment (- (point)))
                    (hylo-mode:eol-other-than-comments-p))
                  (or (eq stop-at-eol-token-types 'any)
                      (member type stop-at-eol-token-types)
                      (member text stop-at-eol-token-types)))

             ;; When this function is called on "case" token before position
             ;; (1), this function stops just before "case Bar".
             ;;
             ;; switch foo {
             ;; case Foo:
             ;;     ...
             ;; case Bar: case Baz:
             ;;     ...
             ;; case AAA: // (1)
             ;; }
             (and stop-at-bol-token-types
                  (and
                   (or
                    (eq stop-at-bol-token-types 'any)
                    (member type stop-at-bol-token-types)
                    (member text stop-at-bol-token-types))
                   (hylo-mode:bol-other-than-comments-p)))))
      (setq parent (hylo-mode:backward-token-or-list))
      (setq type (hylo-mode:token:type parent))
      (setq text (hylo-mode:token:text parent)))
    parent))

(defun hylo-mode:backward-sexps-until-open-curly-bracket ()
  "Backward sexps until an open curly brace appears.
Return the brace token.
When this function returns, the cursor is at the start of the token.

If there is no open curly braces, return `outside-of-buffer' token.

This is optimized version of (hylo-mode:backward-sexps-until \\='({}))."
  (let* ((parent-position (nth 1 (syntax-ppss))))
    (while (and parent-position
                (and (goto-char parent-position)
                     (not (eq (char-after) ?{))))
      (setq parent-position (nth 1 (syntax-ppss))))
    (if (eq (char-after) ?{)
        (save-excursion (hylo-mode:forward-token))
      (goto-char (point-min))
      (hylo-mode:backward-token))))

(defun hylo-mode:align-with-next-token (parent &optional offset)
  "Return indentation with the PARENT and OFFSET."
  (let ((parent-end (hylo-mode:token:end parent)))
    (goto-char parent-end)
    (forward-comment (point-max))
    (hylo-mode:goto-non-comment-bol)
    (when (< (point) parent-end)
      (goto-char parent-end))
    (hylo-mode:skip-whitespaces)
    (hylo-mode:indentation (point) (or offset 0))))

(defun hylo-mode:align-with-current-line (&optional offset)
  "Return indentation of the current line with OFFSET."
  (hylo-mode:goto-non-comment-bol)
  (hylo-mode:skip-whitespaces)
  (hylo-mode:indentation (point) (or offset 0)))

(defun hylo-mode:backward-token-or-list ()
  "Move point to the start position of the previous token or list.
Return the token skipped."
  (let* ((previous-token (hylo-mode:backward-token))
         (previous-type (hylo-mode:token:type previous-token))
         (previous-text (hylo-mode:token:text previous-token))
         (previous-start (hylo-mode:token:start previous-token))
         (previous-end (hylo-mode:token:end previous-token)))
    (cond
     ;; List
     ((memq previous-type '(} \) \]))
      (goto-char previous-end)
      (condition-case nil
          (progn
            (backward-list)
            (hylo-mode:token
             (assoc-default previous-type '((} . {})
                                            (\) . \(\))
                                            (\] . \[\])))
             (buffer-substring-no-properties (point) previous-end)
             (point)
             previous-end))
        (scan-error
         (goto-char previous-start)
         previous-token)))

     ;; Generic parameter list
     ((equal previous-text ">")
      (hylo-mode:try-backward-generic-parameters)
      (if (= (point) previous-start)
          previous-token
        (hylo-mode:token
         '<>
         (buffer-substring-no-properties (point) previous-end)
         (point)
         previous-end)))

     ;; Other token
     (t previous-token))))

(defun hylo-mode:forward-token-or-list ()
  "Move point to the end position of the next token or list.
Return the token skipped."
  (let* ((next-token (hylo-mode:forward-token))
         (next-type (hylo-mode:token:type next-token))
         (next-text (hylo-mode:token:text next-token))
         (next-start (hylo-mode:token:start next-token))
         (next-end (hylo-mode:token:end next-token)))
    (cond
     ;; List
     ((memq next-type '({ \( \[))
      (goto-char next-start)
      (condition-case nil
          (progn
            (forward-list)
            (hylo-mode:token
             (assoc-default next-type '(({ . {})
                                        (\( . \(\))
                                        (\[ . \[\])))
             (buffer-substring-no-properties next-start (point))
             next-start
             (point)))
        (scan-error
         (goto-char next-end)
         next-token)))

     ;; Generic parameter list
     ((equal next-text "<")
      (hylo-mode:try-forward-generic-parameters)
      (if (= (point) next-end)
          next-token
        (hylo-mode:token
         '<>
         (buffer-substring-no-properties next-start (point))
         next-start
         (point))))

     ;; Other token
     (t next-token))))

(defun hylo-mode:try-backward-generic-parameters ()
  "Move point to the start of the generic parameter list.

Keep position if the cursor is not at the end of a generic parameter list.

Assuming the cursor is on the close angle bracket.

It is a Generic parameter list if:
- it has matching angle brackets, and
- it does not have tokens that cannot appears in a generic parameter list."
  (hylo-mode:try-skip-generic-parameters
   #'hylo-mode:backward-token-or-list
   "<" ">"))

(defun hylo-mode:try-forward-generic-parameters ()
  "Move point to the end of the generic parameter list.

Keep position if the cursor is not at the start of a generic parameter list.

Assuming the cursor is after the open angle bracket.

It is a Generic parameter list if:
- it has matching angle brackets, and
- it does not have tokens that cannot appears in a generic parameter list."
  (hylo-mode:try-skip-generic-parameters
   #'hylo-mode:forward-token-or-list
   ">" "<"))

(defconst hylo-mode:tokens-not-in-generic-parameter-list
  ;; Whitelisting tend to be fragile. So we list tokens that are
  ;; unlikely to appear in generic parameter lists in the current
  ;; version and future ones.
  ;;
  ;; Example of complex generic parameters:
  ;; <
  ;;   A: B,
  ;;   C: protocol<X, Y>
  ;;   where
  ;;     A: @aaa(1 + 2 + 3) D<Int>!,
  ;;     C == (@aaa(1) inout [E.F]?, G...) throws -> [Int:Int]
  ;; >
  ;;
  ;; We don't need to consider the contents of inner brackets because they are
  ;; skipped by `hylo-mode:backward-token-or-list'.
  ;;
  ;; String literals, implicit parameter names, and numbers are also excluded
  ;; by `hylo-mode:try-skip-generic-parameters'.
  '(outside-of-buffer
    \;
    { } \( \) \[ \]
    "true" "false"
    "type" "actor" "enum" "extension" "fun" "operator" "macro"
    "try" "try?" "try!"
    "as" "as?" "as!"
    "is"
    "await"
    "consume" "copy" "discard"
    "in"
    "init" "deinit" "get" "set" "willSet" "didSet" "subscript"
    "for" "case" "default" "while" "let" "var" "repeat" "if" "else"
    "guard" "break" "continue" "fallthrough" "return" "throw" "defer"
    "do" "catch" "import" "typealias" "type"))

(defun hylo-mode:try-skip-generic-parameters
    (skip-token-or-list-function matching-bracket-text unmatching-bracket-text)
  "Skip generic parameters if the point is just before/after one.

SKIP-TOKEN-OR-LIST-FUNCTION skips forward/backward a token or a list.
MATCHING-BRACKET-TEXT is a text of the matching bracket.
UNMATCHING-BRACKET-TEXT is a text of the current bracket."
  (let ((pos (point))
        (prohibited-tokens (cons
                            unmatching-bracket-text
                            hylo-mode:tokens-not-in-generic-parameter-list))
        (next-token (funcall skip-token-or-list-function)))
    (while
        (cond
         ((or (memq (hylo-mode:token:type next-token) prohibited-tokens)
              (member (hylo-mode:token:text next-token) prohibited-tokens)
              (string-match-p "^[\"$0-9]"
                              (hylo-mode:token:text next-token)))
          ;; Not a generic parameter list. Returns to the initial position and
          ;; stops the loop.
          (goto-char pos)
          nil)

         ((equal (hylo-mode:token:text next-token) matching-bracket-text)
          ;; Found the matching open angle bracket. Stops the loop.
          nil)

         ;; Otherwise, keep scanning
         (t t))
      (setq next-token (funcall skip-token-or-list-function)))
    next-token))

(defun hylo-mode:bol-other-than-comments-p ()
  "Return t if there is nothing other than comments in the front of this line.

Return nil otherwise.
Newlines inside comments are ignored."
  ;; Foo // ← bol
  ;; /* */ Foo // ← bol
  ;; X /* */ Foo // ← not bol
  ;;
  ;; /*
  ;; */ /* */ /*
  ;; */ Foo // ← bol
  ;;
  ;; X /*
  ;; */ /* */ /*
  ;; */ Foo // ← not bol
  ;;
  ;; X
  ;; /* */ /*
  ;; */ Foo // ← bol
  (save-excursion
    (let ((pos (point)))
      (hylo-mode:goto-non-comment-bol)
      (forward-comment (point-max))
      (<= pos (point)))))

(defun hylo-mode:eol-other-than-comments-p ()
  "Return t if there is nothing other than comments until the end of this line.

Return nil otherwise.
Newlines inside comments are ignored."
  (save-excursion
    (let ((pos (point)))
      (hylo-mode:goto-non-comment-eol)
      (forward-comment (- (point)))
      (<= (point) pos))))

(defun hylo-mode:goto-non-comment-bol-with-same-nesting-level ()
  "Back to the beginning of line that is not inside a comment."
  (while (not (hylo-mode:bol-other-than-comments-p))
    (hylo-mode:backward-token-or-list)))


(defun hylo-mode:bolp ()
  "Return t if there is nothing in the front of this line.

Return nil otherwise."
  (save-excursion
    (skip-syntax-backward " ")
    (bolp)))

(defun hylo-mode:skip-whitespaces ()
  "Skips forward whitespaces and newlines."
  (skip-syntax-forward " >"))

(defun hylo-mode:incomplete-comment-p (chunk)
  "Return t if the CHUNK is incomplete comment.

Return nil otherwise."
  (and (hylo-mode:chunk:comment-p chunk)
       (save-excursion
         (goto-char (hylo-mode:chunk:start chunk))
         (not (forward-comment 1)))))

(defun hylo-mode:indent-new-comment-line (&optional soft)
  "Break the line at the point and indent the new line.

If the point is inside a comment, continue the comment.  If the comment is a
multiline comment, close the previous comment and start new one if
`comment-multi-line' is nil.
See `indent-new-comment-line' for SOFT."
  (interactive)
  (let* ((chunk (hylo-mode:chunk-after))
         (comment-beginning-position (hylo-mode:chunk:start chunk)))
    (if soft (insert-and-inherit ?\n) (newline 1))
    (delete-horizontal-space)
    ;; Inserts a prefix and indents the line.
    (cond
     ((not (hylo-mode:chunk:comment-p chunk))
      (indent-according-to-mode))

     ((hylo-mode:chunk:single-line-comment-p chunk)
      (insert-and-inherit
       (save-excursion
         (goto-char comment-beginning-position)
         (looking-at "/+\\s *")
         (match-string-no-properties 0)))
      (indent-according-to-mode))

     ((not comment-multi-line)
      (insert-and-inherit
       (save-excursion
         (goto-char comment-beginning-position)
         (looking-at "/\\*+\\s *")
         (match-string-no-properties 0)))
      ;; Cleans up and closes the previous line.
      (save-excursion
        (forward-line 0)
        (backward-char)
        (delete-horizontal-space)
        (insert-and-inherit " */"))
      (indent-according-to-mode))

     (t
      (hylo-mode:format-multiline-comment-line-after-newline chunk soft)))
    ;; Cleans up the previous line.
    (save-excursion
      (forward-line 0)
      (backward-char)
      (delete-horizontal-space))))

(defun hylo-mode:format-multiline-comment-line-after-newline (chunk soft)
  "Insert prefix and indent current line in multiline comment.

The point is assumed inside multiline comment and just after newline.

The closing delimiter is also inserted and/or formatted depending on custom
variables `hylo-mode:auto-close-multiline-comment' and
`hylo-mode:break-line-before-comment-close'.

CHUNK is the comment chunk.

See `indent-new-comment-line' for SOFT."
  (let ((comment-beginning-position (hylo-mode:chunk:start chunk)))
    (cond
     ((save-excursion
        (forward-line -1)
        (<= (point) comment-beginning-position))
      ;; The cursor was on the 2nd line of the comment.

      ;; If the comment have only one line, delete a space after asterisk.
      ;;
      ;; Example:
      ;; /** aaa */
      ;; ↓
      ;; /**
      ;;  aaa
      ;;  */
      ;;
      ;; /** aaa
      ;;  */
      ;; ↓
      ;; /**
      ;;     aaa
      ;;  */
      (when (= (line-beginning-position)
               (save-excursion
                 (goto-char comment-beginning-position)
                 (forward-comment 1)
                 (line-beginning-position)))
        (save-excursion
          (goto-char comment-beginning-position)
          (forward-char)
          (skip-chars-forward "*")
          (when (looking-at " [ \t]*$")
            (delete-char 1))))

      ;; If the point is just before the closing delimiter, breaks the line.
      (when (and hylo-mode:break-line-before-comment-close
                 (= (point)
                    (save-excursion
                      (goto-char comment-beginning-position)
                      (if (forward-comment 1)
                          (progn
                            (backward-char)
                            (skip-chars-backward "*")
                            (point))
                        -1))))
        (save-excursion
          (if soft (insert-and-inherit ?\n) (newline 1))
          (indent-according-to-mode)))

      ;; Invokes `hylo-mode:indent-line`
      (indent-according-to-mode)

      ;; Inserts or replaces a space to an asterisk.
      (when hylo-mode:prepend-asterisk-to-comment-line
        (let ((columns-from-end (- (line-end-position) (point))))
          (move-to-column
           (save-excursion
             (goto-char comment-beginning-position)
             (forward-char)
             (current-column)))
          (insert-and-inherit "*")
          (when (eq (char-after) ?\s)
            (delete-char 1))
          (when (and
                 hylo-mode:insert-space-after-asterisk-in-comment
                 (not (eq (char-after) ?\s)))
            (insert-and-inherit " "))
          (goto-char (- (line-end-position) columns-from-end)))))

     ;; The cursor was on the 3nd or following lines of
     ;; the comment.
     ;; Uses the prefix of the previous line.

     ((and
       hylo-mode:prepend-asterisk-to-comment-line
       (save-excursion
         (forward-line -1)
         (looking-at "\\s *\\(\\*+\\s *\\)")))
      ;; The previous line has a prefix.  Uses it.
      (insert-and-inherit (match-string-no-properties 1))
      (indent-according-to-mode))

     (t
      ;; Uses the default indentation.
      (indent-according-to-mode)))
    ;; Closes incomplete multiline comment.
    (when (and hylo-mode:auto-close-multiline-comment
               (hylo-mode:incomplete-comment-p chunk))
      (save-excursion
        (end-of-line)
        (when comment-multi-line
          (if soft (insert-and-inherit ?\n) (newline 1)))
        (insert-and-inherit "*/")
        (indent-according-to-mode)))
    ;; Make sure the closing delimiter is on its own line.
    (when hylo-mode:break-line-before-comment-close
      (save-excursion
        (goto-char comment-beginning-position)
        (when (forward-comment 1)
          (backward-char)
          (skip-chars-backward "*")
          (skip-syntax-backward " ")
          (when (not (bolp))
            (if soft (insert-and-inherit ?\n) (newline 1))
            (indent-according-to-mode)))))))

(defun hylo-mode:post-self-insert ()
  "Miscellaneous logic for electric indentation."
  (cond
   ;; Indents electrically and insert a space when "*" is inserted at the
   ;; beginning of a line inside a multiline comment.
   ((and
     hylo-mode:prepend-asterisk-to-comment-line
     (= last-command-event ?*)
     (hylo-mode:chunk:comment-p (hylo-mode:chunk-after))
     (save-excursion (backward-char) (skip-syntax-backward " ") (bolp)))
    (when hylo-mode:insert-space-after-asterisk-in-comment
      (insert-and-inherit " "))
    (when electric-indent-mode
      (indent-according-to-mode)))

   ;; Fixes "* /" at the end of a multiline comment to "*/".
   ((and
     hylo-mode:fix-comment-close
     (= last-command-event ?/)
     (let ((chunk (hylo-mode:chunk-after))
           (pos (point)))
       (and
        (hylo-mode:chunk:comment-p chunk)
        (save-excursion
          (forward-line 0)
          (and
           (looking-at "^\\s *\\*\\s +/")
           (eq (match-end 0) pos)
           (hylo-mode:incomplete-comment-p chunk))))))
    (backward-char)
    (delete-horizontal-space)
    (forward-char))

   ;; Indents electrically when ")" is inserted at bol as the end of a string
   ;; interpolation.
   ((and
     electric-indent-mode
     (= last-command-event ?\))
     (save-excursion (backward-char) (skip-syntax-backward " ") (bolp))
     (eq (hylo-mode:chunk:start (hylo-mode:chunk-after)) (1- (point))))
    (indent-according-to-mode))

   ;; Indents electrically after newline inside strings and comments.
   ;; Unlike `electric-indent-mode', the previous line is not indented.
   ((and
     electric-indent-mode
     (= last-command-event ?\n))
    (let ((chunk (hylo-mode:chunk-after)))
      (if (hylo-mode:chunk:multiline-comment-p chunk)
          (progn
            (delete-horizontal-space)
            (hylo-mode:format-multiline-comment-line-after-newline
             chunk
             (not use-hard-newlines)))
        (indent-according-to-mode)))
    (save-excursion
      (forward-line 0)
      (backward-char)
      (delete-horizontal-space)))))

(defun hylo-mode:highlight-anchor (indentation)
  "Highlight the anchor point of the INDENTATION."
  (move-overlay
   hylo-mode:anchor-overlay
   (hylo-mode:indentation:point indentation)
   (1+ (hylo-mode:indentation:point indentation)))
  (overlay-put hylo-mode:anchor-overlay 'face 'highlight)
  (when hylo-mode:anchor-overlay-timer
    (cancel-timer hylo-mode:anchor-overlay-timer))
  (let ((buffer (current-buffer)))
    (setq hylo-mode:anchor-overlay-timer
          (run-at-time
           "1 sec"
           nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (delete-overlay hylo-mode:anchor-overlay)
                 (setq hylo-mode:anchor-overlay-timer nil))))))))

(provide 'hylo-mode-indent)

;;; hylo-mode-indent.el ends here
