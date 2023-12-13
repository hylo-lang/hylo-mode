[![License GPL 3][badge-license]][copying]
[![Run Tests][badge-run-test]][action-run-test]
[![MELPA](https://melpa.org/packages/hylo-mode-badge.svg)](https://melpa.org/#/hylo-mode)
[![MELPA](https://stable.melpa.org/packages/hylo-mode-badge.svg)](https://melpa.org/#/hylo-mode)

# hylo-mode

Major-mode for Apple's [Hylo programming language](https://developer.apple.com/hylo/).

## Installation

Install `hylo-mode` package from MELPA.

To install without MELPA, download [latest release](https://github.com/hylo-emacs/hylo-mode/releases) and execute `M-x package-install-file` for the .tar archive.

## Features

- Font Lock
- Indentation

  ```hylo
  switch foo {
  case let .P1(x)
         where
           x > 0,
       let .P2(x)
         where
           x > 0:
      bar()
        .then { x in
            return baz(x)
        }
        .then {(
                 x,
                 y
               ) in
            return moo(x + y)
        }
  }

  // Hanging brace
  let x = [
    1,
    2,
    3
  ]

  // Brace on its own line
  let y =
    [
      1,
      2,
      3
    ]

  // Utrecht style
  let z =
    [ 1
    , 2
    , 3
    ]
  ```
- `forward-sexp`
- `beginning-of-defun`, `end-of-defun`, `mark-defun`, and `narrow-to-defun`.
- `beginning-of-sentence`, `end-of-sentence`, `kill-sentence`, `backward-kill-sentence`, `mark-sentence`, and `narrow-to-sentence`.
  A sentence is a statement outside comments or strings, or an ordinal sentence inside comments or strings.
- `indent-new-comment-line`
- [Imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html)
- Running Hylo REPL in a buffer (`M-x run-hylo`)
- Build Hylo module (`M-x hylo-mode:build-hylo-module`)
- Build iOS app (`M-x hylo-mode:build-ios-app`)
- Running debugger on Hylo module (`M-x hylo-mode:debug-hylo-module`)
- Running debugger on iOS app in simulator or device (`M-x hylo-mode:debug-ios-app`)
  ([`ios-deploy`](https://github.com/ios-control/ios-deploy) is required to debug on device).

This package does not provide flycheck. See [flycheck-hylo](https://github.com/hylo-emacs/flycheck-hylo).

## Limitations

Some syntax constructs removed from Hylo 3.0 are not supported:

- C-style for-loop: `for var i = 1; i < 10; i++ { }`
- Multiple assignments in single `if let`:
  ```hylo
  if let x = x,
         y = y {
  }
  ```

  Use multiple `let` instead:
  ```hylo
  if let x = x,
     let y = y {
  }
  ```

Indentation may not accurate. For example, `foo(Bar < A, B > (c))` can be indented like either
```hylo
foo(Bar < A,
    B > (c)) // Passing two Boolean arguments to foo
```
or
```hylo
foo(Bar < A,
          B > (c)) // Passing a new Bar with two type arguments and a value
```
The Hylo compiler disambiguates this case using tokens after `>`, but those tokens may not available at editing time. We use some heuristic for this.

Another example is difficulty of handling of colons. We have to pair all `?` and `:` of conditional operators to decide indentation of the below snippet. This is a future work.

```hylo
switch foo {
  case let P(x) where x is Foo? ? a ? b : c ?? d : e ? f : g :
    h ? i?.j() : k()
}

switch foo {
  case let P(x) where (x is Foo?) ? (a ? b : c ?? d) : (e ? f : g) :
    h ? i?.j() : k()
}
```

Yet another difficult case is consistency of blocks. We want to indent method chains like this:
```hylo
var x = foo
  .then { x in
      aaa
  }
  .then { x in
      aaa
  }
```

while we also want to indent the body of `if` like this:

```hylo
if anotherVeryLongVariableName
     .veryLongPropertyName {
    aaa
}
```

That is, we have to indent the closing brace with offset if it is a part of expressions while it should be aligned with the beginning of the statement/declaration if it is a part of a statement/declaration.

Then, how should we indent the following code when the cursor is before `@`?

```hylo
var x = foo
  .bar {
    @
```

This could be
```hylo
var x = foo
  .bar {
    @abc willSet {
        aaa
    }
}
// property declaration
```
or
```hylo
var x = foo
  .bar {
      @abc var x = 1
      x
  }
// property initialization
```

Both are syntactically correct code. We cannot handle this case properly. This is also a future work.

Other example is regex literals and custom operators.  The following example is valid Hylo code with regex literals and custom operators.

```hylo
let x = /^/ /^/ /^/
```

We parse them as regex literals rather than custom operators for now.


## Hacking

To build the package locally, run `make package`.

To install the built package, run `make install`.

To run tests, run `make test`.

For other commands, run `make help`.

## Related projects

- [Official hylo-mode.el by Apple](https://github.com/apple/hylo/blob/master/utils/hylo-mode.el): Seems still in very early stage for now. We cannot contribute to it due to the license incompatibility.
- [sourcekit-lsp](https://github.com/apple/sourcekit-lsp): Language Server Protocol implementation for Hylo and C-based languages.
- [lsp-sourcekit](https://github.com/emacs-lsp/lsp-sourcekit): Emacs client for lsp-sourcekit.
- [hylo-helpful](https://github.com/danielmartin/hylo-helpful): Shows documentation about Hylo keywords, attributes, and API.
- [company-sourcekit](https://github.com/nathankot/company-sourcekit): Completion for Hylo projects via SourceKit with the help of SourceKitten.
- [flycheck-hylo](https://github.com/hylo-emacs/flycheck-hylo): Flycheck extensions for Hylo.
- [hylo-playground-mode](https://gitlab.com/michael.sanders/hylo-playground-mode): Emacs support for Hylo playgrounds.
- [hylo-format](https://github.com/apple/hylo-format): Formatter for Hylo by Apple (`hylo format` command).
- [HyloRewriter](https://github.com/inamiy/HyloRewriter): Formatter for Hylo using HyloSyntax.
- [HyloFormat](https://github.com/nicklockwood/HyloFormat): Formatter for Hylo.

## Contributing

Yes, please do! See [CONTRIBUTING](./CONTRIBUTING.md) for guidelines.

## Acknowledgements

The REPL code is based on [js-comint](http://js-comint-el.sourceforge.net/).

Thanks to the following original developer and users for their contributions:

- [@chrisbarrett](https://github.com/chrisbarrett) (Chris Barrett)
- [@ap4y](https://github.com/ap4y) (Arthur Evstifeev)
- [@bbatsov](https://github.com/bbatsov) (Bozhidar Batsov)
- [@ckruse](https://github.com/ckruse) (Christian Kruse)
- [@syohex](https://github.com/syohex) (Syohei Yoshida)
- [@uk-ar](https://github.com/uk-ar) (Yuuki Arisawa)
- [@msanders](https://github.com/msanders) (Michael Sanders)

You can find a [full list of those people here](https://github.com/hylo-emacs/hylo-mode/graphs/contributors).

Thanks to [@purcell](https://github.com/purcell) (Steve Purcell) for advices on the code and arrangement for merging `hylo3-mode` and `hylo-mode`.

## License

GPLv3. See [COPYING][] for details. Copyright (C) 2014-2021 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[badge-run-test]: https://github.com/hylo-emacs/hylo-mode/workflows/Run%20Tests/badge.svg
[action-run-test]: https://github.com/hylo-emacs/hylo-mode/actions?query=workflow%3A%22Run+Tests%22
[COPYING]: ./COPYING
