(require 'generic-x)

(define-generic-mode 'nano-mode

  ;; Comments start with #
  '(?#)

  ;; Keywords
  '("fun" "val" "sig" "type" "infixl" "infixr"
    "let" "in" "if" "then" "else"
    "case" "of" "end"
    "fn" "λ"
    "inl" "inr")

  ;; Extra expressions
  '(

    ;; functions an val names after fun, val and sig keywords
    ("[ \t]*\\(fun\\|val\\|sig\\)\\>[ \t]*\\([a-zA-Z][a-zA-Z0-9_']*\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

    ;; Types
    ("\\<\\([A-Z][a-zA-Z0-9_']*\\)\\>" . 'font-lock-type-face)

    ;; Using variables face for symbols a la haskell-mode
    ("->"   . 'font-lock-variable-name-face)
    (">>"   . 'font-lock-variable-name-face)
    ("<<"   . 'font-lock-variable-name-face)
    ("=>"   . 'font-lock-variable-name-face)
    (":"  . 'font-lock-variable-name-face)
    ("+"  . 'font-lock-variable-name-face)
    ("*"  . 'font-lock-variable-name-face)
    ("-"  . 'font-lock-variable-name-face)
    ("/"  . 'font-lock-variable-name-face)
    ("!"  . 'font-lock-variable-name-face)
    ("%"  . 'font-lock-variable-name-face)
    ("&"  . 'font-lock-variable-name-face)
    ("<"  . 'font-lock-variable-name-face)
    ("="  . 'font-lock-variable-name-face)
    (">"  . 'font-lock-variable-name-face)
    ("\\\\"   . 'font-lock-variable-name-face)
    ("|"  . 'font-lock-variable-name-face)
    ("@"  . 'font-lock-variable-name-face)
    ("\\."  . 'font-lock-variable-name-face)
    ("\\$"  . 'font-lock-variable-name-face)
    ("\\^"  . 'font-lock-variable-name-face)
    ("\\?"  . 'font-lock-variable-name-face)

    ;; Braces, brackets, parents and some other keywords
    ("[[]" . 'font-lock-keyword-face)
    ("[]]" . 'font-lock-keyword-face)
    ("[(]" . 'font-lock-keyword-face)
    ("[)]" . 'font-lock-keyword-face)

    ;; ;; Builtins
    ("\\<\\(fix\\)\\>" . 'font-lock-constant-face)

    ;; ;; Constants
    ("\\<\\(true\\)\\>" . 'font-lock-constant-face)
    ("\\<\\(false\\)\\>" . 'font-lock-constant-face)
    ("\\_<[0-9]+\\_>" . 'font-lock-constant-face)
    )

  ;; File extensions to add to auto-mode-alist
  nil ;; '("\\.nano\\'")

  '(nano-mode-setup)
  )

(defun nano-mode-setup ()
  (smartparens-mode))
