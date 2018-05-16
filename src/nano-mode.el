(require 'generic-x)

(define-generic-mode 'nano-mode
  ;; Comments start with #
  '(?#)
  ;; Keywords
  '("lambda" "λ" "case" "of" "end" "let" "in"
    "forall" "∀" ":" "->" "=>" "|" "=")
  ;; Extra expressions
  '(("[0-9]+" . 'font-lock-constant-face)
    ("{.+}" . 'font-lock-builtin-face)
    (":" . 'font-lock-keyword-face)
    ("default" . 'font-lock-builtin-face)
    ("true" . 'font-lock-type-face)
    ("false" . 'font-lock-type-face)
    ("Integer" . 'font-lock-type-face)
    ("Double" . 'font-lock-type-face)
    ("Bool" . 'font-lock-type-face)
    ("String" . 'font-lock-type-face))
  ;; File extensions to add to auto-mode-alist
  '("\\.nano\\'")
  ;; Call function for more setup
  '('nano-mode-setup)
  "Mode for nano")

(defun nano-mode-setup ()
  "Function for additional setup")
