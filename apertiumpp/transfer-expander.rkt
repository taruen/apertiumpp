#lang br/quicklang

(define-macro (transfer-mb PARSE-TREE)
  #'(#%module-begin
     (define result-string PARSE-TREE)
     (display result-string)))

(provide (rename-out [transfer-mb #%module-begin]))

(define-macro (transfer-char CHAR-TOK-VALUE)
  #'CHAR-TOK-VALUE)

(provide transfer-char)

(define-macro (transfer-file SEXP-OR-JSON-STR ...)
  #'(string-trim (string-append SEXP-OR-JSON-STR ...)))

(provide transfer-file)

(define-macro (transfer-sexp SEXP-STR)
  (with-pattern ([SEXP-DATUM (format-datum '~a #'SEXP-STR)])
    #'SEXP-DATUM))

(provide transfer-sexp)
