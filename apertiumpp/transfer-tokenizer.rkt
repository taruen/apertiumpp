#lang br/quicklang

(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define transfer-lexer
      (lexer
       [(from/to "//" "\n") (next-token)]
       [(from/to "@$" "$@")
        (token 'SEXP-TOK (trim-ends "@$" lexeme "$@"))]
       [any-char (token 'CHAR-TOK lexeme)]))
    (transfer-lexer port))
  next-token)

(provide make-tokenizer)
