#lang br/quicklang

(require "transfer-tokenizer.rkt" "transfer-parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module transfer-module apertiumpp/transfer-expander ,parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)
