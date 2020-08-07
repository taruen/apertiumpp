#lang br/quicklang

;; #lang apertiumpp/morph
;;
;; If the text file example.rkt has the following content:
;; ```
;; #lang apertiumpp/morph
;; "<books"
;;     "book" n pl
;;     "book" vblex pres p3 sg
;; "<reads>"
;;     "read" vblex pres p3 sg
;; ```
;; Passing text through `racket example.rkt', e.g.
;; echo "He books a hotel."
;;
;; will tokenize the text according to example.rkt (by left-to-right longest
;; match principle) and replace tokens with token+readings from example.rkt.
;;
;; Thus, with the above example.rkt, you'll get the following output:
;; ```
;; <He>
;;     "*He"
;; "<books>"
;;     "book" n pl
;;     "book" vblex pres p3 sg
;; ```
;; Optionally you can specify a morphological analyser to query it for analyses
;; of tokens not covered by example.rkt. The contents of the latter will take
;; precedence.
;;
;; The point of this little dsl is to have a way of `patching' and stabilizing
;; the output of a morphological
;; transducer for corpus annotating tasks, without the trouble of dealing
;; with ad-hoc regular expression-based replaces directly (all that happens under
;; the hood). It also allows to postpone the task of correcting the transducer
;; itself (which ideally should be done, escpecially to fix the general source of
;; the erros observed).
;;
;; The data gathered can then be used as automatic tests for the transducer
;; (TODO #lang apertiumpp/morph/test)
;;
;; The more general goal of this dsl is to serve as some kind of a pre-processor
;; to a deterministic non-weighted morphological transducer, doing some guessing
;; and patching work as needed.
;;
;; To heal the divide between annotators and programmers is the motto (by turning
;; annotation into code. By *making* it code).
;;
;; Alternative format (current choice, as it is slightly easier to implement):
;;
;; #lang apertiumpp/morph
;; ^books/book<n><pl>/book<vblex><pres><p3><sg>$
;;
;; TODO Either way, #lang apertiumpp/morph should make sure that the tokens
;; contained in it are unique.

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define module-datum `(module morph "morph.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))

(provide read-syntax)

(define-macro (morph-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...))

(provide (rename-out [morph-module-begin #%module-begin]))

(define-macro-cases handle
  [(handle) (void)]
  [(handle ARG)
   #'(begin
       (define pat (symbol->string (syntax->datum #'ARG)))
       (define lu (string-split (substring pat 1 (- (string-length pat) 1)) "/"))
       (define input (port->string (current-input-port)))
       (display (regexp-replace (first lu) input pat)))])

(provide handle)
