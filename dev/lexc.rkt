#lang racket

(require xml xml/path rackunit)
(require "dictionary.rkt")
(provide (all-defined-out))


;;;;;;;;;;;;
;; Constants


;(define TAGSET (vector-ref (current-command-line-arguments) 0))
(define BEGIN-TAG "%<")
(define END-TAG "%>")


(define ENTRY-1
  '(e ([dir "RL"][use "MB"]) (l "мы") (r "%~мы") (par "QST")))

(define ENTRY-2
  '(e ([dir "LR"]) (l "мы") (r "мы") (par "QST")))

(define ENTRY-3
  '(e (l "китап") (r "китап") (par "N1")))

(define SECTION-1
  `((lm "мы" ,ENTRY-1
        ,ENTRY-2)
    (lm "китап" ,ENTRY-3)))


;;;;;;;;;;;;;;;;;;;;;
;; Functions & Macros


(define-syntax-rule (tag id surf)
  ;; (tag R_ZE "N") sets R_ZE to %<N%> and prints that
  ;; (tag R_ZE (nla R_ZE leipzig NN apertium)) sets R_ZE to %<R_ZE%> if the TAGSET
  ;;   constant is set to "nla", to %<NN%> if the TAGSET constant is set to "leipzig"
  ;;   etc and prints that value
  ;; (tag N R_ZE) sets the value of N to the value of R_ZE and prints it
  (begin

    (define (format tag)
      (string-append BEGIN-TAG tag END-TAG))

    (define id
      (cond
        [(string? (quote surf)) (if (string=? surf "")
                                    ""
                                    (format surf))]
        [(list? (quote surf))
         (format (hash-ref
                  (apply hash (map symbol->string surf))
                  TAGSET))]
        [else surf]))

    id))


(define (entry->string e)
  ;; turn an entry into a .lexc Root lexicon entry
  (let ([l (se-path* '(l) e)]
        [r (se-path* '(r) e)]
        [par (se-path* '(par) e)]
        [dir (se-path* '(e #:dir) e)]
        [use (se-path* '(e #:use) e)])
    (string-append l ":" r " " par " ; ! \"\""
                   (if dir (string-append " Dir/" dir) "")
                   (if use (string-append " Use/" use) ""))))

(module+ test
  (require rackunit)
  (check-equal? (entry->string ENTRY-1)
                "мы:%~мы QST ; ! \"\" Dir/RL Use/MB")
  (check-equal? (entry->string ENTRY-2)
                "мы:мы QST ; ! \"\" Dir/LR")
  (check-equal? (entry->string ENTRY-3)
                "китап:китап N1 ; ! \"\""))


(define (section s)
  ;; turn a `section' with a list of entries into strings which can be put
  ;; into a .lex stems lexicon
  (string-join
   (map entry->string
        (for*/list ([lm s]
                    [x lm]
                    #:when (and (list? x) (equal? (first x) 'e)))
          x)) "\n"))

(module+ test
  (check-equal? (section SECTION-1)
                (string-append
                 "мы:%~мы QST ; ! \"\" Dir/RL Use/MB\n"
                 "мы:мы QST ; ! \"\" Dir/LR\n"
                 "китап:китап N1 ; ! \"\"")))