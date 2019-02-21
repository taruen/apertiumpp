#lang racket

(require (for-syntax syntax/parse))

(require rash)
(require rackunit)
(require apertiumpp)

(define WORDLIST (mutable-set))
(define DIR (make-parameter '/home/selimcan/1Working/1Apertium++/apertium-all/apertium-languages/apertium-kaz/))
(define MODE (make-parameter 'kaz-morph))
(define INPUT (make-parameter "CNN"))

;;;;;;;;;;;;;;;;;;;;;
;; Functions & macros

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ surface (list lexicals ...))
     #'(begin
         (if (set-member? WORDLIST surface)
             (error
              (string-append
               "Multiple test cases for the same surface form: " surface))
             (set-add! WORDLIST surface))
         (define lu
           (parameterize ([INPUT surface])
             (rash "echo (INPUT) | apertium -f none -d (DIR) (MODE)")))
         (for ([lexical (syntax->datum #'(lexicals ...))])
           (if (string-suffix? lexical "!")
               (check-false
                (lu-contains? lu (substring
                                  lexical
                                  0 (- (string-length lexical) 1))))
               (check-equal? (lu-contains? lu lexical) lexical))))]))

(test "бала" ("бала<n><nom"))
(test "кет" ("кет<v><iv><imp><p2><sg>" "кет<v><tv><imp><p2><sg>!"))