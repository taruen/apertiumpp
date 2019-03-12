#lang rash

;; This file lists all lemmas which should and shouldn't be in Apertium's
;; Kazakh dictionary, along with their analyses and translations into other
;; languages.

(require rackunit)
(require (for-syntax racket/base
                     racket/match))

;;;;;;;;;;;;
;; Constants


(define A-KAZ '../../apertium-all/apertium-languages/apertium-kaz)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros / syntax / language


;; expand
;; (> '^ма<qst>$ "~ма")
;; into
;; (check-equal?
;;  #{echo '^ма<qst>$ | apertium -f none -d (values A-KAZ) kaz-gener}
;;  "~ма")

(define-syntax (> stx)
               (match (syntax->list stx)
                      [(list _ lexical surface)
                       (datum->syntax stx `(check-equal?
                                            #{echo (values ,lexical) | apertium -f none -d (values A-KAZ) kaz-gener}
                                            ,surface))]))


;;;;;;;;;;;;;;;;;;;
;; Vocabulary/tests


(check-equal?
 #{echo '^ма<qst>$ | apertium -f none -d (values A-KAZ) kaz-gener}
 "~ма")

(check-equal?
 #{echo '^ма<qst>$ | apertium -f none -d (values A-KAZ) kaz-gener}
 "~ма")

