#lang info
(define collection "apertiumpp")
(define deps '("base" "rash" "html-parsing" "brag" "sxml" "beautiful-racket" "csv-reading"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/apertiumpp.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '("Ilnar Salimzianov"))
(define raco-commands '(("apertiumpp" (submod apertiumpp/private/command raco) "issue apertiumpp command" #f)))
