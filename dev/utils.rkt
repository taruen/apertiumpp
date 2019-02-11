#lang rash

;; Various functions for interacting with the `system-under-test',
;; i.e. Apertium and its components

(require rackunit)

(provide apertium)


;;;;;;;;;;;;
;; Constants


;;;;;;;;;;;;
;; Functions


;; String Symbol Symbol -> String
;; pass input string `in' through `apertium -d dir mode' command and return
;; the result, i.e.: echo $in | apertium -d $dir $mode

(define (apertium in dir mode)
        #{echo (values in) | apertium -d (values dir) (values mode)})

(check-equal? (apertium "!"
                        'apertium-all/apertium-languages/apertium-eng
                        'eng-morph)
              "^!./!.<sent>$")