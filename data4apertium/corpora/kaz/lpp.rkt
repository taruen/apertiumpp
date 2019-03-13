#lang racket

(require rackunit
         apertium-kaz
         apertium-tat
         apertium-kaz-tat)

(define lang "kaz")
(define title "The Little Prince")
(define license "all rights reserved")
(define translation-published-in "2013")
(define isbn "978-601-282-723-1")
(define translator "Ж. Қонаева")

;(define (test sentence words)
;  (begin
;    (printf "~a\n" sentence)
;    (display (kaz-tat sentence))))

;(test
; "Кішкентай ханзада"
; '(("Кішкентай" "кішкентай" (ADJ) A2)
;   ("ханзада" "ханзада" (N NOM) N1)))

(check-equal? (kaz-disam "Кішкентай ханзада") "^Кішкентай/кішкентай<adj>$ ^ханзада/ханзада<n><nom>$")

(check-equal? (tat-disam "Кечкенә ханзадә") "^Кечкенә/кечкенә<adj>$ ^ханзадә/ханзадә<n><nom>$")

(check-equal? (kaz-tat "Кішкентай ханзада") "Кечкенә ханзадә")
(check-equal? (tat-kaz "Кечкенә ханзадә") "Кішкентай ханзада")