#lang racket

(provide explode
         explode-bi-lus)

(require rackunit)


;;;;;;;;;;;;;;;;;;;
;; Data definitions


;; An ULU is a String
;; interp.: an UNAMBIGUOUS lexical unit (LU) with an optional surface form,
;;          followed by a /, followed by lemma, followed by tags,
;;          followed by a $.
;; Not parsing it is a bad idea, but keeping it as a string is ok for now

(define ULU-1 "^and/and<cnjcoo>$")
(define ULU-2 "^and<cnjcoo>$")


;; An ALU is a String
;; interp.: an AMBIGUOUS LU with an optional surface form and 2 or more
;;          analyses
;; Not parsing it is a bad idea, but keeping it as string is ok for now

(define ALU-1 "^books/book<n><pl>/book<vblex><pres><p3><sg>$")
(define ALU-2 "^book<n><pl>/book<vblex><pres><p3><sg>$")


;; A BLU is a String
;; interp.: a bilingual lexical unit with source language lexical form
;; (lemma + tags), followed by one or more target language lexical forms.

(define BLU-1 "^бала<n><nom>/child<n>$")
(define BLU-2 "^бала<n><nom>/child<n>/kid<n>/infant<nt>$")


;;;;;;;;;;;;
;; Functions


;; (or ULU ALU BLU) -> (listof (or ULU unambig. BLU)
;; turn a possibly ambiguous LU into multiple unambiguous LUs
;; ASSUME: there is no escaped /, < or > symbols the given LU
;; NOTE: keeps the surface form, surrounding it with ^$
(define (explode lu)
  (map (λ (lu) (string-append "^" lu "$"))
       (regexp-split
        #rx"/"
        (regexp-replace #rx"\\$$" (regexp-replace #rx"^\\^" lu "") ""))))

(check-equal? (explode "^ма<qst>$")
              '("^ма<qst>$"))
(check-equal? (explode "^ма/ма<qst>$")
              '("^ма$" "^ма<qst>$"))
(check-equal? (explode "^бала/бала<n<nom>/бала<n><attr>$")
              '("^бала$" "^бала<n<nom>$" "^бала<n><attr>$"))
(check-equal? (explode "^бала<n><nom>/мальчик<n><m><aa><nom>/ребёнок<n><m><aa><nom>$")
              '("^бала<n><nom>$" "^мальчик<n><m><aa><nom>$" "^ребёнок<n><m><aa><nom>$"))


(define (explode-bi-lus s)
  ;  (string? . -> . (listof (listof string?)))
  ;; turn a possibly ambiguous *bilingual* lexical unit into multiple
  ;; unambiguous bilingual lexical units

  (define (implode l)
    (let ([left (regexp-replace #rx"\\$$" (first l) "")]
          [rights (map (λ (s) (substring s 1)) (rest l))])
      (map list (map (λ (s) (string-append left "/" s)) rights))))

  (check-equal? (implode '("^бала<n><nom>$" "^child<n>$" "^kid<n>$" "^infant<n>$"))
                '(("^бала<n><nom>/child<n>$") ("^бала<n><nom>/kid<n>$") ("^бала<n><nom>/infant<n>$")))

  (define (^$ s)
    (cond
      [(and (regexp-match? #rx"^\\^" s) (regexp-match? #rx"\\$$" s)) s]
      [(and (regexp-match? #rx"^\\^" s) (not (regexp-match? #rx"\\$$" s)))
       (string-append s "$")]
      [(and (not (regexp-match? #rx"^\\^" s)) (regexp-match? #rx"\\$$" s))
       (string-append "^" s)]
      [else (string-append "^" s "$")]))

  (check-equal? (^$ "foo<n>") "^foo<n>$")
  (check-equal? (^$ "^foo<n>") "^foo<n>$")
  (check-equal? (^$ "foo<n>$") "^foo<n>$")
  (check-equal? (^$ "^foo<n>$") "^foo<n>$")
  
  (define lus (map ^$ (regexp-split #rx"\\$ +\\^" s)))
  (define almost (map implode (map explode lus)))
  (match (length almost)
    [1 (first almost)]
    [2 (for*/list ([i (first almost)]
                   [j (second almost)])
         (append i j))]
    [3 (for*/list ([i (first almost)]
                   [j (second almost)]
                   [k (third almost)])
         (append i j k))]))
                                                   
(check-equal? (explode-bi-lus "^ма<qst>/мы<qst>$")
              '(("^ма<qst>/мы<qst>$")))
(check-equal? (explode-bi-lus "^бала<n><nom>/child<n>/kid<n>$")
              '(("^бала<n><nom>/child<n>$") ("^бала<n><nom>/kid<n>$")))
(check-equal? (explode-bi-lus "^бала<n><nom>/child<n>/kid<n>/infant<n>$")
              '(("^бала<n><nom>/child<n>$") ("^бала<n><nom>/kid<n>$") ("^бала<n><nom>/infant<n>$")))
(check-equal? (explode-bi-lus "^бала<n><nom>/child<n>/kid<n>$ ^е<cop><aor><p3><sg>/be<vbser><pres><p3>/$")
              '(("^бала<n><nom>/child<n>$" "^е<cop><aor><p3><sg>/be<vbser><pres><p3>$")
                ("^бала<n><nom>/child<n>$" "^е<cop><aor><p3><sg>/$")
                ("^бала<n><nom>/kid<n>$" "^е<cop><aor><p3><sg>/be<vbser><pres><p3>$")
                ("^бала<n><nom>/kid<n>$" "^е<cop><aor><p3><sg>/$")))
