#lang racket

(require net/url
         csv-reading)

(module+ test
  (require rackunit))

(provide
 iso-639-3-table
 iso-639-3->refname)


;; Void -> (List (List String))
(define (iso-639-3-table)
  
  (define tsv
    (port->string
     (get-pure-port
      (string->url "https://iso639-3.sil.org/sites/iso639-3/files/downloads/iso-639-3.tab"))))

  (define next-row
    (make-csv-reader
     tsv
     '((separator-chars #\tab))))
  
  (csv->list next-row))


(define ISO-639-3-TABLE (iso-639-3-table))


;; String -> (or/c String #f)
;; given ISO-639-3 code of a language, return its reference name in English
;; if code is known, #f otherwise
(define (iso-639-3->refname code)
  (define entry
    (findf (λ (line) (string=? code (first line))) ISO-639-3-TABLE))
  (if entry
      (list-ref entry 6)
      #f))
      

(module+ test
  (check-equal? (iso-639-3->refname "zkz") "Khazar")
  (check-equal? (iso-639-3->refname "made-up-non-existing-code") #f))


