#lang rash

;; This file lists all lemmas which are expected to be in Apertium's Tatar
;; dictionary, along with their analyses and translations into other languages.
;;
;; The list includes the lemmas from the "Explanatory Dictionary of Tatar"
;; ("Татар теленең аңлатмалы сүзлеге"), volumes 1-3, published in 2015-2017
;; and which can be downloaded from the official website of the publisher
;; (Institute for Language, Literature and Arts of the Academy of Sciences of
;; Tatarstan) at http://antat.ru/ru/iyli/publishing/book/ and also accecced
;; electronically at http://suzlek.antat.ru.
;;
;; By email, the publisher gave us (Ilnar Salimzianov) permission to use the
;; entry words from that dictionary, as well as their POS information, in
;; Apertium's dictionaries, for which we thank them very much.

(require rackunit)


;;;;;;;;;;;;
;; Constants


(define A-TAT '../../apertium-all/apertium-languages/apertium-tat)


;;;;;;;;;;;;;;;;;;;
;; Vocabulary/tests


(check-equal?
 #{echo "а" | apertium -d (values A-TAT) tat-morph}
 "^а/а<ltr>/а<ij>/а<ideo>$")