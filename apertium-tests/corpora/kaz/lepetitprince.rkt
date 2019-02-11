#lang rash

;; The Little Prince
;; License: all rights reserved
;; Publication year of Kazakh translation: 2013
;; ISBN: 978-601-282-723-1
;; Translator: Ж. Қонаева

(require rackunit)


;;;;;;;;;;;;
;; Constants


(define A-KAZ-TAT '../../apertium-all/apertium-trunk/apertium-kaz-tat)
(define A-KAZ-RUS '../../apertium-all/apertium-trunk/apertium-kaz-rus)
(define A-ENG-KAZ '../../apertium-all/apertium-trunk/apertium-eng-kaz)
(define A-TAT-RUS '../../apertium-all/apertium-nursery/apertium-tat-rus)
(define A-TAT-ENG '../../apertium-all/apertium-incubator/apertium-tat-eng)


;;;;;;;;;;;;;;;
;; Corpus/tests


(check-equal?
 #{echo "Кішкентай ханзада" | apertium -d (values A-KAZ-TAT) kaz-tat}
 "Кечкенә ханзадә")
(check-equal?
 #{echo "Кішкентай ханзада" | apertium -d (values A-KAZ-TAT) kaz-tat | apertium -d (values A-TAT-RUS) tat-rus}
 "Маленький принц")
(check-equal?
 #{echo "Кішкентай ханзада" | apertium -d (values A-KAZ-TAT) kaz-tat | apertium -d (values A-TAT-ENG) tat-eng}
 "Little prince")

(check-equal?
 #{echo "Бірде, алты жастағы кезімде, жабайы орман туралы ``Бастан кешкен оқиғалар'' деп аталатын кітаптан ғажайып бір сурет көрдім." | apertium -d (values A-KAZ-TAT) kaz-tat}
 "Бервакыт, алты яшьтәге чагымда, кыргый урман турында ``Баштан кичкән вакыйгалар'' дип аталучы китаптан гаҗәеп бер сурәт күрдем.")
