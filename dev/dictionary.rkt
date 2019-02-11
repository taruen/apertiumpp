#lang racket

;; The aim of this library is to add programmability to Apertium's morphological
;; dictionaries, automate whatever can be automated and eliminate redundancy in
;; them, if any.
;;
;; Technically speaking, the goal here is to define datatypes for representing
;; information contained in a typical Apertium .lexc or monolingual .dix file,
;; in Racket, as well as to write functions for manupilating them.

(provide (all-defined-out))


;;;;;;;;;;;;
;; Constants


;;;;;;;;;;;;;;;;;;;
;; Data definitions


(struct lmnt (attrs name con) #:transparent)
;; An Lmnt is one of:
;; - String
;; - (lmnt Attrs Symbol (listof Lmnt))
;; interp. a-la xml element with attributes and content

(define LMNT1 "")
(define LMNT2 "hargle")
(define LMNT3 (lmnt (hash) 'b  empty))             ; <b/>
(define LMNT4 (lmnt (hash) 'par  '("abuel/o__n"))) ; <par>abuel/o__n</par>
(define LMNT5 (lmnt (hash 'n "abuel/o__n")         ; <par n="abuel/o__n"/>
                    'par  empty))                       
(define LMNT6 (lmnt (hash 'lm "perro") 'e       ; <e lm="perro">
                    `((lmnt (hash) 'l ("perr")) ;   <l>perr</l>
                      (lmnt (hash) 'r ("perr")) ;   <r>perr</r>
                      ,LMNT5)))                 ;   <par n="abuel/o__n"/></e>

(define (fn-for-lmnt lmnt)
  (cond [(string? lmnt) (...)]
        [else (... (fn-for-attrs (lmnt-attrs lmnt)) ;; Attrs
                   (lmnt-name lmnt)                 ;; Symbol
                   (fn-for-listof-lmnt (lmnt-con lmnt)))]))

(define (fn-for-listof-lmnt lol)
  (cond [(empty? lol) (...)]
        [else
         (... (fn-for-lmnt (first lol))
              (fn-for-listof-lmnt (rest lmnt)))]))


;; An Attrs is (hash Symbol->String)
;; interp.: attributes of or comments to an element in a dictionary

(define A-1 (hash 'lm "perro" 'r "LR"))
(define A-2 (hash 'eng "Noun" 'tat "Исем"))

(define (fn-for-attrs . args)
  void)

(struct sdef (attrs name xmpl) #:transparent)
;; An Sdef is (sdef Attrs String String)
;; interp.: a symbol (tag) `name', along with comments -- in the `attrs'
;; field -- in one or more languages, and an example sentence, phrase of word

(define SDEF-1 (sdef A-2 "n" "китап"))

(define (fn-for-sdef s)
  (... (sdef-attrs s)  ;; Attrs
       (sdef-name s)   ;; String
       (sdef-xmpl)))   ;; String


(struct e (attrs l r par) #:transparent)
;; An Entry is (e Attrs Lmnt Lmnt Lmnt)
;; interp.: an entry of a monolingual dictionary with:
;; - attributes such r=LR, r=RL, lang=tat, lm (lemma) and others,
;; - left (= upper, lexical) string `l',
;; - right (= lower, surface) string `r',
;; - linked to continuation lexicon / paradigm `par'

;;              HERE                          ; In Apertium's monolingual .dix
(define E-1 (e (hash 'lang "spa" 'lm "perro") ; <e lang="spa" lm="perro">
               (l (hash) "perr")              ;  <p><l>perr</l>
               (r (hash) "perr")              ;     <r>perr<r/></p>
               (par (hash) "abuel/o__n")))    ;  <par n="abuel/o__n"</par></e>

(define E-2 (e (hash 'lang "tur" 'use "mt")
               (l (hash) "sahip ol")
               (r (hash) "sahip ol")
               (par (hash) "N1")))


(define (fn-for-e e)
  (... (e-attrs e)                ;; Attrs
       (fn-for-lmnt (e-l e))      ;; Lmnt
       (fn-for-lmnt (e-r e))      ;; Lmnt
       (fn-for-lmnt (e-par e))))  ;; Lmnt


(struct dictionary (alphabet sdefs archiphonemes pardefs sections))
;; A Dictionary is (dictionary (listof Symbol)
;;                             (listof Sdef)
;;                             (listof Symbol)
;;                             (listof Pardef)
;;                             (listof Section))
;; interp.: a monolingual dictionary

(define D-1 (dictionary '(А а Ә ә Б б В в Г г Д д Е е Ё ё Ж ж Җ җ З з И и Й й
                            К к Л л М м Н н О о Ө ө П п Р р С с Т т У у Ү ү Ф ф
                            Х х Һ һ Ц ц Ч ч Ш ш Щ щ Ъ ъ Ы ы Ь ь Э э Ю ю Я я)
                        `(SDEF-1 ,(sdef "v" (hash 'eng "Verb"
                                                  'tat "Фигыль") ""))
                        '(L N M G D A I S K n l y o E д т а э ә й л н з т с ☭ i
                            ъ ь)
                        '()
                        '()))


;;;;;;;;;;;;;;;;;;;;;
;; Functions & Macros


;; Allow typing:
;; (r (hash 'use "archaic") "foo")
;; instead of having to type:
;; (lmnt 'r (hash 'use "mt") '("front cover"))
;; Similarly for l and par.
(define-syntax l
  (syntax-rules ()
    [(l attrs con) (lmnt attrs 'l (list con))]))
(define-syntax r
  (syntax-rules ()
    [(r attrs con) (lmnt attrs 'r (list con))]))
(define-syntax par
  (syntax-rules ()
    [(par attrs con) (lmnt attrs 'par (list con))]))


(define (... . args)
  void)


;; wishlist
