#lang at-exp racket

;;A representation of & functions for Apertium's (bilingual) dictionaries

(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

(module+ test
  (require rackunit))


;;;;;;;;;;;;
;; Constants


(define LR "LR")
(define RL "RL")
(define STANDARD "standard")
(define PREBLANK "preblank")
(define POSTBLANK "postblank")
(define INCONDITIONAL "inconditional")


;;;;;;;;;;;;;;;;;;;
;; Data definitions


(struct dictionary (lang alphabet sdefs pardefs sections))

(provide
 (struct*-doc
  dictionary ([lang symbol?]
              [alphabet string?]
              [sdefs (listof sdef?)]
              [pardefs (listof pardef?)]
              [sections (listof section?)])
  ("interpretation: an Apertium dictionary (read: contents of a .dix file)."
   @itemlist[@item{@(racket lang) : ISO 639-3 code(s) of the language(s)}
             @item{@(racket alphabet) : relevant for a monolingual dictionary
              only}
             @item{@(racket sdefs) : grammatical symbols used in dictionary
              entries}
             @item{@(racket pardefs) : paradigm definitions}
             @item{@(racket sections) : lists of dictionary entries}])))

(define D-0
  (dictionary
   'eng
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
   '() '() '()))

(provide
 (thing-doc
  D-0 (dictionary?)
  ("An example of an empty English dictionary:"
   @(racketblock
     (define D-0
       (dictionary
        'eng
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        '() '() '()))))))

(define D-B-0
  (dictionary
   'eng-tat
   ""
   '() '() '()))

(provide
 (thing-doc
  D-B-0 (dictionary?)
  ("An empty English-Tatar bilingual dictionary:"
   @(racketblock
     (define D-B-0
       (dictionary
        'eng-tat
        "" '() '() '()))))))


(struct sdef (n))

(provide
 (struct*-doc
  sdef ([n string?])
  ("interpretation: a symbol definition."
   @itemlist[@item{@(racket sdef-n) : grammatical symbol used in dictionary
              pardefs or entries.}])))

(define SDEF-O (sdef "n"))

(struct pardef (n con))

(provide
 (struct*-doc
  pardef ([n string?] [con (listof e?)])
  ("interpretation: a paradigm definition."
   @itemlist[@item{@(racket pardef-n) : the name of the paradigm}
             @item{@(racket pardef-con) : its content}])))


(struct section (n type con))

(provide
 (struct*-doc
  section ([n string?]
           [type (or/c STANDARD PREBLANK POSTBLANK INCONDITIONAL)]
           [con (listof e?)])
  ("interpretation: a section of a dictionary with entries"
   @itemlist[@item{@(racket section-n) : name/id of the section}
             @item{@(racket section-type) :
             A quote from the @hyperlink{https://taruen.github.io/organisation/index/modules-spec.html}{official
               documentation}:The value of the attribute type is used to
              express the kind of string tokenization applied in each
              dictionary section: the possible values of this attribute are:
              \"standard\", for almost all the forms of the dictionary
              (conditional mode), \"preblank\" and \"postblank\", for the forms
              that require an unconditional tokenisation and the placing of a
              blank (before and after, respectively), and \"inconditional\" for
              the rest of forms that require unconditional tokenization."}
             @item{@(racket section-con) : content (entries)}])))


(struct e (o re lm l r par))

(provide
 (struct*-doc
  e ([o (or/c LR RL)] [re string?] [lm string?] [l string?] [r string?]
                      [par string?])
  ("interpretation: an entry in a dictionary paradigm or section."
   @itemlist[@item{@(racket e-o) : usage restriction, either LR (only analyse
              this form) or RL (only generate this form).}
             @item{@(racket e-re) : regular expression}
             @item{@(racket e-lm) : lemma}
             @item{@(racket e-l) : left/lower/surface string}
             @item{@(racket e-r) : right/upper/lexical string}
             @item{@(racket e-par) : (inflection) paradigm}])))


;(require sxml)
;(ssax:xml->sxml (open-input-file
;                 "/home/selimcan/1Working/1Taruen/apertium-turkic/divan.dix")
;                  '())