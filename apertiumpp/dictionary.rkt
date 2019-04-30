#lang at-exp racket

;; A representation of & functions for Apertium's dictionaries

(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

(module+ test
  (require rackunit))


;;;;;;;;;;;;
;; Constants


(define NA "LR")
(define NG "RL")
(define STANDARD "standard")
(define PREBLANK "preblank")
(define POSTBLANK "postblank")
(define INCONDITIONAL "inconditional")


;;;;;;;;;;;;;;;;;;;
;; Data definitions


(struct dictionary (lang alphabet sdefs pardefs sections attrs))

(provide
 (struct*-doc
  dictionary ([lang (listof symbol?)]
              [alphabet string?]
              [sdefs (listof sdef?)]
              [pardefs (listof pardef?)]
              [sections (listof section?)]
              [attrs (hash/c symbol? string?)])
  ("interpretation: an Apertium dictionary (read: contents of a .dix or .lexc file)."
   @itemlist[@item{@(racket dictionary-lang) : ISO 639-3 code(s) of the
              language(s)}
             @item{@(racket dictionary-alphabet) : relevant for a monolingual
              dictionary only}
             @item{@(racket dictionary-sdefs) : grammatical symbols used in
              dictionary entries}
             @item{@(racket dictionary-pardefs) : paradigm definitions}
             @item{@(racket dictionary-sections) : lists of dictionary entries}
             @item{@(racket dictionary-attrs) : user-defined attributes
              with name and value}])))

(define D-0
  (dictionary
   '(eng)
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
   '() '() '() (hash)))

(provide
 (thing-doc
  D-0 dictionary?
  ("An example of an empty English dictionary:"
   @(racketblock
     (define D-0
       (dictionary
        '(eng)
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        '() '() '() (hash)))))))

(define D-B-0
  (dictionary
   '(eng tat)
   ""
   '() '() '() (hash)))

(provide
 (thing-doc
  D-B-0 dictionary?
  ("An empty English-Tatar bilingual dictionary:"
   @(racketblock
     (define D-B-0
       (dictionary
        '(eng tat)
        "" '() '() '() (hash)))))))


(struct sdef (n attrs))

(provide
 (struct*-doc
  sdef ([n string?] [attrs (hash/c symbol? string?)])
  ("interpretation: a symbol definition."
   @itemlist[@item{@(racket sdef-n) : grammatical symbol used in dictionary
              pardefs or entries.}
             @item{@(racket sdef-attrs) : user-defined attributes with name
              and value}])))

(define SDEF-0 (sdef "n" (hash 'c "Noun")))

(provide
 (thing-doc
  SDEF-0 sdef?
  ("A noun symbol."
   @(racketblock
     (define SDEF-0 (sdef "n" (hash 'c "Noun")))))))


(struct pardef (n con attrs))

(provide
 (struct*-doc
  pardef ([n string?] [con (listof e?)] [attrs (hash/c symbol? string?)])
  ("interpretation: a paradigm definition."
   @itemlist[@item{@(racket pardef-n) : the name of the paradigm}
             @item{@(racket pardef-con) : its content}
             @item{@(racket pardef-attrs) : any additional attributes}])))


(struct section (n type con attrs))

(provide
 (struct*-doc
  section ([n string?]
           [type (or/c STANDARD PREBLANK POSTBLANK INCONDITIONAL)]
           [con (listof e?)]
           [attrs (hash/c symbol? string?)])
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
             @item{@(racket section-con) : content (entries)}
             @item{@(racket section-attrs) : any additional attributes}])))


(struct e (o re lm l r par attrs))

(provide
 (struct*-doc
  e ([o (or/c #f NA NG)]
     [re (or/c #f string?)]
     [lm (or/c #f string?)]
     [l string?]
     [r string?]
     [par (or/c #f string?)]
     [attrs (hash/c symbol? string?)])
  ("interpretation: an entry in a dictionary paradigm or section."
   @itemlist[@item{@(racket e-o) : usage restriction, either NA (`no analysis',
              only generate this form) or NG (`no generation', only analyse
              this form) or #f (not set)}
             @item{@(racket e-re) : regular expression or #f (not set)}
             @item{@(racket e-lm) : lemma or #f (not set)}
             @item{@(racket e-l) : left/lower/surface string}
             @item{@(racket e-r) : right/upper/lexical string}
             @item{@(racket e-par) : name of the (inflection) paradigm or #f
              (not set)}
             @item{@(racket e-attrs) : any additional attributes}])))


;;;;;;;;;;;;
;; Functions


(require xml xml/path)
(permissive-xexprs #t)



;; Wishlist


(define (parse-dix dix)
  (define parsed
    (xml->xexpr (document-element (read-xml (open-input-file dix)))))
  (dictionary
   '(kaz-tat)
   (se-path* '(alphabet) parsed)
   (se-path*/list '(sdef) parsed)
   (se-path*/list '(pardef) parsed)
   (se-path*/list '(section) parsed)
   (hash)))

(provide
 (proc-doc/names
  parse-dix (string? . -> . dictionary?) (bidix)
  ("WISHLIST ITEM. Parse .dix file and return a " (racket dictionary) ".")))

;(se-path*/list '(section) (xml->xexpr (document-element (read-xml (open-input-file "/tmp/apertium-kaz-tat.kaz-tat.dix")))))


(define (parse-lexc lexc)
  D-0)

(provide
 (proc-doc/names
  parse-lexc (string? . -> . dictionary?) (lexc)
  ("WISHLIST ITEM. Parse .lexc file and return a " (racket dictionary) ".")))


(define (dictionary2dix d)
  "")

(provide
 (proc-doc/names
  dictionary2dix (dictionary? . -> . string?) (d)
  ("WISHLIST ITEM. Convert a " (racket dictionary) " into a .dix file.")))


(define (dictionary2lexc d)
  "")

(provide
 (proc-doc/names
  dictionary2lexc (dictionary? . -> . string?) (d)
  ("WISHLIST ITEM. Convert a " (racket dictionary) " into a .lexc file.")))