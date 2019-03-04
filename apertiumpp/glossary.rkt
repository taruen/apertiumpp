#lang racket

;; A glossary of symbols used in the Apertium project.
;;
;; USAGE DOCUMENTATION: [https://taruen.github.io/apertiumpp/apertiumpp/]
;;                      [raco docs apertiumpp]
;;
;; NOTES FOR DEVELOPERS
;; ==================== 
;; A description of the symbols used in Apertium can be found at
;; [http://wiki.apertium.org/wiki/Symbols]. It might be incomplete though.
;;
;; TODO: To make sure that it is complete, walk over all of the
;; apertium-languages, apertium-incubator, apertium-nursery, apertium-staging
;; and apertium-trunk folders and populate the GLOSSARY below with symbols
;; found. The files that we are interested in are: *.lexc, *.dix, *.tNx.
;;
;; TODO: return sensible (read: in user's language) error messages when
;; exn:unk-sym and exn:no-desc occur (along with a suggestion to add missing
;; symbols to the wiki page above or here and to translate symbol descriptions
;; into their language. See UNKNOWN-SYMBOL-MSG and NO-DESCRIPTION-MSG.

(provide (contract-out
          [explain
           (string? symbol? . -> . (or/c string? exn:unk-sym? exn:no-desc?))]))

(module+ test
  (require rackunit))

(struct exn:unk-sym ())
(struct exn:no-desc ())

(define/contract (explain symbol lang)
  (string? symbol? . -> . (or/c string? exn:unk-sym? exn:no-desc?))
  (define s (hash-ref GLOSSARY symbol #f))
  (unless s (raise (exn:unk-sym)))
  (define desc (hash-ref s lang #f))
  (if desc
      desc
      (raise (exn:no-desc))))
  
(module+ test
  (check-equal? (explain "n" 'eng) "Common noun")
  (check-exn exn:unk-sym?
             (λ () (explain "a-made-up-non-existing-symbol" 'eng)))
  (check-exn exn:no-desc?
             (λ () (explain "n" 'a-made-up-non-existing-lang))))
  
(define GLOSSARY
  (hash "n" (hash 'eng "Common noun")
        "vblex" (hash 'eng "Standard (\"lexical\") verb")
        "v" (hash 'eng "Standard verb")
        "vbmod" (hash 'eng "Modal verb")
        "vbser" (hash 'eng "Verb \"to be\"")
        "vbhaver" (hash 'eng "Verb \"to have\"")
        "vaux" (hash 'eng "Auxiliary verb")
        "cop" (hash 'eng "Copula")
        "adj" (hash 'eng "Adjective")
        "post" (hash 'eng "Postposition")
        "adv" (hash 'eng "Adverb")
        "preadv" (hash 'eng "Pre-adverb")
        "postadv" (hash 'eng "Postadverb")
        "mod" (hash 'eng "Modal word")
        "det" (hash 'eng "Determiner")
        "prn" (hash 'eng "Pronoun")
        "pr" (hash 'eng "Preposition")
        "num" (hash 'eng "Numeral")
        "np" (hash 'eng "Proper noun")
        "ij" (hash 'eng "Interjection")
        "cnjcoo" (hash 'eng "Co-ordinating conjunction")
        "cnjsub" (hash 'eng "Sub-ordinating conjunction")
        "cnjadv" (hash 'eng "Conjunctive adverb")
        "sent" (hash 'eng "Sentence-ending punctuation")
        "lquot" (hash 'eng "Left quote")
        "rquot" (hash 'eng "Right quote")
        "lpar" (hash 'eng "Left parenthesis")
        "rpar" (hash 'eng "Right parenthesis")))

(define NO-SYMBOL-MSG
  (hash 'eng "There isn't a description for ~v. It is probably not used in Apertium"))

(define NO-DESCRIPTION-MESSAGE
  (hash 'eng "~v is used in Apertium, but there isn't any description for it in language ~v. Try one of the following: ~v"))