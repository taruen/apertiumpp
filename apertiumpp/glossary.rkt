#lang racket

(provide explain)

(define/contract (explain tag lang)
  (string? symbol? . -> . string?)
  (hash-ref (hash-ref GLOSSARY tag) lang))

(module+ test
  (require rackunit)
  (check-equal? (explain "n" 'eng) "Common noun"))

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