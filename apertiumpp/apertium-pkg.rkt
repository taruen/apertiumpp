#lang at-exp racket

(require scribble/srcdoc
         (for-doc racket/base scribble/manual))

(module+ test
  (require rackunit))

(provide
 (struct*-doc
  apkg ([id string?]
        [gitattributes string?]
        [gitignore string?]
        [authors string?]
        [copying string?]
        [changelog string?]
        [news string?]
        [autogen.sh string?])
  ("An abstract linguistic data package from the Apertium project."
   @itemlist[@item{@(racket id) : by convention ISO 639-3 code of the language,
              e.g. kaz, or a pair of ISO 639-3 codes, e.g. kaz-tat}])))

(struct apkg (id gitattributes gitignore authors copying changelog news
                 autogen.sh))

(provide
 (thing-doc
  APKG-COMMON apkg?
  ("Content common to all Apertium linguistic data packages. Corresponds to "
   @hyperlink["https://git.io/fjUJ2"]{any-module} " of apertium-init.")))

(define APKG-COMMON "")