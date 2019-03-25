723
((3) 0 () 2 ((q lib "apertiumpp/apertium-pkg.rkt") (q 121 . 17)) () (h ! (equal) ((c def c (c (? . 0) q apkg-copying)) c (? . 1)) ((c def c (c (? . 0) q apkg-autogen.sh)) c (? . 1)) ((c def c (c (? . 0) q apkg-id)) c (? . 1)) ((c def c (c (? . 0) q apkg)) c (? . 1)) ((c def c (c (? . 0) q apkg-gitattributes)) c (? . 1)) ((c def c (c (? . 0) q APKG-COMMON)) q (538 . 2)) ((c def c (c (? . 0) q apkg-authors)) c (? . 1)) ((c def c (c (? . 0) q apkg-news)) c (? . 1)) ((q def ((lib "apertiumpp/glossary.rkt") explain)) q (0 . 4)) ((c def c (c (? . 0) q struct:apkg)) c (? . 1)) ((c def c (c (? . 0) q apkg?)) c (? . 1)) ((c def c (c (? . 0) q apkg-gitignore)) c (? . 1)) ((c def c (c (? . 0) q apkg-changelog)) c (? . 1))))
procedure
(explain tag lang) -> (or/c string? exn:unk-sym? exn:no-desc?)
  tag : string?
  lang : symbol?
struct
(struct apkg (id
              gitattributes
              gitignore
              authors
              copying
              changelog
              news
              autogen.sh))
  id : string?
  gitattributes : string?
  gitignore : string?
  authors : string?
  copying : string?
  changelog : string?
  news : string?
  autogen.sh : string?
value
APKG-COMMON : apkg?
