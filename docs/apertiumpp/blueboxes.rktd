2374
((3) 0 () 8 ((q lib "apertiumpp/apertium-pkg.rkt") (q 809 . 17) (q lib "apertiumpp/bidix.rkt") (q 508 . 5) (q 654 . 8) (q 121 . 7) (q 391 . 3) (q 434 . 4)) () (h ! (equal) ((c def c (c (? . 0) q apkg-copying)) c (? . 1)) ((c def c (c (? . 0) q apkg-autogen.sh)) c (? . 1)) ((c def c (c (? . 2) q struct:sdef)) c (? . 6)) ((c def c (c (? . 2) q section-n)) c (? . 3)) ((c def c (c (? . 2) q pardef?)) c (? . 7)) ((c def c (c (? . 2) q section)) c (? . 3)) ((c def c (c (? . 2) q dictionary-pardefs)) c (? . 5)) ((c def c (c (? . 2) q struct:e)) c (? . 4)) ((c def c (c (? . 2) q e-o)) c (? . 4)) ((q def ((lib "apertiumpp/glossary.rkt") explain)) q (0 . 4)) ((c def c (c (? . 2) q section?)) c (? . 3)) ((c def c (c (? . 0) q struct:apkg)) c (? . 1)) ((c def c (c (? . 0) q apkg-gitignore)) c (? . 1)) ((c def c (c (? . 2) q e-re)) c (? . 4)) ((c def c (c (? . 2) q e-par)) c (? . 4)) ((c def c (c (? . 2) q section-type)) c (? . 3)) ((c def c (c (? . 2) q struct:dictionary)) c (? . 5)) ((c def c (c (? . 2) q e-r)) c (? . 4)) ((c def c (c (? . 2) q D-0)) q (333 . 2)) ((c def c (c (? . 0) q apkg-authors)) c (? . 1)) ((c def c (c (? . 0) q APKG-COMMON)) q (1226 . 2)) ((c def c (c (? . 2) q e-lm)) c (? . 4)) ((c def c (c (? . 2) q sdef)) c (? . 6)) ((c def c (c (? . 2) q dictionary-lang)) c (? . 5)) ((c def c (c (? . 2) q e)) c (? . 4)) ((c def c (c (? . 2) q sdef-n)) c (? . 6)) ((c def c (c (? . 0) q apkg-id)) c (? . 1)) ((c def c (c (? . 2) q dictionary-alphabet)) c (? . 5)) ((c def c (c (? . 0) q apkg)) c (? . 1)) ((c def c (c (? . 0) q apkg-gitattributes)) c (? . 1)) ((c def c (c (? . 2) q dictionary?)) c (? . 5)) ((c def c (c (? . 2) q sdef?)) c (? . 6)) ((c def c (c (? . 2) q section-con)) c (? . 3)) ((c def c (c (? . 2) q struct:section)) c (? . 3)) ((c def c (c (? . 2) q e?)) c (? . 4)) ((c def c (c (? . 0) q apkg-news)) c (? . 1)) ((c def c (c (? . 2) q D-B-0)) q (361 . 2)) ((c def c (c (? . 2) q struct:pardef)) c (? . 7)) ((c def c (c (? . 2) q dictionary-sections)) c (? . 5)) ((c def c (c (? . 2) q dictionary)) c (? . 5)) ((c def c (c (? . 2) q pardef)) c (? . 7)) ((c def c (c (? . 2) q pardef-con)) c (? . 7)) ((c def c (c (? . 2) q pardef-n)) c (? . 7)) ((c def c (c (? . 2) q dictionary-sdefs)) c (? . 5)) ((c def c (c (? . 2) q e-l)) c (? . 4)) ((c def c (c (? . 0) q apkg?)) c (? . 1)) ((c def c (c (? . 0) q apkg-changelog)) c (? . 1))))
procedure
(explain tag lang) -> (or/c string? exn:unk-sym? exn:no-desc?)
  tag : string?
  lang : symbol?
struct
(struct dictionary (lang alphabet sdefs pardefs sections))
  lang : symbol?
  alphabet : string?
  sdefs : (listof sdef?)
  pardefs : (listof pardef?)
  sections : (listof section?)
value
D-0 : (dictionary?)
value
D-B-0 : (dictionary?)
struct
(struct sdef (n))
  n : string?
struct
(struct pardef (n con))
  n : string?
  con : (listof e?)
struct
(struct section (n type con))
  n : string?
  type : (or/c STANDARD PREBLANK POSTBLANK INCONDITIONAL)
  con : (listof e?)
struct
(struct e (o re lm l r par))
  o : (or/c LR RL)
  re : string?
  lm : string?
  l : string?
  r : string?
  par : string?
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
