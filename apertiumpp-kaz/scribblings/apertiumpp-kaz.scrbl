#lang scribble/manual

@title[#:version ""]{Apertiumpp-kaz}

@author["Ilnar Salimzianov"]

@hyperlink["https://github.com/taruen/apertiumpp/tree/master/apertiumpp-kaz"]{This
directory} contains our effort to proofread
@hyperlink["https://github.com/apertium/apertium-kaz/"]{apertium-kaz's} lexicon
against the 15-volume [Explanatory] Dictionary of Literary Kazakh (``Қазақ
Әдеби Тілінің Сөздігі''), published by Kazakh
@hyperlink["https://tbi.kz"]{Linguistics Institute} in 2011, and the
single-volume [Explanatory] Dictionary of Kazakh (``Қазақ Сөздігі''), published
by the @hyperlink["https://tbi.kz"]{Linguistics Institute} and Kazakh
@hyperlink["https://mks.gov.kz/kaz/informery/komitety/k_ya_opr/"]{Language
Commitee} in 2013. The goal is to solve issue
@hyperlink["https://github.com/apertium/apertium-kaz/issues/11"]{#11} of
@italic{apertium-kaz}, as well as to extend it with more stems, especially with
common words (as opposed to proper nouns).

In particular, the file
@hyperlink["https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/lexicon.rkt"]{lexicon.rkt}
in this directory contains common words from
@hyperlink["https://github.com/apertium/apertium-kaz/blob/master/apertium-kaz.kaz.lexc"]{apertium-kaz/apertium-kaz.kaz.lexc},
revised and extended with more stems from the aforementioned print
dictionaries.

We plan to merge the results back to @italic{apertium-kaz} once we're done
proof-reading it.

In the meantime, we provide
 @hyperlink["https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/apertium-kaz.kaz.lexc"]{our
 own version} of @italic{apertium-kaz.kaz.lexc} as a drop-in replacement for
 @italic{apertium-kaz}'s file of the same name, as well a slightly modified
 @italic{Makefile.am} for it. To use @italic{apertium-kaz} with our
 modifications to it, you should:

@itemlist[#:style 'ordered

@item{Install
@hyperlink["http://wiki.apertium.org/wiki/Install_Apertium_core_using_packaging"]{Apertium
Core}: @verbatim|{wget https://apertium.projectjj.com/apt/install-nightly.sh -O
- | sudo bash}| @verbatim|{sudo apt-get -f install apertium-all-dev }| OR
@verbatim|{wget https://apertium.projectjj.com/rpm/install-nightly.sh -O - |
sudo bash}| @verbatim|{sudo yum install apertium-all-devel}| or similar,
depending on what kind of GNU/Linux distibution you are using.}

@item{Dowload apetium-kaz: @verbatim|{git clone https://github.com/apertium/apertium-kaz.git}|}

@item{Replace @italic{apertium-kaz/apertium-kaz.kaz.lexc} file with the file
@hyperlink["https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/apertium-kaz.kaz.lexc"]{that
we provide}.}

@item{Replace @italic{apertium-kaz/Makefiel.am} file with the file
@hyperlink["https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/Makefile.am"]{that
we provide}.}

@item{Compile apertium-kaz: @verbatim|{cd apertium-kaz; ./autogen.sh; make}|}

]

Here's a brief comparison of the two main characteristics of
@italic{apertium-kaz}'s state before and after our modifications to it:

@tabular[#:sep @hspace[1] #:style 'boxed
  (list
    (list @bold{Number of stems in .lexc before} @bold{Number of stems in .lexc after})
    (list "~38k" "~98k"))]

@tabular[#:sep @hspace[1] #:style 'boxed
  (list
    (list "" @bold{Naive coverage before} @bold{Naive coverage after})
    (list @bold{Bibel} "x" "y")
    (list @bold{Wikipedia} "1 2 3 => avrg. " "94.79 94.57 91.83 => avrg. "))]

Here's how @italic{apertiumpp-kaz} is different from @italic{apertium-kaz}.

@itemlist[#:style 'ordered

  @item{The main list of stems, @italic{lexicon.rkt}, is implemented in a
  full-fledged programming language (Racket), and not in the Xerox/Helsinki
  Finite State Toolkit's @italic{lexc} formalism.}

  @item{This list of stems is consumed by a
  @hyperlink["https://docs.racket-lang.org/scribble-pp/text.html"]{scribble/text}-based
  template, @italic{apertium-kaz.kaz.lexc.scrbl}. A normal
  @italic{apertium-kaz.kaz.lexc} file is generated when @verbatim{racket apertium-kaz.kaz.lexc.scrbl}
  command is run.}

  @item{As opposed to the 3-element data structure of a @italic{lexc}
  (upper-side string, lower-side string, continuation lexicon), with other
  marks being comments formatted in a particular way by convention, the main
  datatype of @italic{lexicon.rkt} is an @italic{Entry} with 7 fields,
  representing the following information:

  @itemlist[

    @item{upper-side string}
    
    @item{lower-side string}

    @item{continuation lexicon(s)}

    @item{a gloss and various (restictional) marks such as USE/MT, DIR/LR, DIR/RL etc}

    @item{inflected forms of this word, which were unnecessarily lexicalised in
    the @italic{.lexc} file we proofread (or were lexicalised by the authors of
    the print dictionaries, but we thought that it's not necessary to
    lexicalise them in the transducer)}

    @item{stem from which this stem has been derived from in a semi-productive
    way, or a chain of such stems}

    @item{normative spelling(s) of this word}

  ]}

  @item{In the source code, Entries can be wrapped up with function calls,
  which modify entries in various ways (or not), depending on how the functions
  in question are defined, and, ultimately, what defaults a particular
  application of @italic{apertium-kaz} calls for.}

]

Below are four examples of entries, named as E-1, E-2, E-3 and E-4 for easy
re-use in tests.

@(racketblock

(define E-1 (e "абдаста" "абдаста" 'N1 '() '() '() '("әбдесте")))
(define E-2 (e "абдикация" "абдикация" 'N1 '() '() '() '()))
(define E-3 (e "абдырат" "абдырат" 'V-IV `("bother" ,USE/MT) '("абдырату") '("абдыра") '()))
(define E-4 (e "абыр%-дабыр" "абыр%-дабыр" '(IDEO N1) '() '() '() '()))

)

Commentaries on @bold{why} and @bold{how} these modifications were
made follow.

@section{How stems were added to lexicon.rkt?}

@hyperlink["https://github.com/IlnarSelimcan/dot/blob/master/lexikograf.py"]{lexikograf.py}

@section{Acknowledgements}

This work is being funded by the Committee of Science of the Ministry of
Education and Science of the Republic of Kazakhstan, contract#
346/018-2018/33-28, IRN AP05133700, principal investigator Zhenisbek
Assylbekov.

@section{License}

Just like @italic{apertium-kaz}, the contents of this repo are published under
GPL version 3.
