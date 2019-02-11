#lang scribble/manual

@title[#:version ""]{Apertium++, or Apertium 4.0}

This is a project with an aim of:

@itemlist[

 @item{making a free/libre morphological transducer for
  every language of the world;}

 @item{making free/libre machine translators for translating
  between @bold{all} related languages of a particular
  language group (e.g. Turkic, Slavic, Indo-Iranian etc);}

 @item{updating Apertium 3.0's official documentation so
  that it reflects changes which have happened since the time
  of its publication;}

 @item{reducing the code which developers have to maintain
  in a typical Apertium 3.0 monolingual or bilingual package.}]

@margin-note{Hence two pluses in the name of the project...}

The project, in its spirit and aims is similar to the
@hyperlink["https://www.apertium.org"]{Apertium project}, and can be
considered its spinoff, with two additions:

@itemlist[

 @item{for each language, we will maintain a corpus of
  sharable texts, parallel or comparable to corpora for other
  languges}

 @item{machine translators will have a speech recognition &
  speech synthesis front-ends (based on Mozilla's Common Voice
  and DeepSpeech projects).}]

@margin-note{To be frank, if we come up with some useful
 technology, we hope to see it merged to the Apertium's code
 base.}

Since one of the declared goals of ours is being able to
handle speech (not just the written word), and, since we
want to re-evaluate some of the design decisions made in the
Apertium project (and yet are not sure at all whether such
re-evaluations will turn out to be wise desicions), we
decided to make a separate project out of this effort.

@section{What could be improved in Apertium 3.0?}

Apertium is a great project, with quite a few contributors,
but here is a list of things deemed as less-than-optimal in
the current Apertium setup:

@itemlist[

 @item{transfer rules are unidirectional, and currently
  there is no way of marking a transfer rule as
  `bidirectional' and get a transfer rule for the opposite
  direction for free,}

 @item{there can be and usually there are several entries in
  a .lexc or .dix files with the same left-hand side (and
  there is no compile-time checks against mistakenly adding
  the same word twice, possibly with a right and a wrong
  category}

 @item{rather long compile cycles, translators are not
  extensible programmatically}

 @item{?}]

@section{Dependencies}

The @filepath{apertium-tests} contains tests for Apertium's
monolingual and bilingual packages. These tests are written
in the @(link "https://racket-lang.org" "Racket")
programming language using its dialect called
@(link "https://docs.racket-lang.org/rash/index.html" "Rash").

To run the tests, first you'll need to install the Racket
language following the instructions at
@(url "https://racket-lang.org"). Then, install the Rash
package with the @(code "raco pkg install rash") command.