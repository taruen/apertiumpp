#lang scribble/manual

@(require scribble/eval)

@title[#:version ""]{Apertium++!, or Apertium 4.0, or making Apertium DSLs internal/embedded}

This is a project with an aim of:

@itemlist[

 @item{making a free/libre morphological transducer for
  every language of the world;}

 @item{making free/libre machine translators for translating
  between @bold{all} related languages of a particular
  language group (e.g. Turkic, Slavic, Indo-Iranian etc);}

 @item{updating Apertium 3.0's official documentation so
  that it reflects changes which have happened since the time
  of its publication, especially as the result of Google
  Summer of Code projects;}

 @item{reducing the code which developers have to maintain
  in a typical Apertium 3.0 monolingual or bilingual package.}]

The project, in its spirit and aims is similar to the
@hyperlink["https://www.apertium.org"]{Apertium project}, and can be considered
its spinoff, with two additions (hence two pluses in the name of the
project...):

@itemlist[

 @item{for each language, we will maintain a corpus of
  sharable texts, parallel or comparable to corpora for other
  languages}

 @item{machine translators will have a speech recognition &
  speech synthesis front-ends (based on Mozilla's
  @hyperlink["https://voice.mozilla.org"]{Common Voice} and
  @hyperlink["https://github.com/mozilla/DeepSpeech"]{
   DeepSpeech} projects).}]

@margin-note{To be frank, if we come up with some useful
 technology, we hope to see it merged to the Apertium's code
 base (maybe in the form of different campaigns, such as
 "Help us to create a mophological transducer for every human
 language!").}

Since one of the declared goals of ours is being able to
handle speech (not just the written word), and, since we
want to re-evaluate some of the design decisions made in the
Apertium project (and yet are not sure at all whether such
re-evaluations will turn out to be wise decisions), we
decided to make a separate project out of this effort.

@section{What could be improved in Apertium 3.0?}

Apertium is a great project, with quite a few contributors,
but here is a list of things deemed as less-than-optimal in
the current Apertium setup:

@itemlist[

 @item{bilingual dictinoaries support only one-to-one mappings, which leads to
 a situation where monolingual dictionaries as a workaround unnecessarily
 include multiwords}
 
 @item{transfer rules are unidirectional, and currently
  there is no way of marking a transfer rule as
  `bidirectional' and get a transfer rule for the opposite
  direction for free}

 @item{there can be and usually there are several entries in
  a .lexc or .dix files with the same left-hand side (and
  there is no compile-time checks against mistakenly adding
  the same word twice, possibly with a right and a wrong
  category)}

 @item{rather long compile cycles, translators are not
  extensible programmatically}

 @item{not possible to re-use components in a programmatic
  way (i.e. without having to copy-paste manually, which
  sooner or later will lead to out-of-date chunks). What we
  want instead is having a way to say, e.g. something like
  this: @code{from apertium-tat-rus import transfer-rule-x},
  @code{from apertium-symbols import n, v, adj, np}, @code{
   parameterize(transfer-rule-x, lemma="foo")} etc.}

 @item{?}]

We suppose that most of the problems listed will be due the
fact that the domain-specific languages used in Apertium (and
there are a handful of them -- one seen in monolingual and
bilingual @code{.dix} files, one seen in @code{chunking},
@code{ interchunk} and @code{postchunk} transfer rules,
@code{ lexc}, @code{twol} and others) are so-called external
DSLs. They are parsed and compiled. When writing in these
DSLs, you don't have access to a full-fledged programming
language which would allow you to extend the DSL in question
easily and simplify&automate things. You also don't have
access to useful features of a full-fledged programming
language like a proper module system, which would allow you
to re-use code across monolingual and bilingual packages
reliably.

To be fair, Apertium was designed for translating between
closely-related languages, and for that it works sufficiently
well.

@section{Desiderata}

@itemlist[

 @item{Every Apertium program is a valid
  Apertium++! program.}]

@section{Apertiumpp library}

At the core of this project is a @hyperlink["https://racket-lang.org/"]{Racket}
library called @italic{apertiumpp}. The documentation of the library can be
found at @hyperlink["./apertiumpp/"]{
https://taruen.com/apertiumpp/apertiumpp/}.

@section{Tests/data for Apertium 3.0. packages}

@hyperlink["./apertiumpp/"]{apertiumpp} library has an interface to various
textual data which can be used to test apertium packages. Once @tt{apertiumpp}
is installed, you should be able to do the following.

@(define corpus-eval (make-base-eval))
@interaction-eval[#:eval corpus-eval
                  (require rash)]

@verbatim{raco apertiumpp corpus -l <ISO-630-3 code>}

run in the terminal will list the corpora we have for the language:

@examples[
 #:eval corpus-eval
  (rash "raco apertiumpp corpus -l tat")]

@examples[
 #:eval corpus-eval
  (rash "raco apertiumpp corpus -l tat bible.com")]

If full ``path'' is given to a corpus, it will be output to stdout:

@examples[
 #:eval corpus-eval
  (display
    (rash "raco apertiumpp corpus -l tat bible.com 1502-ttrbbl-izge-yazma.csv | head"))]

@section{Turning an Apertium 3.0 package into a Racket package}

For that to work, Apertium package should have an @filepath{info.rkt} file and
a @filepath{main.rkt} file with the functionality you want to export and
test. See @hyperlink["https://github.com/apertium/apertium-kaz"]{apertium-kaz}
and
@hyperlink["https://github.com/apertium/apertium-kaz-tat"]{apertium-kaz-tat}
for an example.

To include non-racket files into the resulting racket package, you'll need to
use the @tt{define-runtime-path} macro, as explained e.g. in
@hyperlink["https://defn.io/2020/06/28/racket-deployment/"]{this blog post}.

@section{Background reading (for potentional contributors, not users)}

@url{http://www.greghendershott.com/fear-of-macros/}
