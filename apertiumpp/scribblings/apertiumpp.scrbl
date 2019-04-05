#lang scribble/manual

@(require (for-label racket/base
                     apertiumpp)
          scribble/eval
          scribble/extract)

@title[#:version "0.0"]{Apertiumpp: various utilities for writing
  @hyperlink["https://www.apertium.org"]{Apertium project's} rule-based machine
  translators}

@author{Ilnar Salimzianov}

@section{Installation}

Here are insttructions on how to install apertiumpp:

@itemlist[
 @item{Install the @hyperlink["https://racket-lang.org"]{Racket
   language}.}
  
 @item{Clone this repository: @code{git clone
   https://github.com/taruen/apertiumpp}.}

 @item{@tt{cd} to the @code{apertiumpp/apertiumpp} directory.}

 @item{Install the apertiumpp package: @code{raco pkg install}.}]

Later on, when there are any updates in the Git repository,
you should run @code{raco setup -p apertiumpp} to compile the
latest version.

@section{Reference}

@subsection{Glossary}

@defmodule[apertiumpp/glossary]

@defproc[(explain [tag string?] [lang symbol?])
         (or/c string? exn:unk-sym? exn:no-desc?)]{

 Return a description, in language @racket{lang} (ISO 639-3
 code), for what a given Apertium symbol stans for.

 Raise @racket{exn:unk-sym} if the symbol is not in the
 glossary, and @racket{exn:no-desc} if it is in the glossary,
 but there isn't a description for it in language @racket{
  lang}.}

@(define glossary-eval (make-base-eval))
@interaction-eval[#:eval glossary-eval
                  (require apertiumpp/glossary)]

@examples[
 #:eval glossary-eval
 (explain "n" 'eng)
 (explain "a-made-up-non-existing-tag" 'eng)
 (explain "n" 'a-made-up-non-existing-lang)]

@subsection{Dictionary}

@defmodule[apertiumpp/dictionary]
@include-extracted[apertiumpp/dictionary]

@subsection{Apertium-pkg}

@defmodule[apertiumpp/apertium-pkg]
@include-extracted[apertiumpp/apertium-pkg]

