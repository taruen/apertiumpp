#lang scribble/manual

@require[@for-label[racket/base
                    apertiumpp]
         scribble/example]

@title[#:version "0.0"]{Apertiumpp: various utilities for writing and
 manipulating @hyperlink["https://www.apertium.org"]{Apertium
  project's} rule-based machine translators}

@author{Ilnar Salimzianov}

@defmodule[apertiumpp/glossary]

@defproc[(explain [tag string?] [lang symbol?])
         string?]{

 Get a description, in language `lang', for what a given
 Apertium tag stands for.}