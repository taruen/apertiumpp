#lang scribble/manual
@require[@for-label[apertiumpp
                    racket/base]]

@title{apertiumpp}
@author{selimcan}

@defmodule[apertiumpp]

@defproc[(explain [tag string?] [lang symbol?])
         string?]{
 Get a description, in language `lang', for what a given Apertium tag stands for.}
