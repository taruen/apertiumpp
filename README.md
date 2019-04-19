# Apertium++!, or Apertium 4.0, or making Apertium DSLs internal/embedded

This is a project with an aim of:

* making a free/libre morphological transducer for every language of the
  world;

* making free/libre machine translators for translating between **all**
  related languages of a particular language group \(e.g. Turkic,
  Slavic, Indo-Iranian etc\);

* updating Apertium 3.0’s official documentation so that it reflects
  changes which have happened since the time of its publication,
  especially as the result of Google Summer of Code projects;

* reducing the code which developers have to maintain in a typical
  Apertium 3.0 monolingual or bilingual package.

> Hence two pluses in the name of the project...

The project, in its spirit and aims is similar to the [Apertium
project](https://www.apertium.org), and can be considered its spinoff,
with two additions:

* for each language, we will maintain a corpus of sharable texts,
  parallel or comparable to corpora for other languges

* machine translators will have a speech recognition & speech synthesis
  front-ends \(based on Mozilla’s [Common
  Voice](https://voice.mozilla.org) and
  [DeepSpeech](https://github.com/mozilla/DeepSpeech) projects\).

> To be frank, if we come up with some useful technology, we hope to see
> it merged to the Apertium’s code base \(maybe in the form of different
> campaigns, such as "Help us to create a mophological transducer for
> every human language!"\).

Since one of the declared goals of ours is being able to handle speech
\(not just the written word\), and, since we want to re-evaluate some of
the design decisions made in the Apertium project \(and yet are not sure
at all whether such re-evaluations will turn out to be wise decisions\),
we decided to make a separate project out of this effort.

## 1. What could be improved in Apertium 3.0?

Apertium is a great project, with quite a few contributors, but here is
a list of things deemed as less-than-optimal in the current Apertium
setup:

* transfer rules are unidirectional, and currently there is no way of
  marking a transfer rule as ‘bidirectional’ and get a transfer rule for
  the opposite direction for free,

* there can be and usually there are several entries in a .lexc or .dix
  files with the same left-hand side \(and there is no compile-time
  checks against mistakenly adding the same word twice, possibly with a
  right and a wrong category

* rather long compile cycles, translators are not extensible
  programmatically

* not possible to re-use components in a programmatic way \(i.e. without
  having to copy-paste manually, which sooner or later will lead to
  out-of-date chunks\). Want we want instead is having a way to say,
  e.g. something like this:
  `from` `apertium-tat-rus` `import` `transfer-rule-x`,
  `from` `apertium-symbols` `import` `n,` `v,` `adj,` `np`,
  `parameterize(transfer-rule-x,` `lemma="foo")` etc.

* ?

We suppose that most of the problems listed will be due the fact that
the domain-specific languages used in Apertium \(and there a handful of
them – one seen in monolingual and bilingual `.dix` files, one seen in
`chunking`,  `interchunk` and `postchunk` transfer rules,  `lexc`,
`twol` and others are so-called external DSLs. They are parsed and
compiled. When writing in these DSLs, you don’t have access to a
full-fledged programming language which would allow you to extend the
DSL in question easily and simplify&automate things. You also don’t have
access to useful features of a full-fledged programming language like a
proper module system, which would allow you to re-use code across
monolingual and bilingual packages reliably.

## 2. Desiderata

* Every Apertium 3.0. program is a valid Apertium 4.0 program.

## 3. Apertiumpp library

At the core of this project is a Racket library called _apertiumpp_. The
documentation of the library can be found at
[https://taruen.github.io/apertiumpp/apertiumpp/](./apertiumpp/).

## 4. Tests/data for Apertium 3.0. packages

The `"data4apertium"` directory contains tests for Apertium’s
monolingual and bilingual packages. These tests are written in the
[Racket](https://racket-lang.org) programming language using its dialect
called [Rash](https://docs.racket-lang.org/rash/index.html).

You can run `".rkt"` tests in `"data4apertium"` with the
`racket` `foo.rkt` command.

Before running tests, you should `cd` to the Apertium linguistic data
package you want to test, and run the `raco` `pkg` `install` command
while in that directory.

This will tell Racket where the Apertium linguictic package is located
on your computer.

For that to work, Apertium package should have an `"info.rkt"` file and
a `"main.rkt"` file with the functionality you want to export and test.
See [apertium-kaz](https://github.com/apertium/apertium-kaz) and
[apertium-kaz-tat](https://github.com/apertium/apertium-kaz-tat) for an
example.

## 5. Background reading \(for potentional contributors, not users\)

[http://www.greghendershott.com/fear-of-macros/](http://www.greghendershott.com/fear-of-macros/)
