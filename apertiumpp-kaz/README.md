<a href="https://taruen.com"><img src="https://taruen.com/assets/img/logo.png" width="125" height="125" align="left" /></a>

# Apertiumpp-kaz

[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](https://taruen.com/apertiumpp/apertiumpp-kaz/)
[![License](https://img.shields.io/badge/license-GPLv3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

[This
directory](https://github.com/taruen/apertiumpp/tree/master/apertiumpp-kaz)
contains our effort to proofread
[apertium-kaz’s](https://github.com/apertium/apertium-kaz/) lexicon
against the 15-volume \[Explanatory\] Dictionary of Literary Kazakh
\(“Қазақ Әдеби Тілінің Сөздігі”\), published by Kazakh [Linguistics
Institute](https://tbi.kz) in 2011 \(EDOK2011\), and the single-volume
\[Explanatory\] Dictionary of Kazakh \(“Қазақ Сөздігі”\), published by
the [Linguistics Institute](https://tbi.kz) and Kazakh [Language
Commitee](https://mks.gov.kz/kaz/informery/komitety/k_ya_opr/) in 2013
\(EDOK2013\). The goal is to solve issue
[\#11](https://github.com/apertium/apertium-kaz/issues/11) of
_apertium-kaz_, as well as to extend it with more stems, especially with
common words \(as opposed to proper nouns\).

In particular, the file
[lexicon.rkt](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/lexicon.rkt)
in this directory contains common words from
[apertium-kaz/apertium-kaz.kaz.lexc](https://github.com/apertium/apertium-kaz/blob/master/apertium-kaz.kaz.lexc),
revised and extended with more stems from the aforementioned print
dictionaries.

We plan to merge the results back to _apertium-kaz_ once we’re done
proof-reading it.

In the meantime, we provide  [our own
version](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/apertium-kaz.kaz.lexc)
of _apertium-kaz.kaz.lexc_ as a drop-in  replacement for
_apertium-kaz_’s file of the same name, as  well a slightly modified
[Makefile.am](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/apertium-kaz.kaz.lexc)
for it. To use _apertium-kaz_ with our modifications to it,  you should:

* Install [Apertium
  Core](http://wiki.apertium.org/wiki/Install_Apertium_core_using_packaging):

  `wget https://apertium.projectjj.com/apt/install-nightly.sh -O`
  `- | sudo bash`                                                

  `sudo apt-get -f install apertium-all-dev `

  OR

  `wget https://apertium.projectjj.com/rpm/install-nightly.sh -O - |`
  `sudo bash`                                                        

  `sudo yum install apertium-all-devel`

  or similar, depending on what kind of GNU/Linux distibution you are
  using. See [this
  article](http://wiki.apertium.org/wiki/Install_Apertium_core_using_packaging)
  for more information. For Windows users, Apertium project provides [a
  Virtualbox image](http://wiki.apertium.org/wiki/Apertium_VirtualBox)
  with all necessary tools installed on it. If you’re using the
  Virtualbox image, you should simply continue with the next step.

* Dowload apetium-kaz:

  `git clone https://github.com/apertium/apertium-kaz.git`

* Replace _apertium-kaz/apertium-kaz.kaz.lexc_ file with the file [that
  we
  provide](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/apertium-kaz.kaz.lexc).

* Replace _apertium-kaz/Makefiel.am_ file with the file [that we
  provide](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/Makefile.am).

* Compile apertium-kaz:

  `cd apertium-kaz; ./autogen.sh; make`

Here’s a brief comparison of the two main characteristics of
_apertium-kaz_’s state before and after our modifications to it:

```racket
**Number of stems in .lexc before** **Number of stems in .lexc after**
~38k                                ~98k                              
```

```racket
              **Naive coverage before**   **Naive coverage after**  
**Bibel**     x                           y                         
**Wikipedia** 92.38 91.73 89.86  => avrg. 94.79 94.57 91.83 => avrg.
```

Here’s how _apertiumpp-kaz_ is different from _apertium-kaz_.

* The main list of stems, _lexicon.rkt_, is implemented in a
  full-fledged programming language
  \([Racket](https://racket-lang.org/)\), and not in the Xerox/Helsinki
  Finite State Toolkit’s _lexc_ formalism.

* This list of stems is consumed by a
  [scribble/text](https://docs.racket-lang.org/scribble-pp/text.html)-based
  template, _apertium-kaz.kaz.lexc.scrbl_. A normal
  _apertium-kaz.kaz.lexc_ file is generated when

  `racket apertium-kaz.kaz.lexc.scrbl`

  command is run.

* As opposed to the 3-element data structure of a _lexc_ \(upper-side
  string, lower-side string, continuation lexicon\), with other marks
  being comments formatted in a particular way by convention, the main
  datatype of _lexicon.rkt_ is an _Entry_ with 7 fields, representing
  the following information:

  * upper-side string

  * lower-side string

  * continuation lexicon\(s\)

  * a gloss and various \(restrictional\) marks such as USE/MT, DIR/LR,
    DIR/RL etc

  * inflected forms of this word, which were unnecessarily lexicalised
    in the _.lexc_ file we proofread \(or were lexicalised by the
    authors of the print dictionaries, but we thought it wasn’t
    necessary to lexicalise them in the transducer\)

  * stem from which this stem has been derived from in a semi-productive
    way, or a chain of such stems

  * normative spelling\(s\) of this word

* In the source code, entries can be wrapped up with function calls,
  which modify entries in various ways \(or not\), depending on how the
  functions in question are defined, and, ultimately, what defaults a
  particular application of _apertium-kaz_ calls for.

Below are four examples of entries, named as E-1, E-2, E-3 and E-4 for
easy re-use in tests.

```racket
(define E-1 (e "абдаста" "абдаста" 'N1 '() '() '() '("әбдесте")))                                     
(define E-2 (e "абдикация" "абдикация" 'N1 '() '() '() '()))                                          
(define E-3 (e "абдырат" "абдырат" 'V-IV `("confuse,embarras" ,USE/MT) '("абдырату") '("абдыра") '()))
(define E-4 (e "абыр-дабыр" "абыр-дабыр" '(IDEO N1) '() '() '() '()))                                 
```

Commentaries on **why** and **how** these modifications were made
follow.

## 1. Why these paper dictionaries were chosen as a source of new entries?

We have chosen the 15-volume and single-volume explanatory dictionaries
of Kazakh as a reference because they are:

* comprehensive,

* up-to-date,

* authoritative,

* developed by publicly-financed organisations, responsible for language
  policy in Kazakhstan, and **not** commercial companies.

Individual words \(entry words, to be exact, which interest us in this
project and which we have extracted from the dictionaries\) are not
copyrightable per se, but the later point is a further safeguard that we
are not violating anyone’s rights.

## 2. How stems were added to lexicon.rkt?

First of all, we copied all common words from _apertium-kaz.kaz.lexc_.
By common words we mean words which are not proper nouns. This includes
open-class words \(nouns, verbs, adjectives, adverbs\), but also
closed-class or functional words like pronouns, determiners,
postpositions etc.

With few rare exceptions, entry words contained in the single-volume
EDOK2013 are a superset of those contained in the 15-volume EDOK2011.
The size of the latter is due example usages and more elaborate
explanations.

Therefore, we proceeded as follows:

* extracted text from EDOK2013’s pdf file

* converted entries in it into \(entry word, rest of the entry\) pairs,
  separated by tabs

* labeled the first N entry words with the right categories, using
  [lexikograf.py](https://github.com/IlnarSelimcan/dot/blob/master/lexikograf.py)

Lexikograf.py expects two command-line arguments: a dictionary in plain
text format, and a number BATCH\_SIZE. Lines in the dictionary of the
following form:

`label \tab entry word \tab rest of the entry`

serve as training data.

Lines in the following form:

`entry word \tab rest of the entry`

are lines for which lexikograf.py will suggest a label, and the user is
requested to either mark the suggested label as correct or, if it is
not, to type in the correct label.

After having seen BATCH\_SIZE new observations, lexikograf.py adds these
new observations to the training data, and \(re\)trains a MaxEnt \(aka
multinomial logistic regression\) classifier. At each step, the
annotation process is backed up in ws.pickle file as a WorldState, which
is compound data structure consisting of the <classifier, labeled
entries, unlabeled entries>.

Apparently we labeled 754 entries in this way, after which the number
errors lexikograf.py made seemed negligible, so that we made it label
the rest of the entries and added the entry words to _lexikon.rkt_, if
such \(word, continuation lexicon pairs\) were not present in it
already.

## 3. Proof-reading lexicon.rkt

As described in the previous section, _lexicon.rkt_ is a union of
entries from two sources:

* common words of the original _apertium-kaz.kaz.lexc_ file, and

* entry words from EDOK2013 \(first 757 of which were hand-labeled with
  correct continuation marks, the rest with labels _lexikograf.py_’s
  classifier has assigned to them\)

The resulting _lexikograf.rkt_ requires manual cheking because:

* errors from the original _apertium-kaz.kaz.lexc_ got carried over
  \(see issue [\#11](https://github.com/apertium/apertium-kaz/issues/11)

* lexikograf.py might have labeled words from EDOK2013 inccorrectly
  \(read: they have a wrong continuation lexicon in lexicon.rkt\)

For mitigating both errors, we open up both _lexicon.rkt_ and EDOK2011,
and read both in parallel. We proof-read _lexicon.rkt_ against EDOK2011,
and not against EDOK2013, because the explanations of the former are
more elaborate, and, more importantly, it includes example usage
sentences for each entry word / sense.

For most of the words in _lexicion.rkt_, reading explanations or
examples was not necessary, as it was apparent whether their
continuation classes were correct or not, for some, reading example
sentences was crucial. Notably, they were helpful for figuring out
whether a verb was transitive or intransitive, or whether an adjective
was A1 or A2. As a side note, we decided to restrict the possible
continuation classes for adjectives two two \(A1 and A2\), thus
eliminating A3 and A4 entirely. The only difference between an A1
adjective and A2 adjective is that the former is actually both an
adjective and an adverb, and thus can modify both nouns and verbs, while
the latter is not is used solely as an attribute in a sentence.

## 4. Acknowledgements

This work is being funded by the Committee of Science of the Ministry of
Education and Science of the Republic of Kazakhstan, contract\#
346/018-2018/33-28, IRN AP05133700, principal investigator Zhenisbek
Assylbekov.

## 5. License

Just like _apertium-kaz_, the contents of this repo are published under
GPL version 3.
