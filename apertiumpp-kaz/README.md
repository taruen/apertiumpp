# Apertiumpp-kaz

Ilnar Salimzianov

[This
directory](https://github.com/taruen/apertiumpp/tree/master/apertiumpp-kaz)
contains our effort to proofread
[apertium-kaz’s](https://github.com/apertium/apertium-kaz/) lexicon
against the 15-volume \[Explanatory\] Dictionary of Literary Kazakh
\(“Қазақ Әдеби Тілінің Сөздігі”\), published by Kazakh [Linguistics
Institute](https://tbi.kz) in 2011, and the single-volume
\[Explanatory\] Dictionary of Kazakh \(“Қазақ Сөздігі”\), publshed by
the [Linguistics Institute](https://tbi.kz) and Kazakh [Language
Commitee](https://mks.gov.kz/kaz/informery/komitety/k_ya_opr/) in 2013.
The goal is to solve issue
[\#11](https://github.com/apertium/apertium-kaz/issues/11) of
apertium-kaz, as well as to extend it with more stems, especially with
common words \(as opposed to proper nouns\).

In particular, the file
[lexicon.rkt](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/lexicon.rkt)
in this directory contains common words from
[apertium-kaz/apertium-kaz.kaz.lexc](https://github.com/apertium/apertium-kaz/blob/master/apertium-kaz.kaz.lexc),
revised and extended with more stems from the aforementioned print
dictionaries.

We plan to merge the results back to apertium-kaz once we’re done
proof-reading.

In the meantime, we TODO provide  [our own
version](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/apertium-kaz.kaz.lexc)
of apertium-kaz.kaz.lexc as a drop-in replacement for  apertium-kaz’s
file of the same name. To use apertium-kaz with our  modifications to
it, you should:

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
  using.

* Dowloand apetium-kaz:

  `git clone https://github.com/apertium/apertium-kaz.git`

* Replace apertium-kaz/apertium-kaz.kaz.lexc file with the file [that we
  provide](https://github.com/taruen/apertiumpp/blob/master/apertiumpp-kaz/apertium-kaz.kaz.lexc).

* Compile apertium-kaz:

  `cd apertium-kaz; ./autogen.sh; make`

Number of stems before: ~38k Number of stems after: ~98k

Naive coverage before: TODO Naive coverage after: TODO

## 1. How stems were added to lexicon.rkt?


## 2. Acknowledgements

This work is being funded by [Nazarbayev
University](https://nu.edu.kz/).

## 3. License

Just like apertium-kaz, the contents of this repo are published under
GPL version 3.
