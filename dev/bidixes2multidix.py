
"""
A program which takes two or more bilingual dictionaries (bidixes)
and spits out a multilingual dictionary (multidix) with nouns, verbs,
adjectives and adverbs where entries are linked to English Wordnet's
(word, definition) pairs.
"""

from collections import namedtuple


#############
## Constants:


####################
## Data definitions:


MonodixEntry = namedtuple("MonodixEntry", ["lang", "lm", "defn", "ex",
                                           "left", "right", "par", "attr"])
## MonodixEntry is MonodixEntry(String, String, String, (Tuple of String),
##                              String, String, String, (Tuple of Attribute))
## interp.: an extended monolingual dictionary entry, where:
##          - lang is iso3 code of the language
##          - lm is the lemma
##          - defn is a definition
##          - ex are example sentences or phrases
##          - left is the left hand side this word will have in a .lexc or
##            .dix entry
##          - right is the right hand side this word will have in a .lexc or
##            .dix entry
##          - par is the paradigm/lexicon this word is linked to
##          - attr is a list of attributes such "Dir/LR" used to trigger
##            certain actions when compiling the .lexc or .dix dictionary
##            or other useful marks

MONO_ENTRY_1 = MonodixEntry("eng",
                            "triangle",
                            "a three-sided polygon",
                            "",
                            "triangle",
                            "triangle",
                            "house__n",
                            ())

MONO_ENTRY_2 = MonodixEntry("eng",
                            "answer",
                            "respond to a signal",
                            ("answer the door", "answer the telephone"),
                            "answer",
                            "answer",
                            "accept__vblex",
                            ())

MONO_ENTRY_3 = MonodixEntry("kaz",
                            "ауыл",
                            "",
                            (),
                            "ауыл",
                            "ау{y}л",
                            "N1",
                            ("Dir/LR",))

MONO_ENTRY_4 = MonodixEntry("kaz",
                            "ауыл",
                            "",
                            (),
                            "ауыл",
                            "ауыл",
                            "N1",
                            ())

MONO_ENTRY_5 = MonodixEntry("kaz",
                            "үшбұрыш",
                            "",
                            (),
                            "үшбұрыш",
                            "үшбұрыш",
                            "N1",
                            ())

MONO_ENTRY_6 = MonodixEntry("kaz",
                            "үшкіл",
                            "",
                            (),
                            "үшкіл",
                            "үшкіл",
                            "N1",
                            ())

MONO_ENTRY_7 = MonodixEntry("tat",
                            "өчпочмак",
                            "",
                            (),
                            "өчпочмак",
                            "өчпочмак",
                            "N1",
                            ())

"""
def fn_for_monodixentry(me):
    ... me.lang   ## String
        me.lm     ## String
        me.defn   ## String
        me.ex     ## (Tuple of String)
        me.left   ## String
        me.right  ## String
        me.par    ## String
        me.attr   ## (Tuple of String)
"""


## Attribute is one of:
##   - "Dir/LR"
##   - "Dir/RL"
##   - "Use/MT"
##   - "Err/Orth"
##   - "Use/Circ"
##   - ...
"""
def fn_for_attr(a):
    if a.lower() == "dir/lr":
        ...
    elif a.lower() == "dir/rl":
        ...
    elif a.lower() == "use/mt":
        ...
    elif a.lower() == "err/orth"
        ...
    elif a.lower() == "use/circ"
        ...
    else:
        ...
"""


## Restriction is one of:
##   - ""
##   - "NA"
##   - "NG"
## interp.: appears in a MultidixEntry. NA stands for "no analysis". If a word
##          of language X is marked as NA, it is only relevant when translating
##          into X. NG stands for "no generation". Words of other languages
##          in a multidix entry won't be translted into a word marked as NG.
"""
def fn_for_restriction(r):
    if r == "":
        ...
    elif r.lower() == "ng":
        ...
    elif r.lower() == "na":
        ...
"""


## MultidixEntry is a tuple of (Restriction, MonodixEntry) tuples

MULTI_ENTRY_1 = (("", MONO_ENTRY_1),
                 ("", MONO_ENTRY_5),
                 ("NG", MONO_ENTRY_6),
                 ("", MONO_ENTRY_7))

MULTI_ENTRY_2 = (("", MONO_ENTRY_2),)


## Multidix is a tuple of MultidixEntries

MULTIDIX_1 = (MULTI_ENTRY_1, MULTI_ENTRY_2)


############
## Functions


## WIP

import nltk
nltk.data.path.append(r"/home/selimcan/local/nltk_data/")
import yaml
from nltk.corpus import wordnet as wn

for s in wn.all_synsets('n'):
    for l in s.lemmas():
        print(yaml.dump(MonodixEntry('eng', l.name(), s.definition(),
                           tuple(s.examples()), l.name(), l.name(), "", ())))
        print('\n')
