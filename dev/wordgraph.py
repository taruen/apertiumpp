
"""
wordgraph.py

A library for converting two or more Apertium bidixes into a Wordgraph (its
definition you can see below) and then doing various things with that
wordgraph such as:
- exporting it as a Multidix, in which entries are *optionally* linked to
  English Wordnet's definitions (see bidixes2multidix.py),
- translating English Wordnet lemmas to other languages via (chain) lookup
  in the wordgraph or in Google/Yandex translate (see enwordnet2twordnet.py),
- or generating new bidixes for language pairs for which you didn't have a
  bidix before (TODO).

USAGE: import wordgraph as wg

TODO:
  - handle LR RL restrictions
"""

import xml.etree.ElementTree as ET
from xml.dom import minidom
import glob
import os.path
from collections import namedtuple, defaultdict
from io import StringIO
import re
from copy import deepcopy
import sys


## Constants
## =========


ISO2_2_ISO3 = {'kz': 'kaz', 'tt': 'tat', 'ky': 'kir', 'tr': 'tur', 'cv': 'chv',
               'uz': 'uzb', 'ba': 'bak', 'tk': 'tuk', 'ug': 'uig', 'az': 'aze',
               'en': 'eng'}
ISO3_2_ISO2 = {'kaz': 'kk', 'tat': 'tt', 'kir': 'ky', 'tur': 'tr', 'chv': 'cv',
               'uzb': 'uz', 'bak': 'ba', 'tuk': 'tk', 'uig': 'ug', 'aze': 'az',
               'eng': 'en'}


## Data definitions
## ================


MonolingEntry = namedtuple("MonolingEntry", ["lang", "lm", "tags"])
##
## MonolingEntry is MonolingEntry(String, String, (Tuple of String))
## interp.: a monolingual dictionary entry, where:
##          - lang is iso3 code of the language
##          - lm is the lemma
##          - tags are the symbols used in Apertium to denote part-of-speech
##            tags and other morphological features (the ones which you'd
##            put into a bidix)

MONOLING_E_1 = MonolingEntry("eng", "", ())  # null translation
MONOLING_E_2 = MonolingEntry("eng", "file", ("n",))
MONOLING_E_3 = MonolingEntry("kaz", "файл", ("n",))
MONOLING_E_4 = MonolingEntry("kaz", "егеу", ("n",))
MONOLING_E_5 = MonolingEntry("tat", "игәү", ("n",))
MONOLING_E_6 = MonolingEntry("eng", "Moscow", ("np", "top"))
MONOLING_E_7 = MonolingEntry("tat", "Мәскәү", ("np", "top", "hargle"))
MONOLING_E_8 = MonolingEntry("rus", "Москва", ("np",))
MONOLING_E_9 = MonolingEntry("tur", "Moskova", ())


## A Graph is a Dictionary which maps Object to a (Set of Object).
## interp.: {node: {its, neighbouring, nodes}

G_1 = {'a': {'b', 'c'},                     ## a---b---d---f
       'b': {'a', 'c', 'd'},                ##  \ /
       'c': {'a', 'b'},                     ##   c     g  h---i
       'd': {'b', 'f'},
       'f': {'d'},
       'g': {},
       'h': {'i'},
       'i': {'h'}}


## WordGraph is a Graph which maps MonolingEntry to
## a (Set of MonolingEntry)
## interp.: {monoling_e_1: {monoling_e_2, monoling_e_3},
##           monoling_e_2: {monoling_e_1},
##           monoling_e_3: {monoling_e_1}}
##
##   means that (monoling_e_1 and monoling_e_2), and
##   (monoling_e_1 and monoling_e_3) were translations of each other in a bidix.

WG_1 = {MONOLING_E_2: {MONOLING_E_3, MONOLING_E_4},
        MONOLING_E_3: {MONOLING_E_2},
        MONOLING_E_4: {MONOLING_E_2}}

WG_2 = {MONOLING_E_2: {MONOLING_E_3, MONOLING_E_4},
        MONOLING_E_3: {MONOLING_E_2},
        MONOLING_E_4: {MONOLING_E_2, MONOLING_E_5},
        MONOLING_E_5: {MONOLING_E_4}}

WG_3 = {MONOLING_E_6: {MONOLING_E_7, MONOLING_E_8, MONOLING_E_9},
        MONOLING_E_7: {MONOLING_E_6},
        MONOLING_E_8: {MONOLING_E_6},
        MONOLING_E_9: {MONOLING_E_6}}


## Functions
## =========

def main(main_bidix, iso_codes):
    """ String (List of String) -> String

    Given the path to the main bidix (read: biggest English-to-X or
    X-to-English dictionary) and a list of iso3 codes of relevant languages,
    construct a multidix, in which English words are linked to
    their Wordnet definitions (in case of nouns, adjectives, verbs and
    adverbs) and their translations to languages listed in iso_codes, and
    return a string representation of that multidix (read: xml).

    A word is considered a translation of the English word if there exists
    a path between the two in the WordGraph constructed out of the bidixes.
    """
    wg = bidixes2wordgraph(
        append_leftiso3_rightiso3(
            get_bidixes(iso_codes)))

    bidix = ET.parse(main_bidix)
    root = bidix.getroot()
    for e in root.iter('e'):
        try:
            left, right = pair2monolings(e[0], 'eng', 'kaz')
        except IndexError:  # <e><re>...</re><p>...</p>
            left, right = pair2monolings(e[1], 'eng', 'kaz')
        if left.lm and len(left.tags) >= 1:
            if left.tags[0] in {'n', 'v', 'adj', 'adv'}:
                for defn in \
                  [synset.definition() for synset in \
                    wn.synsets(left.lm,
                               APERTIUMPOS_2_WNPOS[left.tags[0]])]:
                    if e.text:
                        e.text +=(defn + '\n')
                    else:
                        e.text = defn + '\n'
        e.append(deepcopy(monolinge_2_iso3element(left)))
        e.append(deepcopy(monolinge_2_iso3element(right)))
        for monoling_e in wg_connections(wg, left):
            e.append(deepcopy(monolinge_2_iso3element(monoling_e)))
        for p in e.iter('p'):
            e.remove(p)
 
    return minidom.parseString(ET.tostring(root)).toprettyxml(indent="  ",
                                                              newl="\n")


def manytags2singletag(wg):
    """ WordGraph -> WordGraph

    Iterate through all nodes (= MonolingEntries) of wg and, if
    a monolingentry.tags has many tags, limit it to a single tag
    (part-of-speech tag).
    """
    def _manytags2singletag(me):
        if len(me.tags) > 1:
            return MonolingEntry(me.lang, me.lm, me.tags[:1])
        else:
            return me

    res = defaultdict(set)
    for me in wg:
        if len(me.tags) > 1:
            for neibr in wg[me]:
                res[_manytags2singletag(me)].add(_manytags2singletag(neibr))
        else:
            for neibr in wg[me]:
                res[me].add(_manytags2singletag(neibr))
    return res
 
def test_manytags2singletag():
    assert manytags2singletag(WG_3) == \
        {MonolingEntry("eng", "Moscow", ("np",)):
            {MonolingEntry("tat", "Мәскәү", ("np",)),
             MonolingEntry("rus", "Москва", ("np",)),
             MonolingEntry("tur", "Moskova", ())},
         MonolingEntry("tat", "Мәскәү", ("np",)):
             {MonolingEntry("eng", "Moscow", ("np",))},
         MonolingEntry("rus", "Москва", ("np",)):
             {MonolingEntry("eng", "Moscow", ("np",))},
         MonolingEntry("tur", "Moskova", ()):
             {MonolingEntry("eng", "Moscow", ("np",))}}


def g_connections(graph, start_node):
    """ Graph -> (Generator Object)

    Traverse the graph (avoiding cycles) starting with start_node and yield
    all nodes the start node is connected to.
    """
    frontier = set()
    seen = {start_node}
    for neighbour in graph[start_node]:
        frontier.add(neighbour)
    while frontier:
        current = frontier.pop()
        if current not in seen:
            yield current
            seen.add(current)
            for neighbour in graph[current]:
                frontier.add(neighbour)
        else:
            continue

def test_g_connections():
    assert list(g_connections(G_1, 'g')) == []
    assert list(g_connections(G_1, 'h')) == ['i']
    assert sorted(g_connections(G_1, 'i')) == ['h']
    assert sorted(g_connections(G_1, 'a')) == ['b', 'c', 'd', 'f']
    assert sorted(g_connections(G_1, 'c')) == ['a', 'b', 'd', 'f']


def wg_connections(graph, start_node):
    """ WordGraph -> (Generator MonolingEntry)

    Traverse the graph (avoiding cycles) starting with start_node and yield
    all nodes the start node is connected to.
    """
    frontier = set()
    seen = {start_node.lang}
    for neighbour in graph[start_node]:
        frontier.add(neighbour)
    while frontier:
        current = frontier.pop()
        if current.lang not in seen:
            yield current
            seen.add(current.lang)
            for neighbour in graph[current]:
                if neighbour.lang not in seen:
                    frontier.add(neighbour)
        else:
            continue

def test_wg_connections():
    assert sorted(g_connections(WG_2, MONOLING_E_2)) ==\
           sorted([MONOLING_E_3,
                   MONOLING_E_4,
                   MONOLING_E_5])


def bidixes2wordgraph(bidixes):
    """ (List of (String, String, String) -> WordGraph

    Given a list of (bidix file name, lang1 iso3 code, lang 2 iso3 code)
    tuples, return a WordGraph with all stems contained in those bidix files.
    """
    res = defaultdict(set)
    for bidix, left_lang, right_lang in bidixes:
        try:
            bidix_root = ET.parse(bidix).getroot()
        except ET.ParseError:
            print("Couldn't parse ", bidix, ". Ill-formed xml?",
                  file=sys.stderr)
            continue
        for pair in bidix_root.iter('p'):
            left, right = pair2monolings(pair, left_lang, right_lang)
            res[left].add(right)
            res[right].add(left)
    return res

def test_bidixes2wordgraph():
    eng_kaz = StringIO(u"""<?xml version="1.0" encoding="UTF-8"?>
                     <dictionary>
                       <alphabet></alphabet>
                       <sdefs>
                         <sdef n="n"               c="Noun"/>
                       </sdefs>

                       <section id="main" type="standard">
                         <e><p><l>file<s n="n"/></l><r>файл<s n="n"/></r></p></e>
                         <e><p><l>file<s n="n"/></l><r>егеу<s n="n"/></r></p></e>
                       </section>
                     </dictionary>""")
    kaz_tat = StringIO(u"""<?xml version="1.0" encoding="UTF-8"?>
                     <dictionary>
                       <alphabet></alphabet>
                       <sdefs>
                         <sdef n="n"               c="Noun"/>
                       </sdefs>

                       <section id="main" type="standard">
                         <e><p><l>егеу<s n="n"/></l><r>игәү<s n="n"/></r></p></e>
                       </section>
                     </dictionary>""")

    assert bidixes2wordgraph([(eng_kaz, "eng", "kaz"),
                              (kaz_tat, "kaz", "tat")]) == WG_2


def pair2monolings(pair, left_lang, right_lang):
    """ ElementTree.Element String String -> (MonolingEntry, MonolingEntry)

    Extract the <l>eft and <r>ight hand sides from a <p>air element.
    """
    return MonolingEntry(left_lang,
                         ' '.join(pair[0].itertext()),
                         tuple(s.attrib['n'] for s in pair[0].iter('s'))), \
           MonolingEntry(right_lang,
                         ' '.join(pair[1].itertext()),
                         tuple(s.attrib['n'] for s in pair[1].iter('s')))

def test_pair2monolings():
    assert pair2monolings(ET.fromstring("""<p><l>file<s n="n"/></l><r>файл<s n="n"/></r></p>"""), "eng", "kaz") == \
           (MONOLING_E_2, MONOLING_E_3)


def monolinge_2_iso3element(monoling_e):
    """ MonolingEntry -> ElementTree.Element

    Convert the given monolingual entry into a xml element to be put
    inside of <e> in the final multidix.
    """
    res = ET.Element(monoling_e.lang)
    res.text = monoling_e.lm
    for tag in monoling_e.tags:
        ET.SubElement(res, 's', {'n': tag})
    return res

def test_monolinge_2_iso3element():
    assert ET.tostring(monolinge_2_iso3element(MONOLING_E_1),
                       encoding="unicode") == "<eng />"
    assert ET.tostring(monolinge_2_iso3element(MONOLING_E_6),
                       encoding="unicode") == \
           """<eng>Moscow<s n="np" /><s n="top" /></eng>"""


def append_leftiso3_rightiso3(bidixes):
    """ (List of String) -> (List of (String, String, String))

    Given a list with the names of bidix files, extract the language names
    and return a list with (bidix file name, lang1 iso3 code, lang2 iso3 code)
    tuples.
    ASSUME: bidix files are named following the standard:
            apertium-iso2or3-iso2or3.iso2or3-iso2or3.dix
    """
    res = []
    for bidix in bidixes:
        try:
            parse = re.search(r'.*apertium-([^-]+)-([^-]+).\1-\2.dix', bidix)
            lang1_iso3 = ISO2_2_ISO3.get(parse.group(1), parse.group(1))
            lang2_iso3 = ISO2_2_ISO3.get(parse.group(2), parse.group(2))
            res.append((bidix, lang1_iso3, lang2_iso3))
        except AttributeError:
            raise ValueError("Couldn't figure out the source language and "
                             "target language's iso codes from the bidix name!")
    return res

def test_append_leftiso3_rightiso3():
    assert append_leftiso3_rightiso3(['../apertium-kaz-tat.kaz-tat.dix',
                                      '/home/foo/apertium-tt-ky.tt-ky.dix',
                                      'apertium-ug-kaz.ug-kaz.dix']) == \
           [('../apertium-kaz-tat.kaz-tat.dix', 'kaz', 'tat'),
            ('/home/foo/apertium-tt-ky.tt-ky.dix', 'tat', 'kir'),
            ('apertium-ug-kaz.ug-kaz.dix', 'uig', 'kaz')]


def get_bidixes(apertium_root, skip_folders, iso_codes):
    """ String (List of String) (List of String) -> (List of String)

    Return the paths to all bidixes in apertium_root repo, in which both sl and
    tl are a language in iso_codes (except for bidixes in skip_folders)
    """

    def is_skippable(filepath):
        """ String -> Boolean

        Given a path to a bidix file, return True if it is located in
        a folder which should be skipped (code in branches/,release/ or similar).
        """
        for f in skip_folders:
            if f in filepath:
                return True
        return False

    res = []
    for filename in glob.iglob(apertium_root + '**/*.dix', recursive=True):
        if not is_skippable(filename):
            basename = os.path.basename(filename)
            for frst_iso in iso_codes:
                for scnd_iso in iso_codes:
                    if basename == "apertium-{0}-{1}.{0}-{1}.dix".format(frst_iso,
                                                                         scnd_iso):
                        res.append(filename)
    print('\n'.join(res), file=sys.stderr)
    return res


## Formatters
## ----------


def wordgraph2sexp(wg):
    """ WordGraph -> String

    Return s-expression representation of wg.
    """

    def me2sexp(me):
        return '(' + me.lang + ' "' + me.lm + '" (' + \
               ' '.join(me.tags) + '))'

    return '(' + '\n '.join(me2sexp(k) + \
                           ' (' + \
                           ' '.join(me2sexp(n) for n in sorted(list(v))) + \
                           ')' \
                           for k, v in wg.items()) + \
           ')'

def test_wordgraph2sexp():
    expected = \
    """
    ((eng "file" (n)) ((kaz "егеу" (n))
                       (kaz "файл" (n)))
     (kaz "файл" (n)) ((eng "file" (n)))
     (kaz "егеу" (n)) ((eng "file" (n))))
    """
    assert " ".join(wordgraph2sexp(WG_1).split()) == " ".join(expected.split())


## Runner
## ======

#    print(main(MAIN_BIDIX, RELEVANT_ISOS))
