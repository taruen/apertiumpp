
"""
A script, which, given a list of English words to translate,looks their lemmas
and definitions in English Wordnet and translates them into LANG2 via:
  - (chain) lookup in Apertium bidixes and
  - Yandex Translate

USAGE: python3 englishwords2multidix <file-with-english-words-one-word-per-line>

INPUT:

apple
book
<...>

CURRENT OUTPUT:

given|synset_lemmas|pos|def|apertium|yandex|selimcan|examples
apple|['apple']|n|fruit with red or yellow or green skin and sweet to tart crisp whitish flesh|алма|алма||[]
apple|['apple', 'orchard_apple_tree', 'Malus_pumila']|n|native Eurasian tree widely cultivated in many varieties for its firm rounded edible fruits|алма|алма||[]
book|['book']|n|a written work or composition that has been published (printed on pages bound together)|китап|китап||['I am reading a good book on economics']
<...>

TODO:
  - there is some redundant work because of importing bidixes2multidix, fix that
    (make bidixes2multidix as side-effect-free library and make use of
    it in a separate script)
"""

import sys
import urllib.request as ur
import json
import nltk
nltk.data.path.append(r"/home/selimcan/local/nltk_data/")
from nltk.corpus import wordnet as wn

import bidixes2multidix as b2m


#############
## Constants:



YAT_API_URL = 'https://translate.yandex.net/api/v1.5/tr.json/translate?'
YAT_API_KEY = 'get one yourself first, it is free

LANG1 = 'en'
LANG2 = 'tt'


WG = b2m.bidixes2wordgraph(
    b2m.append_leftiso3_rightiso3(
        b2m.get_bidixes(b2m.RELEVANT_ISOS)))

ENGLEM_ENGPOS_2_TATLEM = {}

for me in WG:
    if me.lang == 'eng':
        for target_me in b2m.wg_connections(WG, me):
            if target_me.lang == 'tat':
                ENGLEM_ENGPOS_2_TATLEM[me.lm] = {me.tags[0:]: target_me.lm}


#############
## Functions:


def main():
    with open(sys.argv[1]) as inf:
        for word in inf:
            word = word.strip()
            for synset in wn.synsets(word):
                print('{}|{}|{}|{}|{}|{}||{}'.format(word,
                                                     synset.lemma_names(),
                                                     synset.pos(),
                                                     synset.definition(),
                                                     translate_apertium(word, (synset.pos(),)),
                                                     translate_yandex(word.replace(' ',
                                                                                   '%20')),
                                                     synset.examples()))


## String -> String
def translate_yandex(word):
    """Translate the given string from LANG1 to LANG2."""
    with ur.urlopen('{}key={}&lang={}-{}&text={}'.format(YAT_API_URL,
                                                         YAT_API_KEY,
                                                         LANG1,
                                                         LANG2,
                                                         word)) as trans:
        return ' '.join(json.loads(trans.read().decode('utf8'))['text'])


## String String -> String
def translate_apertium(word, pos):
    """(Chain) translate using Apertium dictionaries."""
    try:
        return ENGLEM_ENGPOS_2_TATLEM[word][pos]
    except KeyError:
        return ''


main()
