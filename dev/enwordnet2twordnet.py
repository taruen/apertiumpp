
## enwordnet2wordnet.py
##
## A script which walks over the synsets in the English Wordnet and prints
## translations for each English lemma in each synset using Google Translate
## (gt), Yandex Translate (yt) and looking them up in Apertium (ap) bilingual
## dictionaries (turned into a multilingual word graph beforehand).
##
## USAGE: python3 enwordnet2twordnet.py
##
## A snippet from the current output:
##
## def: (botany) a living organism lacking the power of locomotion
## ex: []
##     eng: plant
##         aze-gt: bitki?
##         aze-yt: zavod?
##         bak-yt: завод?
##         kaz-ap: кәсіпорын?
##         kaz-ap: өсімдік?
##         kaz-ap: фабрика?
##         kaz-ap: зауыт?
##         kaz-ap: қондырғы?
##         kaz-ap: көшет?
##         kaz-gt: өсімдік?
##         kaz-yt: зауыт?
##         kir-gt: өсүмдүк?
##         kir-yt: завод?
##         tat-ap: комбинат?
##         tat-ap: үсемлек?
##         tat-ap: завод?
##         tat-yt: завод?
##         tur-gt: bitki?
##         tur-yt: bitki?
##         uzb-gt: o&#39;simlik?
##         uzb-yt: o'simlik?
##     eng: flora
##         aze-gt: flora?
##         aze-yt: Flora?
##         bak-yt: Флора?
##         kaz-ap: флора?
##         kaz-gt: өсімдіктер?
##         kaz-yt: Флора?
##         kir-gt: өсүмдүктөр?
##         kir-yt: Флора?
##         tat-ap: флора?
##         tat-yt: Флора?
##         tur-gt: bitki örtüsü?
##         tur-yt: flora?
##         uzb-gt: flora?
##         uzb-yt: o'simlik?
##     eng: plant life
##         aze-gt: bitki həytı?
##         aze-yt: həyt bitkilər ?
##         bak-yt: үҫемлектәр тормошо ?
##         kaz-gt: Өсімдіктердің өмірі?
##         kaz-yt: өсімдіктердің өмірі ?
##         kir-gt: өсүмдүктөрдүн жашоо?
##         kir-yt: ак-өсүмдүктөрдүн ?
##         tat-yt: тормыш үсемлекләр ?
##         tur-gt: bitki haytı?
##         tur-yt: bitki yaşamı?
##         uzb-gt: o&#39;simlik hayoti?
##         uzb-yt: o'simlik hayoti?
## <...>
## Full output is in the xnet/ folder.

import nltk
nltk.data.path.append(r"/home/selimcan/local/nltk_data")
from nltk.corpus import wordnet as wn
from yandex_translate import YandexTranslate  ## pip install yandex.translate
from googleapiclient.discovery import build

import wordgraph as wg


############
## Constants


APERTIUM_ROOT = '../apertium-all/'

## from here: http://wiki.apertium.org/wiki/Turkic-languages
RELEVANT_ISOS =  ['kaz', 'kz', 'tat', 'tt', 'kir', 'ky', 'tyv', 'tur', 'tr',
                  'chv', 'cv', 'kum', 'kaa', 'uzb', 'uz', 'sah', 'crh', 'krc',
                  'bak', 'ba', 'nog', 'gag', 'tuk', 'tk', 'uig', 'ug', 'kjh',
                  'ota', 'aze', 'az', 'eng', 'en']

SKIP_FOLDERS = ['release', 'branches']  ## only relevant for the old svn repo

MAIN_BIDIX = APERTIUM_ROOT + \
             'apertium-trunk/apertium-eng-kaz/apertium-eng-kaz.eng-kaz.dix'

APERTIUMPOS_2_WNPOS = {'n': wn.NOUN, 'v': wn.VERB, 'adj': wn.ADJ, 'adv': wn.ADV}

POS = 'n'

GT_API_KEY = 'get one yourself if you need to'

GT = build('translate', 'v2', developerKey=GT_API_KEY)

YAT_API_KEY = 'get one yourself if you need to'

YAT = YandexTranslate(YAT_API_KEY)

AWG = wg.manytags2singletag(
          wg.bidixes2wordgraph(
              wg.append_leftiso3_rightiso3(
                  wg.get_bidixes(APERTIUM_ROOT, SKIP_FOLDERS, RELEVANT_ISOS))))

TURKIC = ['alt', 'aze', 'bak', 'chv', 'crh', 'gag', 'kaa', 'kaz', 'kir', 'kjh',
          'krc', 'kum', 'nog', 'ota', 'sah', 'tat', 'tuk', 'tur', 'tyv', 'uig',
          'uzb']

TURKIC_IN_GT = {'aze','kaz', 'kir', 'tur', 'uzb'}

TURKIC_IN_YAT = {'aze', 'bak', 'kaz', 'kir', 'tat', 'tur', 'uzb'}


############
## Functions


def yat_translate(s, lang1, lang2):
    """ (String String String) -> String

    Translate lang1 string s to lang2 with Yandex Translate.
    """
    return ' '.join(YAT.translate(s, lang1 + '-' + lang2)['text'])


def gt_translate(s, lang1, lang2):
    """ (String String String) -> String

    Translate lang1 string s to lang2 with Google Translate.
    """
    return GT.translations().list(source=lang1,
                                  target=lang2, q=s).execute()['translations'][0]['translatedText']


#########
## Runner


for s in list(wn.all_synsets(POS))[:10]:
    print('def:', s.definition())
    print('ex:', s.examples())
    for l in s.lemmas():
        l = l.name().replace('_', ' ')
        print('    eng:', l)
        for lang in TURKIC:
            seen = set()
            try:
                nbrs = AWG[wg.MonolingEntry('eng', l, (POS,))]
                for n in nbrs:
                    if n.lang == lang and n.lm not in seen:
                        print('        ' + lang + '-ap:', n.lm + '?')
                        seen.add(n.lm)
            except KeyError:
                try:
                    nbrs = AWG[wg.MonolingEntry('eng', l, ())]
                    for n in nbrs:
                        if n.lang == lang and n.lm not in seen:
                            print('        ' + lang + '-ap:', n.lm + '?')
                            seen.add(n.lm)
                except KeyError:
                    pass
            if lang in TURKIC_IN_GT:
                print('        ' + lang + '-gt:',
                      gt_translate(l, 'eng', lang) + '?')
            if lang in TURKIC_IN_YAT:
                print('        ' + lang + '-yat:',
                      yat_translate(l, 'en', wg.ISO3_2_ISO2[lang]) + '?')
