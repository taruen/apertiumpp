#lang scribble/text

# ============================================================================ #
#            A COMMENTARY ON FORMATTING OF MAJORITY OF RULES
#              (read: a wannabe FORMATTING GUIDELINES)
# ============================================================================ #

# Lines do not exceed 79 characters.
#
# Rule layout
# ===========
#
# There are two alternative layouts for writing a rule. An example for the
# first one (of course the body of the rule won't be commented out):
#
#        # INPUT: ^терістік/тер<v><tv><coop><ifi><p1><pl>/терістік<n><attr>/
#        #        терістік<n><nom>$ ^бетіндегі/бет<n><px3sp><loc><attr>/
#        #        бет<n><px3sp><loc><subst><nom>/$
#        # OUTPUT: [0]^терістік/терістік<n><attr>/терістік<n><nom>$
#                   ^бетіндегі/бет<n><px3sp><loc><attr>...$
#         REMOVE V IF
#                (0 PRE-N)
#                (1C N)
#        ;
#        # any additional comments here (if necessary)
#
# An abstract example of the second layout:
#
#        # An explanation of what the rule does (if it's not obvious from
#        # code!), a name or any other comment that clarifies its purpose
#         SELECT ABC IF
#                (0 DEF)
#                (1 GHI)
#        ;
#        ## A sentence with the [0]form the rule was written for.
#        ## (!) A corner-case-sentence: the rule mispredicted on it initially
#        ## but was modified accordingly (additional constraints etc.); or the
#        ## rule still mispredicts on it, but this kind of sentences aren't
#        ## that frequent. (Alternatively, place the corner-case sentence on
#        ## the same line as the additional constraint).
#
# The first layout is more appropriate when you come up with a rule inductively
# (i.e. you see a sentence badly translated because of errors in disambiguation
# and want to fix it), the second one when you arrived at it deductively. E.g:
#
#        # Participle appears immediately before a verb (verbal adverb doesn't)
#         REMOVE Participle IF
#                (NOT 1C V OR Vaux)
#        ;
#        ## Есік [0]ашып, ол үйге кірді.
#
# In context of apertium-kaz, the difference between a participle and a verbal
# adverb is that participle immediately precedes a verb and verbal adverb
# doesn't. We know this even without looking at tests, but it is a good idea
# to back up the rule with a test case.
#
# Sometimes you can't fully disambiguate a form with only one rule. In that
# case, several rules dealing with the same thing can be grouped together:
#
#         # What the rule does?
#          SELECT ABC IF
#                 (0 DEF)
#                 (1 KLM)
#         ;
#          REMOVE XYZ IF
#                 (0 DEF)
#                 (1 NOP)
#                 (2 QRS)
#          ;
#          ## Example sentences for both of the rules.
#          ## Another example sentence.
#
# Where a rule is placed?
# =======================

#                                   Any context involved?
#                                   /                   \
#                                 YES                   NO
#                                 /                       \
#                 Disambiguates a particular wordform?   "Heuristics" section
#                   /                         \          at the end of the file
#                 YES                         NO
#                 /                             \
#     "Wordform/lemma specific rules"        Readings of
#            section                        different POS?
#                                           /           \
#                                        YES            NO
#                                        /                \
#                                "POS marking"             \
#                              at the beginning             \
#                                                   Corresponding section
#                                                   ("Verbs", "Adjectives" etc.)

# ============================ #
# END OF FORMATTING GUIDELINES #
# ============================ #

# ========== #
# Delimiters #
# ========== #

DELIMITERS = "<.>" "<!>" "<?>" "<...>" "<¶>";
SOFT-DELIMITERS = "<,>" ;

SUBREADINGS = LTR ; # Alternate, left-to-right (main reading on the left)

# ============= #
# Tags and sets #
# ============= #

SETS

LIST BOS = (>>>) ;
LIST EOS = (<<<) ;

# First-level/Parts-of-speech tags
# ================================

LIST A = adj ;
LIST ADV = adv ;
LIST PRN = prn ;
LIST PRN-PERS = (prn pers) ;
LIST N = n ;
LIST IJ = ij ;
LIST PROP = np ;
LIST V = v ;
LIST VAUX = vaux ;
LIST COP = cop ;
LIST DET = det ;
LIST CC = cnjcoo ;
LIST CS = cnjsub ;
LIST IJ = ij ;
LIST PRES = pres ;
LIST NUM = num ;
LIST POST = post ;
LIST POSTADV = postadv ;
LIST FINAL-CLITIC = mod_ass mod_emo mod qst emph ;
LIST SENT = sent ;
LIST CM = cm ;
LIST RQUOT = rquot ;
LIST EXCL = "!" ;
LIST GUIO = guio ;

# POS sub-categories
# ==================

LIST PERS = pers ;
LIST ITG = itg ;
LIST DEM = dem ; 

LIST IV = iv ; 
LIST TV = tv ;

LIST PASS = pass ;
LIST CAUS = caus ;
LIST COOP = coop ;

LIST ORD = ord ;

# "Syntactic" tags
# ================

LIST ADVL = advl ;
LIST ATTR = attr ;
LIST SUBST = subst ;
LIST N-LIKE = subst n np prn ; # Actually, see NOMINAL

# Morphosyntactic properties
# ==========================

LIST SG = sg ;
LIST Pl = pl ;

LIST NOM = nom ;
LIST GEN = gen ;
LIST DAT = dat ;
LIST ACC = acc ;
LIST ABL = abl ;
LIST LOC = loc ;
LIST INS = ins ;

LIST P1 = p1 ;
LIST P2 = p2 ;
LIST P3 = p3 ;

LIST EQU = equ ;

LIST POSSESSIVES = px1sg px2sg px3sp px1pl px2pl ;

LIST IMP = imp ;

# Specific lexemes 
# ==========================

LIST YEAR = "жылғы"i "жыл"i ; 

LIST COLON = ":" ;

LIST QAYDA = "қайда"i ;

LIST MONTHS = "қаңтар"i "ақпан"i "наурыз"i "сәуір"i "мамыр"i "маусым"i "шілде"i
              "тамыз"i "қыркүйек"i "қазан"i "қараша"i "желтоқсан"i ;

LIST QUANTITY = "есе"i "пайыз"i ;

LIST JOSPARLAN = "жоспарлан"i ;

LIST POST-DAT = "дейін"i "қарағанда"i "қарай"i "қарамастан"i "қоса"i ;

# Noun sets 
# ==============

SET NGDAALI = Nom | Dat | Dat | Acc | Abl | Loc | Ins ;

# Adjective sets
# ==============

LIST A/ADVL = (adj advl) ;
LIST A/SUBST = (adj subst nom) ;
SET A1-NOM = A/Advl | A/Subst ;

# Verb sets
# =========

LIST FINITE-VERB = pres aor past ifi ifi_evid fut fut_plan imp opt pih ;
LIST V-P3 = (v p3) (vaux p3) (cop p3) ;
LIST GERUND = ger ger_ppot ger_past ger_perf ger_impf ger_abs ;
SET V-NOT-GERUND = V - Gerund ;
LIST PARTICIPLE = prc_perf prc_impf prc_cond prc_vol prc_plan ;
LIST PRC-VOL = prc_vol ;
LIST VERBAL-ADVERB = gna_perf gna_cond gna_until gna_after ;
LIST VERBAL-ADJECTVE = gpr_past gpr_fut gpr_pot gpr_impf ;
LIST FOUR-AUXILIARIES = "тұр" "жүр" "отыр" "жат" ;
LIST V-IV = (v iv) ;
LIST V-TV = (v tv) ;

LIST ERR/ORTH = err_orth ;

# All possible word categories
# ============================

SET WORD = N | V | A | Post | Postadv | PRN | Det | Adv | CC | CS | Interj |
           Num | ("\?") ;

SET PRE-N =  A | Det | Postadv | Num | (n gen) | (prn gen) | CC | (attr) ;

SET MARK =  Cm | ("\\") | ("\;") | ("\(") ;

SET WORDMARK = WORD | MARK ;

SET N-MOD = A | Det | Num | (n gen) | (prn gen) ;

SET ADJ-MOD = Postadv | Adv ;

SET NPMARK = N-MOD | ADJ-MOD ;

SET NOMINAL = N | Prop | PRN | Subst | Gerund ;

# Categories which cannot be part of a noun phrase
# ================================================

SET NPNH = WORDMARK - PRE-N ;
SET NPNHA = WORDMARK - PRE-N - Adv ;
SET NOT-ADV = WORDMARK - Adv ;

# Barriers
# ========

SET S-BOUNDARY = CS | Interr | EOS ;

# Universal Dependencies
# ======================

LIST @"@"root = @"@"root ;     # The root of the sentence, often a finite verb
LIST @"@"nsubj = @"@"nsubj ;   # The nominal subject of the sentence
LIST @"@"amod = @"@"amod ;       # 
LIST @"@"advmod = @"@"advmod ; # An adverbial modifier
LIST @"@"case = @"@"case ;     # The relation of an adposition to its head
LIST @"@"acl = @"@"acl ;       # A clause which modifies a nominal
LIST @"@"nmod = @"@"nmod ;     # Nominal modifier 
LIST @"@"dobj = @"@"dobj ;     # The direct object of the sentence
LIST @"@"punct = @"@"punct ;   # Any punctuation
LIST @"@"cop = @"@"cop ;       # 
LIST @"@"nmod:poss = @"@"nmod:poss ;
LIST @"@"obl = @"@"obl ;
LIST @"@"obj = @"@"obj ;
LIST @"@"advcl = @"@"advcl ;
LIST @"@"aux = @"@"aux ;
LIST @"@"parataxis = @"@"parataxis ;
LIST @"@"det = @"@"det ;
LIST @"@"csubj = @"@"csubj ;
LIST @"@"nummod = @"@"nummod ;
LIST @"@"dep = @"@"dep ;       # Any remaining dependency


# ======= #
  SECTION # repair 
# ======= #


REMOVE SUB:1 ERR/ORTH ; 

REMOVE ERR/ORTH ; 

SELECT PASS IF (0 JOSPARLAN) ;

REMOVE EQU IF (0 A/ADVL) ;

REMOVE CAUS IF (0 V-TV) ;

REMOVE COOP IF (0 V-IV) ;


# ======= #
  SECTION # (COPULAS)
# ======= #


#
 "<кейін>" SELECT  IF
                (-1 ABL)
;
## 23. Айгүл санап біткеннен кейін айналасына қарады.



# INPUT: ^оның/ол<prn><dem><gen>/он<num><subst><px2sg><nom>/
#        ол<prn><pers><p3><sg><gen>$ ^үйі/үй<n><px3sp><nom>$
# OUTPUT: [0]^ол<prn><pers><p3><sg><gen>$ ^үй<n><px3sp><nom>$
 SELECT PRN-PERS IF
        (0 ("<оның>"i))
        (1 (n px3sp nom))
;

 REMOVE SUB:1 COP IF
        (NOT 1 EOS OR MARK OR ("де"))
;
 REMOVE SUB:1 COP IF
        (-1 BOS OR MARK) ## Headings or enumerations
        (NOT 1 EOS)
;
##

## Example: handle 'орында' in
## Қазақстанның басқа қалаларымен салыстырғанда тұрғыны жөнінен 3-ші орында ( Алматы мен Астанадан кейін ) .
 SELECT SUB:1 COP IF
        (1 LPAR))
        (2* (RPAR) BARRIER EOS)
        (NOT -1 COLON) 
 ;



#
 SELECT SUB:1 COP IF
        (1 MARK)
        (2*/1 COP BARRIER EOS)
        (NOT 0 IJ) ## Дұрыс, оның мысығы бар.
        (NOT 0 FINITE-VERB) ## 74 ... барлығы 53 ел [0]қатысты.
        (NOT 2 N)
;
## Жоқ, Айгүлдің күшігі [0]жоқ, оның мысығы [0]бар.

 SELECT SUB:1 COP IF
        (1 EOS)
        (NOT 0 V OR VAUX)
 ;

# ======= #
  SECTION
# ======= #



# POS marking
# ===========

"<қандай>" REMOVE N ;

SELECT N IF
        (0 ("сурет"i))
        (-1 (ADJ ADVL) OR DET OR A)
       ;


#INPUT: ^Сіздің/сіз<prn><pers><p2><sg><frm><gen>$ ^атыңыз/ат<v><tv><imp><p2><frm><sg>/ат<n><px2sg><frm><nom>/ат<n><px2sg><frm><nom>+е<cop><aor><p3><pl>/ат<n><px2sg><frm><nom>+е<cop><aor><p3><sg>$ ^кім/кім<det><itg>/кім<prn><itg><nom>/кім<prn><itg><nom>+е<cop><aor><p3><pl>/кім<prn><itg><nom>+е<cop><aor><p3><sg>$^?/?<sent>$^./.<sent>$
#OUPUT: ^Сіздің/сіз<prn><pers><p2><sg><frm><gen>$ ^атыңыз/ат<n><px2sg><frm><nom>$ ^кім/кім<det><itg>$^?/?<sent>$^./.<sent>$
SELECT N IF
        (0 ("ат"i))
        (-1 (PRN PERS))
       ;

# This rule is too general, breaks a few things.Corrected

#INPUT: ^әдемі/әдемі<adj> 
#^жер/же<v><tv><gpr_fut>/жер<n><nom>/жер<n><attr>/же<v><tv><fut><p3><pl>/же<v><tv><fut><p3><sg>/же<v><tv><gpr_fut><subst><nom>/жер<v><iv><imp><p2><sg>/жер<v><tv><imp><p2><sg>/жер<n><nom>+е<cop><aor><p3><pl>/жер<n><nom>+е<cop><aor><p3><sg>$
#OUPUT:  ^әдемі/әдемі<adj> 
#^жер/жер<n><nom>$





SELECT N IF
	(0 ("жер"))
	(-1 A)
;

# INPUT: for examle: "ата келді" translated as "grandfather come" 
SELECT N IF
	(0 ("ата"))
       ( (NOT -1 N) OR (NOT -1 BOS))
	( (NOT -1 (Nacc)) OR (NOT -1 BOS))
;

# INPUT: for examle: "Мен жазуым мүмкін" translated as "I may write" 
# SELECT V IF
#        (-1 PRN-PERS)
#	    (NOT 0 ("сол"))
#;
#
# this rule fails several times when tested against puupankki corpus

#INPUT: ^ластай/лас<n><sim>/лас<adj><subst><sim>/ласта<v><tv><prc_impf>$ ^аласың/ал<vaux><aor><p2><sg>/ал<v><tv><aor><p2><sg>/ала<adj>+е<cop><aor><p2><sg>/ала<adj><subst><nom>+е<cop><aor><p2><sg>$
#OUTPUT:^ластай/ласта<v><tv><prc_impf>$ ^аласың/ал<vaux><aor><p2><sg>

SELECT PARTICIPLE IF (1 VAUX)
; 


# INPUT:  "жүз теңге" translated as "hundred coins", екі жүз адам "two handred people"
#"адамдардың жүзі" translated a "face of people"
# ^жүз/жүз<num>/жүз<n><nom>/жүз<n><attr>/жүз<num><subst><nom>/жүз<v><iv><imp><p2><sg>/жүз<n><nom>+е<cop><aor><p3><pl>/жүз<n><nom>+е<cop><aor><p3><sg>/жүз<num><subst><nom>+е<cop><aor><p3><pl>/жүз<num><subst><nom>+е<cop><aor><p3><sg>$^
SELECT NUM IF
	((0 NUM) OR (-1 ))
	(1 N)
	(NOT 0 ("бірге"i))
	(NOT -1 A)
	(NOT 0 ("бір"i))
	(NOT 0 ("ол"i))
	(NOT 0 ("біреу"i))
	(NOT 0 ("үшін"i))
;
#
# exceptions are added to handle e.g. 'мысығымен бірге үйде отыр', 'ол бір дыбысты естиді', 'үлкен бір ағаш'

REMOVE SUB:1 FINAL-CLITIC IF
	(NOT 1 EOS)
;

## Балықшы бұл ақшаға үй тұрғызды.


 SELECT FINITE-VERB IF
      (1 EOS OR ("де"i))               # FIXME s/.*/SentenceBoundary/
       (NOT 0 ("шығар"i) OR ("бар"i))   # FIXME a better way?
       (NOT 0/1 COP)
;
## (!) 40 . Мүмкін бұл Азамат [0]шығар?
##  (!) 13 . Жоқ, Айгүлдің күшігі жоқ, оның мысығы [0]бар.
## Мағынасы төңкеріліп [0]кетті деп ойламағаным үшін ғафу етіңіз.


#
 SELECT IJ OR CNJADV IF
        (-1 BOS)
        (1 Cm)
;
## "Мысалы, ежелгі заманның өзінде Арал теңізі көп елдерге мәлім болған."


# INPUT: ^Мен/Мен<cnjcoo>/Мен<post>/Мен<prn><pers><p1><sg><nom>$
#        ^қазақпын/қазақ<n><nom>+е<cop><p1><sg>$
# OUTPUT: [0]^Мен/Мен<cnjcoo>/Мен<prn><pers><p1><sg><nom>$
#         ^қазақпын/қазақ<n><nom>+е<cop><p1><sg>$
 REMOVE POST IF
        (NOT -1 NOMINAL)
        (NOT -1 ("\*.+"r))
;
#

# An adjective ending with -LI is ambiguous with: 1) a noun or adjective in
# accusative, and 2) with  a verb in <ifi> form.
 REMOVE ACC IF
        (0 (N ACC) OR (ADJ SUBST NOM))
        (0 A)
        (NOT 1 V)
;
 REMOVE V IF
        (0 A)
        (0 IFI)
        (NOT 1 EOS OR Cm)
;
 SELECT A IF
        (-1 ("аса"i) OR ("өте"i))
        (0 A)
        (0 (N ACC))
;

# Proper noun or not?
 SELECT PROP IF
        (0 ("[:upper:]+[:lower:]*"r))
        (NOT -1 BOS)
        (NOT 0 ("Кеңес"))
        (NOT 0 ("Отан"))
        (NOT 0 ("сен"))
;
## 108. Өйткені бүгінгі [0]Арал деген атау сол XVII ғасырдан бергі жерге берілген.
 SELECT PROP IF
        (0 PROP)
        (0 ("<[:upper:][:upper:]+>"r))
;
## [0]АЗАМАТ ҚАЙДА?
 SELECT PROP IF
        (1 ("мен") + POST)
        (2 PROP)
;
   
## [0]Азамат пен Айгүл бақшада.
### : for "Алма мен Аян"-cnjcoo
SELECT CC IF (-1 N)(0 CC)(1 N);

### : for "қысқа ай"-adj
SELECT A IF (0 N + Dat)(1 N );

#
 SELECT PRN IF
        (0C DET OR PRN)
        (1C ADV)
;
## 44 . [0]Ол енді ол дыбысты анығырақ ести бастады.


# INPUT: ^терістік/тер<v><tv><coop><ifi><p1><pl>/терістік<n><attr>/
#        терістік<n><nom>$ ^бетіндегі/бет<n><px3sp><loc><attr>/
#        бет<n><px3sp><loc><subst><nom>/$
# OUTPUT: [0]^терістік/терістік<n><attr>/терістік<n><nom>$
#         ^бетіндегі/бет<n><px3sp><loc><attr>...$
 REMOVE V IF
        (0 PRE-N)
        (1C N)
        (NOT 0 GERUND)
        (NOT 0 VERBAL-ADJECTIVE)
;
#
# failed for a gerund 'өлшеу' in 'Радианның басқа да бұрышты өлшеу бірліктерімен арақатынасын мына формуламен сипатталады' => excluded gerunds and verbal adjectives in the rule


# INPUT: ^бақша/бақша<n><nom>$ ^арқылы/арқылы<post>/арқылы<adv>$
# OUTPUT: ^бақша<n><nom>$ [0]^арқылы<post>$
 REMOVE ADV IF
        (0 POST)
        (-1 N)
        (NOT 0 ("бірге"i)) # мысығымен бірге<post> үйде<n>
                           # бақшада бірге<adv> ойнайды<v>
                           # бақша арқылы<post> барамын<v>
        (NOT 0 ("бұрын"))  # Төстік бұрын ағалары бар екенін естіген жоқ екен .
        (NOT 1 A)
;
#kaz-eng


# INPUT: ^заңды/заң<n><acc>/заңды<adj>/заң<n>+лы<post>$
# INPUT: ^заңды/заң<n><acc>/заңды<adj>/заң<n>+лы<post>$
# REMOVE Sub:1 POST IF
# 	(0 N)
#	(0 A)
#;


 SELECT CNJCOO OR CNJADV IF
        (-1 BOS)
        (0 ("әлде"i))
 ;


 REMOVE IJ IF
        (0 A)
        (1 N)
;

# e.g., дұрыс! (<ij>) | дұрыс жерден (<adj>)


SELECT Adv + Attr IF (1 NOMINAL) ;


# Determiners
# ===========



# FIXME removes determiner even when there is a noun 1 to the right
# FIXED JNW 2017-08-12
REMOVE DET IF
       (0 DET OR PRN)  # ADDED JNW 2017-08-11
       (NOT 1 N OR SUBST)
		 (NEGATE 1 A LINK 1 N OR SUBST)
;
# 44 . Ол енді ол дыбысты анығырақ ести бастады.
# (!) 34 . Ол Азаматтың қайда екенін білсе де айтқысы келген жоқ.



# Nouns
# =====



#
 REMOVE ATTR IF
       (0 N OR PROP)
       (NOT 0 LOC)
       (NOT 1 N OR PROP)
;
## 176. Орталықта Түркістанның төл тарихына арналған музейлік [0]экспозиция жасақталған.
## (!) 184 . Үшінші қабатта «Тәуелсіз [0]Қазақстандағы жаңа Түркістан» атты экспозициялар қойылған.

# Select nominative reading of the first noun of the II izafet construct
# (as Tatar grammars call it)
 SELECT NOM IF
        (0C N OR PROP)
        (1 N OR PROP OR SUBST)
        (1 PX3SP)
;
# Was: SELECT 'Attr'
#
# this rule didn't work for 'қазба' in:
# Оған ежелгі қоныстарға жүргізілген археологиялық қазба жұмыстары кезінде табылған мәдениет мұралары дәлел бола алады.


## 111. Түркияның оңтүстік-шығысындағы [0]Газиантеп қаласында кеше болған жарылыстан қаза тапқандар саны 9-ға жетті.


# If a former gerund or verbal adjective was lexicalized as a noun, select noun
# SELECT N IF
#        (0 N)
#        (0 GERUND  OR GPR-POT)
#;
# the rule contradicts with some of the analyzes in puupankki, which is considered to be 'gold' standard
#
## professions, such as "оқушы"; -U gerunds , in theory other gerunds too


 SELECT DET IF
        (0 DET)
        (0 GPR_PPOT)
;
## жетерлік has both readings, but we almost always want to translate it as det


REMOVE PersonalPossessives IF
		(0 NOMINAL)
		(NOT 0 GEN)
		(1C PX3SP)
;
## only indefinite possessors can leave off the genitive suffix, and something possessed itself has to be definite,
## so: something possessed can only possess something else if it has gen case,
## so: px* with no gen cannot precede px3sp, so if that's one of the possible readings, you probably don't want it.
## 1929 ж. сырттай өлім жазасына кесілді.


 "<қысым>" REMOVE PersonalPossessives ;


# Adjectives
# ==========



# select adverbial reading of adjectives if any verbal form except gerund follows
# FIXME CHECK it might be a gerund as well
 SELECT ADVL OR ADV IF
         (1C V-NOT-GERUND)
         #(NOT 1C PARTICIPLE)
         (NOT 1 ("бол"i))
         (NOT 0 ("балама"i))
         (NOT 1 ("де"i))
;
##
# example: in 'балама үйретейін' the 'балама' shouldn't be treated as adj.advl (equivalent), it should be n.px1sq.dat (to my son)

REMOVE ADVL IF (1 NOMINAL) ;

# select adj+cop reading at the end of a sentence
 SELECT Sub:1 COP IF
        (0C A1-NOM)
        (1 EOS OR MARK)
;
 REMOVE SUBST IF # we want adj+cop reading here, not adj.subst+cop
        (0C A1-NOM)
        (1 EOS OR MARK)
;
## 19 . Ол еш нәрсені көріп тұрған [0]жоқ, ол санап жатыр.


#
REMOVE SUB:1 P3 IF
       (0/1 COP)
       (0/1 P1)
;
## "Біз [0]қуаныштымыз."



# Postpositions
# =============



#
"<қатар>" SELECT POST IF (-1 PRN + INS)
;
## 174 . Орталық сонымен [0]қатар оқушыларға тәрбие беруде де маңызды рөл атқарады.
## 148 . Ол АҚШ үкіметін WikiLeaks сайтын қудалауын тоқтатуға шақырып,
## өзін ресейлік Pussy Riot панк тобымен және New York Times басылымымен [0]қатар қойды.

SELECT POST IF (0 POST-DAT) (-1 DAT) ;

"<бері>" SELECT Post IF (-1 ABL) ;

# Verbs
# =====

REMOVE PARTICIPLE IF (NOT 1C V OR VAUX) ;

# <prc_cond> vs. <gna_cond>
 REMOVE PRC-COND IF
        (NOT 1 ("бол"i) + V)
;
## 34 . Ол Азаматтың қайда екенін [0]білсе де айтқысы келген жоқ.


#
 SELECT PARTICIPLE IF
        (0C PARTICIPLE OR VERBAL-ADVERB)
        (NOT 0C PRC-COND OR GNA-COND)
        (1 VAUX)
;
## 16 . Азамат ескі үлкен бір ағашқа қарай қатты [?]жүгіріп бара жатыр, ол сол
## ағаштың артына Айгүлден [0]жасырынып жатыр.

REMOVE VAUX IF (NOT -1 PARTICIPLE) ;

SELECT VAUX IF (-1C Participle) (NOT 1 Vaux) ;
## 187 . Біздің дәуірімізден 1 миллион жыл бұрын өмір сүрген "тік жүретін адамнан" [0]бастап, өз замандастарына дейінгі
## кезеңдерді қамтитын, ... орталықтың экспозициясы негізгі сегіз ірі бөлімнен тұрады.


# Imperative or not?
# ------------------


# <Vaux p3> is more common, so let's set it as default
# Rules for selecting imperative reading of these three (if any) must be placed
# before this rule
 SELECT (vaux p3) IF
        (-1 (prc_perf) OR (prc_plan) OR (prc_impf))
        (0 ("<жүр>") OR ("<тұр>") OR ("<отыр>"))
;
## 15. Олардың анасы мысығымен бірге үйде, ол терезеден Азамат пен Айгүлдің
## ойнағанына қарап [0]тұр.
## 36 . Әлі де болса Азаматты табуға әрекет етіп [0]жүр.


# 2p sg imperative form is a bare stem, so it is often ambigious with other
# parts of speech
 REMOVE (imp) IF
        (NOT 1 EOS OR MARK OR ("де") OR Rquot OR (guio))
;
##  96 . Ал оң жақ жағалауы батпақты, онда [0]қалың орман өскен.
## [0]Бар деп айттым мен оған.


# ----------


#
 SELECT (gpr_past) IF
        (0 (ger_past) + Nom)
        (1C N)
        (NOT 0 Det)
;

##  172 . Сот арқылы немесе өзгеде осындай құзыреті бар мемлекеттік органдардың
## шешімі арқылы тәркіленген мұражайлық маңызы бар жинақтардың немесе
## [0]жекелеген заттардың есебінен;


# Numerals
# ========


# ^Еуровидение/Еуровидение<np><al><nom>$ *^2010/2010<num>/2010<num><ord>$* ^ән/ән<n><nom>$ ^конкурсы/конкурс<n><px3sp><nom>$
SELECT ORD IF (-1 PROP) ; 

REMOVE ORD IF (1 QUANTITY) ;


###################   TO BE DONE   ####################


# Deciding about the number of a verb or copula in the 3 person
# Basic idea is to remove plural reading if subject is not in plural,
# but there a lot of corner cases, which lead to lots of mispredictions.
#
# In Tatar, when the subject indicates the number, this LAR affix for the verbs
# in 3 person is kind of optional too, so singular reading will work/is right for
# the majority of cases.

#REMOVE Pl IF (0 V OR Vaux OR Cop) (0 P3) ;

# Thought that the above rule should cover copulas, seems that sometimes it doesn't
  REMOVE SUB:1 Pl IF
        (0/1 Cop)
       (0/1 P3)
;


# Wordform/lemma-specific rules   # Adding new stuff here is discouraged.
# =============================   # Try to generalize, if possible, existing
                                  # rules.

#
 "<өте>" SELECT Adv IF (1 A) ;
## Бірақ кеше _өте_ суық еді!


# ugly and RISKY
 "<сол>" SELECT Det IF
        (NOT 1 ("қол"i) OR ("жақ"i))
;
## (!) Күйеуім - сол жақтың қазағы.


#
 "<аса>" SELECT Adv IF
         (1 A)
;
## "Әл-Истахри келтірген мәліметтер аса құнды саналады."


#
 "<ал>" REMOVE CC IF
        (NOT -1 BOS OR Cm )
;
## Бөлмеде шкаф бар, содан бiр кiтап ал.

#
 "<жылы>" SELECT N IF
        (-1 Num)
;

##
 "<теңге>" SELECT N IF
        (0 A)
;
#
 "<жылы>" SELECT N IF
        (-1 A)
;

#
 "<қарасты>" SELECT A IF
        (-1 (dat))
;
## 177. Орталыққа _қарасты_ Түркістан қаласында Н.Оңдасынов атындағы
## тарихи-мемориалдық мұражайы атты филиал жұмыс жасайды.


#
 "<қайта>" SELECT Adv IF
        (1C V)
;
## 41 . Ол дыбыс қайта естіледі!
## Ол қайта алмайды.


#
 "<енді>" SELECT Adv IF
        (1* Acc BARRIER EOS)
        (1* V BARRIER EOS)
;
##


#
 "<көптеген>" SELECT Det IF
        (1 N + Pl)
;
##


# ambigious with v.pass
 "<құрал>" SELECT N IF
        (-1 A OR (n attr))
;
##


# UGLY
# "сый" is certainly not <v.iv.imp> in the following example,
# I don't think that сый<imp> is ever used
 "<сый>" SELECT N
;
## 171. Жеке және заңды тұлғалардан _сый_ немесе мұра ретінде қабылданған заттар
## есебінен.


# imperative of "ісу" ("to swell") - just like "<сый>" above sounds odd,
# but let's try to deal with it in a less ugly way
 "<іс>" SELECT N IF
        (-1 A)
;
## 178. ...тәрбиелік мәні бар іс-шараларды бірге ұйымдастыру...


# FIXME: could be achieved with a more general rule I think
 "<көрме>" SELECT N IF
        (1 ("зал"i))
;
## 181. Ғимарат 3 қабаттан, 5 көрме залынан, конференция залынан,
## қолөнершілердің шеберханасы...


#
 SELECT N IF
        (0 ("же"i) + (gpr_fut))
        (0 ("жер"i) + N)
;
## 24 . Ол "Азамат қай [0]жерге кетті?  Оны көрдіңдер ме?" деп іздеп жатыр.



#
"<қарайды>" SELECT ("қара") IF
                   (-1 Dat)
;
## 30 . Айгүл терезеде тұрған анасына қарайды.


#
 SELECT CC IF
        (0 ("сондай-ақ"i))
        (-1 BOS OR Cm)
;
## 87 . Сондай-ақ, Арал теңізінің көлемі жайлы, Арал теңізі жайлы ұғымдар сонау
## ерте дүние әдебиеттерінде де кездеседі.


#
 SELECT N IF
        (0 ("құрама"i))
        (-1 PRE-N)
;
## 76 . Қазақстандық құрама «А» тобында соңғы орында қалып қойды.


#
 SELECT N IF
        (0 ("қор"))
        (-1 N)
;
## 169. Музей [0]қоры Қазақстан Республикасы Мәдениет және ақпарат министрінің...



#
 SELECT A IF
        (0 ("құрама"))
        (-1 (np top))
;
## 69. Франция құрамасының бұрынғы ойыншысы Мишель Платини...




#
 "<жинағы>" SELECT N IF
            (-1 N + Pl)
;
## 165. ...тарихи жәдігерлер жинағы арқылы көрсету.
## Қазақстан заңдарының жинағы


#
 "<Ертең>" SELECT Adv
;
 "<ертең>" SELECT Adv IF
           (1 V)
;
## Ертең келесің бе?
## Ертең Елбасы Нұрсұлтан Назарбаев Қазақстанның 2050 жылға дейінгі даму бағдарламасын жариялайды.
## Оны ертең еститін боласыздар.


#
 SELECT N IF
        (0 ("сыра"i))
        (1 ("іш") + V)
;
## Шелекпен сыра ішемін.


# да<postadv/conj>
 SELECT SUB:1 Postadv IF
        (0 (gna_cond))
        (NOT 0 (cnjadv))
;
## 34 . Ол Азаматтың қайда екенін [0]білсе+[0/1_or_-1]де айтқысы келген жоқ.  # with SUBREADINGS = LTR ; option
## Ол Азаматтың қайла екенін [0]білсе+[0/1]де+[0/2_or_-1]мі?                  # with SUBREADINGS = LTR ; option
## Сөйтсе де осы оқыған балалар — ана оқымаған қазақ балаларынан үздік , озық .


#
 SELECT Adv IF
        (0 ("бірге"))
        (NOT -1 Ins)
        (1 V)
;
## 6 . Азамат пен Айгүл ойнағанды жақсы көреді, олар әрдайым үлкен үйдің алдындағы бақшада [0]бірге ойнайды.
## (!) Ол Азаматпен бірге келді.


#
 "<жасар>" SELECT A IF
                  (-1 Num)
;
## 7 . Азамат алты жасар кішкентай бала.

 SELECT PRN IF
        (0 ("<ешкім>"i))
;
## 70. Оның рекордын әзірше ешкім бұза алған жоқ.


 "<атты>" SELECT A IF
                 (1 N)
;
 "<топырақты>" SELECT A IF
                      (1 N)
;
## 53 . Еуро-2012 туралы [0]қызықты статистикаға көз салыңыз.
## 91 . Өйткені, <...> Әл-Истахри келтірген мәліметтер аса [0]құнды саналады.
## 184 . Үшінші қабатта «Тәуелсіз Қазақстандағы жаңа Түркістан» [0]атты экспозициялар қойылған.

"<сәлем"i SELECT Interj IF (-1 BOS) (1 EOS) ;

###################   TEMPORARY HACKS TO GET THE STORY TRANSLATED   ####################
###################   SOME SHOULD BE REWRITTEN LATER                ####################


REMOVE N IF (0 ("ақырын"));
    ## Айгүл оны _ақырын_ тыңдайды.

SELECT A/Advl IF (0 ("ақырын"));
    ## Айгүл оны _ақырын_ тыңдайды.

REMOVE N IF (0 ("жылы")) (NOT -1 N OR Num) ;
    #! Ауа райы бүгін әбден жақсы, ^жылы/жыл<n><px3sp><nom>/жылы<adj>/жылы<adj><subst><nom>.

###############################################################################

REMOVE (pass) IF (0 ("қырыл"));
    ## жасушалардың қырылуы


 "<жаппай>" SELECT A IF
                 (1 N)
;
## Қытай мамандары ауыл тұрғындарының қалаға жаппай ағылуын тоқтататын жаңа
# саясат жүргізуге шақырып отыр.


#
"<бір>" SELECT Det IF (-1C A) (1 N);
## Әхмәт тиз генә иске зур _бер_ агачка йөгерә.
## Әхмәт акрын гына иске зур _бер_ агачка йөгерә.


# Must be placed before "SELECT Det IF (1* NPMARK BARRIER N OR MARK) ;"
"<бар>" REMOVE Det IF
               (1 ("енді"i))
               (2 Rquot) ;
## # Себебі Түрік Әуежолдары сынды бір серігіміз бар енді» деді.


#
"<бар>" SELECT V IF (-1 Dat) ;
"<бар>" SELECT A IF (-1 (px3sp)) ;
"<бар>" SELECT Det IF (1* NPMARK BARRIER N OR MARK) ;
"<бар>" REMOVE Det IF (1 S-BOUNDARY OR MARK) ;
## 172. Сот арқылы немесе өзгеде осындай құзыреті _бар_ мемлекеттік органдардың
## шешімі арқылы тәркіленген мұражайлық маңызы бар жинақтардың немесе жекелеген
## заттардың есебінен.
## Бар<det> җирдән карый, әмма Әхмәтне таба алмый.
## Акча бармы? Бар<adj>.
## Акча бармы? Бар<adj>, ләкин бик аз!


# басым recognized as noun бас, select ADJ form
 "<басым>" SELECT A IF
        (1 (px3sp))
;
## Олардың басым бөлігі – 2011 жылы жұмыс іздеп,


# тұрғыны recognized as noun тұрғы should select тұрғын before қаза
 "<тұрғыны>" SELECT ("тұрғын") IF
        (1 ("қаза"))
;
## 2011 жылы 16 желтоқсанда 16 тұрғыны қаза тапқан


# V-Iп FourAuxiliaries-GAн жоқ-COP
 SELECT (gpr_past) IF
        (-1 (prc_perf))
        (0 FourAuxiliaries)
        (1 ("жоқ"))
;
 "<жоқ>" SELECT SUB:1 Cop IF
        (-2 (prc_perf))
        (-1 FourAuxiliaries)
;
 "<жоқ>" REMOVE Subst IF
        (-2 (prc_perf))
        (-1 FourAuxiliaries)
;
## 19 . Ол еш нәрсені көріп тұрған жоқ, ол санап жатыр.

#
 "<керек>" SELECT A
;
## 45 . Бұл Азамат болу керек!


# Сен бардың ба?
REMOVE (gen) IF
  (0 (ifi p2))
  (-1* (prn nom))
;

# INPUT: ^Мен/Мен<cnjcoo>/Мен<post>/Мен<prn><pers><p1><sg><nom>$
#        ^қазақпын/қазақ<n><nom>+е<cop><p1><sg>$
# OUTPUT: [0]^Мен<prn><pers><p1><sg><nom>$
#         ^қазақ<n><nom>+е<cop><p1><sg>$
 SELECT PRN-Pers IF
        (0 ("<Мен>"))
        (-1 BOS)
;



#kaz-eng

SELECT PRN-Pers IF
        (0 ("<онда>"i))
        (1 N)
;


# INPUT: ^онымен/он<num><subst><px3sp><ins>/ол<prn><pers><p3><sg><ins>$
# OUTPUT: [0]^ол<prn><pers><p3><sg><ins>$
 SELECT PRN-Pers IF
        (0 ("<онымен>"i))
        (NOT -1 (PRN-Pers))
;
#kaz-eng


# INPUT: ^Ол/ол<det><dem>/ол<prn><dem><nom>/ол<prn><pers><p3><sg><nom>$
#        ^дәрігер/дәрігер<n><nom>/дәрігер<n><attr>/дәрігер<n><nom>+
#        е<cop><p3><pl>/дәрігер<n><nom>+е<cop><p3><sg>$^./.<sent>$
# OUTPUT: ^Ол/ол<det><dem>/ол<prn><dem><nom>/ол<prn><pers><p3><sg><nom>$
#         [0]^дәрігер/дәрігер<n><nom>+е<cop><p3><sg>$^./.<sent>$
# SELECT SUB:1 Sg IF
#        (-1 ("<Ол>"i) OR ("<Олар>"i))
#        (0/1 Cop)
#        (0/1 P3)
#;
#kaz-eng
# FIXME this rule is unnecessary because of the next rule


# INPUT: ^Ол/ол<det><dem>/ол<prn><dem><nom>/ол<prn><pers><p3><sg><nom>$
#        ^әдемі/әдемі<adj>/әдемі<adj><advl>/әдемі<adj><subst><nom>$
#        ^қыз/қыз<n><nom>/қыз<n><attr>/қыз<v><iv><imp><p2><sg>/
#        қыз<n><nom>+е<cop><p3><pl>/қыз<n><nom>+е<cop><p3><sg>$^./.<sent>$
# OUTPUT: ^Ол/ол<det><dem>/ол<prn><dem><nom>/ол<prn><pers><p3><sg><nom>$
#         ^әдемі/әдемі<adj>/әдемі<adj><advl>/әдемі<adj><subst><nom>$
#         [0]^қыз/қыз<n><nom>+е<cop><p3><sg>$^./.<sent>$
 
#SELECT SUB:1 Sg IF
#        (-1* ("<Ол>"i) OR ("<Олар>"i))
#        (0/1 Cop)
#        (0/1 P3)
#;

#kaz-eng
# FIXME doesn't work yet



 SELECT N IF
        (0 ("<үстінде>"i)OR ("<астында>" i)OR ("<артында>" i))
        (-1 N)
;
SELECT SUB:1 Pl IF
        (-1 ("<Біз>"i))
        (0/1 Cop)
        (0/1 P1)
;
#Біз бақыттымыз


# this rule doesn't work for 'бақшада' in
# Азамат пен Айгүл бақшада .
#
# REMOVE SUB:1 Pl IF
#		 (NOT -1* ("<Ол>"i) OR ("<Олар>"i))
#;


SELECT  Cop IF
        (-1 ("<керек>"i))
        (0 Cop)
       
;
REMOVE Adv IF
		 (0 ("<емес>"i)) 
;
       

#"<мен>" SELECT Post IF
# 	(-1 NOMINAL)
#	(1 NOMINAL)
#;

#for choosing "астында" as n.px3sp.loc

#SELECT  N IF
#        (-1 N)
#        (0 (n.*.loc) OR Adv)
#       
#;


# two verbs қайт
SELECT V-IV IF
	(0 ("қайт"))
	(-1* Dat BARRIER N-LIKE)
;
## Сол жерге қайтып барасың ба?

SELECT V-TV IF
	(0 ("қайт"))
	(-1* Acc BARRIER N-LIKE)
;
## Соны қайтып қыласың?

SELECT V-IV IF
		(0 ("істе"))
		(NOT -1 NOMINAL)
;


# INPUT:  ^іш<v><tv><prc_vol><p3><pl>/іш<v><tv><prc_vol><p3><sg>/ішкі<adj><subst><px3sp><nom>$
#         кел<vaux><ifi><p3><pl>/кел<vaux><ifi><p3><sg>/кел<v><iv><ifi><p3><pl>/кел<v><iv><ifi><p3><sg>$
# OUTPUT: ^іш<v><tv><prc_vol><p3><pl>/іш<v><tv><prc_vol><p3><sg>$
#         ккел<vaux><ifi><p3><sg>/кел<v><iv><ifi><p3><pl>/кел<v><iv><ifi><p3><sg>$
SELECT Prc-vol IF (1 ("кел")) ;


        
;


# INPUT:  ^Орал/орал<n><nom>/орал<n><attr>/Орал<np><top><nom>/Орал<np><top><attr>/ора<v><tv><pass><imp><p2><sg>/Орал<np><ant><f><nom>/Орал<np><ant><m><nom>/орал<n><nom>+е<cop><aor><p3><pl>/орал<n><nom>+е<cop><aor><p3><sg>/Орал<np><top><nom>+е<cop><aor><p3><pl>/Орал<np><top><nom>+е<cop><aor><p3><sg>/Орал<np><ant><f><nom>+е<cop><aor><p3><pl>/Орал<np><ant><f><nom>+е<cop><aor><p3><sg>/Орал<np><ant><m><nom>+е<cop><aor><p3><pl>/Орал<np><ant><m><nom>+е<cop><aor><p3><sg>$ 
#^тауы/тауы<n><nom>/тауы<n><attr>/тау<n><px3sp><nom>/тауы<n><nom>+е<cop><aor><p3><pl>/тауы<n><nom>+е<cop><aor><p3><sg>/тау<n><px3sp><nom>+е<cop><aor><p3><pl>/тау<n><px3sp><nom>+е<cop><aor><p3><sg>$^

# OUTPUT:  ^Орал/Орал<np><top><attr>
#тау<n><px3sp><nom>
SELECT (np top attr) IF (1 ("тауы")) ;
        
;

###############################################################################
# FMT:

#Орыс мәдениетінің қалыптасу ерекшеліктері негізінен төмендегі факторларға тығыз байланысты болды.

REMOVE Acc IF 
      (1C V-IV) ;

REMOVE VerbalAdjective IF 
       (1 Cm) ;

"<қатар>" SELECT Post IF
                 (-1C N + Ins)
;

REMOVE Num + Subst IF 
      (0C Num) 
      (1C Num LINK 1 N) ;

"<барды>" SELECT V IF
                   (-1 Dat) ;


# this rule didn't work for 'қазба' in:
# Оған ежелгі қоныстарға жүргізілген археологиялық қазба жұмыстары кезінде табылған мәдениет мұралары дәлел бола алады.
#REMOVE N + Attr IF
#      (1 PersonalPossessives)
#      (NOT -1* Gen OR Nom BARRIER (*) - N) ;

REMOVE A + Subst + $$NGDAALI IF 
      (0 N + $$NGDAALI)
      (NOT 0 ("бар"))
;
# could be 'bar' as a drinking place VS 'бар' as 'there is/are'

REMOVE Num + Subst IF 
      (-1C Num) 
      (1C N) ;

REMOVE VerbalAdjective + Subst IF
       (-1C Gen)
       (1C N) ;

REMOVE A + Subst IF
      (0C A - Subst OR A + Subst )
      (NOT 0 ("бар"))
      (NOT 0 ("жоқ"))
;
# example: Кірді , шықты , ілді , қайтты , түбегейлеп қуған бала да жоқ .
# 'жоқ' should be adj.subst here

REMOVE Num + Subst IF (0C Num - Subst OR Num + Subst) ;

REMOVE Num + Subst IF (1C Year) ;

REMOVE Participle IF (NOT 1* Vaux) ;

#morph disambiquashion example еңбектен-(1-verb, 2-noun)
SELECT V IF (0 PRN) (1 N + Abl);

SELECT PRN IF (0 ("не"i)) (1 ("үшін"i)) ;

#Адамдар as people
SELECT N IF (0 ("<адамдар>"i)) ;  

# 100 адам 
SELECT N IF (-1 Num) (0 ("адам")) ;  


#су ішетін
SELECT N IF (0 ("<су>"i))(1 V) ;      

#бұл су
SELECT N IF (-1 Dem) (0 ("<су>"i)) ;    
     
#менің үйім
SELECT (n px1sg) IF (-1 (prn pers p1 sg gen)) ; 

#құрылғымен қауіпсіз.
#INPUT: <қауіпсіз>"	"қауіп" n nom"е" cop aor p2 frm sg
#	"қауіпсіз" adj  "е" cop aor p3 sg
#OUTPUT: "қауіпсіз" adj  "е" cop aor p3 sg

SELECT A IF (-1 N) (0 ("<қауіпсіз>"i)) ;  

SELECT Imp IF (1* Excl) ; 

REMOVE Imp IF (NOT 1* Excl) (0C Imp OR Pres) ; 

REMOVE Coop IF (0 IV - Coop) ;

REMOVE N IF (-1 Dat) (0 Dat) (0C N OR V) ;

SELECT Vaux IF (-1 Participle) ;

SELECT Adv IF (0 Qayda) ;

# қала тұрғындарының санын көбейтті.
SELECT N IF
	(0 ("қала"))
	(1 ("тұрғын"))
;

# бойынша is almost always a postposition, not n.px3sp.equ
SELECT Post IF
	(0 ("бойынша"))
	(-1 NOMINAL)
;

# Толқын ұзындығы, керісінше, энергияға кері пропорционал болады.
# Pretty much always goning to be the adverb reading
# - especially good for translation
SELECT Adv IF
	(0 ("керісінше"))
;

# The ablative reading of жаңадан doesn't really make sense?
REMOVE Abl IF
	(0 ("жаңадан"))
;

SELECT Imp IF (-1 BOS) ;

"<не>"i SELECT (cnjcoo) IF (1* Cm LINK 1* ("<не>"i)) ; 

"<не>"i SELECT (cnjcoo) IF (-1* Cm LINK -1* ("<не>"i)) ; 

"<не>"i REMOVE (cnjcoo) ;

REMOVE (prc_perf) ;

REMOVE (loc subst) IF (NOT -1 BOS) ;

SELECT (det) IF (1 (loc attr)) ;

REMOVE Sub:1 Post IF (0/1 ("лы"i)) (NOT 1 NOMINAL) ;

SELECT (ord) IF (1 MONTH) ;

REMOVE (advl) IF (1 (n)) ;

"<қарайды>"i SELECT ("қара") IF (-1* (dat)) ;

"<бұл>"i SELECT (det) IF (1 (n)) ;
        
REMOVE Sub:1 ("лы" post) IF (0 (adj)) ;

REMOVE (tv) IF (NOT -1* Acc) ;

"<есе>"i SELECT N IF (-1 Num) ;

"<жылда>"i SELECT N IF (-1 Num) ;


#######
SECTION ## selecting the most frequent analysis
#######


"<бұлармен>"i REMOVE V ;

"<біздің>"i SELECT PRN ;

"<үшін>"i SELECT Post ;

"<өсті>"i REMOVE ("өсті") ;


#######
SECTION  ## mapping rules
#######


MAP @"@"advcl (gna_perf) - ("бол") ;
MAP @"@"advcl (adj) IF (1 ("бол") + VerbalAdverb) ;

MAP @"@"amod (num ord) | (loc attr) | (adj) - (advl) | Adv + Attr ;

MAP @"@"cop ("бол" v iv) ;

MAP @"@"punct Sent OR Cm OR Guio ;

MAP @"@"root (n px3sp nom) IF (1 FiniteVerb) (2 (sent)) ;
MAP @"@"root (imp) IF (-1 BOS) (1 Cm) ;
MAP @"@"root FiniteVerb IF (NOT * @"@"root) ;

MAP @"@"nmod:poss (nom) - (px3sp) IF (1* (px3sp)) ;
MAP @"@"nmod:poss (gen) ;

MAP @"@"nsubj (n nom) IF (NOT -1* (@"@"nsubj nom)) ;
MAP @"@"nsubj (nom) IF (NOT * (nom)) ;

MAP @"@"csubj (ger nom) ;

MAP @"@"obl Dat | Loc | Ins;
MAP @"@"obl Nom IF (1 Post) ;
	
MAP @"@"obj ("<не>") IF (1* (tv)) ;
MAP @"@"obj (acc) ;
MAP @"@"obj (nom) IF (-1* (nom)) ;
	
MAP @"@"aux (vaux) ;

MAP @"@"parataxis (v) IF (@"@"1 @"@"root) ;

MAP @"@"det (det) ;

MAP @"@"case Post ;

MAP @"@"nummod (num) - (ord) ;


#######
SECTION  ## disambig rules which rely on mapping tags
#######


SELECT Pl IF (0 V OR Vaux OR Cop) (0 P3) (NOT <*-1 (@"@"nsubj) OR (@"@"csubj)) ;
SELECT Pl IF (0 V OR Vaux OR Cop) (0 P3) (<*-1 (pl @"@"nsubj)) ;
SELECT Sg IF (0 V OR Vaux OR Cop) (0 P3) (<*-1 (@"@"nsubj) OR (@"@"csubj) - (pl));


#######
SECTION  ## attachement
#######


SETPARENT @"@"root TO (@"@"0 (*)) ;

SETPARENT (*) (NEGATE p (*)) TO (0* @"@"root) ;

SETPARENT @"@"nmod:poss TO (1* PersonalPossessives) ;
SETPARENT (@"@"nmod:poss gen) TO (1* (n)) ;

SETPARENT @"@"amod + ("[0-9]+"r) TO (2 (n px3sp)) ;
SETPARENT @"@"amod + (loc attr) TO (1* (n)) ;
SETPARENT (@"@"amod adj) TO (1* (n)) ;
SETPARENT (@"@"amod adv attr) TO (1* (n)) ;


SETPARENT @"@"nsubj TO (1* ("соғыс")) ;

SETPARENT @"@"obl TO (1* ("соғыс")) ;

SETPARENT @"@"obj TO (1* (v)) ;

SETPARENT @"@"aux TO (-1 Participle) ;

SETPARENT @"@"advcl TO (1* @"@"root OR @"@"parataxis) ;

SETPARENT @"@"det TO (1* (n)) ;

SETPARENT @"@"case TO (-1 (*)) ;

SETPARENT @"@"cop TO (-1 (*)) ;

SETPARENT @"@"nummod TO (1* NOMINAL) ;

SETCHILD (gna_perf) TO (-1 @"@"obl) ;

SETCHILD (ger) TO (-1* @"@"obl) ;

@;{

@define[lemma]{<book>}

@define[foo]{
@list{REMOVE N IF (0 @lemma)}}

@foo

}