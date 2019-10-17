#lang scribble/text

@(require "lexicon.rkt")

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   M O R P H O L O G I C A L · T R A N S D U C E R · F O R · K A Z A K H   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! See http://wiki.apertium.org/wiki/Turkic_lexicon

!=================!
 Multichar_Symbols
!=================!

! Followed http://wiki.apertium.org/wiki/Turkic_languages

! Part of speech categories = First-level tags
%<n%>         ! Noun                       ! Зат есім
%<np%>        ! Proper noun                ! Жалқы есім
%<adj%>       ! Adjective                  ! Сын есім
%<num%>       ! Numeral                    ! Сан есім
%<prn%>       ! Pronoun                    ! Есімдік
%<det%>       ! Determiner                 ! Детерминатив
%<v%>         ! Verb                       ! Етістік
%<vaux%>      ! Auxilary verb              ! Көмекші етістік
%<adv%>       ! Adverb                     ! Үстеу
%<post%>      ! Postposition               ! Септеулік шылау
%<postadv%>   ! Postadverb                 ! "Постүстеу" (*1)
%<cnjcoo%>    ! Co-ordinating conjunction  ! Cалаластырғыш жалғаулық
%<cnjsub%>    ! Sub-ordinating conjunction ! Cабақтастырғыш жалғаулық
%<cnjadv%>    ! Adverbial conjunction      ! "Үстеу-жалғаулық"
%<ij%>        ! Interjection               ! Одағай
%<abbr%>      ! Abbreviation               ! Қысқарған сөз
%<cop%>       ! Copula                     ! Копула
%<ideo%>      ! Ideophone                  ! Еліктеу сөз
%<paren%>     ! Parentheses                ! Қыстырма сөз

! *1. Анықтаған сөздерінің артынан келетін үстеулер ("демеуліктер") !# ғана, -ақ

! Proper noun types
%<top%>       ! Toponym      ! Топоним
%<ant%>       ! Anthroponym  ! Кісі есімі
%<cog%>       ! Cognomen     ! Фамилия
%<pat%>       ! Patronym     ! Әке текті есім
%<org%>       ! Organisation ! Ұйым есімі
%<al%>        ! Other        ! Басқалар

!! gender of anthroponyms and cognoms
%<m%>         ! Masculine
%<f%>         ! Feminine
%<mf%>        ! Masculine/feminine !# basically cognoms without -ов/-ова,
                                   ! -ин/-ина endings

! "Syntactic" tags. Attributive use of non-adjectives etc.
%<attr%>      ! Attributive
%<subst%>     ! Substantive
%<advl%>      ! Adverbial

! Number
%<sg%>        ! Singular ! Жекеше
%<pl%>        ! Plural

! Possessives
%<px1sg%>     ! First person singular
%<px2sg%>     ! Second person singular
%<px3sp%>     ! Third person singular/plural
%<px1pl%>     ! First person plural
%<px2pl%>     ! Second person plural
%<px3pl%>     ! Third person plural (for reflexive)
!%<px%>        ! General possessive          !# -нікі  ! now gen.subst

! Cases
%<nom%>       ! Nominative
%<gen%>       ! Genitive
%<dat%>       ! Dative
%<acc%>       ! Accusative
%<abl%>       ! Ablative
%<loc%>       ! Locative
%<ins%>       ! Instrumental

!! some additional ~cases
%<sim%>       ! Similative
              !# DAй
%<abe%>       ! Abessive=Privative ! Лишительный
              !# SIZ (not used after posessives and cases)
%<reas%>      ! not used rigth now, just in case for
              !# LIKTAN
%<equ%>       ! ш{A} 

! Levels of comparison of adjectives
%<comp%>      ! Comparative

! TODO
!! Some adjecives have a "weakening" form, e.g. яшел>яшькелт, сары>саргылт; but
!! also ак>аксыл, кара>карасу. Sometimes they take the comparative RAK affix,
!! e.g. кызгылтрак.

! TODO
!! There is also what can be called a synthetic superlative form (as opposed
!! to the /ең<adv>+X<adj>/ construction) -- кап-кара, тип-тигез, төп-төгәл etc.
!! Probably easiest way would be just to lexicalize them, but make sure that
!! they don't take the comparative affix.

! Pronoun types
%<pers%>      ! Personal
%<recip%>     ! Reciprocal

!! Pronoun&Determiner types
%<dem%>       ! Demonstrative
%<ind%>       ! Indefinite
%<itg%>       ! Interrogative
%<qnt%>       ! Quantifier
%<neg%>       ! Negative       !# ешкім
              ! (NOTE: also used to denote negation in verbs, i.e for м{A})
%<ref%>       ! Reflexive

! Numeral types
%<ord%>       ! Ordinal
%<coll%>      ! Collective
%<dist%>      ! Distributive

! Verbal features

!! Mood
%<imp%>       ! Imperative
%<opt%>       ! Optative/jussive
%<evid%>      ! Evidential, a.k.a. "indirect" / non-eyewitness / hearsay

!! Derivation
%<caus%>      ! Causative
%<pass%>      ! Passive
%<coop%>      ! Cooperative

!! Tense / finite forms
%<pres%>      ! for "жатыр", "тұр", "отыр" and "жүр" ""
%<aor%>       ! -{E}
%<past%>      ! -{G}{A}н
%<ifi%>       ! -{D}{I}
%<ifi_evid%>  ! -{I}пт{I}
%<fut%>       ! -{I}р
%<fut_plan%>  ! -{M}{A}{K}
%<pih%>       ! -{E}тін       ! ~past imperfect ~"used to"

!! Non-Finite verb forms

!!! Participles
%<prc_perf%>  ! Perfect participle
              ! -{I}п
              !# "Бірақ мысығы үйде, _ұйықтап_ жатыр.";
%<prc_impf%>  ! Imperfect participle
              ! -{E}
              !# "...олар далада _ойнай_ алмады...";
%<prc_vol%>   ! Volition participle
              ! -{G}{I}
              !# барғым келмейді;
%<prc_cond%>  ! Conditional participle
              ! -с{A}
              !# ...жесең болады...;
%<prc_fplan%> ! Future plan participle
              ! -{M}{A}{K},
              ! -{M}{A}{K}ш{I} Dir/LR
              !# "Шал қазға бармақшы болады.";
%<prc_plan%>  ! Plan participle
              ! -{G}{A}л{I}
              !# мен сөйлескелі келдім;
!%<prc_irre%> ! Irrealis participle
              ! -{E}{T}{I}н
              ! (This form is analyzed as <gpr_impf> and <ger_impf>)
              ! (FIXME this might be wrong)

!!! Verbal adverbs ! Көсемшелер !Глагольные наречия
%<gna_perf%>  ! -{I}п
              !# "...ул вакытта инде кояш _баеп_, йолдызлар күренә башлаган
              !# иде..." (Ф.Хөсни);
%<gna_impf%>  ! -{A}	      
%<gna_cond%>  ! -с{A}
              !# ...қайда екенін _білсе_, маған бұл туралы айтыр еді...;
%<gna_until%> ! -{G}{A}нш{A}
              !# "Мен кеткенше ол жауап бермейді.";
%<gna_after%> ! -{G}{A}л{I}
              ! (NOTE: ambiguous with prc_plan)
              !# "Сабақ _біткелі_ екі сағат өтті."; Ол кеткелі біз жұмыс
              !# істемейдік;

!!! Verbal adjectives ! Есімшелер ! Глагольные причастия
%<gpr_past%>  ! -{G}{A}н               ! past verbal adjective
              !# келген адам; оқылмаған кітап;
%<gpr_impf%>  ! -{E}{T}{I}н            ! imperfect verbal adjective
              !# басқаратын = руководящий
%<gpr_pot%>   ! -{U}ш{I}               ! potential verbal adjective
              !# "...өзінің қадір-қасиетін арттыруға _тырысушы_ саясаткер...";
%<gpr_ppot%>  ! -{A}рл{I}{K}
              !# сүйсінерлік ерлік;
%<gpr_fut%>   ! -{I}р                  ! future verbal adjective
              !# ""Барар жерің Балкан тау, ол да біздің көрген тау";

!!! Gerunds (verbal nouns)
%<ger%>       ! -{U}
%<ger_past%>  ! -{G}{A}н
%<ger_perf%>  ! -{G}{A}нл{I}{K}   (stresses the fact that something happened)
%<ger_ppot%>  ! -{A}рл{I}{K}      (~the ability to do the denoted action)
              !# "_Сүйсінерлігі_ сол, өз сеніміне берік те адал халқымыз дінін
              !# сатпады, өзгенің тәтті де сылдыр сөзіне ермеді...";
%<ger_abs%>   ! -уш{I}л{I}{K}          FIXME CHECK
%<ger_fut%>   ! -{A}р
              !# "Мен айтарымды айттым";
%<ger_impf%>  ! -{E}{T}{I}н
              !# "Ол бүгін кешкілік болатынын айтты";
%<abs%>       ! -LIK as e.g. seen in the -{E}{T}{I}нд{I}{K} form
%<ger_obs%>   ! -{М}{А}{К}         ("obsolete": used only in archaic registers)
              !# "Адам өлмек үшін туған"
%<ger2%>      ! - {I}c
              ! "Ұйқыдан тұрысымен оқушылар жуынып, тамақтануға асханаға барады"

!! Transitivity
%<tv%>        ! Transitive
%<iv%>        ! Intransitive 

! Person
%<p1%>        ! First person
%<p2%>        ! Second person
%<p3%>        ! Third person
%<frm%>       ! Formality

! Modal particles
%<qst%>       ! Modal question particle
              !# м{A}
%<emph%>      ! Emphasizing modal particle
              !# -шы/-ші
%<mod_ass%>   ! Assertive modal particle
              !# ғой/қой
%<mod_emo%>   ! Emotative modal particles
              !# -ай, -ау
%<mod%>       ! Other modal words (шығар, сияқты etc)

%<unk%>       ! For foreign tokens

%<err_orth%>  ! For orthographic errors

! Punctuation
%<percent%>   ! Percent
%<sent%>      ! Sentence marker
%<guio%>      ! Hyphen
%<cm%>        ! Comma
%<apos%>      ! Apostrophe
%<rquot%>     ! Quote marker (right hand side)
%<lquot%>     ! Quote marker (left hand side)
%<rpar%>      ! Parenthetical marker (right hand side)
%<lpar%>      ! Parenthetical marker (left hand side)
%<ltr%>       ! Letter

! Archiphonemes and escaped symbols

%{L%}         ! Archiphoneme 'l': realised as л, д, т
%{N%}         ! Archiphoneme 'n': realised as н, д, т
%{M%}         ! Archiphoneme 'm': realised as м, б, п
%{G%}         ! Archiphoneme 'g': realised as к, қ, г, ғ
%{G%}         ! Archiphoneme 'k': realised as к, қ  >>>FIXME ???<<<
%{D%}         ! Archiphoneme 'd': realised as д, т
%{A%}         ! Archiphoneme 'a': realsied as е, а
%{I%}         ! Archiphoneme 'i': realised as і, ы
%{S%}         ! Realised as 'с' or ''; only used in 3rd person morphology
%{K%}         ! Realised as 'к' or 'қ'
%{n%}         ! Realised as 'н' or '';
              ! only used in 3rd person morphology and -NIKI
%{l%}         ! Realised as 'л' or 'н'; only used in passive -{I}{l}
%{y%}         ! Realised as '' or '{I}'; only used in epenthesis for nouns
%{o%}         ! Realised as ''; triggers dialectal interpretation of Iп
%{E%}         ! Realised as 'а', 'е', 'й' (={A}/й)
%{д%}         ! Realised as д before vowels, 0 otherwise
%{т%}         ! Realised as т before vowels, 0 otherwise


%{а%}         ! Archiphoneme for back vowel numerals/abbreviation
%{э%}         ! Archiphoneme for front vowel numerals/abbreviation
%{ә%}         ! Surfaces as ә, but:
              ! triggers front harmony for {I} and back harmony for {A}

%{й%}         !
%{л%}         !
%{н%}         !
%{з%}         !
%{т%}         !
%{с%}         !

%{☭%}         ! Used to tell the phonology about a Russian word
%{і%}         !

%{❗%}         ! Obstacles on the road!!!
%{ъ%}         ! Force back harmony
%{ь%}         ! Force front harmony

%>            ! Morpheme boundary
%,            !
%.            !
%             ! Space
%-            !
%%            !

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                       M O R P H O T A C T I C S                         !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!============!
 LEXICON Root
!============!

Common ;
Hardcoded ;
Abbreviations ;
Punctuation ;
Guesser ;
Digits ; ! Use/Circ
Proper ;
Copula ;

!==============================================================================!
!
!                               N U M E R A L S
!
!    WARNING: This section should not be modified without consultation.
!             Test cases are in dev/numerals.
!==============================================================================!

LEXICON CASE-NONOM

! This continuation lexicon is a copy of the normal case lexicon,
! only it does not include nominative
!   1<num><nom> = 1, 1<num><acc> = 1-{N}{I}

%<gen%>:%>%{N%}%{I%}ң CLITICS-NO-COP ;
%<dat%>:%>%{G%}%{A%} CLITICS-NO-COP ;
%<acc%>:%>%{N%}%{I%} CLITICS-NO-COP ;
%<abl%>:%>%{D%}%{A%}н # ;
%<loc%>:%>%{D%}%{A%} # ;
%<ins%>:%>%{M%}ен CLITICS-NO-COP ;
%<ins%>:%>%{M%}енен CLITICS-NO-COP ; ! Dir/LR  ! It should be analysed like this, but generated as мен/бен/пен

LEXICON CASE-ETC-NONOM

CASE-NONOM ;
GENERAL-POSSESSIVE-ETC ;

LEXICON FULL-NOMINAL-INFLECTION-NONOM

POSSESSIVES ;
CASE-ETC-NONOM ;

LEXICON SUBST-NONOM

FULL-NOMINAL-INFLECTION-NONOM ;
%<pl%>:%>%{L%}%{A%}р FULL-NOMINAL-INFLECTION-NONOM ;

LEXICON SQRD
! This lexicon includes common ways of writing squared, cubed etc.

²%<num%>:2 # ;  ! Dir/LR
³%<num%>:3 # ;  ! Dir/LR
²%<num%>:² # ;
³%<num%>:³ # ;
%<num%>: # ;

LEXICON NUM-DIGIT

%<num%>: # ;
%<num%>%<subst%>%<nom%>: # ;
%<num%>%<subst%>:%- SUBST-NONOM ;
%<num%>%<subst%>: SUBST-NONOM ; ! Dir/LR

%<num%>%<ord%>:%-%{I%}нш%{I%} # ;
%<num%>%<ord%>%<subst%>%<nom%>:%-%{I%}нш%{I%} # ;
%<num%>%<ord%>%<subst%>:%-%{I%}нш%{I%} SUBST-NONOM ;
%<num%>%<ord%>:%-ш%{I%} # ; ! Dir/LR
%<num%>%<ord%>%<subst%>%<nom%>:%-ш%{I%} # ; ! Dir/LR
%<num%>%<ord%>%<subst%>:%-ш%{I%} SUBST-NONOM ; ! Dir/LR

%<num%>%<coll%>%<advl%>:%-%{A%}у # ;
%<num%>%<coll%>%<subst%>%<nom%>:%-%{A%}у # ;
%<num%>%<coll%>%<subst%>:%-%{A%}у SUBST-NONOM ;

%<num%>%<percent%>%<nom%>:%% # ;

%<num%>%<percent%>:%%%{а%}%{з%}%- SUBST-NONOM ;
%<num%>%<percent%>:%%%{а%}%{з%} SUBST-NONOM ; ! Dir/LR
%<num%>%<percent%>:%%%{э%}%{с%}%- SUBST-NONOM ; ! Dir/LR
%<num%>%<percent%>:%%%{э%}%{с%} SUBST-NONOM ; ! Dir/LR

°С%<num%>:°С # ;
°С%<num%>%<subst%>%<nom%>:°С # ;
°С%<num%>%<subst%>:°С%{а%}%{с%}%- SUBST-NONOM ;
°С%<num%>%<subst%>:°С%{э%}%{с%}%- SUBST-NONOM ; ! Dir/LR
°С%<num%>:ºС # ; ! Dir/LR
°С%<num%>%<subst%>%<nom%>:ºС # ; ! Dir/LR
°С%<num%>%<subst%>:ºС%{а%}%{с%}%- SUBST-NONOM ; ! Dir/LR
°С%<num%>%<subst%>:ºС%{э%}%{с%}%- SUBST-NONOM ; ! Dir/LR
°С%<num%>:С # ; ! Dir/LR
°С%<num%>%<subst%>%<nom%>:С # ; ! Dir/LR
°С%<num%>%<subst%>:С%{а%}%{с%}%- SUBST-NONOM ; ! Dir/LR
°С%<num%>%<subst%>:С%{э%}%{с%}%- SUBST-NONOM ; ! Dir/LR

С%<num%>:° # ; ! Dir/LR
С%<num%>%<subst%>%<nom%>:° # ; ! Dir/LR
С%<num%>%<subst%>:°%{а%}%{с%}%- SUBST-NONOM ; ! Dir/LR
С%<num%>%<subst%>:°%{э%}%{с%}%- SUBST-NONOM ; ! Dir/LR

! Measurement units

км:км SQRD ;
мм:мм SQRD ;
см:см SQRD ;
м:м SQRD ;

LEXICON LASTDIGIT

1:1%{э%}%{й%}           NUM-DIGIT ; ! "бір"
2:2%{э%}                NUM-DIGIT ; ! "екі"
3:3%{э%}%{с%}           NUM-DIGIT ; ! "үш"
4:4%{э%}%{с%}           NUM-DIGIT ; ! "төрт"
5:5%{э%}%{с%}           NUM-DIGIT ; ! "бес"
6:6%{а%}                NUM-DIGIT ; ! "алты"
7:7%{э%}                NUM-DIGIT ; ! "жеті"
8:8%{э%}%{з%}           NUM-DIGIT ; ! "сегіз"
9:9%{а%}%{з%}           NUM-DIGIT ; ! "тоғыз"

LEXICON LASTDIGIT-REST

1%0:1%0%{а%}%{н%}       NUM-DIGIT ; ! "он"
2%0:2%0%{а%}            NUM-DIGIT ; ! "қырық"
3%0:3%0%{а%}%{з%}       NUM-DIGIT ; ! "отыз"
4%0:4%0%{а%}%{с%}       NUM-DIGIT ; ! "қырық"
5%0:5%0%{э%}%{й%}       NUM-DIGIT ; ! "елу"
6%0:6%0%{а%}%{с%}       NUM-DIGIT ; ! "алпыс"
7%0:7%0%{э%}%{с%}       NUM-DIGIT ; ! "жетпіс"
8%0:8%0%{э%}%{н%}       NUM-DIGIT ; ! "сексен"
9%0:9%0%{а%}%{н%}       NUM-DIGIT ; ! "тоқсан"

LEXICON POWERS
%0%0:%0%0%{э%}%{з%}     NUM-DIGIT ; ! "жүз"
%0%0%0:%0%0%0%{а%}%{н%} NUM-DIGIT ; ! "мың"

LEXICON LOOP

%,:%, DIGITLEX ;
%.:%. DIGITLEX ;
%,:%, LASTDIGIT ;
%.:%. LASTDIGIT ;
%,:%, LASTDIGIT-REST ;
%.:%. LASTDIGIT-REST ;
%,:%, POWERS ;
%.:%. POWERS ;
      DIGITLEX ;
      LASTDIGIT ;
      LASTDIGIT-REST ;
      POWERS ;

LEXICON DIGITLEX

%0:%0 LOOP ;
1:1   LOOP ;
2:2   LOOP ;
3:3   LOOP ;
4:4   LOOP ;
5:5   LOOP ;
6:6   LOOP ;
7:7   LOOP ;
8:8   LOOP ;
9:9   LOOP ;

!==============================================================================!
!          N U M E R A L     L E X I C O N     E N D S     H E R E
!==============================================================================!

!==============================================================================!
! CLITICS
!==============================================================================!

! First, let's list all the clitics we have

LEXICON CLIT-EMPH

%+шы%<emph%>:%>ш%{I%} # ;
# ;

LEXICON CLIT-QST-MA

%+ма%<qst%>:%>% %{M%}%{A%} # ;

LEXICON CLIT-QST

CLIT-QST-MA ;

LEXICON CLIT-MODEMO

%+ау%<mod_emo%>:%-ау # ;
%+ай%<mod_emo%>:%-ай # ;

LEXICON CLIT-MODASS

%+ғой%<mod_ass%>:% %{G%}ой # ;
%+гөр%<mod_ass%>:% гөр # ;
# ;

LEXICON CLIT-GHANA-ETC

%+%-ақ%<postadv%>:%-ақ # ; ! ""
%+ғана%<postadv%>:% %{G%}ана # ; ! ""
%+да%<cnjcoo%>:% %{D%}%{A%} # ; ! ""
%+да%<postadv%>:% %{D%}%{A%} # ; ! ""
# ;

LEXICON CLIT-COP

%+е%<cop%>%<aor%>: V-PERS-S1 ;
%+ма%<qst%>+е%<cop%>%<aor%>%<evid%>:% %{M%}екен V-PERS-S1 ;

! Now, let's group them depending on what they can attach to

LEXICON CLITICS-NO-COP

CLIT-QST ;
CLIT-MODASS ;
CLIT-MODEMO ;
CLIT-EMPH ;
CLIT-GHANA-ETC ;

LEXICON CLITICS-NO-COP-NO-QST

CLIT-MODASS ;
CLIT-MODEMO ;
CLIT-GHANA-ETC ;

LEXICON CLITICS-INCL-COP         ! Can appear only after nominals
                                 ! (and only after some cases).
CLITICS-NO-COP ;
CLIT-COP ;

!==============================================================================!
!                         NOMINAL INFLECTION
! This section contains everything needed to describe inflection of nominals.
! It starts with the LEXICON FULL-NOMINAL-INFLECTION
!==============================================================================!

!-------------------------------------------------------------------------------
! The following combinations are possible (examples in Tatar):
!
! stem(plural) > case                                     # әти<nom>, әтине<acc>
!----
! stem(plural) > possessives > case                       !# әтиемне<px1sg><acc>
! stem(plural) > possessives > ныкы/дагы/дай(*1)
                                               !# әтиемнеке,китабындагы,әтиемдәй
! stem(plural) > possessives > ныкы > case
! stem(plural) > possessives > дагы(plural(*2)) > case
                                   !# Монда _эчеңдәгене_ тышка чыгарырга ярамый.
! stem(plural) > possessives > дагы(plural) > possessives > case
                     !# _Эчендәгесе_(*3) – тышында, диләр андый кешеләр турында.
!----
! stem(plural) > ныкы/дагы/дай                         !# әтинеке/әтидәге/әтидәй
! stem(plural) > ныкы/дагы > case  !#Бу соңгы _еллардагының_ өчтән икесе чамасы.
!-----
! stem(plural) > сыз
! stem > сыз(plural) > case
! stem > сыз(plural) > possessives > case
!----
! *1. "ныкы" form in some Tatar grammars is called "predicative genitive".
!     Indeed, when not substantivized, it is used as a predicate:
!     "Бу яулык әнинеке." (vs "Әнинең яулыгы.)
!
!     Currently we don't mark it with a <subst> tag, but probably we should,
!     since we let it continue with case tags.
!
!     An alternative to a [predicative general possessive <px>] vs
!     [substantivized general possessive <px>subst>] opposition is to view it as
!     an always substantivized thing and hence mark it with a <subst> tag
!     (see e.g. http://wiki.apertium.org/wiki/Turkic_languages/Ki)
!     At the end of sentences it will have attached copulas, which will indicate
!     predicative use anyway.
! *2. TODO: Does double plural in a word make sence? Ерак илләрдәгеләр looks ok,
!     and occurs in the RFERL copus (though very rarely). Plural affix both
!     before and after the the general possessive does not look ok.
! *3. This doesn't work atm (it's recursive), but it doesn't seem to be very
!     frequent also.
!     Most likely it is restricted to px3sp.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! A note on abessive SIZ:
! It doesn't appear after possessives.
! We don't let it take copula suffixes here. A counterexample could be e.g.
! "жұмыс-сыз-бын", but that's only because "жұмыссыз" has lexicalized as an
! adjective (try to grep "с[ыі]з[мбп][ыі]н" on a large corpora -- that's just a
! few words which are rather  adjectives.
!-------------------------------------------------------------------------------

LEXICON CASES-COMMON

%<gen%>:%>%{N%}%{I%}ң CLITICS-NO-COP ;
%<dat%>:%>%{G%}%{A%} CLITICS-NO-COP ;

%<acc%>:%>%{N%}%{I%} CLITICS-NO-COP ;
%<ins%>:%>%{M%}ен CLITICS-NO-COP ;
%<ins%>:%>%{M%}енен CLITICS-NO-COP ; ! Dir/LR

LEXICON CASES

CASES-COMMON ;
%<nom%>: CLITICS-INCL-COP ;
%<abl%>:%>%{D%}%{A%}н CLITICS-INCL-COP ;
%<loc%>:%>%{D%}%{A%} CLITICS-INCL-COP ;

!! LEXICON CASES-WITHOUT-COPULAS
!! 
!! CASES-COMMON ;
!! %<nom%>: CLITICS-NO-COP ;
!! %<abl%>:%>%{D%}%{A%}н CLITICS-NO-COP ;
!! %<loc%>:%>%{D%}%{A%} CLITICS-NO-COP ;

LEXICON ATTR-SUBST

%<attr%>: # ;
%<subst%>: CASES ;
%<subst%>: POSSESSIVES ;
%<subst%>%<pl%>:%>%{L%}%{A%}р CASES ;
%<subst%>%<pl%>:%>%{L%}%{A%}р POSSESSIVES ;

%<attr%>: GENERAL-POSSESSIVE-ETC ;

LEXICON GENERAL-POSSESSIVE-ETC

!------------------------------------------------------------------------------!
! <n.pl.нікі.pl> sequence should not be allowed. This form is rare (1 (!)
! match in RFERL corpus for 'нікілар'), probably not worh bothering at all.
! It could be handled in a way abessive is handled, but since NIKI in contrast
! to the abessive SIZ can appear after possessves, that would require
! duplicating possessives as well.
!------------------------------------------------------------------------------!
! try нікілер?  -JNW

!gen.subst part was:
!%<px%>:%>%{N%}ікі%{n%} CASES ;
!%<px%>%<pl%>:%>%{N%}ікі%>л%{A%}р CASES ;

%<gen%>%<subst%>:%>%{N%}ікі%{n%} CASES ;
%<gen%>%<subst%>%<pl%>:%>%{N%}ікі%>л%{A%}р CASES ;
%<loc%>:%>%{D%}%{A%}%{G%}%{I%} ATTR-SUBST ;
%<sim%>:%>%{D%}%{A%}й CLITICS-INCL-COP ;
%<sim%>:%>%{D%}%{A%}й POSSESSIVES ;
%<sim%>:%>%{D%}%{A%}й CASES ;   ! FIXME: should these be sim.subst?
%<sim%>:%>%{D%}%{A%}й PLURAL ;
%<equ%>:%>ш%{A%} CLITICS-INCL-COP ;

LEXICON CASES-ETC

CASES ;
GENERAL-POSSESSIVE-ETC ;

LEXICON POSSESSIVES

%<px1sg%>:%>%{I%}м CASES-ETC ;
%<px2sg%>:%>%{I%}ң CASES-ETC ;
%<px3sp%>:%>%{S%}%{I%}%{n%} CASES-ETC ;
%<px1pl%>:%>%{I%}м%{I%}з CASES-ETC ;
%<px2pl%>:%>%{I%}ңд%{A%}р CASES-ETC ;

%<px2sg%>%<frm%>:%>%{I%}ң%{I%}з CASES-ETC ;
%<px2pl%>%<frm%>:%>%{I%}ң%{I%}зд%{A%}р CASES-ETC ;

LEXICON ABESSIVE-POSTPOSITION

%+сыз%<post%>:%>с%{I%}з CLITICS-NO-COP ;

LEXICON LI-POSTPOSITION

%+лы%<post%>:%>%{L%}%{I%} CLITICS-NO-COP ;

LEXICON PLURAL

%<pl%>:%>%{L%}%{A%}р CASES-ETC ;
%<pl%>:%>%{L%}%{A%}р POSSESSIVES ;
%<pl%>:%>%{L%}%{A%}р ABESSIVE-POSTPOSITION ;

LEXICON FULL-NOMINAL-INFLECTION

PLURAL ;
CASES-ETC ;
POSSESSIVES ;
ABESSIVE-POSTPOSITION ;
LI-POSTPOSITION ;

!!!!!!!!!!!!!!!!!!!!!!     ADJECTIVE'S (ADJECTIVIAL) INFLECTION (ADJ-COMMON)

! FIXME: this section might need some refactoring
!! i.e. change <adj><comp><subst> to <adj><subst><comp>
!! and make use of ADJ-LEVELS class

!! LEXICON COMPARATIVE
!! 
!! %<comp%>:%>%{I%}р%{A%}%{K%} CLITICS-NO-COP ;
!! CLITICS-NO-COP ;

LEXICON A1                ! adjectives that can be both substantivised and andverbialised;
                          ! all three readings (<adj>, <adj.subst> and <adj.advl>) have comparison levels.
                          !# жақсы, тез

%<adj%>: CLITICS-INCL-COP ;                             !# жақсы адам
%<adj%>%<comp%>:%>%{I%}р%{A%}%{K%} CLITICS-INCL-COP ;   !# жақсырақ іс
%<adj%>%<comp%>:%>%{L%}%{A%}у CLITICS-INCL-COP ;        !# жақсырақ іс Dir/LR

%<adj%>%<subst%>: FULL-NOMINAL-INFLECTION ;                             !# жақсыны таптым
%<adj%>%<comp%>%<subst%>:%>%{I%}р%{A%}%{K%} FULL-NOMINAL-INFLECTION ;   !# жақсырақты таптым
%<adj%>%<comp%>%<subst%>:%>%{L%}%{A%}у FULL-NOMINAL-INFLECTION ;        !# жақсырақты таптым Dir/LR
%<adj%>%<comp%>%<advl%>:%>%{L%}%{A%}у CLITICS-NO-COP ;                  !# "...ион көп болса соғұрлым тоқ жақсырақ өтеді." Dir/LR

%<adj%>%<advl%>: CLITICS-NO-COP ;                               !# жақсы білемін
%<adj%>%<comp%>%<advl%>:%>%{I%}р%{A%}%{K%} CLITICS-NO-COP ;     !# "...ион көп болса соғұрлым тоқ жақсырақ өтеді."

LEXICON A2                 ! (derived/not fully lexicalised) adjectives without adverbial reading;
                           ! <adj> and <adj.subst> readings have comparison levels.
                           !# ескі

%<adj%>: CLITICS-INCL-COP ;                             !# ескі дос
%<adj%>%<comp%>:%>%{I%}р%{A%}%{K%} CLITICS-INCL-COP ;   !# ескірек заманда
%<adj%>%<comp%>:%>%{L%}%{A%}у CLITICS-INCL-COP ;        !# ескірек заманда Dir/LR

%<adj%>%<subst%>: FULL-NOMINAL-INFLECTION ;                             !# "Ол ескіні ертеден есінде сақтаған, жаңаның жалынды жаршысы болған ақын."
%<adj%>%<comp%>%<subst%>:%>%{I%}р%{A%}%{K%} FULL-NOMINAL-INFLECTION ;   !# "Ихтимал, бу бина башкасы, искерәге урынында утырадыр."
%<adj%>%<comp%>%<subst%>:%>%{L%}%{A%}у FULL-NOMINAL-INFLECTION ;        !# "Ихтимал, бу бина башкасы, искерәге урынында утырадыр." Dir/LR

LEXICON A3                 ! (derived/not fully lexicalised) adjectives without adverbial reading,
                           ! so-called "predicatives" (бар, жоқ);
                           ! no comparison levels at all.
                           !# көктемгі, басты, бар, жоқ

%<adj%>: CLITICS-INCL-COP ;         !# көктемгі су тасқыны
%<adj%>%<subst%>: FULL-NOMINAL-INFLECTION ;           !# "...Самаранч ең бастыны түсінді..."
                                    !# "Аты бардың заты бар."

! FIXME:NOTE:loc.attr/subst form of some of them seems to be pure overgeneration:
! e.g. *язгыдагы

LEXICON A4                 ! "pure" adjectives - no adverbial and substantive readings, no comparison levels;
                           !# ұлттық

%<adj%>: CLITICS-INCL-COP ;   !# ұлттық тағам

LEXICON A6                 ! (derived/not fully lexicalised) adjectives without comparative reading,
                           ! no comparison levels at all.
                           !# examples go here

%<adj%>: CLITICS-INCL-COP ;                           !# example 1
%<adj%>%<subst%>: FULL-NOMINAL-INFLECTION ;           !# example 2
%<adj%>%<advl%>: CLITICS-NO-COP ;                     !# example 3

LEXICON V-ADJ

# ;
%<subst%>: GER-INFL ; ! Don't be confused by the name of the cont.class,
                      ! it's basically just a simplified FULL-NOMINAL-INFLECTION

!!!!!!!!!!!!!!!!!!!!!!     VERBAL INFLECTION (V-INFL-COMMON)

LEXICON V-PERS-S1-NO3PERSON

! Pointed to by: V-PERS-S1 and V-PERS-AOR
! Irregular p1.sg form is in V-PERS-AOR

%<p1%>%<sg%>:%>%{M%}%{I%}н CLITICS-NO-COP ;
%<p2%>%<sg%>:%>с%{I%}ң CLITICS-NO-COP ;
%<p1%>%<pl%>:%>%{M%}%{I%}з CLITICS-NO-COP ;
%<p2%>%<pl%>:%>с%{I%}ңд%{A%}р CLITICS-NO-COP ;

%<p2%>%<frm%>%<sg%>:%>с%{I%}з CLITICS-NO-COP ;
%<p2%>%<frm%>%<pl%>:%>с%{I%}зд%{A%}р CLITICS-NO-COP ;

LEXICON V-PERS-S1

! Used for: -AR-<fut>, -GAn-<past>, -MAK-<fut_plan>, copula

V-PERS-S1-NO3PERSON ;
%<p3%>%<sg%>: CLITICS-NO-COP ;
%<p3%>%<pl%>: CLITICS-NO-COP ;

LEXICON V-PERS-S2

! Used for: -DI-<ifi>, -sA-<prc_cond>, -sA-<gna_cond>

%<p1%>%<sg%>:%>м CLITICS-NO-COP ;
%<p2%>%<sg%>:%>ң CLITICS-NO-COP ;
%<p3%>%<sg%>: CLITICS-NO-COP ;
%<p1%>%<pl%>:%>%{K%} CLITICS-NO-COP ;
%<p2%>%<pl%>:%>ңд%{A%}р CLITICS-NO-COP ;
%<p3%>%<pl%>: CLITICS-NO-COP ;

%<p2%>%<frm%>%<sg%>:%>ң%{I%}з CLITICS-NO-COP ;
%<p2%>%<frm%>%<pl%>:%>ң%{I%}зд%{A%}р CLITICS-NO-COP ;

LEXICON V-PERS-AOR

! As the name indicates, used for -{E}-<aor>
! NOTE that: Алма театрға бара<aor><p3><sg> ма? – Алма в театр идёт?

V-PERS-S1-NO3PERSON ;

%<p1%>%<sg%>:%>м CLITICS-NO-COP ;                ! Dir/LR

%<p3%>%<sg%>:%>д%{I%} CLITICS-NO-COP-NO-QST ;
%<p3%>%<sg%>:%>д%{I%} CLIT-QST ;              ! Dir/LR
%<p3%>%<sg%>: CLIT-QST ;

%<p3%>%<pl%>:%>д%{I%} CLITICS-NO-COP-NO-QST ;
%<p3%>%<pl%>:%>д%{I%} CLIT-QST ;              ! Dir/LR
%<p3%>%<pl%>: CLIT-QST ;

LEXICON V-PERS-IFI_EVID

V-PERS-S1-NO3PERSON ;
%<p3%>%<sg%>:%>т%{I%} CLITICS-NO-COP ;
%<p3%>%<pl%>:%>т%{I%} CLITICS-NO-COP ;

LEXICON VOL-ENDINGS

%<p1%>%<sg%>:%>м CLIT-GHANA-ETC ;
%<p2%>%<sg%>:%>ң CLIT-GHANA-ETC ;
%<p3%>%<sg%>:%>с%{I%} CLIT-GHANA-ETC ;
%<p1%>%<pl%>:%>м%{I%}з CLIT-GHANA-ETC ;
%<p2%>%<pl%>:%>л%{A%}р%>%{I%}ң CLIT-GHANA-ETC ;
%<p3%>%<pl%>:%>с%{I%} CLIT-GHANA-ETC ;
%<p3%>%<pl%>:%>л%{A%}р%>%{I%} CLIT-GHANA-ETC ;             ! Dir/LR

%<p2%>%<frm%>%<sg%>:%>ң%{I%}з CLIT-GHANA-ETC ;
%<p2%>%<frm%>%<pl%>:%>л%{A%}р%>%{I%}ң%{I%}з CLIT-GHANA-ETC ;

!!!!!!!   G E R U N D S   I N F L E C T I O N

! This is a somewhat simplified version of FULL-NOMINAL-INFLECTION.
! The idea was to avoid overgeneration (I don't think that ger+DAGI would take
! case endings e.g. Same for ger+NIKI). Still, this cont.class might be not
! suitable for all of the gerunds. /IS/

LEXICON GER-CASE-NO_COPULA

CASES-COMMON ;
%<nom%>: CLITICS-NO-COP ;
%<abl%>:%>%{D%}%{A%}н CLITICS-NO-COP ;
%<loc%>:%>%{D%}%{A%} CLITICS-NO-COP ;

LEXICON GER-ATTR/SUBST

%<attr%>: CLITICS-NO-COP ;
!%<subst%>: CASES ;
!%<subst%>%<pl%>:%>%{L%}%{A%}р CASES ;

!! LEXICON GER-ABE-ATTR/ADVL/SUBST
!! 
!! !%<attr%>: CLITICS-NO-COP ;
!! %<advl%>: CLITICS-NO-COP ;
!! !%<subst%>: CASE-2 ;
!! !%<subst%>%<pl%>:%>%{L%}%{A%}р CASE-2 ;

LEXICON GER-ABE-ETC          ! Stuff which doesn't appear after possessives

%+сыз%<post%>:%>с%{I%}з CLITICS-NO-COP ; ! 2014-08-16//FMT: this was "GER-ABE-ATTR/ADVL/SUBST"

LEXICON GER-GENERAL-POSSESSIVE-ETC      ! Stuff which can appear after possessives too

%<gen%>%<subst%>%<nom%>:%>%{N%}ікі%{n%} CLITICS-NO-COP ; ! 2015-01-18//JNW: added <nom>, since it needs a case for transfer, and no other cases are ever added, or even presumed by the transducer to be okay.  But worth CHECK ing..
 

%<loc%>:%>%{D%}%{A%}%{G%}%{I%} GER-ATTR/SUBST ;
%<sim%>:%>%{D%}%{A%}й CLITICS-NO-COP ;

LEXICON GER-CASE-ETC

GER-CASE-NO_COPULA ;
GER-GENERAL-POSSESSIVE-ETC ;

LEXICON GER-POSSESSIVES

%<px1sg%>:%>%{I%}м GER-CASE-ETC ;
%<px2sg%>:%>%{I%}ң GER-CASE-ETC ;
%<px3sp%>:%>%{S%}%{I%}%{n%} GER-CASE-ETC ;
%<px1pl%>:%>%{I%}м%{I%}з GER-CASE-ETC ;
%<px2pl%>:%>%{I%}ңд%{A%}р GER-CASE-ETC ;

%<px2sg%>%<frm%>:%>%{I%}ң%{I%}з GER-CASE-ETC ;
%<px2pl%>%<frm%>:%>%{I%}ң%{I%}зд%{A%}р GER-CASE-ETC ;

LEXICON GER-INFL

GER-POSSESSIVES ;
GER-CASE-ETC ;
%<pl%>:%>%{L%}%{A%}р GER-POSSESSIVES ;
%<pl%>:%>%{L%}%{A%}р GER-CASE-ETC ;
GER-ABE-ETC ;

!!!!!!!   end of gerunds inflection lexicon

LEXICON V-FINITE-IRREGULAR_NEGATIVE

%<fut%>:%>%{A%}р V-PERS-S1 ;
%<neg%>%<fut%>:%>%{M%}%{A%}с V-PERS-S1 ;

%<fut_plan%>:%>%{M%}%{A%}%{K%} V-PERS-S1 ;
%<fut_plan%>:%>%{M%}%{A%}%{K%}%>ш%{I%} V-PERS-S1 ; ! Dir/LR "...Менің қошақанымды қайда алып кетпекшісің?... (Экзюперидан)"
%<neg%>%<fut_plan%>:%>%{M%}%{A%}%{K%}% емес V-PERS-S1 ;

%<past%>:%>%{G%}%{A%}н V-PERS-S1 ;
%<neg%>%<past%>:%>%{G%}%{A%}н% емес V-PERS-S1 ;  ! more colloquial than regular negative

%<ifi%>:%>%{D%}%{I%} V-PERS-S2 ;
%<neg%>%<ifi%>:%>%{G%}%{A%}н% жоқ V-PERS-S1 ;    ! more colloquial than regular negative


LEXICON V-FINITE-REGULAR_NEGATIVE

%<aor%>:%>%{E%} V-PERS-AOR ;
%<past%>:%>%{G%}%{A%}н V-PERS-S1 ;      ! Dir/LR - see irregular forms above
%<ifi%>:%>%{D%}%{I%} V-PERS-S2 ;        ! Dir/LR - see irregular forms above
%<pih%>:%>%{E%}т%{I%}н V-PERS-S1 ;

%<aor%>%<evid%>:%>%{E%}д%{I%}% екен V-PERS-S1 ;    ! барады екенмін / бармайды екенмін
%<past%>%<evid%>:%>%{G%}%{A%}н% екен V-PERS-S1 ;   ! барған екенмін / бармаған екенмін
%<ifi%>%<evid%>:%>%{I%}п V-PERS-IFI_EVID ;         ! барыппын / бармаппын
%<ifi%>%<evid%>:%>%{o%}%{I%}п V-PERS-IFI_EVID ;    ! бопты   ! Dir/LR
%<neg%>%<ifi%>%<evid%>:%>%{G%}%{A%}н% жоқ% екен V-PERS-S1 ; ! барған жоқ екенмін

LEXICON V-NONFINITE-IRREGULAR_NEGATIVE

! Participles
%<prc_perf%>:%>%{I%}п CLIT-GHANA-ETC ;
%<prc_perf%>:%>%{o%}%{I%}п CLIT-GHANA-ETC ;            ! Dir/LR
       ! for things like боп, сап, кеп, қап, қып, etc.
%<neg%>%<prc_perf%>:%>%{M%}%{A%}%>%{E%} CLIT-GHANA-ETC ;
%<prc_vol%>:%>%{G%}%{I%} VOL-ENDINGS ;
%<prc_fplan%>:%>%{M%}%{A%}%{K%} CLITICS-NO-COP ;
%<prc_fplan%>:%>%{M%}%{A%}%{K%}ш%{I%} CLITICS-NO-COP ;           ! Dir/LR
%<prc_plan%>:%>%{G%}%{A%}л%{I%} CLIT-GHANA-ETC ;

! Verbal adverbs
%<gna_perf%>:%>%{I%}п CLITICS-NO-COP ;
%<neg%>%<gna_perf%>:%>%{M%}%{A%}%>%{E%} CLITICS-NO-COP ;
%<neg%>%<gna_perf%>:%>%{M%}%{A%}%>ст%{A%}н CLITICS-NO-COP ; ! Dir/LR
%<neg%>%<gna_perf%>:%>%{M%}%{A%}%>й%{I%}нш%{A%} CLITICS-NO-COP ;       ! Dir/LR
%<gna_until%>:%>%{G%}%{A%}нш%{A%} CLITICS-NO-COP ;

! Verbal Adjectives
%<gpr_fut%>:%>%{A%}р V-ADJ ;
%<neg%>%<gpr_fut%>:%>%{M%}%{A%}%>%с V-ADJ ;

%<gpr_ppot%>:%>%{A%}р%>л%{I%}%{K%} GER-INFL ;
%<neg%>%<gpr_ppot%>:%>%{M%}%{A%}%>ст%{I%}%{K%} GER-INFL ;

!%<neg%>%<gpr_pot2%>:%>%{G%}%{I%}с # ; FIXME looks wrong

! Gerunds
%<ger_fut%>:%>%{A%}р GER-POSSESSIVES ;  ! FIXME: check that e.g. no possession + accusative isn't valid (I think it is) -JNW
%<neg%>%<ger_fut%>:%>%{M%}%{A%}%>с GER-POSSESSIVES ;

%<ger_ppot%>:%>%{A%}р%>л%{I%}%{K%} GER-POSSESSIVES ;
%<neg%>%<ger_ppot%>:%>%{M%}%{A%}%>ст%{I%}%{K%} GER-POSSESSIVES ;

%<ger_obs%>:%>%{M%}%{A%}%{K%} GER-INFL ;
! FIXME <ger_obs>:
! - does it have a negative form?  if regular, with -МА,
!   put in the REGULAR_NEGATIVE category
! - does it take possessive forms?  if not, change
!   continuation lexicon to something other than GER-INFL


LEXICON V-NONFINITE-REGULAR_NEGATIVE

! Participles
%<prc_impf%>:%>%{E%} CLIT-GHANA-ETC ;
%<prc_cond%>:%>с%{A%} V-PERS-S2 ;

! Verbal adverbs
%<gna_cond%>:%>с%{A%} V-PERS-S2 ;
%<gna_impf%>:%>%{E%} CLIT-GHANA-ETC ;
%<gna_after%>:%>%{G%}%{A%}л%{I%} CLITICS-NO-COP ;

! Verbal adjectives
%<gpr_past%>:%>%{G%}%{A%}н V-ADJ ;
%<gpr_impf%>:%>%{E%}т%{I%}н V-ADJ ;
%<gpr_pot%>:%>у%>ш%{I%} V-ADJ ;

! Gerunds
%<ger%>:%>у GER-INFL ;
%<ger_past%>:%>%{G%}%{A%}н GER-INFL ;
%<ger_perf%>:%>%{G%}%{A%}н%>%{L%}%{I%}%{K%} GER-INFL ;
%<ger_abs%>:%>у%>ш%{I%}%>%{L%}%{I%}%{K%} GER-INFL ;
%<ger_impf%>:%>%{E%}т%{I%}н GER-POSSESSIVES ;
%<ger_impf%>%<abs%>:%>%{E%}т%{I%}н%>д%{I%}%{K%} GER-POSSESSIVES ;
%<ger2%>:%>%{I%}c GER-INFL ; ! NEED AN ADDITIONAL CHECK ! "Ол үйге барысымен телефонның тұтқасына жармасты"


! Imperatives
%<imp%>%<p2%>%<sg%>: CLIT-EMPH ;
%<imp%>%<p2%>%<pl%>:%>%{I%}ң%>д%{A%}р CLIT-EMPH ;
%<imp%>%<p2%>%<frm%>%<sg%>:%>%{I%}ң%{I%}з CLIT-EMPH ;
%<imp%>%<p2%>%<frm%>%<sg%>:%>%{I%}ң CLIT-EMPH ;                ! Dir/LR FIXME CHECK
%<imp%>%<p2%>%<frm%>%<pl%>:%>%{I%}ң%{I%}з%>д%{A%}р CLIT-EMPH ;

! Optative/jussive forms
%<opt%>%<p1%>%<sg%>:%>%{A%}й%{I%}н CLITICS-NO-COP ;
%<opt%>%<p1%>%<sg%>:%>%{E%}%{I}%н CLITICS-NO-COP ; ! FOR CASES LIKE "тоқиын/оқиын"
!%<opt%>%<p2%>%<sg%>:%>%{G%}%{I%}н CLITICS-NO-COP ;              ! FIXME check
%<opt%>%<p3%>%<sg%>:%>с%{I%}н CLITICS-NO-COP ;
%<opt%>%<p1%>%<pl%>:%>%{A%}й%{I%}%{K%} CLITICS-NO-COP ;
!%<opt%>%<p2%>%<pl%>:%>%{G%}%{I%}л%{A%} CLITICS-NO-COP ;         ! FIXME check
%<opt%>%<p3%>%<pl%>:%>с%{I%}н CLITICS-NO-COP ;


!%<opt%>%<p2%>%<frm%>%<sg%>:%>%{G%}%{I%}л%{A%} CLITICS-NO-COP ;  ! FIXME check
!%<opt%>%<p2%>%<frm%>%<pl%>:%>%{G%}%{I%}л%{A%} CLITICS-NO-COP ;  ! FIXME check

LEXICON V-COMMON

V-FINITE-REGULAR_NEGATIVE ;
%<neg%>:%>%{M%}%{A%} V-FINITE-REGULAR_NEGATIVE ;
%<neg%>%<gpr_past%>:%>%{M%}%{A%}%>%{I%}п CLITICS-NO-COP ; !Ұзақ жол жүріп ұйықтамап едім... !Dir/LR
                                                          !TODO this is a hack, doesn't feel right

V-FINITE-IRREGULAR_NEGATIVE ;

V-NONFINITE-REGULAR_NEGATIVE ;
%<neg%>:%>%{M%}%{A%} V-NONFINITE-REGULAR_NEGATIVE ;

V-NONFINITE-IRREGULAR_NEGATIVE ;

LEXICON V-DER

%<coop%>:%>%{I%}с V-COMMON ;

LEXICON V-TV-NOPASS

%<v%>%<tv%>: V-COMMON ;
%<v%>%<tv%>: V-DER ;

LEXICON V-TV-PASS

! e.g. көрін
%<v%>%<tv%>%<pass%>: V-COMMON ;

LEXICON V-TV

%<v%>%<tv%>: V-COMMON ;
%<v%>%<tv%>: V-DER ;
%<v%>%<tv%>%<pass%>:%>%{I%}%{l%} V-COMMON ;

LEXICON V-TV-REFL
V-TV ;

LEXICON V-TV-CAUS
V-TV ;

LEXICON V-IV

%<v%>%<iv%>: V-COMMON ;
%<v%>%<iv%>: V-DER ;

LEXICON V-IV-COOP
V-IV ;

LEXICON V-IV-REFL
V-IV ;


!!!!!!!!!!!!!!!!!!!!!!     auxiliary verbs

LEXICON Vinfl-AUX-IMPF

%<vaux%>%<pres%>: V-PERS-S1 ; ! /жүр, жатыр, тұр, отыр/
%<vaux%>%<pres%>%<evid%>:% екен V-PERS-S1 ; ! e.g., "жатыр екенмін"

LEXICON Vinfl-AUX-IMPF-NEG

%<vaux%>%<neg%>%<pres%>: V-PERS-S1 ; ! /жатқан жоқ/
%<vaux%>%<neg%>%<pres%>%<evid%>:% екен V-PERS-S1 ; ! e.g., "жатқан жоқ екенмін"

LEXICON Vinfl-AUX

%<vaux%>: V-COMMON ; ! these four verbs in other tenses and all other aux. verbs

!!!!!!!!!!!!!!!!!!!!!!     copula

LEXICON Copula

е%<cop%>%<ger_past%>:екен GER-POSSESSIVES ;
е%<cop%>%<ger_perf%>:екен%>%{L%}%{I%}к GER-POSSESSIVES ;

! This is an evidential aorist form, not past -JNW
е%<cop%>%<aor%>%<evid%>:екен V-PERS-S1 ;
е%<cop%>%<neg%>%<aor%>%<evid%>:емес% екен V-PERS-S1 ;

е%<cop%>%<ifi%>:е%>%{D%}%{I%} V-PERS-S2 ;

е%<cop%>%<neg%>%<aor%>:емес V-PERS-S1 ;

!!!!!!!!!!!!!!!!!!!!!!     PRONOUN'S INFLECTION

!!!!!!!   P E R S O N A L

LEXICON PRON-P12SG-CASES

%<nom%>:ен CLITICS-NO-COP ;
%<gen%>:енің CLITICS-NO-COP ;
%<dat%>:аған CLITICS-NO-COP ;
%<acc%>:ені CLITICS-NO-COP ;
%<abl%>:енен CLITICS-NO-COP ;
%<loc%>:енде CLITICS-NO-COP ;
%<ins%>:енімен CLITICS-NO-COP ;
%<ins%>:еніменен CLITICS-NO-COP ; ! Dir/LR
%<gen%>%<subst%>:енікі%{n%} CASES ;
%<loc%>:ендегі ATTR-SUBST ;
%<sim%>:ендей CLITICS-NO-COP ;
%<advl%>:еніңше CLITICS-NO-COP ;

LEXICON PRON-P3SG-CASES

%<nom%>:л CLITICS-NO-COP ;
%<nom%>: CLITICS-NO-COP ; ! Dir/LR
%<gen%>:ның CLITICS-NO-COP ;
%<dat%>:ған CLITICS-NO-COP ;
%<acc%>:ны CLITICS-NO-COP ;
%<abl%>:дан CLITICS-NO-COP ;
%<loc%>:нда CLITICS-NO-COP ;
%<ins%>:нымен CLITICS-NO-COP ;
%<ins%>:ныменен CLITICS-NO-COP ; ! Dir/LR
%<gen%>%<subst%>:нікі%{n%} CASES ;
%<loc%>:ндағы ATTR-SUBST ;
%<sim%>:ндай CLITICS-NO-COP ;
%<advl%>:ныңша CLITICS-NO-COP ;


LEXICON PRON-PERS-INFL

CASES-ETC ;

LEXICON PRON-PERS

мен%<prn%>%<pers%>%<p1%>%<sg%>:м PRON-P12SG-CASES ;

сен%<prn%>%<pers%>%<p2%>%<sg%>:с PRON-P12SG-CASES ;

ол%<prn%>%<pers%>%<p3%>%<sg%>:о PRON-P3SG-CASES ;
! the following severely overgenerates.  Please don't uncomment it. -JNW 2018-06-28
!ол%<prn%>%<pers%>%<p3%>%<sg%>:а PRON-P3SG-CASES ;
ол%<prn%>%<pers%>%<p3%>%<sg%>%<nom%>%+үшін:аның% үшін POST ; ! Dir/LR

біз%<prn%>%<pers%>%<p1%>%<pl%>:біз PRON-PERS-INFL ;
біз%<prn%>%<pers%>%<p1%>%<pl%>:біз%>дер PRON-PERS-INFL ; ! Dir/LR
біз%<prn%>%<pers%>%<p1%>%<pl%>%<advl%>:біздіңше CLITICS-NO-COP ;

сендер%<prn%>%<pers%>%<p2%>%<pl%>:сендер PRON-PERS-INFL ;
сендер%<prn%>%<pers%>%<p2%>%<pl%>%<advl%>:сендерше CLITICS-NO-COP ;

олар%<prn%>%<pers%>%<p3%>%<pl%>:олар PRON-PERS-INFL ;
олар%<prn%>%<pers%>%<p3%>%<pl%>%<advl%>:оларша CLITICS-NO-COP ;

сіз%<prn%>%<pers%>%<p2%>%<sg%>%<frm%>:сіз PRON-PERS-INFL ;
сіз%<prn%>%<pers%>%<p2%>%<sg%>%<frm%>%<advl%>:сіздіңше CLITICS-NO-COP ;
сіздер%<prn%>%<pers%>%<p2%>%<pl%>%<frm%>:сіздер PRON-PERS-INFL ;
сіздер%<prn%>%<pers%>%<p2%>%<pl%>%<frm%>%<advl%>:сіздерше CLITICS-NO-COP ;

!!!!!!!   D E M O N S T R A T I V E

! FIXME CHECK (CLITICS-INCL-COP vs CLIT) I can't imagine forms like "бумын, бусың" etc.
! in Tatar, but not sure how it works in Kazakh /I.S./
!-----
! Good idea is to link prn.dem.sim forms alternatively to CLIT and CASE,
! so that it's "adverbial reading" (e.g. "шундый каты кычкырды") doesn't
! receive <nom> tag /IS/

LEXICON PRON-DEM-INFL

%<gen%>:ның CLITICS-NO-COP ;
%<dat%>:ған CLITICS-NO-COP ;
%<acc%>:ны CLITICS-NO-COP ;
%<abl%>:дан CLITICS-INCL-COP ;
%<abl%>:нан CLITICS-INCL-COP ; ! Dir/LR FIXME actually only "осы" seems to take this.
%<loc%>:нда CLITICS-INCL-COP ;
%<gen%>%<subst%>:нікі%{n%} CASES ;
%<loc%>:ндагы ATTR-SUBST ;
%<sim%>:ндай CASES ;  ! FIXME: should have <det>, etc.
%<advl%>:лай CLITICS-INCL-COP ;
%<qnt%>:нша CLITICS-NO-COP ;         !

%<pl%>:%>%{L%}%{A%}р CASES-ETC ;
%<pl%>%<px3sp%>:%>%{L%}%{A%}р%{I%}%{n%} CASES ;

! Commented out till these forms get implemented in tat.lexc too
!%<TD%>:шалық CLITICS-NO-COP ;        ! "that much"  ! FIXME: check cont lex
!%<reas%>%<abl%>:ндықтан # ; ! "because of"  ! FIXME: not attested with осы?

! FIXME: 1) link the below to PRON-DEM-INFL if possible
!            (later, once we are certain about things)

LEXICON PRON-DEM-BUL

! FIXME CHECK (бұны vs мұны) Grammars I have list only forms with "б", but variants
! starting with "м" seem to be more frequent in the (wikipedia) corpus /I.S./
!-----
! <s>FIXME</s> (бұл%<prn%>%<dem%>%<pl%>:бұлар FULL-NOMINAL-INFLECTION ;) FULL-NOMINAL-INFLECTION might lead to
! overgeneration with p1,p2 possessives (they seem to take onle <px3sp> ending);
! on the other hand, with something like бу%<prn%>%<dem%>%<px3sp%>:монысы%{n%} CASES ;
! we loose things like бу<prn><dem><pl><px>:боларныкы /I.S./

бұл%<prn%>%<dem%>%<nom%>:бұл CLITICS-INCL-COP ;
бұл%<prn%>%<dem%>%<nom%>:бұ CLITICS-INCL-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<gen%>:бұның CLITICS-NO-COP ;
бұл%<prn%>%<dem%>%<gen%>:мұның CLITICS-NO-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<dat%>:бұған CLITICS-NO-COP ;
бұл%<prn%>%<dem%>%<acc%>:бұны CLITICS-NO-COP ;
бұл%<prn%>%<dem%>%<acc%>:мұны CLITICS-NO-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<abl%>:бұдан CLITICS-INCL-COP ;
бұл%<prn%>%<dem%>%<abl%>:мұнан CLITICS-INCL-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<loc%>:бұнда CLITICS-INCL-COP ;
бұл%<prn%>%<dem%>%<loc%>:мұнда CLITICS-INCL-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<ins%>:бұнымен CLITICS-NO-COP ;
бұл%<prn%>%<dem%>%<ins%>:бұныменен CLITICS-NO-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<ins%>:мұнымен CLITICS-NO-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<ins%>:мұныменен CLITICS-NO-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<gen%>%<subst%>:мұнікі%{n%} CASES ; ! FIXME: check "м" vs "б"
бұл%<prn%>%<dem%>%<loc%>:бұндағы ATTR-SUBST ;
бұл%<prn%>%<dem%>%<loc%>:мұндағы ATTR-SUBST ; ! Dir/LR
бұл%<prn%>%<dem%>%<sim%>:мұндай CASES ;
бұл%<prn%>%<dem%>%<sim%>:бұндай CASES ; ! Dir/LR
бұл%<prn%>%<dem%>%<sim%>%<px3sp%>:мұндай%{I%}%{n%} CASES ;
бұл%<prn%>%<dem%>%<sim%>%<pl%>:мұндайлар CASES ;
бұл%<prn%>%<dem%>%<sim%>%<pl%>%<px3sp%>:мұндайлары%{n%} CASES ;
бұл%<prn%>%<dem%>%<advl%>:былай CLITICS-INCL-COP ;
бұл%<prn%>%<dem%>%<advl%>:бұлай CLITICS-INCL-COP ; ! Dir/LR
бұл%<prn%>%<dem%>%<qnt%>:мұнша CLITICS-NO-COP ;
бұл%<prn%>%<dem%>%<px3sp%>:бұнысы%{n%} CASES ;

бұл%<prn%>%<dem%>%<pl%>:бұлар CASES-ETC ;
бұл%<prn%>%<dem%>%<pl%>%<px3sp%>:бұлары%{n%} CASES ;

LEXICON PRON-DEM-MINA

мына%<prn%>%<dem%>%<nom%>:мына CLITICS-INCL-COP ;
мына%<prn%>%<dem%>%<gen%>:мынаның CLITICS-NO-COP ;
мына%<prn%>%<dem%>%<dat%>:мынаған CLITICS-NO-COP ;
мына%<prn%>%<dem%>%<acc%>:мынаны CLITICS-NO-COP ;
мына%<prn%>%<dem%>%<abl%>:мынадан CLITICS-INCL-COP ;
мына%<prn%>%<dem%>%<loc%>:мынада CLITICS-INCL-COP ;
мына%<prn%>%<dem%>%<ins%>:мынамен CLITICS-NO-COP ;
мына%<prn%>%<dem%>%<ins%>:мынаменен CLITICS-NO-COP ; ! Dir/LR
!мына%<prn%>%<dem%>%<px%>:мынанікі%{n%} CASES ;
!мына%<prn%>%<dem%>%<loc%>:мынадағы ATTR-SUBST ;
мына%<prn%>%<dem%>%<sim%>:мынадай CASES ;
мына%<prn%>%<dem%>%<advl%>:мыналай CLITICS-INCL-COP ;
мына%<prn%>%<dem%>%<px3sp%>:мына%<prn%>%<dem%>%{S%}%{I%}%{n%} CASES ;

мына%<prn%>%<dem%>%<pl%>:мыналар CASES-ETC ;
мына%<prn%>%<dem%>%<pl%>%<px3sp%>:мыналары%{n%} CASES ;

LEXICON PRON-DEM-ALGI
!NEED AN ADDITIONAL CHECK 
әлгі%<prn%>%<dem%>%<nom%>:әлгі CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<gen%>:әлгінің CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<dat%>:әлгіге CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<acc%>:әлгіні CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<abl%>:әлгіден CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<loc%>:әлгіде CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<ins%>:әлгімен CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<ins%>:әлгіменен CLITICS-INCL-COP ;
әлгі%<prn%>%<dem%>%<sim%>:әлгідей CASES ;


LEXICON PRON-DEM-MINAU

мынау%<prn%>%<dem%>%<nom%>:мынау CLITICS-INCL-COP ;
!мынау%<prn%>%<dem%>%<px3sp%>%<nom%>:мынаусы CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>%<gen%>:мынаусының CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>%<dat%>:мынаусына CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>%<acc%>:мынаусын CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>%<loc%>:мынаусында CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>%<abl%>:мынаусынан  CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>%<ins%>:мынаусымен CLITICS-NO-COP ;
мынау%<prn%>%<dem%>%<px3sp%>%<ins%>:мынаусыменен CLITICS-NO-COP ; ! Dir/LR 

мынау%<prn%>%<dem%>%<px1sg%>:мынауым CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px2sg%>:мынауың CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>:мынауы CLITICS-INCL-COP ;
мынау%<prn%>%<dem%>%<px3sp%>:мынауcы CLITICS-INCL-COP ; ! Dir/LR

LEXICON PRON-DEM-ANA

%<nom%>: CLITICS-INCL-COP ;
%<gen%>:ның CLITICS-NO-COP ;
%<dat%>:ған CLITICS-NO-COP ;
%<acc%>:ны CLITICS-NO-COP ;
%<abl%>:дан CLITICS-INCL-COP ;
%<loc%>:да CLITICS-INCL-COP ;
%<ins%>:мен CLITICS-NO-COP ;
%<ins%>:менен CLITICS-NO-COP ; ! Dir/LR
%<gen%>%<subst%>:нікі%{n%} CASES ;
%<loc%>:дағы ATTR-SUBST ;
%<sim%>:дай CASES ;
%<sim%>:ндай CASES ; ! Dir/LR
!%<adv%>:лай CLITICS-INCL-COP ;
!%<px3sp%>:сы%{n%} CASES ;

%<pl%>:лар CASES-ETC ;
%<pl%>%<px3sp%>:лар%>%{S%}%{I%}%{n%} CASES ;

LEXICON DemonstrativePronouns

PRON-DEM-BUL ;
PRON-DEM-MINA ;
PRON-DEM-MINAU ;
PRON-DEM-ALGI ;

ана%<prn%>%<dem%>:ана PRON-DEM-ANA ;

сол%<prn%>%<dem%>%<nom%>:сол CLITICS-INCL-COP ;
сол%<prn%>%<dem%>:со PRON-DEM-INFL ;
сол%<prn%>%<dem%>%<ins%>:сонымен CLITICS-NO-COP ;
сол%<prn%>%<dem%>%<ins%>:соныменен CLITICS-NO-COP ; ! Dir/LR
сол%<prn%>%<dem%>%<px3sp%>:сонысы%{n%} CASES ;
сол%<prn%>%<dem%>%<loc%>%<attr%>:сондағы CLITICS-INCL-COP ; ! NEED AN ADDITIONAL CHECK


ол%<prn%>%<dem%>%<nom%>:ол CLITICS-INCL-COP ;
ол%<prn%>%<dem%>:о PRON-DEM-INFL ;
ол%<prn%>%<dem%>%<ins%>:онымен CLITICS-NO-COP ;
ол%<prn%>%<dem%>%<ins%>:оныменен CLITICS-NO-COP ; ! Dir/LR
ол%<prn%>%<dem%>%<px3sp%>:онысы%{n%} CASES ;

осы%<prn%>%<dem%>%<nom%>:осы CLITICS-NO-COP ;
осы%<prn%>%<dem%>:осы PRON-DEM-INFL ;
осы%<prn%>%<dem%>%<ins%>:осымен CLITICS-NO-COP ;
осы%<prn%>%<dem%>%<ins%>:осыменен CLITICS-NO-COP ; ! Dir/LR
осы%<prn%>%<dem%>%<px3sp%>:осысы%{n%} CASES ;

! FIXME DISCUSS: Two below were lexicalized as conjunctional adverbs.
! -ндықтан affix is a feature of verbs as well though, so it will be handled as a
! productive derivation.

!ол%<prn%>%<dem%>%<reas%>%<abl%>:ондықтан # ; ! "because of that"
!сол%<prn%>%<dem%>%<reas%>%<abl%>:сондықтан # ; ! "because of that"


!!!!!!!   I N T E R R O G A T I V E

! Some of the words listed in the grammars I have as interrogative pronouns
! were placed in other root lexicons depending on what POS they substitute or
! act like syntactically. So (as planned for today):
! 1) "неше" is a <num><itg>;
! 2) "қайда, қайдан, қашан, қалай, қалайша" are <adv><itg> (the first three
! should link to a class containing location cases btw, as many current adverbs
! do);
! 3) "қанша" is added both as an interrogative pronoun and as an interrogative
! adverb /IS/ and det.qnt /JNW/
!
! What already added as interrogative pronouns are: кім, ни, нәрсе
! And to be added as interrogative pronouns are: қайсы, қандай, қанша.
! I'd like to mark қандай as <sim> form (with қандай for the upper side though),
! but wouldn't go too deeply into the structure of other two and not derive them
! from "қай" (that's what they have most likely originated from) IS
!
! Of course, "қай", "қайсы" and all other pronouns which can be used as an
! attribute were added as determiners linking to CLIT as well.
! Good idea is to have an rlx rule selecting determiner reading when a word is
! ambiguous between (prn X nom) and (det X) and there is noun to the right;
! and selecting pronoun if there is a verb to the right.

LEXICON PRON-ITG-INFL

%<prn%>%<itg%>: FULL-NOMINAL-INFLECTION ;


LEXICON PRON-ITG-QAYSI-CASES

%<nom%>: CLITICS-NO-COP ;
%<gen%>:ның CLITICS-NO-COP ;
%<dat%>:ған CLITICS-NO-COP ;
%<acc%>:ны CLITICS-NO-COP ;
%<abl%>:дан CLITICS-NO-COP ;
%<loc%>:да CLITICS-NO-COP ;
%<ins%>:мен CLITICS-NO-COP ;
%<ins%>:менен CLITICS-NO-COP ; ! Dir/LR

LEXICON PRON-ITG-QAYSI

%<prn%>%<itg%>: PRON-ITG-QAYSI-CASES ;
%<prn%>%<itg%>: GER-POSSESSIVES ;

LEXICON PRON-ITG-QANSHA

! Might be not complete (қаншалар?)
! Currently қанша<prn><itg> is translated with ничә<num><itg><subst> and
! қанша<adv><itg> is going to be translated with "күпме" or "никадәр"

%<prn%>%<itg%>: CASES ;

LEXICON PRON-ITG-QANDAY

! Might be not complete (қандайларығыз? and possessive stuff in general)

%<prn%>%<itg%>%<sim%>: CLITICS-NO-COP ;
! ^ FIXME Similative form of demonstrative pronouns should receive an alternati-
! ve continuation with CLIT too. Consider e,g.: "Ең жақсы Интернет қандай?" Here
! it shouldn't receive <nom> tag I think /IS/
%<prn%>%<itg%>%<sim%>: CASES ;
%<prn%>%<itg%>%<sim%>%<px3sp%>:ы%{n%} CASES ;
%<prn%>%<itg%>%<sim%>%<pl%>:лар CASES ;
%<prn%>%<itg%>%<sim%>%<pl%>%<px3sp%>:лары%{n%} CASES ;

LEXICON InterrogativePronouns

қайсы:қайсы PRON-ITG-QAYSI ;
қанша:қанша PRON-ITG-QANSHA ;
қандай:қандай PRON-ITG-QANDAY ;
нәрсе:нәрсе PRON-ITG-INFL ; ! "thing" ! Use/MT
не:не PRON-ITG-INFL ; ! "what"
немене:немене PRON-ITG-INFL ; ! "what"
кім:кім PRON-ITG-INFL ; ! "who"

LEXICON PRON-QNT-POSS

%<prn%>%<qnt%>: GER-POSSESSIVES ;

LEXICON PRON-QNT

%<prn%>%<qnt%>: GER-INFL ;

LEXICON PRON-QNT-AR
PRON-QNT ;

LEXICON PRON-IND

%<prn%>%<ind%>: CASES ;

LEXICON PRON-IND-PL

%<prn%>%<ind%>%<pl%>: CASES ;

LEXICON PRON-DEM

%<prn%>%<dem%>: CASES ;

LEXICON PRON-NEG

%<prn%>%<neg%>: CASES ;

LEXICON PRNNEG

%<prn%>%<neg%>: CASES ;
%<prn%>%<neg%>: GER-POSSESSIVES ;

LEXICON PRON-RECIP

%<prn%>%<recip%>: GER-POSSESSIVES ;

LEXICON PRON-REF-ADV

CASES-ETC ;
%<advl%>:ше CLITICS-NO-COP ;

LEXICON PRON-REF

! FIXME JNW please have a look at this
! Listing all forms here because of overgeneration with *өздерім,
! *өздеріміз, *өзіңдер etc while linking to POSSESSIVES. This was the idea.
! Now they are handled more like personal pronouns (the same tags in the same
! order), so they should pass transfer without any trouble.
!-----
! A remaining problem with reflexive pronoun is that it takes personal copula
! suffixes "selectively": e.g. prn.refl.px1sg would take only cop.px1sg
! (prn.ref.px3sp seems to take all of them though).
! A little bit of overgeneration won't hurt anyone :), but I think that odd
! forms could be filtered out somehow later. Don't know how to do it./04Aug, IS/

! FIXME DISCUSS Write full forms for the lower side and put only PRON-DEF in the
! Root Lexicon

%<prn%>%<ref%>%<px1sg%>:%>%{I%}м PRON-REF-ADV ;
%<prn%>%<ref%>%<px2sg%>:%>%{I%}ң PRON-REF-ADV ;
%<prn%>%<ref%>%<px3sp%>:%>%{S%}%{I%}%{n%} PRON-REF-ADV ; ! FIXME <px3sg>?
%<prn%>%<ref%>%<px1pl%>:%>%{I%}м%{I%}з PRON-REF-ADV ;
%<prn%>%<ref%>%<px2pl%>:%>д%{A%}р%>%{I%}ң PRON-REF-ADV ;
%<prn%>%<ref%>%<px3pl%>:%>д%{A%}р%>%{S%}%{I%}%{n%} PRON-REF-ADV ;

%<prn%>%<ref%>%<px2sg%>%<frm%>:%>%{I%}ң%{I%}з PRON-REF-ADV ;
%<prn%>%<ref%>%<px2pl%>%<frm%>:%>д%{A%}р%>%{I%}ң%{I%}з PRON-REF-ADV ;

!!!!!!!!!!!!!!!!!!!!!!     DETERMINERS

LEXICON DET-DEM

%<det%>%<dem%>: # ;

LEXICON DET-QNT

%<det%>%<qnt%>: # ;

LEXICON DET-IND

%<det%>%<ind%>: # ;

LEXICON DET-ITG

%<det%>%<itg%>: # ;

LEXICON DET-NEG

%<det%>%<neg%>: # ;

LEXICON DET-REF

%<det%>%<ref%>: # ;


!!!!!!!!!!!!!!!!!!!!!!     OTHER PART-OF-SPEECH LEXICONS

LEXICON N1-ABBR

%<n%>%<attr%>: # ;
%<n%>:%- POSSESSIVES ;
%<n%>:%- CASES-ETC ;
%<n%>: CASES-ETC ; ! Dir/LR


LEXICON N1

%<n%>%<attr%>: # ;
%<n%>: FULL-NOMINAL-INFLECTION ;

LEXICON N1-PL
N1 ;

LEXICON N1-VN
N1 ;

LEXICON N1-NAT
N1 ;

LEXICON N1-ADR
N1 ;

LEXICON N1-Ә

:%{ә%} N1 ;            ! generate front for {I} and back for {A} - куәні, куәға
:ә%{ъ%} N1 ;  ! Dir/LR ! analyse all back forms
:ә N1 ;       ! Dir/LR ! analyse all front forms

LEXICON N-COMPOUND-PX-COMMON
 
:%>%{S%}%{I%}%{n%} CASES-ETC ;
POSSESSIVES ;       ! for stuff like "біздің ауа райымыз" and even "оның ауа райы",
                  ! which should have a <px3sp> analysis


LEXICON N-COMPOUND-PX ! consider "N2" for the name
                      ! equiv of kir:LEXICON N-INFL-3PX-COMPOUND

%<n%>: N-COMPOUND-PX-COMMON ;
%<n%>%<pl%>:%>%{L%}%{A%}р%>%{S%}%{I%}%{n%} CASES-ETC ; ! (e.g. чик сакчылары=пограничники)
                                                      !  FIXME CHECK: Is {S} needed here ?

LEXICON N-COMPOUND-PX-PL
! for things like Олимпиада ойындары

%<n%>%<pl%>: N-COMPOUND-PX-COMMON ; ! FIXME: do we want these to get <pl>??


LEXICON N3
! Singularia tantum

%<n%>%<attr%>: # ;
%<n%>: FULL-NOMINAL-INFLECTION ;

LEXICON N5

:%{☭%} N1 ;

LEXICON N6

:%{і%} N1 ; 
N1 ;  ! Dir/LR     ! We want to analyse forms without %{і%}, since they're used
                   ! but "properly" we want to generate forms with %{і%}

LEXICON N-INFL-NKI

%<n%>:%{I%}н KI ; ! Dir/LR - common variant in both Kazakh and Kyrgyz
%<n%>:%{I%}ң KI ; 
%<n%>: FULL-NOMINAL-INFLECTION ;


!LEXICON NAT
! Nationalites

!A1 ;              ! nationality's adjectival form, e.g. "Turkish [food]" ! Consider <n.attr> reading for this 
!N1 ;              ! a member of the nationality, e.g. "A Turk"
!:%>ш%{A%} ADV ;   ! as / in the style of the nat., e.g. "à la Turk"
!:%>ш%{A%} ADV ;   ! in the language of the nationality, e.g. "in Turkish" ! I guess ADJ was meant /I.S./
!:%>ш%{A%} N1 ;    ! the language of the nationality, e.g. "Turkish"

LEXICON ADV-LANG

N1 ;
ADV ;

LEXICON NP-COMMON
FULL-NOMINAL-INFLECTION ;

LEXICON NP-TOP-RUS

%<np%>%<top%>:%{☭%} NP-COMMON ;
%<np%>%<top%>%<attr%>: # ;

LEXICON NP-TOP

%<np%>%<top%>: NP-COMMON ;
%<np%>%<top%>%<attr%>: # ;

!%<adj%>%<top%>:%{L%}%{I%}%{K%} ADJ-COMMON ;  ! FIXME: do these tags make sense?

LEXICON NP-TOP-ASSR

%<np%>%<top%>%<attr%>: # ;

%<np%>%<top%>: NP-COMMON ; ! Dir/LR
%<np%>%<top%>:%- NP-COMMON ;

LEXICON NP-TOP-COMPOUND

%<np%>%<top%>: N-COMPOUND-PX-COMMON ;
%<np%>%<top%>%<attr%>:%>%{s%}%{I%}%{n%} # ;

LEXICON NP-TOP-ABBR

%<np%>%<top%>%<attr%>: # ;
%<np%>%<top%>%<nom%>: CLITICS-INCL-COP ;
%<np%>%<top%>:%- NP-COMMON ;

%<np%>%<top%>: NP-COMMON ;    ! Dir/LR   ! also allow like this to be parsed, but generate with -


LEXICON NP-ANT-M

%<np%>%<ant%>%<m%>: NP-COMMON ;
%<np%>%<ant%>%<m%>%<pl%>:%>%{L%}%{A%}р NP-COMMON ;

LEXICON NP-ANT-F

%<np%>%<ant%>%<f%>: NP-COMMON ;
%<np%>%<ant%>%<f%>%<pl%>:%>%{L%}%{A%}р NP-COMMON ;

LEXICON NP-COG-M

%<np%>%<cog%>%<m%>: NP-COMMON ;
%<np%>%<cog%>%<m%>%<pl%>:%>%{L%}%{A%}р NP-COMMON ;

LEXICON NP-COG-MF

! Dual-gender cognoms.
! Don't derive anything /I.S./

%<np%>%<cog%>%<mf%>: NP-COMMON ;
%<np%>%<cog%>%<mf%>%<pl%>:%>%{L%}%{A%}р NP-COMMON ;

LEXICON NP-COG-OBIN-FEM

%<np%>%<cog%>%<f%>:а NP-COMMON ;
%<np%>%<cog%>%<f%>%<pl%>:а%>%{L%}%{A%}р NP-COMMON ;


LEXICON NP-COG-OB

! For cognoms ending with -ов, -ев
! They derive feminine equivalets taking -а /I.S./
! Also, even the ев ones are normally pronounced
!    (and take harmony) as if they end in йыф/йып

%<np%>%<cog%>%<m%>:%{☭%} NP-COMMON ;                       ! Dir/LR
%<np%>%<cog%>%<m%>%<pl%>:%{☭%}%>%{L%}%{A%}р NP-COMMON ;    ! Dir/LR

%<np%>%<cog%>%<m%>:%{а%}%{с%} NP-COMMON ;
%<np%>%<cog%>%<m%>%<pl%>:%{а%}%{с%}%>%{L%}%{A%}р NP-COMMON ;

NP-COG-OBIN-FEM ;


LEXICON NP-COG-IN

! For cognoms ending with -ов, -ев, -ин
! They derive feminine equivalets taking -а /I.S./

%<np%>%<cog%>%<m%>:%{☭%} NP-COMMON ;
%<np%>%<cog%>%<m%>%<pl%>:%{☭%}%>%{L%}%{A%}р NP-COMMON ;

NP-COG-OBIN-FEM ;

! LEXICON NPCOGFLEX

! This isn't used at the moment,
! but e.g. ?Polish cognoms ending with -ска would take this cont.class /I.S./ 

LEXICON NP-PAT-VICH

! For patronyms ending with -вич
! Their feminine equivalets end with -вна /I.S./

%<np%>%<pat%>%<m%>:вич%{☭%} NP-COMMON ;

%<np%>%<pat%>%<f%>:вна NP-COMMON ;

! WAS: (JNW 2015-01-17)
!LEXICON NP-ORG
!
!%<np%>%<org%>: CASES ; 

LEXICON NP-ORG

%<np%>%<org%>%<nom%>: CLITICS-INCL-COP ;
%<np%>%<org%>: FULL-NOMINAL-INFLECTION-NONOM ;
%<np%>%<org%>%<attr%>: # ;

LEXICON NP-ORG-LAT

%<np%>%<org%>%<nom%>: CLITICS-INCL-COP ;
%<np%>%<org%>: FULL-NOMINAL-INFLECTION-NONOM ; ! Dir/LR
%<np%>%<org%>:%- FULL-NOMINAL-INFLECTION-NONOM ;
! This one doesn't work right because of twol:
!%<np%>%<org%>:%' FULL-NOMINAL-INFLECTION-NONOM-NONOM ; ! Dir/LR
%<np%>%<org%>%<attr%>: # ;

LEXICON NP-ORG-COMPOUND 

%<np%>%<org%>: N-COMPOUND-PX-COMMON ;   ! COMMON because no plural

LEXICON NP-ORG-ABBR             ! for abbreviations like FIFA
%<np%>%<org%>%<attr%>: # ;
%<np%>%<org%>%<nom%>: CLITICS-INCL-COP ;
%<np%>%<org%>:%- NP-COMMON ;
%<np%>%<org%>: NP-COMMON ;    ! Dir/LR   ! also allow like this to be parsed, but generate with -

LEXICON NP-AL

%<np%>%<al%>: CASES ;

LEXICON NP-AL-ABBR              ! for abbreviations like ӘЧ - Әлем Чемпионаты
%<np%>%<al%>%<attr%>: # ;
%<np%>%<al%>%<nom%>: CLITICS-INCL-COP ;
%<np%>%<al%>:%- NP-COMMON ;
%<np%>%<al%>: NP-COMMON ;    ! Dir/LR   ! also allow like this to be parsed, but generate with -


LEXICON NUM-COMMON

%<num%>: # ;
%<num%>%<subst%>: FULL-NOMINAL-INFLECTION ;

LEXICON NUM

NUM-COMMON ;
%<num%>%<ord%>:%>%{I%}нш%{I%} # ;             ! FIXME: base form, <det> reading
%<num%>%<ord%>%<subst%>:%>%{I%}нш%{I%} FULL-NOMINAL-INFLECTION ;

! <ord> is "det" is base form, <ord><subst> is "prn" reading

LEXICON NUM-TWENTY

NUM-COMMON ;
%<num%>%<ord%>:%>с%{I%}нш%{I%} # ;
%<num%>%<ord%>%<subst%>:%>с%{I%}нш%{I%} FULL-NOMINAL-INFLECTION ;

LEXICON NUM-ITG ! This is a temporal solution
                ! (could be integrated with the above)

%<num%>%<itg%>: # ;
%<num%>%<itg%>%<subst%>: FULL-NOMINAL-INFLECTION ;
%<num%>%<itg%>%<ord%>:%>%{I%}нш%{I%} FULL-NOMINAL-INFLECTION ;

LEXICON NUM-COLL

%<num%>%<coll%>%<subst%>: FULL-NOMINAL-INFLECTION ;
%<num%>%<coll%>%<advl%>: # ;

LEXICON NUM-ROMAN

%<num%>%<ord%>: # ;

LEXICON LTR

%<ltr%>: # ;

!LEXICON DIGITLEX
!
!%<num%>: # ;

LEXICON POST

%<post%>: CLITICS-NO-COP ;

LEXICON POST-NOM
POST ;

LEXICON POST-NOM-GEN
POST ;

LEXICON POST-DAT
POST ;

LEXICON POST-ABL
POST ;

LEXICON POST-INS
POST ;

LEXICON POSTADV

%<postadv%>: # ;

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

LEXICON KI

%<attr%>:%>%{G%}%{I%} CLITICS-NO-COP ;
%<subst%>:%>%{G%}%{I%} FULL-NOMINAL-INFLECTION ;

LEXICON ADV

%<adv%>: CLITICS-NO-COP ;

LEXICON ADV-WITH-KI

ADV ;
%<adv%>: KI ;
%<adv%>%<abl%>:%>%{D%}%{A%}н # ; ! FIXME: overgenerates a bit

LEXICON ADV-WITH-KI-I     ! Used for бері<adv>/бергі<adv><attr> 'right now' and similar

:%{I%} ADV ;
%<adv%>: KI ;

LEXICON ADV-ITG

%<adv%>%<itg%>: CLITICS-INCL-COP ;

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

LEXICON CC

%<cnjcoo%>: CLIT-GHANA-ETC ;

LEXICON CC-SOYED
CC ;

LEXICON CC-PROTIV
CC ;

LEXICON CC-RAZDEL
CC ;

LEXICON CS

%<cnjsub%>: # ; 

LEXICON CA

%<cnjadv%>: CLIT-GHANA-ETC ;

LEXICON QST

%<qst%>: # ;

LEXICON MOD-ASS

%<mod_ass%>: # ;

LEXICON MOD

! шығар, сияқты etc.

%<mod%>: # ;

LEXICON INTERJ

%<ij%>: # ;

LEXICON IDEO

%<ideo%>: # ;

LEXICON ABBR

%<abbr%>: # ;

LEXICON PAREN

%<paren%>: # ; 

LEXICON Digits ! Use/Circ

%0:%0 NUM ;  ! Use/Circ
№:№ DIGITLEX ;  ! Use/Circ
DIGITLEX ; ! Use/Circ
LASTDIGIT ; ! Use/Circ
LASTDIGIT-REST ; ! Use/Circ

!LEXICON Guesser
!
!<(а | ә | б | в | г | ғ | д | е | ё | ж | з | и | і | й | к | қ | л | м | н | ң | о | ө | п | р | с | т | у | ұ | ү | ф | х | һ | ц | ч | ш | щ | ь | ы | ъ | э | ю | я)+> N1 ; 

LEXICON NP-UNK 

%<np%>%<unk%>: # ;

LEXICON A1-SUP
A1 ;
LEXICON A2-SUP
A2 ;
LEXICON A2-COMP
A2 ;
LEXICON NUM-APPROX
NUM ;
LEXICON N1-AR
N1 ;
LEXICON N1-LANG
N1 ;
LEXICON NUM-DIST
NUM ;
LEXICON V-TV-LESS
V-TV ;
LEXICON V-TV-REP
V-TV ;
LEXICON V-IV-LESS
V-IV ;
LEXICON V-IV-REP
V-IV ;
LEXICON IJ
INTERJ ;
LEXICON PREP
# ;
LEXICON V-TD
V-TV ;
V-IV ;

LEXICON GUESS-QUEUE

<(a | b | c | d | e | f | g | h | j | k | l | m | n | o | p | q | r | s | t | u | v | w | x | y | z)+> NP-UNK ;

LEXICON Guesser

A:A GUESS-QUEUE ; 
B:B GUESS-QUEUE ; 
C:C GUESS-QUEUE ; 
D:D GUESS-QUEUE ; 
E:E GUESS-QUEUE ; 
F:F GUESS-QUEUE ; 
G:G GUESS-QUEUE ; 
H:H GUESS-QUEUE ; 
J:J GUESS-QUEUE ; 
K:K GUESS-QUEUE ; 
L:L GUESS-QUEUE ; 
M:M GUESS-QUEUE ; 
N:N GUESS-QUEUE ; 
O:O GUESS-QUEUE ; 
P:P GUESS-QUEUE ; 
Q:Q GUESS-QUEUE ; 
R:R GUESS-QUEUE ; 
S:S GUESS-QUEUE ; 
T:T GUESS-QUEUE ; 
U:U GUESS-QUEUE ; 
V:V GUESS-QUEUE ; 
W:W GUESS-QUEUE ; 
X:X GUESS-QUEUE ; 
Y:Y GUESS-QUEUE ; 
Z:Z GUESS-QUEUE ; 

<( а | ә | б | в | г | ғ | д | е | ё | ж | з | и | і | й | к | қ | л | м | н |
   ң | о | ө | п | р | с | т | у | ұ | ү | ф | х | һ | ц | ч | ш | щ | ь | ы |
   ъ | э | ю | я )> LTR ;

<(M | D | C | L | X | V | I)+> NUM-ROMAN ; ! ""


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!                                STEMS                                    !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! There are 5 lexicons below:
!! - Common (read: not a proper noun, abbreviation or punctuation)
!! - Hardcoded (any entry which has a %<tag%> in it)
!! - Abbreviations
!! - Punctuation
!! - Proper (proper nouns)
!!
!! In each section, lines are sorted alphabetically with the
!! `LC_ALL=kk_KZ.utf8 sort' command.
!!
!! See apertium-kaz's documentation at https://apertium.github.io/apertium-kaz/
!! for more information.


LEXICON Common

PRON-PERS ;
DemonstrativePronouns ;
InterrogativePronouns ;

@(string-join (map raw-entry->lexc entries) "\n")

LEXICON Hardcoded

!cиық:си%{y%}қ N1 ; ! "exterior"
дені% дұрыстау%<adj%>%<comp%>:дені% дұрыс # ;
қайт%<v%>%<tv%>%<p1%>%<pl%>:қайтеміз # ;
қайт%<v%>%<tv%>%<p1%>%<sg%>:қайтемін # ;
қайт%<v%>%<tv%>%<p2%>%<pl%>:қайтесіңдер # ;
қайт%<v%>%<tv%>%<p2%>%<sg%>:қайтесің # ;
қайт%<v%>%<tv%>%<p3%>%<pl%>:қайтеді # ;
қайт%<v%>%<tv%>%<p3%>%<sg%>:қайтеді # ;
ол%<prn%>%<dem%>%<px3sp%>%<dat%>:онысына # ;
пенсия%<n%>%<dat%>:пенсияға # ;
полюс%<n%>%<loc%>%<attr%>:полюстегі # ;
полюс%<n%>%<loc%>:полюсте # ;
сиық%<n%>%<px1sg%>%<nom%>:сиқым # ;
сиық%<n%>%<px2sg%>%<nom%>:сиқың # ;
сиық%<n%>%<px3sp%>%<nom%>:сиқы # ;
сый%<v%>%<iv%>%<gpr_impf%>:сиятын # ;
сый%<v%>%<iv%>%<gpr_ppot%>%<sim%>:сиярлықтай # ;
сый%<v%>%<iv%>%<gpr_ppot%>:сиярлық # ;
сый%<v%>%<iv%>%<neg%>%<gna_impf%>:симай # ;
сый%<v%>%<iv%>%<pih%>%<p3%>%<pl%>:сиятын # ;
сый%<v%>%<iv%>%<pih%>%<p3%>%<sg%>:сиятын # ;
уақыт%<n%>%<px3sp%>%<loc%>:уақытысында # ;
шәй%<n%>%<abl%>:шәйдан # ;
шәй%<n%>%<acc%>:шәйды # ;
шәй%<n%>%<dat%>:шәйға # ;
шәй%<n%>%<gen%>:шәйдың # ; !Dir/LR
шәй%<n%>%<gen%>:шәйдің # ;  
шәй%<n%>%<ins%>:шәймен # ;
шәй%<n%>%<loc%>:шәйда # ;

LEXICON Abbreviations

AFP:AFP ABBR ; ! ""
ANP:ANP ABBR ; !"Use/MT"
ATP:ATP ABBR ; !Use/MT
AББ:AББ ABBR ; !"Use/MT"
BAE:BAE ABBR ; ! ""
BG:BG ABBR ; ! ""
BP:BP ABBR ; !"Use/MT"
CAM:CAM ABBR ; !"Use/MT"
CCTV:CCTV ABBR ; ! ""
CD:CD ABBR ; ! ""
CEZ:CEZ ABBR ; ! ""
CGI:CGI ABBR ; ! ""
CNPC:CNPC ABBR ; ! ""
DVD:DVD ABBR ; ! "DVD" !Use/MT
EITI:EITI ABBR ; ! ""
EPO:EPO ABBR ; !"Use/MT"
ER:ER ABBR ; ! ""
FARC:FARC ABBR ; ! ""
FBI:FBI ABBR ; ! ""
GP:GP ABBR ; !"Use/MT"
GPS:GPS ABBR ; !"Use/MT"
HAS:HAS ABBR ; ! ""
HIV:HIV ABBR ; ! ""
HRH:HRH ABBR ; !"Use/MT"
http:http ABBR ; ! ""
ICU:ICU ABBR ; ! ""
ICU:ICU ABBR ; ! ""
IDF:IDF ABBR ; ! ""
IDF:IDF ABBR ; ! ""
IMD:IMD ABBR ; ! ""
IME:IME ABBR ; !"Use/MT"
IP:IP ABBR ; ! ""
ISO:ISO ABBR ; ! ""
Kaz:Kaz ABBR ; ! ""
kPa:kPa ABBR ; ! ""
KPO:KPO ABBR ; ! ""
kz:kz ABBR ; ! ""
KZ:KZ ABBR ; ! ""
MGM:MGM ABBR ; !"Use/MT"
MRI:MRI ABBR ; ! ""
NASA:NASA ABBR ; ! ""
NBA:NBA  ABBR ; ! ""
NBC:NBC ABBR ; ! ""
NDS:NDS ABBR ; !"Use/MT"
OHL:OHL ABBR ; ! ""
OMLT:OMLT ABBR ; !"Use/MT"
PDA:PDA ABBR ; ! ""
PKK:PKK ABBR ; ! ""
ppm:ppm ABBR ; ! ""
PSM:PSM ABBR ; ! ""
PSP:PSP ABBR ; ! ""
RAF:RAF ABBR ; ! ""
RSS:RSS ABBR ; ! ""
RTL:RTL ABBR ; ! ""
SAS:SAS ABBR ; ! ""
SME:SME ABBR ; ! ""
SMS:SMS ABBR ; ! ""
TNT:TNT ABBR ; ! ""
UDF:UDF ABBR ; ! ""
UEFA:UEFA ABBR ; ! ""
USB:USB ABBR ; ! ""
UTC:UTC ABBR ; !"Use/MT"
x:x ABBR ; ! "3x4"
ZDF:ZDF ABBR ; ! ""
А%.:А%. ABBR ;
ААҚ:ААҚ ABBR ; ! "Ашық акционердық коғам=Public company"
авг%.:авг%. ABBR ;
АЕК:АЕК ABBR ; ! "USA"
акад.:акад. ABBR ; ! ""
АК:АК ABBR ; ! ""
АКСР:АКСР ABBR ; ! ""
акцион%.:акцион%.  ABBR ; !"Use/MT"
АҚ:АҚ ABBR ; ! ""
АҚШ:АҚШ ABBR ; ! "USA"
АМТ:АМТ ABBR ; ! ""
англ:англ ABBR ;
АӨСШК:АӨСШК ABBR ; ! ""
апр%.:апр%. ABBR ;
апр:апр ABBR ;
аум%.:аум%. ABBR ; ! ""
АФ:АФ ABBR ; ! ""
АЦАТ:АЦАТ ABBR ; ! ""
АЦАТ:АЦАТ ABBR ; !"Use/MT"
ӘқБК:ӘқБК ABBR ; ! ""
ӘҚБК:ӘҚБК ABBR ; ! ""
ӘӨК:ӘӨК ABBR ; ! ""
ӘЧ:ӘЧ NP-AL-ABBR ; ! ""
б.% б.:б.% б. ABBR ; ! ""
ББС:ББС ABBR ; !"Use/MT"
б%.з%.б%.:б%.з%.б%. ABBR ; ! "CE"
б%.з%.б%.:б%.з%.б ABBR ; ! "CE" Dir/LR
б%.з%.д%.:б%.з%.д%. ABBR ; ! "BCE"
б%.з%.д%.:б%.з%.д ABBR ; ! "BCE" Dir/LR
биол%.:биол%. ABBR ; ! ""
Биохим:Биохим ABBR ; ! "Разведка&Добыча"
биікт%.:биікт%. ABBR ; ! ""
БМТРК:БМТРК NP-TOP-ABBR ; ! ""
БОАК:БОАК ABBR ; ! ""
БӨ:БӨ ABBR ; ! ""
БСН:БСН ABBR ; !"Use/MT"
БТА:БТА ABBR ; ! "a bank in Kazakhstan"
б%.т%.:б%.т%. ABBR ; ! "болып табылады"
БҰҰ:БҰҰ ABBR ; ! "United Nations"
БХООО:БХООО NP-TOP-ABBR ; ! ""
б%.э%.д%.:б%.э%.д%. ABBR ; ! "" ! Use/MT
ВИЧ:ВИЧ ABBR ; ! ""
ВСООНЛ:ВСООНЛ ABBR ; !"Use/MT"
г:г ABBR ; ! "грамм"
геогр%.:геогр%. ABBR ; ! ""
геол%.:геол%. ABBR ; ! ""
Гленкор:Гленкор ABBR ; ! ""
ГСБП:ГСБП ABBR ; ! ""
ГСДП:ГСДП ABBR ; ! ""
ГУЛАГ:ГУЛАГ NP-TOP-ABBR ; ! ""
ГЭС:ГЭС ABBR ; ! ""
ғ%.:ғ%. ABBR ;
ғ.:ғ. ABBR ; ! ""
Ғ%.:Ғ%. ABBR ; ! ""
ғ%.:ғ%. ABBR ; ! "" Use/MT
Ғ%.С%.:Ғ%.С%. ABBR ; ! ""
дек%.:дек%. ABBR ;
ДК:ДК ABBR ; ! ""
ДНҚ:ДНҚ%{а%} ABBR ; ! "DNA"
ДСҰ:ДСҰ ABBR ; ! "WTO"
ЕАҚК:ЕАҚК ABBR ; ! "EADS"
ЕАЭЫ:ЕАЭЫ NP-TOP-ABBR ; ! ""
ЕҚЫҰ:ЕҚЫҰ ABBR ; ! ""
ЕҚЫҰ:ЕҚЫҰ ABBR ; ! "eng. OSCE, rus. ОБСЕ"
Ембімұнайгаз:Ембімұнайгаз ABBR ; ! ""
ЕО:ЕО ABBR ; ! "" ! Use/MT
ЕурАзЭҚ:ЕурАзЭҚ ABBR ; ! ""
Еуроодақ:Еуроодақ ABBR ; ! ""
ЕҰУ:ЕҰУ ABBR ; ! "" ! Use/MT
ЕЭЫ:ЕЭЫ NP-TOP-ABBR ; ! ""
ж%.:ж%. ABBR ; ! "year"
ж%.:ж ABBR ; ! "year" Dir/LR
жж%.:жж%. ABBR ; ! ""
ЖКО:ЖКО NP-TOP-ABBR ; ! ""
ЖКТ:ЖКТ ABBR ; !"Use/MT"
ЖҚҚ:ЖҚҚ NP-TOP-ABBR ; ! ""
ЖОО:ЖОО ABBR ; ! ""
ЖСДП:ЖСДП ABBR ; ! "Жалпыұлттық социал демократиялық партия=Гомуммилли Социаль Демократик Партия"
ЖСК:ЖСК ABBR ; !"Use/MT"
ЖТСХ:ЖТСХ ABBR ; ! ""
ЖХЛ:ЖХЛ NP-TOP-ABBR ; ! ""
ЖШС:ЖШС ABBR ; ! "LLC"
ЖІӨ:ЖІӨ ABBR ; ! ""
ЖЭК:ЖЭК ABBR ; ! ""
зоо:зоо N1 ; !"Use/MT"
И%.:И%. ABBR ; ! ""
ИНТА:ИНТА ABBR ; !"Use/MT"
ИСАФ:ИСАФ ABBR ; !"Use/MT"
КамАЗ:КамАЗ ABBR ; ! ""
КГБ:КГБ ABBR ; ! ""
кг:кг ABBR ; ! "kg"
КЕУ:КЕУ ABBR ; ! ""
КИМЕП:КИМЕП ABBR ; ! ""
км²:км² ABBR ; ! "km²"
км²:км2 ABBR ; ! "km²" Dir/LR
км³:км³ ABBR ; ! "km³"
км³:км3 ABBR ; ! "km³" Dir/LR
км:км ABBR ; !"Use/MT"
км:км ABBR ; ! "км"
КМС:КМС NP-TOP-ABBR ; ! ""
КОКП:КОКП ABBR ; ! "СPSU" !Use/MT
КОТА:КОТА ABBR ; ! ""
КСР:КСР ABBR ; ! ""
КСРО:КСРО ABBR ; ! "USSR"
КСРО:КСРО NP-TOP-ABBR ; ! ""
КТУ:КТУ NP-TOP-ABBR ; ! ""
КХДР:КХДР ABBR ; ! ""
КХДР:КХДР NP-TOP-ABBR ; ! ""
Қазатомпром:Қазатомпром ABBR ; ! ""
ҚазКСР:ҚазКСР ABBR ; ! ""
Қазмұнайгаз:Қазмұнайгаз ABBR ; ! ""
Қазпошта:Қазпошта ABBR ; ! ""
ҚазТАГ:ҚазТАГ ABBR ; ! ""
ҚазҰУ:ҚазҰУ ABBR ; ! "Al-Farabi Kazakh National Universty"
ҚазҰУ:ҚазҰУ ABBR ; ! "Al-Farabi Kazakh National Universty"
ҚК:ҚК ABBR ; ! "км"
ҚКП:ҚКП ABBR ; ! ""
Қ%.:Қ%. ABBR ; ! "" Use/MT
ҚҚС:ҚҚС ABBR ; !"Use/MT"
ҚМДБ:ҚМДБ ABBR ; ! "Казакъстан Мөселманнары Диния Нәзараты"
ҚР:ҚР ABBR ; ! "Republic of Kazakhstan"
ҚХР:ҚХР ABBR ; ! ""
лат%.:лат%. ABBR ; ! ""
м²:м² ABBR ; ! "m²"
м²:м2 ABBR ; ! "m²" Dir/LR
м³:м³ ABBR ; ! "m³"
м³:м3 ABBR ; ! "m³" Dir/LR
МАГАТЭ:МАГАТЭ ABBR ; ! ""
МАЖ:МАЖ ABBR ; ! ""
май%.:май%. ABBR ;
Максам:Максам ABBR ; ! ""
Мб:Мб  ABBR ; ! ""
МБФ:МБФ ABBR ; ! ""
МВт:МВт ABBR ; ! ""
Мемдум:Мемдум NP-TOP-ABBR ; ! ""
мемл:мемл ABBR ; ! "мемлекет"
мин:мин ABBR ; ! "m"
МҚО:МҚО NP-TOP-ABBR ; ! ""
млн:млн ABBR ; ! "млн"
млрд:млрд ABBR ; ! "млрд"
М%.:М%. ABBR ;
М%.:М%. ABBR ;
м:м ABBR ; ! "m"
мм.:мм. ABBR ; !"Use/MT"
мм:мм ABBR ; !"Use/MT"
МР:МР NP-TOP-ABBR ; ! ""
МСОП:МСОП ABBR ; ! ""
МТК:МТК ABBR ; ! ""
МТ:МТ NP-TOP-ABBR ; ! ""
мыс%.:мыс%. ABBR ; ! ""
НАСА:НАСА ABBR ; ! ""
НАТО:НАТО ABBR ; ! ""
НКВД:НКВД ABBR ; ! ""
нояб%.:нояб%. ABBR ;
НҰСЖП:НҰСЖП ABBR ; ! ""
ОАР:ОАР ABBR ; ! ""
ОББ:ОББ ABBR ; ! ""
обл%.:обл%. ABBR ; ! ""
ОГПУ:ОГПУ ABBR ; ! ""
ОЕБ:ОЕБ ABBR ; !"Use/MT"
окт%.:окт%. ABBR ;
оңт%.:оңт%. ABBR ; ! "оңтүстік,көньяк,south" FIXME:CHECK -->"
ОПЕК:ОПЕК ABBR ; ! ""
ӨГК:ӨГК ABBR ; ! ""
Өзенмұнайгаз:Өзенмұнайгаз ABBR ; ! ""
ӨҰҚ:ӨҰҚ ABBR ; !"Use/MT"
ӨФ:ӨФ ABBR ; ! ""
ПӘК:ПӘК ABBR ; ! ""
пед%.:пед%. ABBR ; ! ""
ПИҚ:ПИҚ ABBR ; ! ""
РЖМБ:РЖМБ NP-TOP-ABBR ; ! ""
РКФСР:РКФСР ABBR ; ! ""
РЛДП:РЛДП NP-TOP-ABBR ; ! ""
РНҚ:РНҚ ABBR ; ! ""
Р%.:Р%. ABBR ;
РСФСР:РСФСР ABBR ; ! ""
РТЖ:РТЖ NP-TOP-ABBR ; ! ""
руб:руб ABBR ; ! ""
РФКП:РФКП NP-TOP-ABBR ; ! ""
РФ:РФ ABBR ; ! "Russian Federation"
СБД:СБД ABBR ; ! ""
СБЛ:СБЛ NP-TOP-ABBR ; ! ""
СВС:СВС ABBR ; !"Use/MT"
СВУ:СВУ ABBR ; !"Use/MT"
СДУ:СДУ ABBR ; ! ""
сент%.:сент%. ABBR ;
СЕС:СЕС ABBR ; !"Use/MT"
см:см ABBR ; ! "cm"
СНПС:СНПС ABBR ; ! ""
солт%.:солт%. ABBR ; ! "солтүстік,төньяк,north" FIXME:CHECK -->"
солт%.:солт ABBR ; ! "солтүстік,төньяк,north" FIXME:CHECK -->" Dir/LR
СООНО:СООНО ABBR ; !"Use/MT"
СПБМУ:СПБМУ ABBR ; ! ""
С%.:С%. ABBR ;
ССРО:ССРО ABBR ; ! ""
ССР:ССР ABBR ; ! ""
СССР:СССР ABBR ; ! "USSR"
ССС:ССС ABBR ; ! ""
с.% ш.:с.% ш. ABBR ; ! ""
СЭС:СЭС ABBR ; ! ""
т%.б%.:т%.б%. ABBR ; ! "etc."
т%.б%.:т%.б%.  ABBR ; !"Use/MT"
ТВ:ТВ ABBR ; ! ""
тереңд%.:тереңд%. ABBR ; ! ""
тех%.:тех%. ABBR ; ! ""
ТЖҚ:ТЖҚ ABBR ; ! ""
ТЖ:ТЖ ABBR ; ! ""
ТИМ:ТИМ NP-TOP-ABBR ; ! ""
ТМД:ТМД ABBR ; ! "CIS"
төм%.:төм%. ABBR ; ! "" ! Use/MT
трлн:трлн ABBR ; ! ""
ТР:ТР ABBR ; ! "Republic of Tatarstan"
т.% с.% с:т.% с.% с  ABBR ; ! ""
т%.с%.с%.:т%.с%.с%. ABBR ; !"Use/MT"
Т%.:Т%. ABBR ; ! ""
т%.:т%. ABBR ; ! "tonne"
т:т ABBR ; ! "тонна"
т:т%{а%} N1-ABBR ; ! "ton"
ТЭЦ:ТЭЦ ABBR ; ! ""
УАЗ:УАЗ ABBR ; ! ""
УЕФА:УЕФА ABBR ; ! ""
УК:УК ABBR ; ! ""
ҰҚК:ҰҚК ABBR ; ! "Ұжымдық қауіпсіздік келісімі"
ҰҚШҰ:ҰҚШҰ ABBR ; ! "Collective Security Treaty Organisation"
февр%.:февр%. ABBR ; 
ФКҚҚ:ФКҚҚ ABBR ; ! ""
ФҚҚ:ФҚҚ ABBR ; !"Use/MT"
ФСБ:ФСБ ABBR ; ! ""
ХВҚ:ХВҚ NP-TOP-ABBR ; ! ""
ХДО:ХДО ABBR ; ! ""
ХДП:ХДП ABBR ; ! ""
хим%.:хим%. ABBR ; ! "chemical"
ХҚКО:ХҚКО ABBR ; !
ХТҚО:ХТҚО ABBR ; ! ""
ЦАС:ЦАС ABBR ; ! "DSL"
ЦТП:ЦТП ABBR ; ! "DTP"
ш.% б.:ш.% б. ABBR ; ! ""
ШҰАР:ШҰАР ABBR ; ! "Xinjiang Uyghur Autonomous Region"
Ш%.:Ш%. ABBR ;
ШЫҰ:ШЫҰ ABBR ; ! ""
ШЫҰ:ШЫҰ ABBR ; ! "Shanghai Cooperaton Organisation"
ІІМ:ІІМ ABBR ; ! "Ішке істер министрліғі"
экон%.:экон%. ABBR ; ! "economic"
ЭКСПО:ЭКСПО ABBR ; ! "EXPO"
ЭҚК:ЭҚК ABBR ; ! ""
ЭӨКК:ЭӨКК ABBR ; ! ""
ЭЫДҰ:ЭЫДҰ ABBR ; ! ""
ЮНЕСКО:ЮНЕСКО ABBR ; !"Use/MT"
янв%.:янв%. ABBR ; 

LEXICON Punctuation

%'%<apos%>:%' # ;
,%<cm%>:%, # ;
%-%<guio%>:%- # ;
%–%<guio%>:%– # ;
%—%<guio%>:%— # ;
%(%<lpar%>:%( # ;
%[%<lpar%>:%[ # ;
%«%<lquot%>:%« # ;
%“%<lquot%>:%“ # ;
%)%<rpar%>:%) # ;
%]%<rpar%>:%] # ;
%»%<rquot%>:%» # ;
%”%<rquot%>:%” # ;
%!%<sent%>:%! # ;
%"%<sent%>:%" # ;
%.%.%.%<sent%>:%.%.%. # ;
%.%<sent%>:%. # ;
%/%<sent%>:%/ # ;
%:%<sent%>:%: # ;
%;%<sent%>:%; # ;
%?%<sent%>:%? # ;
%\%<sent%>:%\ # ;

LEXICON Proper

ABBA:ABBA NP-ORG-LAT ; ! ""
ABC:ABC NP-ORG-LAT ; ! ""
ACLU:ACLU NP-ORG-LAT ; ! ""
Adobe:Adobe NP-ORG-LAT ; ! ""
Airbus:Airbus NP-ORG-LAT ; ! ""
Ajax:Ajax NP-ORG-LAT ; ! ""
Alma:Alma NP-ANT-F; !"Use/MT"
Althea:Althea NP-ORG-LAT ; !""
AMD:AMD NP-ORG-LAT ; ! ""
Amiga:Amiga NP-ORG-LAT ; ! ""
Amnesty% International:Amnesty% International NP-ORG-LAT ; ! ""
Amway:Amway NP-ORG-LAT ; ! ""
AOL:AOL NP-ORG-LAT ; ! ""
Aon:Aon NP-ORG-LAT ; ! ""
Apertium:Apertium NP-AL ; ! ""
ASCII:ASCII NP-ORG-LAT ; ! ""
Atari:Atari NP-ORG-LAT ; ! ""
Audi:Audi NP-ORG-LAT ; ! ""
Autodesk:Autodesk NP-ORG-LAT ; !""
Aйтғали:Aйтғали NP-ANT-M ; ! "Aytğalıy" (Arabic)
Beatles:Beatles NP-AL ; !"Use/MT"
Camus:Camus NP-ORG-LAT ; ! ""
Chevron% Corporation:Chevron% Corporation NP-ORG-LAT ; ! ""
Christie:Christie NP-ORG-LAT ; ! ""
Columbia:Columbia NP-ORG-LAT ; ! ""
Company:Company NP-ORG-LAT ; ! ""
Conoco:Conoco NP-ORG-LAT ; ! ""
Consolidated% Contractors% Company:Consolidated% Contractors% Company NP-ORG-LAT ; ! ""
Efe:Efe NP-ORG-LAT ; !""
Endesa:Endesa NP-ORG-LAT ; !""
Erasmus:Erasmus NP-AL ; ! ""
Ericsson:Ericsson NP-ORG-LAT ; !""
Eurovision:Eurovision NP-AL ; ! ""
Expo:Expo NP-AL ; ! ""
Exxon:Exxon NP-ORG-LAT ; ! ""
Exxon% Mobil% Corporation:Exxon% Mobil% Corporation NP-ORG-LAT ; ! ""
Facebook:Facebook NP-ORG-LAT ; ! ""
! Fixme -- contains ascii characters ! Xaceh NP-ANT-M ; ! "Xaceh" (Arabic)
! Fixme -- contains ascii characters ! Әmет NP-ANT-M ; ! "Ämet" (Arabic)
! Fixme -- contains ascii characters ! Бatыpajiы NP-ANT-M ; ! "Batıpajiı" (Kazakh)
! Fixme -- contains ascii characters ! Бepeke NP-ANT-M ; ! "Bepeke" (Kazakh)
! Fixme -- contains ascii characters ! Байғaли NP-ANT-M ; ! "Bayğalıy" (Kazakh)
! Fixme -- contains ascii characters ! Барлac NP-ANT-M ; ! "Barlac" (Old Turkic)
! Fixme -- contains ascii characters ! Кeheш NP-ANT-M ; ! "Kehesh" (Old Turkic)
! Fixme -- contains ascii characters ! Шamat NP-ANT-M ; ! "Shamat" (Arabic)
! Fixme -- contains ascii characters ! Шыhtemіp NP-ANT-M ; ! "Shıhtemip" (Kazakh)
Freedom% House:Freedom% House NP-ORG-LAT ; ! ""
Friendfeed:Friendfeed NP-ORG-LAT ; !""
Global:Global NP-AL ; ! ""
Google:Google%{а%}%{л%} NP-ORG-LAT ; ! ""
Group:Group NP-AL ; ! ""
Guardian:Guardian NP-ORG-LAT ; ! ""
Halal:Halal NP-AL ; ! ""
Iberdrola:Iberdrola NP-ORG-LAT ; !"T"
IBM:IBM NP-ORG-LAT ; ! ""
Inc:Inc NP-ORG-LAT ; ! ""
Independent:Independent NP-AL ; ! ""
Inpex:Inpex NP-ORG-LAT ; ! ""
INSEE:INSEE NP-ORG-LAT ; ! "Францияның ұлттық статистика және экономикалық зерттеулер институты (фр. Institut national de la statistique et des études économiques, қысқ. INSEE немесе Insee"
Instructables:Instructables NP-ORG-LAT ; ! ""
Investment:Investment NP-ORG-LAT ; ! ""
Ipad:Ipad NP-ORG-LAT ; !"Use/MT"
iPhone:iPhone NP-ORG-LAT ; !"Use/MT"
iPod:iPod NP-ORG-LAT ; !"Use/MT"
Javascript:Javascript%{э%}%{т%} NP-ORG-LAT ; !"Use/MT"
Jenington:Jenington NP-ORG-LAT ; ! ""
Karachaganak% Petroleum% Operating:Karachaganak% Petroleum% Operating NP-ORG-LAT ; ! ""
KazakhGold:KazakhGold NP-ORG-LAT ; ! ""
Kazakhmys:Kazakhmys NP-ORG-LAT ; ! ""
Kazakhmys% Plc:Kazakhmys% Plc NP-ORG-LAT ; ! ""
Kazakhstan:Kazakhstan NP-AL ; ! ""
Leaks:Leaks NP-AL ; ! ""
Limited:Limited NP-ORG-LAT ; ! ""
Linux:Linux NP-ORG-LAT ; !"Use/MT"
Ltd:Ltd NP-ORG-LAT ; ! ""
Metallica:Metallica NP-ORG-LAT ; !"Use/MT"
Mobil:Mobil NP-AL ; ! ""
Mobil:Mobil NP-ORG-LAT ; ! ""
Mozilla:Mozilla NP-ORG-LAT ; !""
MySpace:MySpace NP-ORG-LAT ; !""
Nauryz:Nauryz NP-AL ; ! ""
News% of% the% World:News% of% the% World NP-AL ; ! ""
New% York% Times:New% York% Times NP-AL ; ! ""
NGC:NGC NP-ORG-LAT ; ! "Жаңа жалпы каталог = New General Catalogue"
NGO:NGO NP-ORG-LAT ; ! 
Nike:Nike NP-ORG-LAT ; !""
Nokia:Nokia NP-ORG-LAT ; !""
Operating:Operating NP-ORG-LAT ; ! ""
Petroleum:Petroleum NP-AL ; ! ""
Petroleum:Petroleum NP-ORG-LAT ; ! ""
Plc:Plc NP-ORG-LAT ; ! ""
SCAT:SCAT NP-ORG-LAT ; ! ""
SFOR:SFOR NP-ORG-LAT ; ! ""
Shell:Shell NP-ORG-LAT ; ! ""
Times:Times NP-AL ; ! ""
Time:Time NP-AL ; ! ""
Twitter:Twitter NP-ORG-LAT ; ! ""
Warner:Warner NP-ORG-LAT ; ! ""
WikiLeaks:WikiLeaks NP-AL ; ! ""
Wiki:Wiki NP-AL ; ! ""
Witness:Witness NP-AL ; ! ""
Wordpress:Wordpress NP-ORG-LAT ; !"Use/MT"
Wrigley:Wrigley NP-ORG-LAT ; ! ""
Yahoo:Yahoo NP-AL ; ! ""
yahoo:yahoo NP-ORG-LAT ; !"Use/MT"
YouTube:YouTube NP-ORG-LAT ; ! ""
Аарон:Аарон NP-ANT-M ; !"Use/MT"
Аб:Аб NP-ANT-M ; !"Use/MT"
Абай:Абай NP-ANT-M ; ! "Abay"
Абай:Абай NP-ANT-M ; ! "Abay" (Kazakh)
Абай:Абай NP-TOP ; ! ""
Абайділданов:Абайділданов NP-COG-OB ; ! ""
Абайжан:Абайжан NP-ANT-M ; ! ""
Абакан:Абакан NP-TOP ; ! ""
Абалаков:Абалаков NP-COG-OB ; ! "" ! Use/MT
Абат:Абат NP-ANT-M ; ! "Abat" (Arabic)
Аббас:Аббас NP-ANT-M ; ! "Abbas" (Arabic)
Абд:Абд NP-ANT-M ; !"Use/MT"
Абдель:Абдель NP-ANT-M ; !"Use/MT"
Абди:Абди NP-ANT-M ; !"Use/MT"
Абдо:Абдо NP-ANT-M ; !"Use/MT"
Абдолла:Абдолла NP-ANT-M ; !"Use/MT"
Абдолов:Абдолов NP-COG-OB ; ! ""
Абдрахманов:Абдрахманов NP-COG-OB ; ! ""
Абдрашитов:Абдрашитов NP-COG-OB ; ! "" ! Use/MT
Абдужапаров:Абдужапаров NP-COG-OB ; ! ""
Абдул:Абдул NP-ANT-M ; ! "" 
Абдулатипов:Абдулатипов NP-COG-OB ; ! ""
Абдулла:Абдулла NP-ANT-M ; ! "" 
Абдуллаев:Абдуллаев NP-COG-OB ; ! ""
Абель:Абель NP-ANT-M ; !"Use/MT"
Абердин:Абердин NP-TOP ; ! "Aberdeen"
Абердор:Абердор NP-TOP ; ! ""
Абзал:Абзал NP-ANT-M ; ! "Abzal" (Arabic)
Абиджан:Абиджан NP-TOP ; ! ""
Абильдинов:Абильдинов NP-COG-OB ; ! ""
Абира:Абира NP-ANT-F ; ! "Abıyra" (Persian)
Абиссинии:Абиссинии NP-TOP ; !"Use/MT" 
Аблакетка:Аблакетка NP-TOP ; ! ""
Аблякимов:Аблякимов NP-COG-OB ; ! ""
Абрамов:Абрамов NP-COG-OB ; ! ""
Абрахам:Абрахам NP-ANT-M ; !"Use/MT"
Абриал:Абриал NP-COG-MF ; !"Use/MT"
Абрил:Абрил NP-ANT-F ; !"Use/MT"
Абрумов:Абрумов NP-COG-OB ; ! ""
Абруццо:Абруццо NP-TOP ; !"Use/MT"
Абсиех:Абсиех NP-COG-M ; !"Use/MT"
Абубакир:Абубакир NP-ANT-M ; ! "" 
Абуджа:Абуджа NP-TOP ; ! "" 
Абхазия:Абхазия NP-TOP ; ! ""
Абыз:Абыз NP-ANT-M ; ! "Abız" (Arabic)
Абызтөбе:Абызтөбе NP-TOP ; ! ""
Абылай:Абылай NP-ANT-M ; ! "Abılay" (Arabic)
Абылайкит:Абылайкит NP-TOP ; ! ""
Абылғазы:Абылғазы NP-ANT-M ; ! "Abılğazı" (Arabic)
Абыралы:Абыралы NP-TOP ; ! ""
Ав:Ав NP-ANT-M ; !"Use/MT"
Авангард:Авангард NP-AL ; ! ""
Аваров:Аваров NP-COG-OB ; ! ""
Аввакум:Аввакум NP-TOP ; ! ""
Август:Август NP-ANT-M ; !"Use/MT"
Августин:Августин NP-ANT-M ; !"Use/MT"
Авдеев:Авдеев NP-COG-OB ; ! ""
Авейру:Авейру NP-TOP ; !"Use/MT"
Авиа:Авиа NP-ORG ; !"Use/MT"
Авила:Авила NP-TOP ; !"Use/MT"
Авиценна:Авиценна NP-ANT-M ; !"Use/MT"
Авраменко:Авраменко NP-COG-MF ; ! ""
Аврора:Аврора NP-ANT-F ; !"Use/MT"
Австралия:Австралия NP-TOP ; ! ""
Австралия:Австралия NP-TOP ; ! "Australia"
Австрия:Австрия NP-TOP ; ! "Austria"
Австро%-Венгрия:Австро%-Венгрия NP-TOP ; ! ""
Аг:Аг NP-ANT-M ; !"Use/MT"
Агаев:Агаев NP-COG-OB ; ! ""
Агаков:Агаков NP-COG-OB ; ! ""
Агамемнон:Агамемнон NP-ANT-M ; !"Use/MT"
Агар:Агар NP-ANT-F ; !"Use/MT"
Агасси:Агасси NP-COG-MF ; !"Use/MT"
Агата:Агата NP-ANT-F ; !"Use/MT"
Агафонов:Агафонов NP-COG-OB ; ! ""
Агеев:Агеев NP-COG-OB ; ! ""
Агнес:Агнес NP-ANT-F ; !"Use/MT"
Агнесса:Агнесса NP-ANT-F ; ! "Agnessa" (Latin)
Агнешка:Агнешка NP-ANT-F ; !"Use/MT"
Агню:Агню NP-COG-MF ; ! ""
Агостино:Агостино NP-ANT-M ; !"Use/MT"
Агриппина:Агриппина NP-ANT-F ; !"Use/MT"
Агуреев:Агуреев NP-COG-OB ; ! ""
Агустин:Агустин NP-ANT-M ; !"Use/MT"
Ағабек:Ағабек NP-ANT-M ; ! "Ağabek" (Arabic)
Ағадыр:Ағадыр NP-TOP ; ! ""
Ағайсенов:Ағайсенов NP-COG-OB ; ! ""
Ағайша:Ағайша NP-ANT-F ; ! "Ağaysha" 
Ағеділ:Ағеділ NP-TOP ; ! "reka Belaya in Tatarstan/Bashkortstan"
Ағеділ:Ақеділ NP-TOP ; ! "" Dir/LR
Ағзам:Ағзам NP-ANT-M ; ! "Ağzam" (Arabic)
Ағила:Ағила NP-ANT-F ; ! "Ağıyla" (Arabic)
Ағлая:Ағлая NP-ANT-F ; ! "Ağlaya" 
Ағыбайұлы:Ағыбайұлы NP-COG-M ; ! "Use/MT"
Ағын:Ағын NP-ANT-M ; ! "Ağın" (Kazakh)
Ада:Ада NP-ANT-F ; ! "Ada" (Ancient Hebrew)
Адай:Адай NP-ANT-M ; ! "Aday" (Old Turkic)
Адай:Адай NP-TOP ; ! ""
Адам:Адам NP-ANT-M ; ! "Adam" (Ancient Hebrew)
Адамбай:Адамбай NP-ANT-M ; ! "Adambay" (Ancient Hebrew)
Адамбек:Адамбек NP-ANT-M ; ! "Adambek" (Ancient Hebrew)
Адамс:Адамс NP-COG-MF ; !"Use/MT"
Адела:Адела NP-ANT-F ; !"Use/MT"
Аделаиде:Аделаиде NP-ANT-F ; !"Use/MT"
Аделина:Аделина NP-ANT-F ; !"Use/MT"
Аджанта:Аджанта NP-ORG ; !"Use/MT"
Аджип:Аджип NP-AL ; ! ""
Адидже:Адидже NP-TOP ; !"Use/MT"
Адлер:Адлер NP-TOP ; ! ""
Адольф:Адольф NP-ANT-M ; !"Use/MT"
Адонис:Адонис NP-ANT-M ; !"Use/MT"
Адриана:Адриана NP-ANT-F ; !"Use/MT"
Адриан:Адриан NP-ANT-M ; ! "Adrıyan" (Latin)
Адриано:Адриано NP-ANT-M ; !"Use/MT"
Адриат:Адриат NP-TOP ; ! ""
Адриен:Адриен NP-ANT-M ; !"Use/MT"
Адрия:Адрия NP-ANT-F ; !"Use/MT"
Адыраспантөбе:Адыраспантөбе NP-TOP ; ! ""
Адырбай:Адырбай NP-ANT-M ; ! "Adırbay" (Kazakh)
Адырбеков:Адырбеков NP-COG-OB ; ! "" ! Use/MT
Адюков:Адюков NP-COG-OB ; ! ""
Ажар:Ажар NP-ANT-F ; ! "Ajar" (Kazakh)
Ажарбек:Ажарбек NP-ANT-M ; ! "Ajarbek" (Kazakh)
Ажаргүл:Ажаргүл NP-ANT-F ; ! "Ajargül" (Kazakh)
Ажархан:Ажархан NP-ANT-F ; ! "Ajarxan" (Kazakh)
Аза:Аза NP-ANT-F ; ! "Aza" (Persian)
Аз:Аз NP-ANT-M ; !"Use/MT"
Азамат:Азамат NP-ANT-M ; ! "Azamat" (Arabic)
Азаров:Азаров NP-COG-OB ; ! "" ! Use/MT
Азат:Азат NP-ANT-M ; ! "Azat" (Persian)
Азатбан:Азатбан NP-ANT-M ; ! "Azatban" (Persian)
Азатбек:Азатбек NP-ANT-M ; ! "Azatbek" (Persian)
Азеведо:Азеведо NP-COG-MF ; ! "" ! Use/MT 
Азизов:Азизов NP-COG-OB ; ! ""
Азимов:Азимов NP-COG-OB ; ! ""
Азия:Азия NP-TOP ; ! "Asia"
Азнақай:Азнақай NP-TOP ; ! ""
Азов:Азов NP-COG-OB ; ! ""
Аида:Аида NP-ANT-F ; ! "Aida" (Arabic)
Аимии:Аимии NP-ANT-F ; !"Use/MT"
Ай:Ай NP-TOP ; ! ""
Айәділ:Айәділ NP-ANT-M ; ! "Ayädil" (Kazakh)
Айбала:Айбала NP-ANT-F ; ! "Aybala" (Kazakh)
Айбар:Айбар NP-ANT-M ; ! "Aybar" (Kazakh)
Айбарша:Айбарша NP-ANT-F ; ! "Aybarsha" (Kazakh)
Айбас:Айбас NP-ANT-M ; ! "Aybas" (Kazakh)
Айбас:Айбас NP-TOP ; ! ""
Айбас-Қайбас:Айбас-Қайбас NP-TOP ; ! ""
Айбат:Айбат NP-ANT-M ; ! "Aybat" (Kazakh)
Айбек:Айбек NP-ANT-M ; ! "Aybek" (Kazakh)
Айбибе:Айбибе NP-ANT-F ; ! "Aybıybe" (Kazakh)
Айбиби:Айбиби NP-ANT-F ; ! "Aybıybıy" (Kazakh)
Айбике:Айбике NP-ANT-F ; ! "Aybıyke" (Kazakh)
Айбол:Айбол NP-ANT-M ; ! "Aybol" (Kazakh)
Айболат:Айболат NP-ANT-M ; ! "Aybolat" (Kazakh)
Айбын:Айбын NP-ANT-M ; ! "Aybın" (Kazakh)
Айвенов:Айвенов NP-COG-OB ; ! ""
Айвз:Айвз NP-ANT-M ; !"Use/MT"
Айгүл:Айгүл NP-ANT-F ; ! "Aygül" (Kazakh)
Айғали:Айғали NP-ANT-M ; ! "Ayğalıy" (Kazakh)
Айғанша:Айғанша NP-ANT-F ; ! "Ayğansha" (Kazakh)
Айғыз:Айғыз NP-ANT-F ; ! "Ayğız"
Айғыз:Айғыз NP-ANT-F ; ! "Ayğız" (Kazakh)
Айғыз:Айғыз NP-TOP ; ! ""
Айғыркөл:Айғыркөл NP-TOP ; ! ""
Айғырқұм:Айғырқұм NP-TOP ; ! ""
Айдабол:Айдабол NP-TOP ; ! ""
Айдай:Айдай NP-ANT-F ; ! "Ayday" (Kazakh)
Айдана:Айдана NP-ANT-F ; ! "Aydana" (Kazakh)
Айдан:Айдан NP-ANT-M ; ! "Aydan" (Kazakh)
Айдар:Айдар NP-ANT-M ; ! "Aydar" (Kazakh)
Айдарбаев:Айдарбаев NP-COG-OB ; ! ""
Айдарбек:Айдарбек NP-ANT-M ; ! "Aydarbek" (Kazakh)
Айдарғали:Айдарғали NP-ANT-M ; ! "Aydarğalıy" (Arabic)
Айдария:Айдария NP-ANT-F ; ! "Aydarıya" (Kazakh)
Айдаркөл:Айдаркөл NP-TOP ; ! ""
Айдарлы:Айдарлы NP-ANT-M ; ! "Aydarlı" (Kazakh)
Айдарлы:Айдарлы NP-TOP ; ! ""
Айдарлы:Айдарлы NP-TOP ; ! ""
Айдаров:Айдаров NP-COG-OB ; ! ""
Айдархан:Айдархан NP-ANT-M ; ! "Aydarxan" (Kazakh)
Айдархан:Айдархан NP-TOP ; ! ""
Айдахо:Айдахо NP-TOP ; !"Use/MT"
Айдос:Айдос NP-ANT-M ; ! "Aydos" (Kazakh)
Айдос:Айдос NP-TOP ; ! ""
Айдын:Айдын NP-ANT-M ; ! "Aydın" (Kazakh)
Айжамал:Айжамал NP-ANT-F ; ! "Ayjamal" (Kazakh)
Айжан:Айжан NP-ANT-F ; ! "Ayjan" (Kazakh)
Айжан:Айжан NP-TOP ; ! ""
Айжангүл:Айжангүл NP-ANT-F ; ! ""
Айжанов:Айжанов NP-COG-OB ; ! ""
Айжарқын:Айжарқын NP-ANT-M ; ! "Ayjarqın" (Kazakh)
Айжарық:Айжарық NP-ANT-M ; ! "Ayjarıq" (Kazakh)
Айжас:Айжас NP-ANT-M ; ! "Ayjas" (Kazakh)
Айзада:Айзада NP-ANT-F ; ! "Ayzada" (Kazakh)
Айзат:Айзат NP-ANT-M ; ! "Ayzat" (Kazakh)
Айзенберг:Айзенберг NP-COG-MF ; !"Use/MT"
Айзере:Айзере NP-ANT-F ; ! "Ayzere" (Kazakh)
Айзиба:Айзиба NP-ANT-F ; ! "Ayzıyba" (Kazakh)
Айзия:Айзия NP-ANT-F ; ! "Ayzıya" (Kazakh)
Айк:Айк NP-ANT-M ; !"Use/MT"
Айкене:Айкене NP-TOP ; ! ""
Айкүміс:Айкүміс NP-ANT-F ; ! "Aykümis" (Kazakh)
Айкүн:Айкүн NP-ANT-F ; ! "Aykün" (Kazakh)
Айқын:Айқын NP-ANT-M ; ! "Ayqın" (Kazakh)
Айла:Айла NP-ANT-F ; !"Use/MT"
Аймағамбет:Аймағамбет NP-ANT-M ; ! "Aymağambet" (Kazakh)
Аймағанов:Аймағанов NP-COG-OB ; ! ""
Аймақ:Аймақ NP-ANT-M ; ! "Aymaq" (Kazakh)
Айманай:Айманай NP-ANT-F ; ! "Aymanay" (Kazakh)
Айман:Айман NP-ANT-F ; ! "Ayman" (Kazakh)
Аймангүл:Аймангүл NP-ANT-F ; ! "Aymangül" (Kazakh)
Айманов:Айманов NP-COG-OB ; ! ""
Аймаңдай:Аймаңдай NP-AL ; ! ""
Аймауытов:Аймауытов NP-COG-OB ; ! ""
Аймаханов:Аймаханов NP-COG-OB ; ! "" ! Use/MT
Аймира:Аймира NP-ANT-F ; ! "Aymıyra" (Kazakh)
Аймұрат:Аймұрат NP-ANT-M ; ! "Aymurat" (Kazakh)
Айна:Айна NP-ANT-F ; ! "Ayna" (Arabic)
Айнабек:Айнабек NP-ANT-M ; ! "Aynabek" (Kazakh)
Айнабеков:Айнабеков NP-COG-OB ; ! ""
Айнабұлақ:Айнабұлақ NP-TOP ; ! ""
Айнагүл:Айнагүл NP-ANT-F ; ! "Aynagül" (Persian)
Айнағұлов:Айнағұлов NP-COG-OB ; ! ""
Айнажан:Айнажан NP-ANT-F ; ! "Aynajan" (Persian)
Айназия:Айназия NP-ANT-F ; ! "Aynazıya" (Persian)
Айнан:Айнан NP-ANT-M ; ! "Aynan" (Arabic)
Айнанур:Айнанур NP-ANT-F ; ! "Aynanıwr" (Persian)
Айнаша:Айнаша NP-ANT-F ; ! "Aynasha" (Kazakh)
Айнұр:Айнұр NP-ANT-F ; ! "Aynur" (Kazakh)
Айова:Айова NP-TOP ; ! ""
Айпара:Айпара NP-ANT-F ; ! "Aypara" (Kazakh)
Айрат:Айрат NP-ANT-M ; ! "Ayrat" (Arabic)
Айса:Айса NP-ANT-M ; ! "Aysa" (Arabic)
Айсары:Айсары NP-TOP ; ! ""
Айсауле:Айсауле NP-ANT-F ; ! "Aysawle" 
Айсауыт:Айсауыт NP-ANT-M ; ! "Aysawıt" (Kazakh)
Айсұлу:Айсұлу NP-ANT-F ; ! "Aysulıw" 
Айсүгір:Айсүгір NP-ANT-M ; ! "Aysügir" (Kazakh)
Айтақын:Айтақын NP-ANT-M ; ! "Aytaqın" (Arabic)
Айтаң:Айтаң NP-ANT-M ; ! "Aytaŋ" (Kazakh)
Айтас:Айтас NP-ANT-M ; ! "Aytas" (Kazakh)
Айтау:Айтау NP-TOP ; ! ""
Айтбаев:Айтбаев NP-COG-OB ; ! ""
Айтбай:Айтбай NP-ANT-M ; ! "Aytbay" (Arabic)
Айтбек:Айтбек NP-ANT-M ; ! "Aytbek" (Arabic)
Айтемір:Айтемір NP-ANT-M ; ! "Aytemir" (Kazakh)
Айтжан:Айтжан NP-ANT-M ; ! "Aytjan" (Arabic)
Айтқали:Айтқали NP-ANT-M ; ! "Aytqalıy" (Arabic)
Айтқұлов:Айтқұлов NP-COG-OB ; ! ""
Айтматов:Айтматов NP-COG-OB ; ! ""
Айтмұхаметов:Айтмұхаметов NP-COG-OB ; ! ""
Айтуар:Айтуар NP-ANT-M ; ! "Aytıwar" (Kazakh)
Айтуған:Айтуған NP-ANT-M ; ! "Aytıwğan" (Kazakh)
Айтуды:Айтуды NP-ANT-M ; ! "Aytıwdı" (Arabic)
Айша:Айша NP-ANT-F ; ! "Aysha" (Arabic)
Айша:Айша NP-ANT-F ; !"Use/MT"
Айшаиым:Айшаиым NP-ANT-F ; ! "Ayshaiım" (Kazakh)
Айшолпан:Айшолпан NP-ANT-F ; ! "Aysholpan" (Kazakh)
Айшуақ:Айшуақ NP-ANT-M ; ! "Ayshıwaq" (Kazakh)
Айшуақов:Айшуақов NP-COG-OB ; ! ""
Айшық:Айшық NP-ANT-M ; ! "Ayshıq" (Kazakh)
Айым:Айым NP-ANT-F ; ! "Ayım" (Kazakh)
Айымбетов:Айымбетов NP-COG-OB ; ! "" ! Use/MT
Айымгүл:Айымгүл NP-ANT-F ; ! "Ayımgül" (Kazakh)
Айымжан:Айымжан NP-ANT-F ; ! "Ayımjan" 
Айымша:Айымша NP-ANT-F ; ! "Ayımsha" 
Айырқызыл:Айырқызыл NP-TOP ; ! ""
Айыртау:Айыртау NP-TOP ; ! ""
Айыртау:Айыртау NP-TOP ; ! ""
Ак:Ак NP-ANT-M ; !"Use/MT"
Акеев:Акеев NP-COG-OB ; ! "" ! Use/MT
Акеми:Акеми NP-ANT-F ; !"Use/MT"
Акира:Акира NP-ANT-F ; !"Use/MT"
Акколь:Акколь NP-TOP ; ! ""
Аккрингтон:Аккрингтон NP-TOP ; ! "Accrington"
Аккурган:Аккурган NP-TOP ; ! ""
Аксай:Аксай NP-TOP ; ! ""
Аксаков:Аксаков NP-COG-OB ; ! ""
Аксёнов:Аксёнов NP-COG-OB ; ! ""
Ак%-Су:Ак%-Су NP-TOP ; ! ""
Аксум:Аксум NP-TOP ; ! ""
Ақаев:Ақаев NP-COG-OB ; ! ""
Ақай:Ақай NP-ANT-M ; ! "Aqay" (Kazakh)
Ақанаев:Ақанаев NP-COG-OB ; ! "" ! Use/MT
Ақан:Ақан NP-ANT-M ; ! ""
Ақанбарақ:Ақанбарақ NP-TOP ; ! ""
Ақанов:Ақанов NP-COG-OB ; ! "" ! Use/MT
Ақарал:Ақарал NP-TOP ; ! ""
Ақатұлы:Ақатұлы NP-COG-M ; ! ""
Ақәжетай:Ақәжетай NP-ANT-F ; ! ""
Ақбай:Ақбай NP-ANT-M ; ! "Aqbay" (Kazakh)
Ақбақай:Ақбақай NP-TOP ; ! ""
Ақбала:Ақбала NP-ANT-F ; ! "Aqbala" (Kazakh)
Ақбастау:Ақбастау NP-TOP ; ! ""
Ақбасты:Ақбасты NP-TOP ; ! ""
Ақбатес:Ақбатес NP-ANT-F ; ! "Aqbates" (Russian)
Ақбауыр:Ақбауыр NP-TOP ; ! ""
Ақбейіт:Ақбейіт NP-TOP ; ! ""
Ақберді:Ақберді NP-ANT-M ; ! "Aqberdi" (Kazakh)
Ақбет:Ақбет NP-TOP ; ! ""
Ақбешім:Ақбешім NP-TOP ; ! ""
Ақболат:Ақболат NP-ANT-M ; ! "Aqbolat" (Kazakh)
Ақбота:Ақбота NP-ANT-F ; ! "Aqbota" 
Ақбұлақ:Ақбұлақ NP-TOP ; ! ""
Ақбұлым:Ақбұлым NP-TOP ; ! ""
Ақдала:Ақдала NP-TOP ; ! ""
Ақдаласор:Ақдаласор NP-TOP ; ! ""
Ақдөң:Ақдөң NP-TOP ; ! ""
Ақеділ:Ақеділ NP-ANT-M ; ! "Aqedil" (Kazakh)
Ақеспе:Ақеспе NP-TOP ; ! ""
Ақжайқын:Ақжайқын NP-TOP ; ! ""
Ақжайық:Ақжайық NP-TOP ; ! ""
Ақжал:Ақжал NP-TOP ; ! ""
Ақжан:Ақжан NP-ANT-M ; ! "Aqjan" (Kazakh)
Ақжан:Ақжан NP-TOP ; ! ""
Ақжанат:Ақжанат NP-ANT-M ; ! ""
Ақжар:Ақжар NP-TOP ; ! ""
Ақжарқын:Ақжарқын NP-ANT-M ; ! "Aqjarqın" (Kazakh)
Ақжарма:Ақжарма NP-TOP ; ! ""
Ақжелке:Ақжелке NP-ANT-M ; ! ""
Ақжол:Ақжол NP-ANT-M ; ! "Aqjol" (Kazakh)
Ақжігіт:Ақжігіт NP-ANT-M ; ! "Aqjigit" (Kazakh)
Ақкесене:Ақкесене NP-TOP ; ! ""
Ақкиізтоғай:Ақкиізтоғай NP-TOP ; ! ""
Ақкөл:Ақкөл NP-TOP ; ! ""
Ақкөл:Ақкөл NP-TOP ; ! ""
Аққаба:Аққаба NP-TOP ; ! ""
Аққайнар:Аққайнар NP-TOP ; ! ""
Аққайың:Аққайың NP-TOP ; ! ""
Аққал:Аққал NP-ANT-F ; ! "Aqqal" (Persian)
Аққал:Аққал NP-ANT-M ; ! "Aqqal" (Arabic)
Аққанбұрлық:Аққанбұрлық NP-TOP ; ! ""
Аққасқа:Аққасқа NP-AL ; ! ""
Аққожа:Аққожа NP-ANT-M ; ! "Aqqoja" (Kazakh)
Аққозы:Аққозы NP-ANT-M ; ! "Aqqozı" (Kazakh)
Аққоңыр:Аққоңыр NP-TOP ; ! ""
Аққорған:Аққорған NP-TOP ; ! ""
Аққу:Аққу NP-AL ; ! ""
Аққу:Аққу NP-TOP ; ! ""
Аққұдық:Аққұдық NP-TOP ; ! ""
Аққұлы:Аққұлы NP-COG-MF ; ! ""
Аққұм:Аққұм NP-TOP ; ! ""
Аққұяш:Аққұяш NP-TOP ; ! ""
Аққыз:Аққыз NP-ANT-F ; ! "Aqqız" 
Аққыр:Аққыр NP-TOP ; ! ""
Аққыстау:Аққыстау NP-TOP ; ! ""
Ақлима:Ақлима NP-ANT-F ; ! "Aqlıyma" (Arabic)
Ақмарал:Ақмарал NP-ANT-F ; ! "Aqmaral" (Arabic)
Ақмая:Ақмая NP-TOP ; ! ""
Ақмәмбет:Ақмәмбет NP-TOP ; ! ""
Ақмешіт:Ақмешіт NP-TOP ; ! ""
Ақмешіт:Ақмешіт NP-TOP ; ! "old name of Қызылорда" 
Ақмола:Ақмола NP-TOP ; ! ""
Ақмола:Ақмола NP-TOP ; ! ""
Ақназар:Ақназар NP-ANT-M ; ! "Aqnazar" (Arabic)
Ақоба:Ақоба NP-TOP ; ! ""
Ақорда:Ақорда NP-TOP ; ! ""
Ақпанбет:Ақпанбет NP-ANT-M ; ! "Aqpanbet" (Kazakh)
Ақпар:Ақпар NP-ANT-M ; ! "Aqpar" (Arabic)
ақпараттану:ақпараттану N1 ; !"Use/MT"
Ақсай:Ақсай NP-TOP ; ! ""
Ақсақалов:Ақсақалов NP-COG-OB ; ! ""
Ақсейіт:Ақсейіт NP-TOP ; ! ""
Ақсораң:Ақсораң NP-TOP ; ! ""
Ақсу:Ақсу NP-TOP ; ! ""
Ақсу:Ақсу NP-TOP ; ! ""
Ақсуат:Ақсуат NP-TOP ; ! ""
Ақсу%-Аюлы:Ақсу%-Аюлы NP-TOP ; ! ""
Ақсубай:Ақсубай NP-TOP ; ! ""
Ақсұлтан:Ақсұлтан NP-ANT-M ; ! "Aqsultan" (Kazakh)
Ақсүйек:Ақсүйек NP-TOP ; ! ""
Ақсүмбе:Ақсүмбе NP-TOP ; ! ""
Ақтай:Ақтай NP-TOP ; ! ""
Ақтайлақ:Ақтайлақ NP-TOP ; ! ""
Ақтамақ:Ақтамақ NP-ANT-F ; ! "Aqtamaq" 
Ақтам:Ақтам NP-TOP ; ! ""
Ақтаныш:Ақтаныш NP-TOP ; ! ""
Ақтаң:Ақтаң NP-ANT-M ; ! "Aqtaŋ" (Kazakh)
Ақтас:Ақтас NP-TOP ; ! ""
Ақтасты:Ақтасты NP-TOP ; ! ""
Ақтастысай:Ақтастысай NP-TOP ; ! ""
Ақтау:Ақтау NP-TOP ; ! ""
Ақтау:Ақтау NP-TOP ; ! "Aqtaw"
Ақтерек:Ақтерек NP-TOP ; ! ""
Ақтоғай:Ақтоғай NP-TOP ; ! ""
Ақтоған:Ақтоған NP-TOP ; ! ""
Ақтоқты:Ақтоқты NP-ANT-F ; ! "Aqtoqtı" 
Ақтөбе:Ақтөбе NP-TOP ; ! ""
Ақтөбе:Ақтөбе NP-TOP ; ! "Aqtöbe"
Ақтөбемұнайгаз:Ақтөбемұнайгаз NP-ORG ; ! ""
Ақтөбемұнайгаз:Ақ% төбемұнайгаз NP-ORG ; ! "" ! Dir/LR
Ақтөбемұнайгаз:Ақ% төбемұнайгаз NP-ORG ; ! "" ! Dir/LR
Ақтөбемұнайгаз:СНПС%-Ақтөбемұнайгаз NP-ORG ; ! "" ! Dir/LR
Ақтүбек:Ақтүбек NP-TOP ; ! ""
Ақтілек:Ақтілек NP-ANT-M ; ! "Aqtilek" (Kazakh)
АҚШ:АҚШ%{а%}%{с%} NP-TOP-ABBR ; ! "USA"
Ақшатау:Ақшатау NP-TOP ; ! ""
Ақшәулі:Ақшәулі NP-TOP ; ! ""
Ақшеңгелді:Ақшеңгелді NP-TOP ; ! ""
Ақши:Ақши NP-TOP ; ! ""
Ақшоқы:Ақшоқы NP-TOP ; ! ""
Ақшора:Ақшора NP-ANT-M ; ! "Aqshora" (Kazakh)
Ақылбай:Ақылбай NP-ANT-M ; ! "Aqılbay" (Arabic)
Ақылбек:Ақылбек NP-ANT-M ; ! "Aqılbek" (Arabic)
Ақын:Ақын NP-ANT-M ; ! "Aqın" (Persian)
Ақынжан:Ақынжан NP-ANT-M ; ! "Aqınjan" (Arabic)
Ақынжанов:Ақынжанов NP-COG-OB ; ! ""
Ақыраб:Ақыраб NP-TOP ; ! ""
Ақырап:Ақырап NP-ANT-M ; ! "Aqırap" (Arabic)
Ақыртас:Ақыртас NP-TOP ; ! ""
Ақыртөбе:Ақыртөбе NP-TOP ; ! ""
Ақышев:Ақышев NP-COG-OB ; ! ""
Алабама:Алабама NP-TOP ; !"Use/MT"
Алабота:Алабота NP-TOP ; ! ""
Алабуға:Алабуға NP-TOP ; ! ""
Алай:Алай NP-ANT-M ; ! "Alay" (Old Turkic)
Алай:Алай NP-TOP ; ! "Alay"
Алай:Алай NP-TOP ; ! ""
Алайн:Алайн NP-ANT-M ; !"Use/MT"
Алакөл:Алакөл NP-TOP ; ! ""
Алакөл:Алакөл NP-TOP ; ! ""
Алақын:Алақын NP-ANT-M ; ! "Alaqın" (Persian)
Аламесек:Аламесек NP-TOP ; ! ""
Аламида:Аламида NP-TOP ; ! "Alameda"
Алан:Алан NP-ANT-M ; ! ""
Алангов:Алангов NP-COG-OB ; ! ""
Аланис:Аланис NP-ANT-F ; !"Use/MT"
Аланов:Аланов NP-COG-OB ; ! ""
Аларик:Аларик NP-ANT-M ; ! ""
Алатау:Алатау NP-TOP ; ! ""
Алатау:Алатау NP-TOP ; ! ""
Алатөбе:Алатөбе NP-TOP ; ! ""
Алау:Алау NP-ANT-M ; ! "Alaw" (Kazakh)
Алаш:Алаш NP-TOP ; ! ""
Алашбеков:Алашбеков NP-COG-OB ; ! "" ! Use/MT
Алашорда:Алашорда NP-TOP ; ! ""
Албания:Албания NP-TOP ; ! "Albania"
Албат:Албат NP-ANT-M ; ! "Albat" (Arabic)
Алвар:Алвар NP-ANT-M ; !"Use/MT"
Алвис:Алвис NP-ANT-M ; !"Use/MT"
Алгереев:Алгереев NP-COG-OB ; ! ""
Алға:Алға NP-ANT-M ; ! "Alğa" (Kazakh)
Алға:Алға NP-TOP ; ! ""
Алғабас:Алғабас NP-TOP ; ! ""
Алғазы:Алғазы NP-TOP ; ! ""
Алғыр:Алғыр NP-ANT-M ; ! "Alğır" (Kazakh)
Алдабергенов:Алдабергенов NP-COG-OB ; ! ""
Алдабергенов:Алдабергенов NP-TOP ; ! ""
Алдажаров:Алдажаров NP-COG-OB ; ! ""
Алдамұратов:Алдамұратов NP-COG-OB ; ! ""
Алдан:Алдан NP-ANT-M ; ! "Aldan" (Kazakh)
Алдар:Алдар NP-ANT-M ; ! "Aldar" (Old Turkic)
Алдар% Көсе:Алдар% Көсе NP-ANT-M ; ! "Aldar kose" USE/MT
Алек:Алек NP-ANT-M ; !"Use/MT"
Алекса:Алекса NP-ANT-F ; !"Use/MT"
Алекс:Алекс NP-ANT-F ; !"Use/MT"
Алекс:Алекс NP-ANT-M ; !"Use/MT"
Александра:Александра NP-ANT-F ; !"Use/MT"
Александр:Александр NP-ANT-M ; ! "Alexander"
Александрия:Александрия NP-ANT-F ; !"Use/MT"
Александрия:Александрия NP-TOP ; ! ""
Александров:Александров NP-COG-OB ; ! ""
Александрович:Александро NP-PAT-VICH ; ! ""
Александровск:Александровск NP-TOP ; !""
Александрос:Александрос NP-ANT-M ; !"Use/MT"
Алексеев:Алексеев NP-COG-OB ; ! ""
Алексеевич:Алексее NP-PAT-VICH ; ! ""
Алексеевка:Алексеевка NP-TOP ; ! ""
Алексеевск:Алексеевск NP-TOP ; ! ""
Алексей:Алексей NP-ANT-M ; ! "" 
Алексия:Алексия NP-ANT-F ; !"Use/MT"
Ален:Ален NP-ANT-M ; !"Use/MT"
Ален:Ален NP-TOP ; ! "Aalen"
Алентежу:Алентежу NP-TOP ; !"Use/MT"
Алепауыл:Алепауыл NP-TOP ; ! ""
Алессандро:Алессандро NP-ANT-M ; ! ""
Алехандро:Алехандро NP-ANT-M ; !"Use/MT"
Алехо:Алехо NP-ANT-M ; !"Use/MT"
Алёна:Алёна NP-ANT-F ; ! ""
Алёна:Алёна NP-ANT-F ; ! ""
Алжир:Алжир NP-TOP ; ! "Algeria"
Али:Али NP-ANT-M ; !"Use/MT"
Алиев:Алиев NP-COG-OB ; ! "" ! Use/MT
Ализа:Ализа NP-ANT-F ; !"Use/MT"
Алик:Алик NP-ANT-M ; ! "" 
Алимасов:Алимасов NP-COG-OB ; ! ""
Алина:Алина NP-ANT-F ; ! ""
Алипинов:Алипинов NP-COG-OB ; ! ""
Алиса:Алиса NP-ANT-F ; ! ""
Алисия:Алисия NP-ANT-F ; !"Use/MT"
Алисса:Алисса NP-ANT-F ; !"Use/MT"
Алита:Алита NP-ANT-F ; !"Use/MT"
Алия:Алия NP-ANT-F ; !"Use/MT"
Алкей:Алкей NP-ANT-M ; ! ""
Алқа:Алқа NP-ANT-F ; ! "Alqa" (Kazakh)
Алқабек:Алқабек NP-TOP ; ! ""
Алқагүл:Алқагүл NP-ANT-F ; ! "Alqagül" 
Алқакөлқұм:Алқакөлқұм NP-TOP ; ! ""
Алқамерген:Алқамерген NP-TOP ; ! ""
Алқатерек:Алқатерек NP-TOP ; ! ""
Аллан:Аллан NP-ANT-M ; !"Use/MT"
Аллах:Аллах NP-ANT-M ; !"Use/MT"
Аллен:Аллен NP-ANT-M ; !"Use/MT"
Алма:Алма NP-ANT-F ; ! "Alma" (Kazakh)
Алмаарасан:Алмаарасан NP-TOP ; ! ""
Алмабай:Алмабай NP-ANT-M ; ! "Almabay" (Kazakh)
Алмабек:Алмабек NP-ANT-M ; ! "Almabek" (Kazakh)
Алмабеков:Алмабеков NP-COG-OB ; ! ""
Алмагүл:Алмагүл NP-ANT-F ; ! "Almagül" (Kazakh)
Алмажан:Алмажан NP-ANT-F ; ! "Almajan" (Kazakh)
Алмаз:Алмаз NP-ANT-M ; ! "" ! Use/MT
Алмазбек:Алмазбек NP-ANT-M ; ! "" ! Use/MT
Алмалы:Алмалы NP-TOP ; ! ""
Алмалыбақ:Алмалыбақ NP-TOP ; ! ""
Алмалық:Алмалық NP-TOP ; ! ""
Алмания:Алмания NP-TOP ; ! "Germany"
Алмас:Алмас NP-ANT-F; !"Use/MT"
Алмас:Алмас NP-ANT-M ; ! "Almas" (Persian)
Алмасбай:Алмасбай NP-ANT-M ; ! "Almasbay" (Persian)
Алмасбек:Алмасбек NP-ANT-M ; ! "Almasbek" (Persian)
Алмасхан:Алмасхан NP-ANT-M ; ! "Almasxan" (Persian)
Алматай:Алматай NP-ANT-M ; ! "Almatay" (Kazakh)
Алматов:Алматов NP-COG-OB ; ! ""
Алмату:Алмату NP-TOP ; ! ""
Алматы:Алмата NP-TOP ; ! "Almaty" ! Dir/LR
Алматы:Алматы NP-TOP ; ! ""
Алматы:Алматы NP-TOP ; ! "Almaty"
Алмахан:Алмахан NP-ANT-F ; ! "Almaxan" (Kazakh)
Алмұрт:Алмұрт NP-ANT-M ; ! "Almurt" (Greek)
Алпамыс:Алпамыс NP-ANT-M ; ! "Alpamıs" (Old Turkic)
Алпамыс:Алпамыс NP-ANT-M  ; !"Use/MT"
Алпар:Алпар NP-ANT-M ; ! "Alpar" (Arabic)
Алпысбаев:Алпысбаев NP-COG-OB ; ! ""
Алпысбай:Алпысбай NP-ANT-M ; ! "Alpısbay" (Kazakh)
Алтай:Алтай NP-ANT-M ; ! "Altay" 
Алтай:Алтай NP-TOP ; ! ""
Алтай:Алтай NP-TOP ; ! ""
Алтыбай:Алтыбай NP-ANT-M ; ! "Altıbay" (Kazakh)
Алтыбай:Алтыбай NP-TOP ; ! ""
Алтыбайсор:Алтыбайсор NP-TOP ; ! ""
Алтықарасу:Алтықарасу NP-TOP ; ! ""
Алтынай:Алтынай NP-ANT-F ; ! "Altınay" (Kazakh)
Алтын:Алтын NP-ANT-F ; ! "Altın" (Kazakh)
Алтынасар:Алтынасар NP-TOP ; ! ""
Алтынбаев:Алтынбаев NP-COG-OB ; ! ""
Алтынбек:Алтынбек NP-ANT-M ; ! "Altınbek" (Kazakh)
Алтынгүл:Алтынгүл NP-ANT-F ; ! "Altıngül" (Kazakh)
Алтынемел:Алтынемел NP-TOP ; ! ""
Алтын% Қыран:Алтын% Қыран NP-AL ; ! ""
Алтын% орда:Алтын% орда NP-TOP ; ! ""
Алтынсарин:Алтынсарин NP-TOP ; ! ""
Алтынсары:Алтынсары NP-ANT-M ; ! "Altınsarı" (Kazakh)
Алтынтау:Алтынтау NP-TOP ; ! ""
Алтынтөбе:Алтынтөбе NP-TOP ; ! ""
Алтыншаш:Алтыншаш NP-ANT-F ; ! "Altınshash" (Kazakh)
Алтыүй:Алтыүй NP-TOP ; ! ""
Алтышаһар:Алтышаһар NP-TOP ; ! ""
Алуа:Алуа NP-ANT-F ; ! "Alıwa" (Arabic)
Алшымбаев:Алшымбаев NP-COG-OB ; ! ""
Алшынбай:Алшынбай NP-ANT-M ; ! "Alşınbay"
Алымов:Алымов NP-COG-OB ; ! ""
Аль:Аль NP-ANT-M ; !"Use/MT"
Альба:Альба NP-ANT-F ; !"Use/MT"
Альбано:Альбано NP-ANT-M ; !"Use/MT"
Альберта:Альберта NP-TOP ; ! "Alberta"
Альберт:Альберт NP-ANT-M ; ! ""
Альберто:Альберто NP-ANT-M ; !"Use/MT"
Альбина:Альбина NP-ANT-F ; ! ""
Альбион:Альбион NP-TOP ; ! ""
Альбукерке:Альбукерке NP-TOP ; !"Use/MT"
Альваро:Альваро NP-ANT-M ; !"Use/MT"
Альден:Альден NP-ANT-M ; !"Use/MT"
Альдо:Альдо NP-ANT-M ; !"Use/MT"
Аль% Капоне:Аль% Капоне NP-COG-MF ; !"Use/MT"
Альмуния:Альмуния NP-COG-MF ; !"Use/MT"
Альмяшев:Альмяшев NP-COG-OB ; ! ""
Альпі:Альпі NP-TOP ; !"Use/MT"
Альта:Альта NP-ANT-F ; !"Use/MT"
Альтов:Альтов NP-COG-OB ; ! ""
Альфонс:Альфонс NP-ANT-M ; !"Use/MT"
Альфред:Альфред NP-ANT-M ; ! ""
Альфред:Альфред NP-ANT-M ; ! ""
Альфредо:Альфредо NP-ANT-M ; !"Use/MT"
Альхазен:Альхазен NP-ANT-M ; !"Use/MT"
Аляска:Аляска NP-TOP ; ! ""
Амадеус:Амадеус NP-ANT-M ; !"Use/MT"
Амадо:Амадо NP-ANT-M ; !"Use/MT"
Амазонка:Амазонка NP-TOP ; ! ""
Амалдық:Амалдық NP-ANT-M ; !"Use/MT"
Амалия:Амалия NP-ANT-F ; !"Use/MT"
Амальрик:Амальрик NP-ANT-M ; !
Амальрик:Амальрик NP-COG-MF ; ! ""
Аман:Аман NP-ANT-F ; ! "Aman" (Arabic)
Аман:Аман NP-ANT-M ; ! "Aman" (Arabic)
Аманбаев:Аманбаев NP-COG-OB ; ! ""
Аманбай:Аманбай NP-ANT-M ; ! "Amanbay" (Arabic)
Аманбай:Аманбай NP-TOP ; ! ""
Аманбала:Аманбала NP-ANT-F ; ! "Amanbala" (Arabic)
Аманбек:Аманбек NP-ANT-M ; ! "Amanbek" (Arabic)
Амангелді:Амангелді NP-ANT-M ; ! "" 
Амангелді:Амангелді NP-TOP ; ! ""
Амангелдіұлы:Амангелдіұлы NP-ANT-M ; !
Амангүл:Амангүл NP-ANT-F ; ! "Amangül" (Arabic)
Аманғайша:Аманғайша NP-ANT-F ; ! "Amanğaysha" (Arabic)
Аманда:Аманда NP-ANT-F ; !"Use/MT"
Аманджулов:Аманджулов NP-COG-OB ; ! ""
Амандос:Амандос NP-ANT-M ; ! "" ! Use/MT
Амандосов:Амандосов NP-COG-OB ; ! ""
Амандык:Амандык NP-ANT-M ; ! "Amandık" (Arabic)
Аманжол:Аманжол NP-ANT-M ; ! "Amanjol" (Arabic)
Аманжол:Аманжол NP-TOP ; ! ""
Аманжолов:Аманжолов NP-COG-OB ; ! ""
Аманиязов:Аманиязов NP-COG-OB ; ! ""
Аманкелді:Аманкелді NP-ANT-M ; ! "Amankeldi" (Kazakh)
Аманқали:Аманқали NP-ANT-M ; ! "Amanqalıy" (Arabic)
Аманқарағай:Аманқарағай NP-TOP ; ! ""
Аманов:Аманов NP-COG-OB ; ! ""
Аманөткел:Аманөткел NP-TOP ; ! ""
Амантай:Амантай NP-ANT-M ; ! "USE/MT"
Амантоғай:Амантоғай NP-TOP ; ! ""
Амаро:Амаро NP-ANT-M ; !"Use/MT"
Амбосели:Амбосели NP-TOP ; ! ""
Амброуз:Амброуз NP-ANT-M ; !"Use/MT"
Амели:Амели NP-ANT-F ; !"Use/MT"
Амелия:Амелия NP-ANT-F ; !"Use/MT"
Америка:Америка NP-TOP ; ! "America"
Аминев:Аминев NP-COG-OB ; ! ""
Амира:Амира NP-ANT-F ; !"Use/MT"
Амир:Амир NP-ANT-M ; !"Use/MT"
Амиров:Амиров NP-COG-OB ; ! ""
Амман:Амман NP-TOP ; !"Use/MT"
Амнести% Интернэшнл:Амнести% Интернэшнл NP-ORG ; ! ""
Амстердам:Амстердам NP-TOP ; ! ""
Амстердам:Амстердам NP-TOP ; ! "" 
Амур:Амур NP-TOP ; ! ""
Амьен:Амьен NP-TOP ; !""
Ана:Ана NP-ANT-F ; !"Use/MT"
Анадолы:Анадолы NP-TOP ; ! ""
Анаксагор:Анаксагор NP-ANT-M ; !
Ананьев:Ананьев NP-COG-OB ; ! ""
Анар:Анар NP-ANT-F ; ! "Anar" (Arabic)
Анар:Анар NP-TOP ; ! ""
Анаргүл:Анаргүл NP-ANT-F ; ! "Anargül" (Arabic)
Анаржан:Анаржан NP-ANT-F ; ! "Anarjan" (Arabic)
Анархан:Анархан NP-ANT-F ; ! "Anarxan" (Arabic)
Анастасия:Анастасия NP-ANT-F ; ! ""
Анатолий:Анатолий NP-ANT-M ; ! "" 
Анатольевич:Анатолье NP-PAT-VICH ; ! ""
Ангела:Ангела NP-ANT-F ; ! "" 
Ангелина:Ангелина NP-ANT-F ; ! ""
Ангелино:Ангелино NP-ANT-M ; !"Use/MT"
Ангелов:Ангелов NP-COG-OB ; !"Use/MT"
Ангилья:Ангилья NP-TOP ; !""
Ангилья:Ангилья NP-TOP ; ! "Anguilla"
Англия:Англия NP-TOP ; ! "England"
Ангола:Ангола NP-TOP ; ! ""
Андалусия:Андалусия NP-TOP ; ! "Andalusia"
Анд:Анд NP-TOP ; ! ""
Андасай:Андасай NP-TOP ; ! ""
Андерсен:Андерсен NP-ANT-M ; !"Use/MT"
Андерсон:Андерсон NP-ANT-M ; !"Use/MT"
Андес:Андес NP-ANT-M ; !"Use/MT"
Анджелина:Анджелина NP-ANT-F ; !"Use/MT"
Анджело:Анджело NP-ANT-M ; !"Use/MT"
Андорра:Андорра NP-TOP ; ! ""
Андорра%-ла%-Велья:Андорра%-ла%-Велья NP-TOP ; ! ""
Андорра%-ла%-Велья:Андорра%-ла%-Велья NP-TOP ; ! "" 
Андреа:Андреа NP-ANT-F ; !"Use/MT"
Андре:Андре NP-ANT-M ; ! ""
Андреас:Андреас NP-ANT-M ; !"Use/MT"
Андреев:Андреев NP-COG-OB ; ! ""
Андреевич:Андрее NP-PAT-VICH ; ! ""
Андрей:Андрей NP-ANT-M ; ! "" 
Андрес:Андрес NP-ANT-M ; !"Use/MT"
Андрис:Андрис NP-COG-MF ; ! ""
Андромеда:Андромеда NP-AL ; ! ""
Андромеда:Андромеда NP-TOP ; !"Use/MT"
Андронов:Андронов NP-COG-OB ; ! ""
Андропов:Андропов NP-COG-OB ; ! ""
Андропов:Андропов NP-TOP ; ! ""
Анжела:Анжела NP-ANT-F ; ! ""
Аника:Аника NP-ANT-F ; !"Use/MT"
Аниса:Аниса NP-ANT-F ; ! "Anıysa" (Arabic)
Анисимов:Анисимов NP-COG-OB ; ! ""
Анита:Анита NP-ANT-F ; !"Use/MT"
Анищенко:Анищенко NP-COG-MF ; ! ""
Анкара:Анкара NP-TOP ; ! "Ankara"
Анна:Анна NP-ANT-F ; ! "Anna" (Ancient Hebrew)
Аннамұрадов:Аннамұрадов NP-COG-OB ; ! ""
Анненков:Анненков NP-COG-OB ; ! ""
Аннет:Аннет NP-ANT-F ; !"Use/MT"
Анри:Анри NP-ANT-M ; !"Use/MT"
Ансальдо:Ансальдо NP-ANT-M ; !"Use/MT"
Антананариву:Антананариву NP-TOP ; ! "" 
Антанас:Антанас NP-ANT-M ; ! ""
Антарктида:Антарктида NP-TOP ; ! ""
Анте:Анте NP-ANT-M ; !"Use/MT"
Антигуа:Антигуа NP-TOP ; ! "Antigua"
Антигуа% және% Барбуда:Антигуа% және% Барбуда NP-TOP ; ! ""
Антиллы:Антиллы NP-TOP ; !""
Антиль:Антиль NP-TOP ; ! ""
Антиль% аралдары:Антиль% аралдар NP-TOP-COMPOUND ; ! ""
Антипов:Антипов NP-COG-OB ; ! ""
Антон:Антон NP-ANT-M ; ! ""
Антоненко:Антоненко NP-COG-MF ; ! ""
Антоний:Антоний NP-ANT-M ; !"Use/MT"
Антонио:Антонио NP-ANT-M ; !"Use/MT"
Антонов:Антонов NP-COG-OB ; ! ""
Антуан:Антуан NP-ANT-M ; !"Use/MT"
Антуанетта:Антуанетта NP-ANT-F ; !"Use/MT"
Анук:Анук NP-ANT-F ; !"Use/MT"
Анфиса:Анфиса NP-ANT-F ; ! "Anfisa" (Greek)
Анхальт:Анхальт NP-TOP ; !"Use/MT"
Аня:Аня NP-ANT-F ; ! ""
Аңдас:Аңдас NP-ANT-M ; ! "Aŋdas" (Arabic)
Аңсаған:Аңсаған NP-ANT-M ; ! "Aŋsağan" (Kazakh)
Аңсар:Аңсар NP-ANT-M ; ! "Aŋsar" (Kazakh)
Аңырақай:Аңырақай NP-TOP ; ! ""
Апас:Апас NP-TOP ; ! ""
Апиа:Апиа NP-TOP ; !"Use/MT"
Аполло:Аполло NP-AL ; ! ""
Аполло:Аполло NP-ANT-M ; !"Use/MT"
Аполлон:Аполлон NP-AL ; ! ""
Аппақ:Аппақ NP-ANT-M ; ! "Appaq" (Kazakh)
Аппанов:Аппанов NP-COG-OB ; ! ""
Апсалықов:Апсалықов NP-COG-OB ; ! "" ! Use/MT
Апулия:Апулия NP-TOP ; !"Use/MT"
Арабия:Арабия NP-TOP ; ! ""
Аравия:Аравия NP-TOP ; !"Use/MT"
Арагон:Арагон NP-TOP ; !"Use/MT"
Арай:Арай NP-ANT-M ; ! "Aray" (Kazakh)
Арай:Арай NP-TOP ; ! ""
Арайша:Арайша NP-ANT-F ; ! "Araysha" (Kazakh)
Аракс:Аракс NP-TOP ; ! ""
Арақарағай:Арақарағай NP-TOP ; ! ""
Арал:Арал NP-TOP ; ! ""
Арал:Арал NP-TOP ; ! ""
Аралбаев:Аралбаев NP-COG-OB ; ! ""
Аралбай:Аралбай NP-ANT-M ; ! "Aralbay" (Kazakh)
Аралқұм:Аралқұм NP-TOP ; ! ""
Аралсор:Аралсор NP-TOP ; ! ""
Аралтоғай:Аралтоғай NP-TOP ; ! ""
Аралтөбе:Аралтөбе NP-TOP ; ! ""
Арам:Арам NP-ANT-M ; !"Use/MT"
Арамнов:Арамнов NP-COG-OB ; ! ""
Арапов:Арапов NP-COG-OB ; ! ""
Ар:Ар NP-ANT-M ; ! "Ar" (Arabic)
Арасан:Арасан NP-TOP ; ! ""
Арафат:Арафат NP-COG-MF ; !"Use/MT"
Арафур:Арафур NP-TOP ; ! ""
Арбек:Арбек NP-ANT-M ; ! "Arbek" (Kazakh)
Арбол:Арбол NP-ANT-M ; ! "Arbol" (Kazakh)
Аргау:Аргау NP-TOP; !"Use/MT"
Аргентина:Аргентина NP-TOP ; ! "Argentina"
Арғанаты:Арғанаты NP-TOP ; ! ""
Арғынбаев:Арғынбаев NP-COG-OB ; ! ""
Ардақ:Ардақ NP-ANT-F ; ! "Ardaq" (Kazakh)
Ард:Ард NP-ANT-M ; !"Use/MT"
Ардатов:Ардатов NP-COG-OB ; ! ""
Арден:Арден NP-COG-MF ; ! "" ! Use/MT
Ариана:Ариана NP-ANT-F ; !"Use/MT"
Ариан:Ариан NP-ANT-M ; !"Use/MT"
Ари:Ари NP-ANT-M ; !"Use/MT"
Ариас:Ариас NP-COG-MF ; !"Use/MT"
Аризона:Аризона NP-TOP ; !"Use/MT"
Арина:Арина NP-ANT-F ; ! "Arıyna" (Greek)
Арипов:Арипов NP-COG-OB ; ! ""
Аристов:Аристов NP-COG-OB ; ! ""
Аристотель:Аристотель NP-ANT-M ; ! ""
Аркажах:Аркажах NP-TOP ; ! ""
Арканзас:Арканзас NP-TOP ; !"Use/MT"
Арктика:Арктика NP-TOP ; ! "Arctic Region"
Аркук:Аркук NP-TOP ; ! ""
Арқайым:Арқайым NP-TOP ; ! ""
Арқалық:Арқалық NP-TOP ; ! ""
Арқалық:Арқалық NP-TOP ; ! ""
Арқанкерген:Арқанкерген NP-TOP ; ! ""
Арқарлы:Арқарлы NP-TOP ; ! ""
Арқат:Арқат NP-ANT-M ; ! "Arqat" (Kazakh)
Арқат:Арқат NP-TOP ; ! ""
Арман:Арман NP-ANT-M ; ! "Arman;" (Persian)
Арман:Арман NP-ANT-M; !"Use/MT"
Арман:Арман NP-COG-MF ; !"Use/MT"
Армандо:Армандо NP-ANT-M ; !"Use/MT"
Армения:Армения NP-TOP ; ! "Armenia"
Арна:Арна NP-ANT-F ; ! "Arna" (Kazakh)
Арнольд:Арнольд NP-ANT-M ; !"Use/MT"
Арон:Арон NP-ANT-M ; ! ""
Аррениус:Аррениус NP-COG-MF ; ! "" ! Use/MT
Арсалиев:Арсалиев NP-COG-OB ; ! ""
Арсен:Арсен NP-ANT-M ; ! "Arsen" (Arabic)
Арсений:Арсений NP-ANT-M ; ! ""
Арсентьев:Арсентьев NP-COG-OB ; ! ""
Арсланов:Арсланов NP-COG-OB ; ! ""
Артаев:Артаев NP-COG-OB ; ! ""
Артамонов:Артамонов NP-COG-OB ; ! ""
Артеев:Артеев NP-COG-OB ; ! ""
Артем:Артем NP-ANT-M ; ! ""
Артем:Артем NP-TOP ; ! ""
Артемида:Артемида NP-AL ; ! ""
Артемида:Артемида NP-ANT-F ; !"Use/MT"
Артемид:Артемид NP-TOP ; ! "" ! Use/MT
Артемьев:Артемьев NP-COG-OB ; ! ""
Артур:Артур NP-ANT-M ; ! "Artıwr" 
Артық:Артық NP-ANT-M ; ! "Artıq" (Kazakh)
Артықбай:Артықбай NP-ANT-M ; ! "Artıqbay" (Kazakh)
Артыққали:Артыққали NP-ANT-M ; ! "Artıqqalıy" (Kazakh)
Артыш:Артыш NP-ANT-M ; ! "Артыш" (Tuvan)
Ару:Ару NP-ANT-F ; ! "Arıw" (Arabic)
Аруба:Аруба NP-TOP ; !"Use/MT"
Архангельск:Архангельск NP-TOP ; ! ""
Архандеев:Архандеев NP-COG-OB ; ! ""
Архимед:Архимед NP-ANT-M ; ! ""
Архипов:Архипов NP-COG-OB ; ! ""
Арцыбышев:Арцыбышев NP-COG-OB ; ! ""
Арча:Арча NP-TOP ; ! ""
Арчибальд:Арчибальд NP-ANT-M ; !"Use/MT"
Аршалы:Аршалы NP-TOP ; ! ""
Аршат:Аршат NP-ANT-F ; ! "Arshat" (Arabic)
Аршинов:Аршинов NP-COG-OB ; ! ""
Арықбалық:Арықбалық NP-TOP ; ! ""
Арықты:Арықты NP-TOP ; ! ""
Арын:Арын NP-ANT-M ; ! "Arın" (Kazakh)
Арынбай:Арынбай NP-ANT-M ; ! "Arınbay" (Kazakh)
Арынбек:Арынбек NP-ANT-M ; ! "Arınbek" (Kazakh)
Арынғазы:Арынғазы NP-ANT-M ; ! "Arınğazı" (Kazakh)
Арындық:Арындық NP-ANT-M ; ! "Arındıq" (Kazakh)
Арынов:Арынов NP-COG-OB ; ! ""
Арыс:Арыс NP-TOP ; ! ""
Арысбай:Арысбай NP-ANT-M ; ! "Arısbay" (Kazakh)
Арысқұм:Арысқұм NP-TOP ; ! ""
Арыстан:Арыстан NP-ANT-M ; ! "Arıstan" (Kazakh)
Арыстанбай:Арыстанбай NP-ANT-M ; ! "Arıstanbay" (Kazakh)
Арыстанбек:Арыстанбек NP-ANT-M ; ! "Arıstanbek" (Kazakh)
Арыстанды:Арыстанды NP-TOP ; ! ""
Арыстанқали:Арыстанқали NP-ANT-M ; ! "Arıstanqalıy" (Kazakh)
Арыстанов:Арыстанов NP-COG-OB ; ! ""
Арыс%-Түркістан:Арыс%-Түркістан NP-TOP ; ! ""
Арьян:Арьян NP-ANT-M ; !"Use/MT"
Аса:Аса NP-ANT-M ; !"Use/MT"
Асабай:Асабай NP-ANT-M ; ! "Asabay" (Kazakh)
Асад:Асад NP-ANT-M ; !"Use/MT"
Асад:Асад NP-COG-MF ; ! ""
Асайын:Асайын NP-ANT-M ; ! "Asayın" (Arabic)
Асамат:Асамат NP-ANT-M ; ! ""
Асан:Асан NP-ANT-M ; ! "Asan" (Arabic)
Асанас:Асанас NP-TOP ; ! ""
Асанбай:Асанбай NP-ANT-M ; ! "Asanbay" (Arabic)
Асанбек:Асанбек NP-ANT-M ; ! "Asanbek" (Arabic)
Асанжан:Асанжан NP-ANT-M ; ! "Asanjan" (Arabic)
Асанкелді:Асанкелді NP-ANT-M ; ! "Asankeldi" (Arabic)
Асанқали:Асанқали NP-ANT-M ; ! "Asanqalıy" (Arabic)
Асанқан:Асанқан NP-ANT-M ; ! "Asanqan" (Arabic)
Асанқожа:Асанқожа NP-ANT-M ; ! "Asanqoja" (Arabic)
Асанмұрат:Асанмұрат NP-ANT-M ; ! "Asanmurat" (Arabic)
Асанов:Асанов NP-COG-OB ; ! ""
Ас:Ас NP-ANT-M ; !"Use/MT"
Асаубалық:Асаубалық NP-TOP ; ! ""
Асаутай:Асаутай NP-ANT-M ; ! "" 
Асаф:Асаф NP-ANT-M ; !"Use/MT"
Асель:Асель NP-ANT-F ; !"Use/MT"
Асем:Асем NP-ANT-F ; ! "Assem"  see also "Әсем"
Асимов:Асимов NP-COG-OB ; ! ""
Аскольд:Аскольд NP-COG-MF ; ! ""
Асқан:Асқан NP-ANT-M ; ! "Asqan" (Kazakh)
Асқаралы:Асқаралы NP-ANT-M ; ! "Asqaralı" (Kazakh)
Асқар:Асқар NP-ANT-M ; ! "Asqar" (Kazakh)
Асқарбай:Асқарбай NP-ANT-M ; ! "Asqarbay" (Kazakh)
Асқарбек:Асқарбек NP-ANT-M ; ! "Asqarbek" (Kazakh)
Асқарбекұлы:Асқарбекұлы NP-ANT-M ; !
Асқаржан:Асқаржан NP-ANT-M ; ! "Asqarjan" (Kazakh)
Асқаров:Асқаров NP-COG-OB ; ! ""
Аслан:Аслан NP-ANT-M ; ! "" 
Асланбек:Асланбек NP-ANT-M ; ! "USE/MT" 
Асли:Асли NP-ANT-F ; ! "Aslıy" (Arabic)
Аснар:Аснар NP-COG-MF ; !"Use/MT"
Аспазия:Аспазия NP-ANT-F ; ! "Aspazıya" (Greek)
Аспандиаров:Аспандиаров NP-COG-OB ; ! ""
Аспандияр:Аспандияр NP-ANT-M ; ! "Aspandıyar" (Persian)
Аспантау:Аспантау NP-TOP ; ! ""
Аспара:Аспара NP-TOP ; ! ""
Аспара:Аспара NP-TOP ; ! "" 
Аспен:Аспен NP-ANT-F ; !"Use/MT"
Аспижаб:Аспижаб NP-TOP ; ! ""
Ассанж:Ассанж NP-COG-MF ; ! ""
Ассельборн:Ассельборн NP-COG-MF ; !"Use/MT"
Ассур:Ассур NP-ANT-M ; ! ""
Астам:Астам NP-ANT-M ; ! "Astam" (Kazakh)
Астана:Астана NP-TOP ; ! ""
Астана:Астана NP-TOP ; ! "Astana"
Астор:Астор NP-ANT-M ; !"Use/MT"
Астрахан:Астрахан NP-TOP ; ! ""
Астурия:Астурия NP-TOP ; !"Use/MT"
Асубұлақ:Асубұлақ NP-TOP ; ! ""
Асулан:Асулан NP-ANT-M ; ! "USE/MT" 
Асхат:Асхат NP-ANT-M ; ! "Asxat" (Arabic)
Асы:Асы NP-TOP ; ! ""
Асықата:Асықата NP-TOP ; ! ""
Асыл:Асыл NP-ANT-F ; ! "Asıl" (Arabic)
Асыл:Асыл NP-ANT-M ; ! "Asıl" (Arabic)
Асылбек:Асылбек NP-ANT-M ; ! "Asılbek"
Асылбеков:Асылбеков NP-COG-OB ; ! ""
Асылов:Асылов NP-COG-OB ; ! ""
Асылтас:Асылтас NP-ANT-F ; ! "Asıltas" (Kazakh)
Асылхан:Асылхан NP-ANT-M ; ! "" 
Асылша:Асылша NP-ANT-F ; ! "Asılsha" (Arabic)
Асысаға:Асысаға NP-TOP ; ! ""
Атабаев:Атабаев NP-COG-OB ; ! ""
Атабай:Атабай NP-TOP ; ! ""
Атабек:Атабек NP-ANT-M ; ! "Atabek" (Kazakh)
Атабеков:Атабеков NP-COG-OB ; ! ""
Атағозиев:Атағозиев NP-COG-OB ; ! ""
Аталай:Аталай NP-COG-MF ; ! ""
Аталанта:Аталанта NP-ORG ; !"Use/MT"
Аталық:Аталық NP-ANT-M ; ! "Atalıq" (Kazakh)
Атаманов:Атаманов NP-COG-OB ; ! ""
Атамбаев:Атамбаев NP-COG-OB ; ! "" ! Use/MT
Атамұрат:Атамұрат NP-ANT-M ; ! "Atamurat" (Kazakh)
Атанасов:Атанасов NP-COG-OB ; ! ""
Атанияз:Атанияз NP-ANT-M ; ! "Atanıyaz" (Kazakh)
Атаниязов:Атаниязов NP-COG-OB ; ! ""
Атасу:Атасу NP-TOP ; ! ""
Ат:Ат NP-ANT-M ; !"Use/MT"
Атбасар:Атбасар NP-TOP ; ! ""
Атена:Атена NP-ANT-F ; !"Use/MT"
Атжақты:Атжақты NP-TOP ; ! ""
Аткинсон:Аткинсон NP-COG-MF ; ! "" ! Use/MT
Атланта:Атланта NP-TOP ; !"Use/MT"
Атлант:Атлант NP-AL ; ! ""
Атлантика:Атлантика NP-TOP ; !""
Атлантик:Атлантик NP-TOP ; ! ""
Атласов:Атласов NP-COG-OB ; ! ""
Атрохов:Атрохов NP-COG-OB ; ! ""
Атрошенко:Атрошенко NP-COG-MF ; ! ""
Аттила:Аттила NP-ANT-M ; ! ""
Атшабаров:Атшабаров NP-COG-OB ; ! ""
Атшыбаев:Атшыбаев NP-COG-OB ; ! ""
Атымтай:Атымтай NP-ANT-M ; ! "Atımtay" (Arabic)
Атырау:Атырау NP-TOP ; ! ""
Атырау:Атырау NP-TOP ; ! "Atyraw"
Ауан:Ауан NP-TOP ; ! "" 
Аугусто:Аугусто NP-ANT-M ; !"Use/MT"
Ауғанстан:Ауғанстан NP-TOP ; ! "Afghanistan"
Ауқатты:Ауқатты NP-TOP ; ! ""
Аурангзеб:Аурангзеб NP-ANT-M ; !"Use/MT"
Аурелио:Аурелио NP-ANT-M ; !"Use/MT"
Аурелия:Аурелия NP-ANT-M ; !"Use/MT"
Аустралия:Аустралия NP-TOP ; ! ""
Аустрия:Аустрия NP-TOP ; !"Use/MT"
Афанасьев:Афанасьев NP-COG-OB ; ! ""
Афанасьев:Афанасьев NP-TOP ; ! ""
Афина:Афина NP-TOP ; ! "Athens"
Афон:Афон NP-ANT-M ; !"Use/MT"
Афонсо:Афонсо NP-COG-MF ; !"Use/MT"
Африка:Африка NP-TOP ; ! "Africa"
Ахаб:Ахаб NP-ANT-M ; ! ""
Ахан:Ахан NP-ANT-M ; ! "Axan" (Persian)
Аханов:Аханов NP-COG-OB ; ! ""
Ахат:Ахат NP-ANT-M ; ! "Axat" (Arabic)
Ах:Ах NP-ANT-M ; !"Use/MT"
Ахен:Ахен NP-TOP ; ! "Aachen"
Ахерн:Ахерн NP-COG-MF ; !"Use/MT"
Ахиллес:Ахиллес NP-ANT-M ; !"Use/MT"
Ахмад:Ахмад NP-ANT-M ; !"Use/MT"
Ахмадиев:Ахмадиев NP-COG-OB ; ! ""
Ахмадинежад:Ахмадинежад NP-COG-MF ; ! ""
Ахмар:Ахмар NP-ANT-M ; ! "Axmar" (Arabic)
Ахматканов:Ахматканов NP-COG-OB ; ! ""
Ахматов:Ахматов NP-COG-OB ; ! ""
Ахмед:Ахмед NP-ANT-M ; !"Use/MT"
Ахмединежад:Ахмединежад NP-COG-M ; ! ""
Ахмедияр:Ахмедияр NP-ANT-M ; ! "Axmedıyar" (Arabic)
Ахмедияров:Ахмедияров NP-COG-OB ; ! ""
Ахмедов:Ахмедов NP-COG-OB ; ! ""
Ахмеров:Ахмеров NP-COG-OB ; ! ""
Ахмет:Ахмет NP-ANT-M ; ! "Axmet" (Arabic)
Ахметбеков:Ахметбеков NP-COG-OB ; ! ""
Ахметжан:Ахметжан NP-ANT-M ; !"Use/MT"
Ахметжанов:Ахметжанов NP-COG-OB ; ! ""
Ахметов:Ахметов NP-COG-OB ; ! ""
Ахметов:Ахметов NP-COG-OB ; ! "" ! Use/MT
Ахметсапа:Ахметсапа NP-ANT-M ; ! "Axmetsapa" (Arabic)
Ахрам:Ахрам NP-ANT-M ; ! "Axram" (Arabic)
Ашанти:Ашанти NP-TOP ; ! ""
Ашимханов:Ашимханов NP-COG-OB ; ! ""
Аширбаев:Аширбаев NP-COG-OB ; ! ""
Ашкеров:Ашкеров NP-COG-OB ; ! ""
Аюбаев:Аюбаев NP-COG-OB ; ! "" ! Use/MT
Аюханов:Аюханов NP-COG-OB ; ! ""
Аюшат:Аюшат NP-TOP ; ! ""
Аягөз:Аягөз NP-TOP ; ! ""
Аяғанов:Аяғанов NP-COG-OB ; ! "" ! Use/MT
Аязқала:Аязқала NP-TOP ; ! ""
Аяққамыр:Аяққамыр NP-TOP ; ! ""
Аян:Аян NP-ANT-M ; ! "Ayan" (Kazakh)
Аяубаев:Аяубаев NP-COG-OB ; ! ""
Аяччо:Аяччо NP-TOP ; ! "Ajaccio"
Әбiшев:Әбiшев NP-COG-OB ; ! ""
Әбденов:Әбденов NP-COG-OB ; ! ""
Әбдиев:Әбдиев NP-COG-OB ; ! ""
Әбді:Әбді NP-ANT-M ; ! "Äbdi" (Arabic)
Әбдібай:Әбдібай NP-ANT-M ; ! "Äbdibay" (Kazakh)
Әбдібақи:Әбдібақи NP-ANT-M ; ! "Äbdibaqıy" (Arabic)
Әбдібақыт:Әбдібақыт NP-ANT-M ; ! "Äbdibaqıt" (Arabic)
Әбдібек:Әбдібек NP-ANT-M ; ! "Äbdibek" (Kazakh)
Әбдіғазиз:Әбдіғазиз NP-ANT-M ; ! "Äbdiğazıyz" (Arabic)
Әбдіғали:Әбдіғали NP-ANT-M ; ! "Äbdiğalıy" (Arabic)
Әбдіғаппар:Әбдіғаппар NP-ANT-M ; ! "Äbdiğappar" (Arabic)
Әбдіжапар:Әбдіжапар NP-ANT-M ; ! "Äbdijapar" (Arabic)
Әбдіжәміл:Әбдіжәміл NP-ANT-M ; ! "Äbdijämil" (Arabic)
Әбдікәрім:Әбдікәрім NP-ANT-M ; ! "Äbdikärim" (Arabic)
Әбдікәрімов:Әбдікәрімов NP-COG-OB ; ! "" ! Use/MT
Әбдіков:Әбдіков NP-COG-OB ; ! ""
Әбдіқадір:Әбдіқадір NP-ANT-M ; ! "Äbdiqadir" (Arabic)
Әбдіқайым:Әбдіқайым NP-ANT-M ; ! "Äbdiqayım" (Arabic)
Әбдіқайыр:Әбдіқайыр NP-ANT-M ; ! "Äbdiqayır" (Arabic)
Әбдіқалықов:Әбдіқалықов NP-COG-OB ; ! ""
Әбдіқожа:Әбдіқожа NP-ANT-M ; ! "Äbdiqoja" (Arabic)
Әбділдин:Әбділдин NP-COG-IN ; ! ""
Әбділманов:Әбділманов NP-COG-OB ; ! ""
Әбдіманап:Әбдіманап NP-ANT-M ; ! "Äbdimanap" (Arabic)
Әбдімәлік:Әбдімәлік NP-ANT-M ; ! "Äbdimälik" (Arabic)
Әбдінасыр:Әбдінасыр NP-ANT-M ; ! "Äbdinasır" (Arabic)
Әбдінұр:Әбдінұр NP-ANT-M ; ! "Äbdinur" (Arabic)
Әбдіразақ:Әбдіразақ NP-ANT-M ; ! "Äbdirazaq" (Arabic)
Әбдірайымов:Әбдірайымов NP-COG-OB ; ! ""
Әбдірақым:Әбдірақым NP-ANT-M ; ! "Äbdiraqım" (Arabic)
Әбдірақымұлы:Әбдірақымұлы NP-COG-M ; ! "USE/MT"
Әбдірасұл:Әбдірасұл NP-ANT-M ; ! "Äbdirasul" (Arabic)
Әбдірахман:Әбдірахман NP-ANT-M ; ! "Äbdiraxman" (Arabic)
Әбдірахманов:Әбдірахманов NP-COG-OB ; ! ""
Әбдірахымов:Әбдірахымов NP-COG-OB ; ! ""
Әбдірашев:Әбдірашев NP-COG-OB ; ! ""
Әбдірашид:Әбдірашид NP-ANT-M ; ! "Äbdirashıyd" (Arabic)
Әбдіров:Әбдіров NP-COG-OB ; ! ""
Әбдісалам:Әбдісалам NP-ANT-M ; ! "Äbdisalam" (Arabic)
Әбдісамат:Әбдісамат NP-ANT-M ; ! "Äbdisamat" (Arabic)
Әбдісаттар:Әбдісаттар NP-ANT-M ; ! "Äbdisattar" (Arabic)
Әбдісейіт:Әбдісейіт NP-ANT-M ; ! "Äbdiseyit" (Arabic)
Әбдіхалық:Әбдіхалық NP-ANT-M ; ! "Äbdixalıq" (Arabic)
Әбдіхамит:Әбдіхамит NP-ANT-M ; ! "Äbdixamıyt" (Arabic)
Әбдішев:Әбдішев NP-COG-OB ; ! ""
Әбдішүкір:Әбдішүкір NP-ANT-M ; ! "Äbdishükir" (Arabic)
Әбиба:Әбиба NP-ANT-F ; ! "Äbiyba" (Arabic)
Әблязов:Әблязов NP-COG-OB ; ! ""
Әбсалам:Әбсалам NP-ANT-M ; ! "Äbsalam" (Arabic)
Әбсамат:Әбсамат NP-ANT-M ; ! "Äbsamat" (Arabic)
Әбу:Әбу NP-ANT-M ; ! "Äbiw" (Arabic)
Әбубәкір:Әбубәкір NP-ANT-M ; ! "Äbiwbäkir" (Arabic)
Әбуғали:Әбуғали NP-ANT-M ; ! "Äbiwğaliy" (Arabic)
Әбуғалым:Әбуғалым NP-ANT-M ; ! "Äbiwğalım" (Arabic)
Әбу%-Даби:Абу% Даби NP-TOP ; ! "" ! Dir/LR
Әбу%-Даби:Абу%-Даби NP-TOP ; ! "" ! Dir/LR
Әбу%-Даби:Әбу%-Даби NP-TOP ; ! ""
Әбу%-Даби:Әбу% Даби NP-TOP ; ! "" ! Dir/LR
Әбунасыр:Әбунасыр NP-ANT-M ; ! "Äbiwnasır" (Arabic)
Әбусадық:Әбусадық NP-ANT-M ; ! "Äbiwsadıq" (Arabic)
Әбуталиев:Әбуталиев NP-COG-OB ; ! ""
Әбутәліп:Әбутәліп NP-ANT-M ; ! "Äbiwtälip" (Arabic)
Әбіқаев:Әбіқаев NP-COG-OB ; ! ""
Әбіл:Әбіл NP-ANT-M ; ! "Äbil" (Arabic)
Әбіләзов:Әбіләзов NP-COG-OB ; ! "" ! Use/MT
Әбілғазиз:Әбілғазиз NP-ANT-M ; ! "Äbilğazıyz" (Arabic)
Әбілдаев:Әбілдаев NP-COG-OB ; ! ""
Әбілев:Әбілев NP-COG-OB ; ! ""
Әбілқазы:Әбілқазы NP-ANT-M ; ! "Äbilqazı" (Arabic)
Әбілқайым:Әбілқайым NP-ANT-M ; ! "Äbilqayım" (Arabic)
Әбілқайыр:Әбілқайыр NP-ANT-M ; ! "Äbilqayır" (Arabic)
Әбілқан:Әбілқан NP-ANT-M ; ! "Äbilqan" (Kazakh)
Әбілқасым:Әбілқасым NP-ANT-M ; ! "Äbilqasım" (Arabic)
Әбілқасымов:Әбілқасымов NP-COG-OB ; ! ""
Әбілов:Әбілов NP-COG-OB ; ! ""
Әбілпайыз:Әбілпайыз NP-ANT-M ; ! "Äbilpayız" (Arabic)
Әбілтаев:Әбілтаев NP-COG-OB ; ! "" ! Use/MT
Әбілханов:Әбілханов NP-COG-OB ; ! ""
Әбіров:Әбіров NP-COG-OB ; ! ""
Әбіш:Әбіш NP-ANT-M ; ! "Äbіsh"
Әбішев:Әбішев NP-COG-OB ; ! "" ! Use/MT
Әбішұлы:Әбішұлы NP-COG-M ; ! ""
Әгерже:Әгерже NP-TOP ; ! ""
Әдекенов:Әдекенов NP-COG-OB ; ! "" ! Use/MT
Әдемі:Әдемі NP-ANT-F ; ! "Ädemi" (Kazakh)
Әди:Әди NP-ANT-M ; ! "Ädiy" (Arabic)
Әдиба:Әдиба NP-ANT-F ; ! "Ädiyba" (Arabic)
Әдила:Әдила NP-ANT-F ; ! "Ädiyla" (Kazakh)
Әдия:Әдия NP-ANT-F ; ! "Ädıya" (Arabic)
Әділ:Әділ NP-ANT-M ; ! "Ädil" (Arabic)
Әділбай:Әділбай NP-ANT-M ; ! "Ädilbay" (Arabic)
Әділбек:Әділбек NP-ANT-M ; ! "Ädilbek" (Arabic)
Әділғазы:Әділғазы NP-ANT-M ; ! "Ädilğazı" (Arabic)
Әділев:Әділев NP-COG-OB ; ! ""
Әділжан:Әділжан NP-ANT-M ; ! "Ädiljan" (Arabic)
Әділхан:Әділхан NP-ANT-M ; ! "Ädilxan" (Arabic)
Әділханұлы:Әділханұлы NP-ANT-M ; !
Әділханұлы:Әділханұлы NP-COG-M ; ! "USE/MT"
Әжмұхамбет:Әжмұхамбет NP-ANT-M ; ! "Äjmuxambet" (Kazakh)
Әжібай:Әжібай NP-ANT-M ; ! "Äjibay" (Kazakh)
Әжібек:Әжібек NP-ANT-M ; ! "Äjibek" (Kazakh)
Әжіғали:Әжіғали NP-ANT-M ; ! "Äjiğalıy" (Kazakh)
Әжіғалиев:Әжіғалиев NP-COG-OB ; ! ""
Әжікерей:Әжікерей NP-ANT-M ; ! "Äjikerey" (Kazakh)
Әжімұхамбет:Әжімұхамбет NP-ANT-M ; ! "Äjimuxambet" (Kazakh)
Әзел:Әзел NP-ANT-M ; ! "Äzel" (Arabic)
Әзербай:Әзербай NP-ANT-M ; ! "Äzerbay" (Persian)
Әзиза:Әзиза NP-ANT-F ; ! "Äziyza" (Arabic)
Әзима:Әзима NP-ANT-F ; ! "Äziyma" (Arabic)
Әзіз:Әзіз NP-ANT-M ; ! "Äziz" (Arabic)
Әзізбай:Әзізбай NP-ANT-M ; ! "Äzizbay" (Arabic)
Әзізбек:Әзізбек NP-ANT-M ; ! "Äzizbek" (Arabic)
Әзізжан:Әзізжан NP-ANT-M ; ! "Äzizjan" (Arabic)
Әзізқали:Әзізқали NP-ANT-M ; ! "Äzizqalıy" (Arabic)
Әзізқожа:Әзізқожа NP-ANT-M ; ! "Äzizqoja Äzizäliy" (Arabic)
Әзізсейіт:Әзізсейіт NP-ANT-M ; ! "Äzizseyit" (Arabic)
Әзізхан:Әзізхан NP-ANT-M ; ! "Äzizxan" (Arabic)
Әзілхан:Әзілхан NP-ANT-M ; ! "Äzilxan" (Old Turkic)
Әзім:Әзім NP-ANT-M ; ! "Äzim" (Arabic)
Әзімбаев:Әзімбаев NP-COG-OB ; ! ""
Әзімбай:Әзімбай NP-ANT-M ; ! "Äzimbay" (Arabic)
Әзімбек:Әзімбек NP-ANT-M ; ! "" 
Әзімжан:Әзімжан NP-ANT-M ; ! "Äzimjan" (Arabic)
Әзімхан:Әзімхан NP-ANT-M ; ! "Äzimxan" (Arabic)
Әзірбаев:Әзірбаев NP-COG-OB ; ! ""
Әзірбайжан:Әзербайжан NP-TOP ; ! "Azerbaijan" Dir/LR
Әзірбайжан:Әзірбайжан NP-TOP ; ! "Azerbaijan"
Әйгерім:Әйгерім NP-ANT-F ; ! "Äygerim" (Kazakh)
Әйке:Әйке NP-TOP ; ! ""
Әйнек:Әйнек NP-ANT-F ; ! "Äynek" (Arabic)
Әйтенов:Әйтенов NP-COG-OB ; ! ""
Әйүп:Әйүп NP-ANT-M ; ! ""
Әкежан:Әкежан NP-ANT-M ; ! "" 
Әкрам:Әкрам NP-ANT-M ; ! "Äkram" (Arabic)
Әкрамбай:Әкрамбай NP-ANT-M ; ! "Äkrambay" (Arabic)
Әкрамбек:Әкрамбек NP-ANT-M ; ! "Äkrambek" (Arabic)
Әкім:Әкім NP-ANT-M ; ! "Äkim" (Arabic)
Әкімбай:Әкімбай NP-ANT-M ; ! "Äkimbay" (Arabic)
Әкімбек:Әкімбек NP-ANT-M ; ! "Äkimbek" (Arabic)
Әкімжан:Әкімжан NP-ANT-M ; ! "Äkimjan" (Arabic)
Әкімқан% Әлғазы:Әкімқан% Әлғазы NP-ANT-M ; ! "Äkimqan Älğazı" (Arabic)
Әкімқожа:Әкімқожа NP-ANT-M ; ! "Äkimqoja" (Arabic)
Әкімнұр:Әкімнұр NP-ANT-M ; ! "Äkimnur" (Arabic)
Әкімтай:Әкімтай NP-ANT-M ; ! "Äkimtay" (Arabic)
әл-Аббас:әл-Аббас NP-ANT-M ; ! "Abbas" (Arabic)
әл%-Басри:әл%-Басри NP-COG-MF ; !"Use/MT"  
Әлғазы:Әлғазы NP-ANT-M ; ! "Äkimqan Älğazı" (Arabic)
Әлемгүл:Әлемгүл NP-ANT-F ; ! "Älemgül" (Kazakh)
әлеуметтану:әлеуметтану N1 ; !"Use/MT"
Әлжанов:Әлжанов NP-COG-OB ; ! ""
Әлиайдар:Әлиайдар NP-ANT-M ; ! "Äliyaydar" (Arabic)
Әлиақпар:Әлиақпар NP-ANT-M ; ! "Äliyaqpar" (Arabic)
Әлиасқар:Әлиасқар NP-ANT-M ; ! "Äliyasqar" (Arabic)
Әлиахмет:Әлиахмет NP-ANT-M ; ! "Äliyaxmet" (Arabic)
Әли:Әли NP-ANT-M ; ! "Äliy" (Arabic)
Әлиев:Әлиев NP-COG-OB ; ! ""
Әлиев:Әлиев NP-COG-OB ; ! ""
Әлиев:Әлиев NP-COG-OB ; ! ""
Әлима:Әлима NP-ANT-F ; ! "Äliyma" (Arabic)
Әлипа:Әлипа NP-ANT-F ; ! "Äliypa" (Arabic)
Әл%-Истахри:Әл%-Истахри NP-COG-MF ; ! ""
Әл%-Истахри:Әл%-Истахри NP-COG-MF ; ! ""
Әлихан:Әлихан NP-ANT-M ; ! "Äliyxan" (Arabic)
Әлиша:Әлиша NP-ANT-F ; ! "Äliysha" 
Әлишама:Әлишама NP-ANT-F ; ! ""
Әлия:Әлия NP-ANT-F ; ! "Älıya" 
Әлки:Әлки NP-TOP ; ! ""
Әл%-Қаида:Әл%-Қаида NP-ORG ; ! ""
Әл%-Масуди:Әл%-Масуди NP-COG-MF ; ! ""
Әлмәт:Әлмәт NP-TOP ; ! ""
Әлнұр:Әлнұр NP-ANT-M ; ! "Älnur" (Arabic)
Әл%-Фараби:Әл%-Фараби NP-COG-MF ; ! ""
Әлфия:Әлфия NP-ANT-F ; ! "Älfiya" (Arabic)
Әл%-Хасан:Әл%-Хасан NP-COG-MF ; ! "" !Use/MT
Әлібек:Әлібек NP-ANT-M ; ! "USE/MT" 
Әлібеков:Әлібеков NP-COG-OB ; ! ""
Әліби:Әліби NP-ANT-M ; ! "Älibiy" (Kazakh)
Әлім:Әлім NP-ANT-M ; ! "Älim" (Arabic)
Әлімбаев:Әлімбаев NP-COG-OB ; ! ""
Әлімбай:Әлімбай NP-ANT-M ; ! "Älimbay" (Arabic)
Әлімбек:Әлімбек NP-ANT-M ; ! "Älimbek" (Arabic)
Әлімбеков:Әлімбеков NP-COG-OB ; ! "" ! Use/MT
Әлімбетов:Әлімбетов NP-COG-OB ; ! ""
Әлімжан:Әлімжан NP-ANT-M ; ! "Älimjan" (Arabic)
Әлімжанов:Әлімжанов NP-COG-OB ; ! ""
Әлімқан:Әлімқан NP-ANT-M ; ! "Älimqan" (Arabic)
Әлімқұл:Әлімқұл NP-ANT-M ; ! "Älimqul" (Arabic)
Әлішер:Әлішер NP-ANT-M ; ! "Älisher" (Arabic)
Әмила:Әмила NP-ANT-F ; ! "Ämiyla" (Arabic)
Әмина:Әмина NP-ANT-F ; ! "Ämiyna" (Arabic)
Әмин:Әмин NP-ANT-M ; ! "Ämiyn" (Arabic)
Әминов:Әминов NP-COG-OB ; ! ""
Әмира:Әмира NP-ANT-F ; ! "Ämiyra" (Kazakh)
Әмудария:Амудария NP-TOP ; ! "" Dir/LR
Әмудария:Әмудария NP-TOP ; ! ""
Әмір:Әмір NP-ANT-M ; ! "Ämir" (Kazakh)
Әмірбай:Әмірбай NP-ANT-M ; ! "Ämirbay" (Kazakh)
Әмірбек:Әмірбек NP-ANT-M ; ! "Ämirbek" (Arabic)
Әміржаи:Әміржаи NP-ANT-M ; ! "Ämirjai" (Kazakh)
Әміров:Әміров NP-COG-OB ; ! ""
Әміртай:Әміртай NP-ANT-M ; ! "Ämirtay" (Kazakh)
Әмір% Темір:Әмір% Темір NP-ANT-M ; ! ""
Әнапия:Әнапия NP-ANT-F ; ! "Änapıya" (Arabic)
Әнес:Әнес NP-ANT-M ; ! "Änes" (Arabic)
Әниса:Әниса NP-ANT-F ; ! "Äniysa" (Arabic)
Әнуар:Әнуар NP-ANT-M ; ! "Äniwar" (Arabic)
Әнуарбек:Әнуарбек NP-ANT-M ; ! "Äniwarbek" (Arabic)
Әнуархан:Әнуархан NP-ANT-M ; ! "Äniwarxan" (Arabic)
Әрипа:Әрипа NP-ANT-F ; ! "Äriypa" (Arabic)
Әріп:Әріп NP-ANT-M ; ! "Ärip" (Arabic)
Әріпбай:Әріпбай NP-ANT-M ; ! "Äripbay" (Arabic)
Әріпжан:Әріпжан NP-ANT-M ; ! "Äripjan" (Arabic)
Әріпқан:Әріпқан NP-ANT-M ; ! "Äripqan" (Arabic)
Әсел:Әсел NP-ANT-F ; ! "Äsel" (Arabic)
Әселхан:Әселхан NP-ANT-F ; ! "Äselxan" 
Әсем:Әсем NP-ANT-F ; ! "Äsem" (Kazakh)
Әсемгүл:Әсемгүл NP-ANT-F ; ! "Äsemgül" (Kazakh)
Әсемхан:Әсемхан NP-ANT-F ; ! "Äsemxan" (Kazakh)
Әсер:Әсер NP-ANT-M ; ! "Äser" (Arabic)
Әсет:Әсет NP-ANT-M ; ! "Äset" (Arabic)
Әсима:Әсима NP-ANT-F ; ! "Äsiyma" (Arabic)
Әсия:Әсия NP-ANT-F ; ! "Äsıya" (Persian)
Әскер:Әскер NP-ANT-M ; ! "Äsker" (Arabic)
әс-Саффах:әс-Саффах NP-ANT-M ; ! "Saffah" (Arabic)
Әтнә:Әтнә NP-TOP ; ! ""
Әубәкір:Әубәкір NP-ANT-M ; ! ""
Әубәкіров:Әубәкіров NP-COG-OB ; ! ""
Әубөкіров:Әубөкіров NP-COG-OB ; ! ""
Әуез:Әуез NP-ANT-M ; ! "Äwez" (Arabic)
Әуезбай:Әуезбай NP-ANT-M ; ! "Äwezbay" (Arabic)
Әуезбек:Әуезбек NP-ANT-M ; ! "Äwezbek" (Arabic)
Әуезқан:Әуезқан NP-ANT-M ; ! "Äwezqan" (Arabic)
Әуезов:Әуезов NP-COG-OB ; ! ""
Әуезов:Әуезов NP-COG-OB ; ! "" ! Use/MT
Әуезов:Әуезов NP-TOP ; ! ""
Әуел:Әуел NP-ANT-M ; ! "Äwel" (Arabic)
Әуелбай:Әуелбай NP-ANT-M ; ! "Äwelbay" (Arabic)
Әуелбек:Әуелбек NP-ANT-M ; ! "Äwelbek" (Arabic)
Әуелбек:Әуелбек NP-TOP ; ! ""
Әуелқан:Әуелқан NP-ANT-M ; ! "Äwelqan" (Arabic)
Әужа:Әужа NP-ANT-F ; ! "Äwja" (Arabic)
Әулиеата:Әулиеата NP-TOP ; ! ""
Әулие%-Ата:Әулие%-Ата NP-TOP ; ! "" 
Әулие:Әулие NP-TOP ; ! ""
Әулиебастау:Әулиебастау NP-TOP ; ! ""
Әулиебұлақ:Әулиебұлақ NP-TOP ; ! ""
Әулие% Китт% және% Невис:Әулие% Китт% және% Невис NP-TOP ; ! "" 
Әулиекөл:Әулиекөл NP-TOP ; ! ""
Әулиетас:Әулиетас NP-TOP ; ! ""
Әшекеев:Әшекеев NP-COG-OB ; ! ""
Әшекей:Әшекей NP-ANT-M ; ! "Äshekey" (Kazakh)
Әшеккев:Әшеккев NP-COG-OB ; ! ""
Әшім:Әшім NP-ANT-M ; ! "Äshim" (Arabic)
Әшімбаев:Әшімбаев NP-COG-OB ; ! "" ! Use/MT
Әшімбай:Әшімбай NP-ANT-M ; ! "Äshimbay" (Kazakh)
Әшімов:Әшімов NP-COG-OB ; ! ""
Әшімхан:Әшімхан NP-ANT-M ; ! "Äshimxan" (Arabic)
Әшіралы:Әшіралы NP-ANT-M ; ! "Äshiralı" (Arabic)
Әшір:Әшір NP-ANT-M ; ! "Äshir" (Arabic)
Әшірбаев:Әшірбаев NP-COG-OB ; ! ""
Әшірбай:Әшірбай NP-ANT-M ; ! "Äshirbay" (Arabic)
Әшірбек:Әшірбек NP-ANT-M ; ! "Äshirbek" (Arabic)
Әшірхан:Әшірхан NP-ANT-M ; ! "Äshirxan" (Arabic)
Ба:Ба NP-ANT-M ; !"Use/MT"
Баба:Баба NP-ANT-M ; ! "Baba" (Persian)
Бабағали:Бабағали NP-ANT-M ; ! "Babağalıy" (Arabic)
Бабажан:Бабажан NP-ANT-M ; ! "Babajan" (Persian)
Бабажанов:Бабажанов NP-COG-OB ; ! ""
Бабайқорған:Бабайқорған NP-TOP ; ! ""
Бабақожа:Бабақожа NP-ANT-M ; ! "Babaqoja" (Arabic)
Бабақұмаров:Бабақұмаров NP-COG-OB ; ! "" ! Use/MT
Бабамкожа:Бабамкожа NP-COG-MF ; ! ""
Бабан:Бабан NP-TOP ; ! ""
Бабанияз:Бабанияз NP-ANT-M ; ! "Babanıyaz" (Persian)
Бабас:Бабас NP-ANT-M ; ! "Babas" (Arabic)
Бабата:Бабата NP-TOP ; ! ""
Бабатоған:Бабатоған NP-TOP ; ! ""
Бабахан:Бабахан NP-ANT-M ; ! "Babaxan" (Persian)
Бабенко:Бабенко NP-COG-MF ; ! ""
Бабичев:Бабичев NP-COG-OB ; ! "" ! Use/MT
Бабурин:Бабурин NP-COG-IN ; ! ""
Бабықұм:Бабықұм NP-TOP ; ! ""
Бабыл:Бабыл NP-TOP ; ! ""
Бавария:Бавария NP-TOP ; !"Use/MT"
Бав:Бав NP-ANT-M ; !"Use/MT"
Бавдинов:Бавдинов NP-COG-OB ; ! ""
Багамалар:Багамалар NP-TOP ; ! "" 
Багам% арал:Багам% арал N-COMPOUND-PX ; ! " "
Багам% аралдары:Багам% аралдар NP-TOP-COMPOUND ; ! ""
Багдад:Багдад NP-TOP ; ! "" 
Багул:Багул NP-ANT-F ; ! "" 
Бағамалар:Бағамалар NP-TOP ; ! ""
Бағашар:Бағашар NP-ANT-M ; ! "Bağashar" (Kazakh)
Бағдад:Бағдад NP-TOP-RUS ; ! ""
Бағдат:Бағдат NP-ANT-M ; ! "Bağdat" (Arabic)
Бағдат:Бағдат NP-TOP ; ! ""
Бағдатұлы:Бағдатұлы NP-ANT-M ; !
Бағила:Бағила NP-ANT-F ; ! "Bağıyla" (Arabic)
Бағлан:Бағлан NP-ANT-M ; ! "Bağlan" (Kazakh)
Бағланов:Бағланов NP-COG-OB ; ! ""
Бағыбай:Бағыбай NP-ANT-M ; ! "Bağıbay" (Kazakh)
Бағымша:Бағымша NP-ANT-F ; ! "Bağımsha" (Arabic)
Бағырлай:Бағырлай NP-TOP ; ! ""
Бағытқали:Бағытқали NP-ANT-M ; ! "" 
Бадам:Бадам NP-TOP ; ! ""
Бадамбай:Бадамбай NP-ANT-M ; ! "Badambay" (Kazakh)
Бадамшы:Бадамшы NP-TOP ; ! ""
Бадер:Бадер NP-ANT-F ; ! "Bader" (Arabic)
Бадиға:Бадиға NP-ANT-F ; ! "Badıyğa" (Arabic)
Бадігулжамал:Бадігулжамал NP-ANT-F ; ! "Badigıwljamal" (Arabic)
Базаев:Базаев NP-COG-OB ; ! ""
Базан:Базан NP-ANT-M ; ! "Bazan" (Arabic)
Базарайм:Базарайм NP-ANT-F ; ! "Bazaraym" (Kazakh)
Базаралы:Базаралы NP-ANT-M ; ! "Bazaralı" (Arabic)
Базарбаев:Базарбаев NP-COG-OB ; ! ""
Базар:Базар NP-ANT-M ; ! "Bazar" (Arabic)
Базар:Базар NP-TOP ; ! ""
Базарбай:Базарбай NP-ANT-M ; ! "Bazarbay" (Kazakh)
Базаргүл:Базаргүл NP-ANT-F ; ! "Bazargül" (Arabic)
Базартөбе:Базартөбе NP-TOP ; ! ""
Базаршолан:Базаршолан NP-TOP ; ! ""
Баз:Баз NP-ANT-M ; !"Use/MT"
Базель:Базель NP-TOP ; ! ""
Базиликата:Базиликата NP-TOP ; !"Use/MT"
Базилио:Базилио NP-ANT-M ; !"Use/MT"
Базыл:Базыл NP-ANT-M ; ! "Bazıl" (Arabic)
Базылбек:Базылбек NP-ANT-M ; ! "Bazılbek" (Arabic)
Байақын:Байақын NP-ANT-M ; ! "Bayaqın" (Persian)
Байарыстан:Байарыстан NP-ANT-M ; ! "Bayarıstan" (Kazakh)
Байахмет:Байахмет NP-ANT-M ; ! "Bayaxmet" (Kazakh)
Бай:Бай NP-ANT-M ; ! "Bay" (Kazakh)
Байбақты:Байбақты NP-ANT-M ; ! "Baybaqtı" (Kazakh)
Байбарыс:Байбарыс NP-ANT-M ; ! "Baybarıs" (Kazakh)
Байбек:Байбек NP-ANT-M ; ! "Baybek" (Kazakh)
Байболат:Байболат NP-ANT-M ; ! "Baybolat" (Kazakh)
Байбол:Байбол NP-ANT-M ; ! "Baybol" (Kazakh)
Байболды:Байболды NP-ANT-M ; ! "Bayboldı" (Kazakh)
Байболсын:Байболсын NP-ANT-M ; ! "Baybolsın" (Kazakh)
Байбосын:Байбосын NP-ANT-M ; ! "Baybosın" (Kazakh)
Байбосынов:Байбосынов NP-COG-OB ; ! ""
Байбөрі:Байбөрі NP-ANT-M ; ! "Bayböri" (Kazakh)
Байбөрілі:Байбөрілі NP-TOP ; ! ""
Байгелдиев:Байгелдиев NP-COG-OB ; ! ""
Байгозы:Байгозы NP-ANT-M ; ! "Baygozı" (Kazakh)
Байғабыл:Байғабыл NP-TOP ; ! ""
Байғани:Байғани NP-ANT-M ; ! "Bayğanıy" (Kazakh)
Байғанин:Байғанин NP-TOP ; ! ""
Байғожа:Байғожа NP-ANT-M ; ! "Bayğoja" (Kazakh)
Байдалы:Байдалы NP-ANT-M ; ! ""
Байдар:Байдар NP-ANT-M ; ! "Baydar" (Kazakh)
Байдәулет:Байдәулет NP-ANT-M ; ! "Baydäwlet" (Kazakh)
Байден:Байден NP-COG-MF ; ! ""
Байдеряков:Байдеряков NP-COG-OB ; ! ""
Байдуков:Байдуков NP-COG-OB ; ! ""
Байділда:Байділда NP-ANT-M ; ! "Baydilda" (Kazakh)
Байер% Мюнхен:Байер% Мюнхен%{☭%} NP-ORG ; ! "FC Bayern München"
Байжанбай:Байжанбай NP-ANT-M ; ! "Bayjanbay" (Kazakh)
Байжан:Байжан NP-ANT-M ; ! "Bayjan" (Kazakh)
Байжанов:Байжанов NP-COG-OB ; ! ""
Байжансай:Байжансай NP-TOP ; ! ""
Байжарас:Байжарас NP-ANT-M ; ! "Bayjaras" (Kazakh)
Байжол:Байжол NP-ANT-M ; ! "Bayjol" (Kazakh)
Байжігіт:Байжігіт NP-ANT-M ; ! "Bayjigit" (Kazakh)
Байзақ:Байзақ NP-ANT-M ; ! "Bayzaq" (Kazakh)
Байзақ:Байзақ NP-TOP ; ! ""
Байзақов:Байзақов NP-COG-OB ; ! ""
Байзақов:Байзақов NP-TOP ; ! ""
Байкал:Байкал NP-TOP ; ! ""
Байкал:Байкал NP-TOP ; ! ""
Байков:Байков NP-COG-OB ; ! ""
Байқабыл:Байқабыл NP-TOP ; ! ""
Байқадамов:Байқадамов NP-COG-OB ; ! ""
Байқанат:Байқанат NP-ANT-M ; ! "Bayqanat" (Kazakh)
Байқара:Байқара NP-TOP ; ! ""
Байқожа:Байқожа NP-TOP ; ! ""
Байқоныс:Байқоныс NP-TOP ; ! ""
Байқоңыр:Байқоңыр NP-TOP ; ! ""
Байқоңыр:Байқоңыр NP-TOP ; ! "Baikonur"
Байқоңыров:Байқоңыров NP-COG-OB ; ! ""
Байқошқар:Байқошқар NP-ANT-M ; ! "Bayqoshqar" (Kazakh)
Байқұт:Байқұт NP-ANT-M ; ! "Bayqut" (Kazakh)
Байқұтты:Байқұтты NP-ANT-M ; ! "Bayquttı" (Kazakh)
Байлы:Байлы NP-ANT-M ; ! "Baylı" (Kazakh)
Баймағамбет:Баймағамбет NP-ANT-M ; ! "Baymağambet" (Kazakh)
Баймағамбетов:Баймағамбетов NP-COG-OB ; ! ""
Баймағанбетов:Баймағанбетов NP-COG-OB ; ! ""
Баймағанбетов:Баймағанбетов NP-COG-OB ; ! "" ! Use/MT
Баймади:Баймади NP-ANT-M ; ! "Baymadıy" (Kazakh)
Байман:Байман NP-ANT-M ; ! "Bayman" (Kazakh)
Баймаханов:Баймаханов NP-COG-OB ; ! "" ! Use/MT
Байменов:Байменов NP-COG-OB ; ! ""
Баймұрат:Баймұрат NP-ANT-M ; ! "Baymurat" (Kazakh)
Баймұратов:Баймұратов NP-COG-OB ; ! ""
Баймырза:Баймырза NP-ANT-M ; ! "Baymırza" (Kazakh)
Баймырза:Баймырза NP-TOP ; ! ""
Баймырзаев:Баймырзаев NP-COG-OB ; ! ""
Байназар:Байназар NP-ANT-M ; ! "Baynazar" (Arabic)
Байнұр:Байнұр NP-ANT-M ; ! "Baynur" (Kazakh)
Байпаков:Байпаков NP-COG-OB ; ! "" ! Use/MT
Байпақов:Байпақов NP-COG-OB ; ! ""
Байрон:Байрон NP-ANT-M ; !"Use/MT"
Байрон:Байрон NP-COG-MF ; ! ""
Байсаков:Байсаков NP-COG-OB ; ! ""
Байсал:Байсал NP-ANT-M ; ! "Baysal"
Байсал:Байсал NP-ANT-M ; ! "Baysal" (Kazakh)
Байсан:Байсан NP-ANT-M ; ! "Baysan" (Kazakh)
Байсары:Байсары NP-ANT-M ; ! "Baysarı" (Kazakh)
Байсейiтов:Байсейiтов NP-COG-OB ; ! ""
Байсейіт:Байсейіт NP-TOP ; ! ""
Байсейітов:Байсейітов NP-COG-OB ; ! ""
Байсерке:Байсерке NP-ANT-M ; ! "Bayserke" (Kazakh)
Байсерке:Байсерке NP-TOP ; ! ""
Байсұлтан:Байсұлтан NP-ANT-M ; ! "Baysultan" (Arabic)
Байтазиев:Байтазиев NP-COG-OB ; ! "" ! Use/MT
Байтақ:Байтақ NP-ANT-M ; ! "Baytaq" (Kazakh)
Байтақкөл:Байтақкөл NP-TOP ; ! ""
Байтас:Байтас NP-ANT-M ; ! "Baytas" 
Байтемір:Байтемір NP-ANT-M ; ! "Baytemir" (Kazakh)
Байту:Байту NP-ANT-M ; ! "Baytıw" (Kazakh)
Байтуғай:Байтуғай NP-ANT-M ; ! "Baytıwğay" (Kazakh)
Байтұр:Байтұр NP-ANT-M ; ! "Baytur" (Kazakh)
Байтұрсынов:Байтұрсынов NP-COG-OB ; ! ""
Байтұрсынұлы:Байтұрсынұлы NP-COG-M ; ! "" ! Use/MT
Байұзақ:Байұзақ NP-ANT-M ; ! "Bayuzaq" (Kazakh)
Байшонас:Байшонас NP-TOP ; ! ""
Байшұлақов:Байшұлақов NP-COG-OB ; ! ""
Байызбай:Байызбай NP-ANT-M ; ! "Bayızbay" (Kazakh)
Байылдыр:Байылдыр NP-TOP ; ! ""
Байынқол:Байынқол NP-TOP ; ! ""
Байыр:Байыр NP-ANT-M ; ! "Bayır" (Kazakh)
Байырқұм:Байырқұм NP-TOP ; ! ""
Байысов:Байысов NP-COG-OB ; ! ""
Бак:Бак NP-ANT-M ; !"Use/MT"
Бакиев:Бакиев NP-COG-OB ; ! "Bakiyev"
Бакримович:Бакримо NP-PAT-VICH ; ! "" ! Use/MT 
Баку:Баку NP-TOP ; ! ""
Баку:Баку NP-TOP ; ! "" 
Бакчиев:Бакчиев NP-COG-OB ; ! ""
Бақай:Бақай NP-ANT-M ; ! "Baqay" (Arabic)
Бақай:Бақай NP-TOP ; ! ""
Бақалы:Бақалы NP-TOP ; ! ""
Бақанас:Бақанас NP-TOP ; ! ""
Бақашы:Бақашы NP-TOP ; ! ""
Бақбақты:Бақбақты NP-TOP ; ! ""
Бақбергенов:Бақбергенов NP-COG-OB ; ! ""
Бақи:Бақи NP-ANT-M ; ! "Baqıy" (Arabic)
Баққожа:Баққожа NP-ANT-M ; ! "Baqqoja" (Arabic)
Бақсай:Бақсай NP-TOP ; ! ""
Бақтаев:Бақтаев NP-COG-OB ; ! ""
Бақталап:Бақталап NP-ANT-M ; ! "Baqtalap" (Kazakh)
Бақтарай:Бақтарай NP-ANT-M ; ! "Baqtaray" (Kazakh)
Бақтыбай:Бақтыбай NP-ANT-M ; ! "Baqtıbay" (Kazakh)
Бақты:Бақты NP-TOP ; ! ""
Бақтыгүл:Бақтыгүл NP-ANT-F ; ! "Baqtıgül" (Arabic)
Бақтығұл:Бақтығұл NP-ANT-M ; ! "Baqtığul"
Бақшасарай:Бақшасарай NP-TOP ; ! ""
Бақыртау:Бақыртау NP-TOP ; ! ""
Бақыт:Бақыт NP-ANT-M ; !"Use/MT"
Бақытбек:Бақытбек NP-ANT-M ; ! "Baqıtbek" 
Бақытжан:Бақытжан NP-ANT-M ; ! "Baqıtjan" (Arabic)
Бақытжар:Бақытжар NP-ANT-M ; ! "Baqıtjar" (Kazakh)
Бақыткереев:Бақыткереев NP-COG-OB ; ! ""
Балабай:Балабай NP-ANT-M ; ! "Balabay" (Kazakh)
Балажанов:Балажанов NP-COG-OB ; ! ""
Балакөл:Балакөл NP-TOP ; ! ""
Балақаев:Балақаев NP-COG-OB ; ! ""
Балақай:Балақай NP-ANT-M ; ! "Balaqay" (Kazakh)
Балақұлболды:Балақұлболды NP-TOP ; ! ""
Баланды:Баланды NP-TOP ; ! ""
Баласағұн:Баласағұн NP-TOP ; ! ""
Балаталдық:Балаталдық NP-TOP ; ! ""
Балатопар:Балатопар NP-TOP ; ! ""
Балауса:Балауса NP-ANT-F ; ! "Balawsa" (Kazakh)
Балаұйық:Балаұйық NP-TOP ; ! ""
Балахмет:Балахмет NP-ANT-M ; ! "Balaxmet" (Arabic)
Бал:Бал NP-ANT-M ; !"Use/MT"
Балбала:Балбала NP-ANT-F ; ! "Balbala" (Kazakh)
Балбырауын:Балбырауын NP-TOP ; ! ""
Балгүл:Балгүл NP-ANT-F ; ! "Balgül" 
Балғабаев:Балғабаев NP-COG-OB ; ! ""
Балғабек:Балғабек NP-ANT-M ; ! "Balğabek" (Kazakh)
Балғаным:Балғаным NP-ANT-F ; ! "Balğanım" (Kazakh)
Балғасын:Балғасын NP-TOP ; ! ""
Балғымбаев:Балғымбаев NP-COG-OB ; ! "" ! Use/MT
Балдай:Балдай NP-ANT-F ; ! "Balday" (Kazakh)
Балдр:Балдр NP-ANT-M ; !
Балдық:Балдық NP-TOP ; ! ""
Балжан:Балжан NP-ANT-F ; ! "Baljan" 
Балиев:Балиев NP-COG-OB ; ! "" ! Use/MT
Балкан:Балкан NP-TOP ; ! ""
Балкашев:Балкашев NP-COG-OB ; ! ""
Балкашино:Балкашино NP-TOP ; ! ""
Балқадиша:Балқадиша NP-ANT-F ; ! "Balqadıysha" (Arabic)
Балқаш%-Алакөл:Балқаш%-Алакөл NP-TOP ; ! ""
Балқаш%-Алакөл:Балқаш%-Алакөл NP-TOP ; ! ""
Балқаш:Балқаш NP-TOP ; ! "Lake Balkhash"
Балқаш:Балқаш NP-TOP ; ! ""
Балқожа:Балқожа NP-ANT-M ; ! "Balqoja" (Arabic)
Балқұдық:Балқұдық NP-TOP ; ! ""
Балқыбай:Балқыбай NP-ANT-M ; ! "Balqıbay" (Arabic)
Балқы:Балқы NP-ANT-M ; ! "Balqı" (Kazakh)
Балқыбек:Балқыбек NP-ANT-M ; ! "Balqıbek" (Arabic)
Балмағамбетов:Балмағамбетов NP-COG-OB ; ! ""
Балмұқан:Балмұқан NP-ANT-M ; ! "Balmuqan" (Arabic)
Балтабай:Балтабай NP-ANT-M ; ! "Baltabay" (Kazakh)
Балтабай:Балтабай NP-TOP ; ! ""
Балта:Балта NP-ANT-M ; ! "Balta" (Kazakh)
Балтабек:Балтабек NP-ANT-M ; ! "Baltabek" (Kazakh)
Балтабеков:Балтабеков NP-COG-OB ; ! ""
Балтаев:Балтаев NP-COG-OB ; ! ""
Балтасов:Балтасов NP-COG-OB ; ! ""
Балтатарақ:Балтатарақ NP-TOP ; ! ""
Балтач:Балтач NP-TOP ; ! ""
Балтиев:Балтиев NP-COG-OB ; ! ""
Балтика:Балтика NP-TOP ; ! ""
Балтимор:Балтимор NP-TOP ; !"Use/MT"
Балтық:Балтық NP-TOP ; ! ""
Балтық:Балтық NP-TOP ; ! ""
Балтық% теңізі:Балтық% теңіз NP-TOP ; ! "" FIXME: similar to N-COMPUND-PX cont.class for proper nouns
Балуанғали:Балуанғали NP-ANT-M ; ! "Balıwanğalıy" (Arabic)
Балшекер:Балшекер NP-ANT-F ; ! "Balsheker" (Kazakh)
Балық:Балық NP-TOP ; ! ""
Балық% Бистәсі:Балық% Бистәсі NP-TOP ; ! ""
Балықтыкөл:Балықтыкөл NP-TOP ; ! ""
Балым:Балым NP-ANT-F ; ! "Balım" (Kazakh)
Балыұшы:Балыұшы NP-TOP ; ! ""
Бальбоа:Бальбоа NP-COG-MF ; !"Use/MT"
Бамако:Бамако NP-TOP ; ! "" 
Бангалор:Бангалор NP-TOP ; !"Use/MT"
Бангкок:Бангкок NP-TOP ; ! "" 
Бангладеш:Бангладеш NP-TOP ; ! ""
Бандар%-Сери%-Бегаван:Бандар%-Сери%-Бегаван NP-TOP ; ! "" 
Бандерас:Бандерас NP-COG-MF ; !"Use/MT"
Банко:Банко NP-ANT-M ; !
Банко:Банко NP-COG-MF ; ! ""
Бану:Бану NP-ANT-F ; ! "Banıw" (Arabic)
Баңладеш:Баңладеш NP-TOP ; ! ""
Бапа:Бапа NP-ANT-M ; ! "Bapa" (Arabic)
Бапов:Бапов NP-COG-OB ; ! ""
Бапы:Бапы NP-ANT-M ; ! "Bapy" khan USE/MT
Барабба:Барабба NP-ANT-M ; ! ""
Барабы:Барабы NP-TOP ; ! ""
Бараев:Бараев NP-COG-OB ; ! ""
Барак:Барак NP-ANT-M ; ! "Barack"
Барақ:Барақ NP-ANT-M ; ! "Baraq" (Arabic)
Барақкөл:Барақкөл NP-TOP ; ! ""
Баранов:Баранов NP-COG-OB ; ! ""
Барат:Барат NP-ANT-M ; ! "Barat" (Arabic)
Барахас:Барахас NP-TOP ; !"Use/MT"
Барбадос:Барбадос NP-TOP ; !"Use/MT"
Барбара:Барбара NP-ANT-F ; !"Use/MT"
Бардасов:Бардасов NP-COG-OB ; ! ""
Баржақсы:Баржақсы NP-ANT-M ; ! "Barjaqsı" (Kazakh)
Бари:Бари NP-ANT-M ; ! "Barıy" (Arabic)
Барклай:Барклай NP-COG-MF ; !"Use/MT"
Барклейз:Барклейз NP-TOP ; !"Use/MT"
Барлас:Барлас NP-ANT-M ; ! ""
Барлоу:Барлоу NP-COG-MF ; ! "" ! Use/MT
Барлыбай:Барлыбай NP-ANT-M ; ! "Barlıbay" (Kazakh)
Барлыбай:Барлыбай NP-TOP ; ! ""
Барлықарасан:Барлықарасан NP-TOP ; ! ""
Барлық:Барлық NP-TOP ; ! ""
Бармаққұм:Бармаққұм NP-TOP ; ! ""
Бароссо:Бароссо NP-COG-MF ; ! ""
Барретт:Барретт NP-ANT-M ; !"Use/MT"
Барри:Барри NP-ANT-M ; !"Use/MT"
Барро:Барро NP-COG-MF ; !"Use/MT"
Барсакелмес:Барсакелмес NP-TOP ; ! ""
Барселона:Барселона NP-ORG ; ! "Futbol Club Barcelona"
Барселона:Барселона NP-TOP ; !"Use/MT"
Бартоғай:Бартоғай NP-TOP ; ! ""
Бартушка:Бартушка NP-COG-MF ; ! ""
Барханқұм:Барханқұм NP-TOP ; ! ""
Баршақұм:Баршақұм NP-TOP ; ! ""
Баршатас:Баршатас NP-TOP ; ! ""
Баршын:Баршын NP-TOP ; ! ""
Баршынкент:Баршынкент NP-TOP ; ! ""
Барыс:Барыс NP-ANT-M ; ! "Barıs" (Kazakh)
Барышников:Барышников NP-COG-OB ; ! ""
Басарал:Басарал NP-TOP ; ! ""
Басар:Басар NP-ANT-M ; ! "Basar" (Kazakh)
Басбаев:Басбаев NP-COG-OB ; ! ""
Бас:Бас NP-ANT-M ; !"Use/MT"
Баскаков:Баскаков NP-COG-OB ; ! ""
Басқамыр:Басқамыр NP-TOP ; ! ""
Басқан:Басқан NP-ANT-M ; ! "Basqan" (Kazakh)
Басқан:Басқан NP-TOP ; ! ""
Басқұншақ:Басқұншақ NP-TOP ; ! ""
Басқыншы:Басқыншы NP-TOP ; ! ""
Баспаев:Баспаев NP-COG-OB ; ! ""
Басра:Басра NP-TOP ; ! ""
Бастаубай:Бастаубай NP-ANT-M ; ! "Bastawbay" (Kazakh)
Бастиан:Бастиан NP-ANT-M ; !"Use/MT"
Басши:Басши NP-TOP ; ! ""
Басықара:Басықара NP-TOP ; ! ""
Басым:Басым NP-ANT-M ; ! "Basım" (Kazakh)
Басымов:Басымов NP-COG-OB ; ! ""
Баталов:Баталов NP-COG-OB ; ! "" ! Use/MT
Баташов:Баташов NP-COG-OB ; ! ""
Батист:Батист NP-ANT-M ; !"Use/MT"
Баторий:Баторий NP-COG-M ; ! ""
Батпайсағыр:Батпайсағыр NP-TOP ; ! ""
Батпаққара:Батпаққара NP-TOP ; ! ""
Батпақты:Батпақты NP-TOP ; ! ""
Батраков:Батраков NP-COG-OB ; ! ""
Батсайы:Батсайы NP-ANT-F ; ! "Batsayı" (Kazakh)
Баттал:Баттал NP-ANT-M ; ! "Battal" (Persian)
Батталхан:Батталхан NP-ANT-M ; ! "Battalxan" (Persian)
Батуров:Батуров NP-COG-OB ; ! ""
Батый:Батый NP-ANT-M ; ! ""
Батыр:Батыр NP-ANT-M ; ! "Batır" (Kazakh)
Батырбек:Батырбек NP-ANT-M ; ! "Batırbek" (Kazakh)
Батырлан:Батырлан NP-ANT-M ; ! "Batırlan" (Kazakh)
Батырхан:Батырхан NP-ANT-M ; ! "Batırxan" (Kazakh)
Батырша:Батырша NP-ANT-M ; ! "Batırsha" 
Баубек:Баубек NP-ANT-M ; ! "Bawbek" (Kazakh)
Баулы:Баулы NP-TOP ; ! ""
Бауыржан:Бауыржан NP-ANT-M ; ! "Bawırjan" (Kazakh)
Бафинов:Бафинов NP-COG-OB ; ! ""
Бахар:Бахар NP-TOP ; ! ""
Бахмисов:Бахмисов NP-COG-OB ; ! ""
Бахрейн:Бахрейн NP-TOP ; ! ""
Бахтия:Бахтия NP-COG-MF ; !"Use/MT"
Бахтияр:Бахтияр NP-ANT-M ; ! "Baxtıyar" (Persian)
Бачериков:Бачериков NP-COG-OB ; ! ""
Баччан:Баччан NP-COG-MF ; !"Use/MT"
Башадар:Башадар NP-TOP ; ! ""
Башар:Башар NP-ANT-M ; ! ""
Башкиров:Башкиров NP-COG-OB ; ! ""
Башқұртстан:Башқұртстан NP-TOP ; ! ""
Башмаков:Башмаков NP-COG-OB ; ! "" ! Use/MT
Баязит:Баязит NP-ANT-M ; ! "Bayazıyt" (Arabic)
Баянаула:Баянаула NP-TOP ; ! ""
Баянауыл:Баянауыл NP-TOP ; ! ""
Баянауыл:Баянауыл NP-TOP ; ! "" 
Баян:Баян NP-ANT-F ; ! "Bayan" (Arabic)
Баян:Баян NP-ANT-M ; ! "Bayan" (Old Turkic)
Баянды:Баянды NP-ANT-M ; ! "Bayandı" (Kazakh)
Баянжүрек:Баянжүрек NP-TOP ; ! ""
Баянсұлу:Баянсұлу NP-ANT-F ; ! "Bayansulıw" (Kazakh)
Баят:Баят NP-ANT-M ; ! "Bayat" (Arabic)
Бәдинұр:Бәдинұр NP-ANT-F ; ! "Bädiynur" (Arabic)
Бәзила:Бәзила NP-ANT-F ; ! "Bäziyla"
Бәйбішетау:Бәйбішетау NP-TOP ; ! ""
Бәйгеқұм:Бәйгеқұм NP-TOP ; ! ""
Бәйдібек:Бәйдібек NP-TOP ; ! ""
Бәйменов:Бәйменов NP-COG-OB ; ! "" ! Use/MT
Бәйсейітов:Бәйсейітов NP-COG-OB ; ! ""
Бәйте:Бәйте NP-TOP ; ! ""
Бәйтен:Бәйтен NP-ANT-F ; ! "Baiten" 
Бәйтереков:Бәйтереков NP-COG-OB ; ! ""
Бәйімбетов:Бәйімбетов NP-COG-OB ; ! ""
Бәке:Бәке NP-ANT-M ; ! "Bake" ! "Need to check"
Бәке:Бәкең NP-ANT-M ; ! "Bake" ! "Need to check"
Бәкиев:Бәкиев NP-COG-OB ; ! ""
Бәкизат:Бәкизат NP-ANT-F ; ! "Bäkiyzat" (Persian)
Бәкібала:Бәкібала NP-ANT-F ; ! "Bäkibala" (Kazakh)
Бәкімбаев:Бәкімбаев NP-COG-OB ; ! ""
Бәкір:Бәкір NP-ANT-M ; ! "Bäkir" (Arabic)
Бәлиға:Бәлиға NP-ANT-F ; ! "Bäliyğa" (Arabic)
Бәлиев:Бәлиев NP-COG-OB ; ! "" ! Use/MT
Бәлия:Бәлия NP-ANT-F ; ! "Bälıya" (Arabic)
Бәрібаев:Бәрібаев NP-TOP ; ! ""
Бәсенов:Бәсенов NP-COG-OB ; ! ""
Бәтес:Бәтес NP-ANT-F ; ! "Bätes" (Russian)
Бәтима:Бәтима NP-TOP ; ! ""
Бәшір:Бәшір NP-ANT-M ; ! "Bäshir" (Arabic)
Беатрис:Беатрис NP-ANT-F ; !"Use/MT"
Бегалиев:Бегалиев NP-COG-OB ; ! ""
Беген:Беген NP-TOP ; ! ""
Бегеш:Бегеш NP-ANT-M ; ! "Begesh" (Old Turkic)
Бегімбет:Бегімбет NP-TOP ; ! ""
Бедарев:Бедарев NP-COG-OB ; ! ""
Беделбай:Беделбай NP-ANT-M ; ! "Bedelbay" (Kazakh)
Беделбек:Беделбек NP-ANT-M ; ! "Bedelbek" (Kazakh)
Беделхан:Беделхан NP-ANT-M ; ! "Bedelxan" (Kazakh)
Бедров:Бедров NP-COG-OB ; ! ""
Бежінтау:Бежінтау NP-TOP ; ! ""
Без:Без NP-ANT-M ; !"Use/MT"
Безсонов:Безсонов NP-COG-OB ; ! ""
Бейбітгүл:Бейбітгүл NP-ANT-F ; ! "Beybitgül" (Kazakh)
Бейбітжан:Бейбітжан NP-ANT-M ; ! "Beybitjan" (Persian)
Бейжің:Бейжің NP-TOP ; ! ""
Бейқан:Бейқан NP-ANT-M ; ! "Beyqan" (Arabic)
Бейқұт:Бейқұт NP-ANT-M ; ! "Beyqut" 
Бейнеу:Бейнеу NP-TOP ; ! ""
Бейрут:Бейрут NP-TOP ; ! ""
Бейрут:Бейрут NP-TOP ; ! "" 
Бейсалы:Бейсалы NP-ANT-M ; ! "Beysalı" (Persian)
Бейсбеков:Бейсбеков NP-COG-OB ; ! ""
Бейсекеев:Бейсекеев NP-COG-OB ; ! ""
Бейсеков:Бейсеков NP-COG-OB ; ! ""
Бейсенов:Бейсенов NP-COG-OB ; ! ""
Бейтен:Бейтен NP-ANT-F ; ! "Beiten" (Kazakh)
Бейімбет:Бейімбет NP-ANT-M ; ! "Beyimbet" (Kazakh)
Бейісбай:Бейісбай NP-ANT-M ; ! "Beyisbay" (Persian)
Бейіс:Бейіс NP-ANT-M ; ! "Beyis" (Persian)
Бейісбек:Бейісбек NP-ANT-M ; ! "Beyisbek" (Persian)
Бейісқали:Бейісқали NP-ANT-M ; ! "Beyisqalıy" (Persian)
Бекайдар:Бекайдар NP-ANT-M ; ! "Bekaydar" (Kazakh)
Бекәділ:Бекәділ NP-ANT-M ; ! "Bekädil" (Arabic)
Бекбаев:Бекбаев NP-COG-OB ; ! ""
Бекбай:Бекбай NP-ANT-M ; ! "Bekbay" (Kazakh)
Бекбатыр:Бекбатыр NP-ANT-M ; ! "Bekbatır" (Kazakh)
Бекбау:Бекбау NP-ANT-M ; ! "Bekbaw" 
Бек:Бек NP-ANT-M ; ! "Bek" (Kazakh)
Бек:Бек NP-COG-MF ; ! ""
Бекберген:Бекберген NP-ANT-M ; ! "Bekbergen" (Kazakh)
Бекбие:Бекбие NP-TOP ; ! ""
Бекболат:Бекболат NP-ANT-M ; ! "Bekbolat" (Kazakh)
Бекбол:Бекбол NP-ANT-M ; ! "Bekbol" 
Бекбосынов:Бекбосынов NP-COG-OB ; ! ""
Бекдайыр:Бекдайыр NP-ANT-M ; ! "Bekdayır" (Kazakh)
Бекдана:Бекдана NP-ANT-F ; ! "Bekdana" (Kazakh)
Бекежан:Бекежан NP-ANT-M ; ! "Bekejan" (Kazakh)
Бекежанов:Бекежанов NP-COG-OB ; ! ""
Бекей:Бекей NP-ANT-M ; ! "Bekey" (Kazakh)
Бекембай:Бекембай NP-ANT-M ; ! "Bekembay" (Kazakh)
Бекенов:Бекенов NP-COG-OB ; ! ""
Бекер:Бекер NP-ANT-M ; ! "Beker" (Old Turkic)
Бекер:Бекер NP-COG-MF ; ! ""
Бекетаев:Бекетаев NP-COG-OB ; ! ""
Бекетов:Бекетов NP-COG-OB ; ! ""
Бекжан:Бекжан NP-ANT-M ; ! "Bekjan" 
Бекжасар:Бекжасар NP-ANT-M ; ! "Bekjasar" (Kazakh)
Бекжігіт:Бекжігіт NP-ANT-M ; ! "Bekjigit" (Kazakh)
Бекзат:Бекзат NP-ANT-M ; ! "Bekzat" (Arabic)
Беккелді:Беккелді NP-ANT-M ; ! "Bekkeldi" (Kazakh)
Беккет:Беккет NP-COG-MF ; !"Use/MT"
Беккожа:Беккожа NP-ANT-M ; ! "Bekkoja" (Arabic)
Бекқали:Бекқали NP-ANT-M ; ! "Bekqalıy" (Kazakh)
Беклан:Беклан NP-ANT-M ; ! "Beklan" 
Бекмамбетов:Бекмамбетов NP-COG-OB ; ! ""
Бекмаханов:Бекмаханов NP-COG-OB ; ! ""
Бекмәмбетов:Бекмәмбетов NP-COG-OB ; ! ""
Бекмұрат:Бекмұрат NP-ANT-M ; ! "Bekmurat" (Arabic)
Бекмұхамбет:Бекмұхамбет NP-ANT-M ; ! "Bekmuxambet" (Kazakh)
Бекмұхамбетов:Бекмұхамбетов NP-COG-OB ; ! "" ! Use/MT
Бекмұханбетов:Бекмұханбетов NP-COG-OB ; ! ""
Бекназар:Бекназар NP-ANT-M ; ! "Beknazar" (Arabic)
Бекназаров:Бекназаров NP-COG-OB ; ! ""
Бекнар:Бекнар NP-ANT-M ; ! "Beknar" (Arabic)
Бекнияз:Бекнияз NP-ANT-M ; ! "Beknıyaz" (Arabic)
Бексейіт:Бексейіт NP-ANT-M ; ! "Bekseyit" (Arabic)
Бексұлтан:Бексұлтан NP-ANT-M ; ! "Beksultan" (Arabic)
Бексұлу:Бексұлу NP-ANT-F ; ! "Beksulıw" (Kazakh)
Бектаев:Бектаев NP-COG-OB ; ! ""
Бектауата:Бектауата NP-TOP ; ! ""
Бектемір:Бектемір NP-ANT-M ; ! "Bektemir" (Kazakh)
Бектеміс:Бектеміс NP-ANT-M ; ! "Bektemis" (Kazakh)
Бектен:Бектен NP-ANT-M ; ! "Bekten" 
Бектөбе:Бектөбе NP-TOP ; ! ""
Бектұр:Бектұр NP-ANT-M ; ! "Bektur" 
Бектұрган:Бектұрган NP-ANT-M ; ! "Bekturgan" (Kazakh)
Бектұрғанов:Бектұрғанов NP-COG-OB ; ! ""
Бектұрсын:Бектұрсын NP-ANT-M ; ! "Bektursın" (Kazakh)
Бектілеу:Бектілеу NP-ANT-M ; ! "Bektilew" 
Бекшентаев:Бекшентаев NP-COG-OB ; ! ""
Бекі:Бекі NP-TOP ; ! ""
Бекімов:Бекімов NP-COG-OB ; ! ""
Бекін:Бекін NP-ANT-M ; ! "Bekin" (Kazakh)
Белағаш:Белағаш NP-TOP ; ! ""
Беларус:Беларус NP-AL ; ! ""
Беларуссия:Беларуссия NP-TOP ; ! ""
Беларусь:Беларусь NP-TOP ; ! "Belarus"
Беларусь:Белорусь NP-TOP ; ! "Belarus" Dir/LR
Беласар:Беласар NP-ANT-M ; ! "Belasar" (Kazakh)
Беласар:Беласар NP-TOP ; ! ""
Беласыл:Беласыл NP-ANT-M ; ! "Belasıl" (Kazakh)
Белбасар:Белбасар NP-TOP ; ! ""
Белберді:Белберді NP-ANT-M ; ! "Belberdi" (Kazakh)
Белбұлақ:Белбұлақ NP-TOP ; ! ""
Белгород:Белгород NP-TOP ; ! ""
Белград:Белград NP-TOP ; ! "Belgrade"
Белгібай:Белгібай NP-ANT-M ; ! "Belgibay" (Old Turkic)
Белен:Белен NP-ANT-F ; !"Use/MT"
Белес:Белес NP-ANT-M ; ! "Beles" (Kazakh)
Белиз:Белиз NP-TOP ; !"Use/MT"
Белинда:Белинда NP-ANT-F ; !"Use/MT"
Белинский:Белинский NP-COG-M ; ! ""
Белинский:Белинский NP-COG-M ; ! ""
Белков:Белков NP-COG-OB ; ! ""
Белкөл:Белкөл NP-TOP ; ! ""
Белла:Белла NP-ANT-F ; ! "Bella" (Latin)
Беллини:Беллини NP-COG-M ; ! ""
Белов:Белов NP-COG-OB ; ! ""
Беловощев:Беловощев NP-COG-OB ; ! ""
Белоглазов:Белоглазов NP-COG-OB ; ! ""
Белогорский:Белогорский NP-TOP ; ! ""
Белозёров:Белозёров NP-COG-OB ; ! ""
Белоруссия:Белоруссия NP-TOP ; ! ""
Белоусов:Белоусов NP-COG-OB ; ! ""
Белоусовка:Белоусовка NP-TOP ; ! ""
Белсексеуіл:Белсексеуіл NP-TOP ; ! ""
Белтам:Белтам NP-TOP ; ! ""
Белтерек:Белтерек NP-TOP ; ! ""
Бельгия:Бельгия NP-TOP ; ! "Belgium"
Бельтов:Бельтов NP-COG-OB ; ! ""
Беляев:Беляев NP-COG-OB ; ! ""
Беляков:Беляков NP-COG-OB ; ! ""
Беназир:Беназир NP-ANT-F ; !"Use/MT"
Бен:Бен NP-ANT-M ; !"Use/MT"
Бенгази:Бенгази NP-TOP ; !"Use/MT"
Бенгал:Бенгал NP-TOP ; ! ""
Бенедикт:Бенедикт NP-ANT-M ; !"Use/MT"
Бенин:Бенин NP-TOP ; ! ""
Бенита:Бенита NP-ANT-F ; !"Use/MT"
Бенито:Бенито NP-ANT-M ; !"Use/MT"
Беннетт:Беннетт NP-ANT-M ; !"Use/MT"
Бер:Бер NP-ANT-M ; !"Use/MT"
Берг:Берг NP-TOP ; ! ""
Берген:Берген NP-ANT-M ; ! "Bergen" (Kazakh)
Бергман:Бергман NP-ANT-M ; ! ""
Берғалиев:Берғалиев NP-COG-OB ; ! ""
Бердалиев:Бердалиев NP-COG-OB ; ! "" ! Use/MT
Берден:Берден NP-ANT-M ; ! "Berden" (Kazakh)
Бердешов:Бердешов NP-COG-OB ; ! "" ! Use/MT
Бердичев:Бердичев NP-COG-OB ; ! ""
Бердияр:Бердияр NP-ANT-M ; ! "Berdıyar" (Kazakh)
Бердников:Бердников NP-COG-OB ; ! ""
Бердығалиев:Бердығалиев NP-COG-OB ; ! ""
Бердыев:Бердыев NP-COG-OB ; ! ""
Бердібаев:Бердібаев NP-COG-OB ; ! ""
Бердібек:Бердібек NP-ANT-M ; ! "" ! Use/MT
Берді:Берді NP-ANT-M ; ! "Berdi" (Kazakh)
Бердімұрат:Бердімұрат NP-ANT-M ; ! "Berdimurat" (Kazakh)
Бердімұхамедов:Бердімұхамедов NP-COG-OB ; ! "" ! Use/MT
Бердімұхаммедов:Бердімұхаммедов NP-COG-OB ; ! ""
Бердінияз:Бердінияз NP-ANT-M ; ! "Berdinıyaz" (Persian)
Береговой:Береговой NP-TOP ; ! ""
Березов:Березов NP-COG-OB ; ! ""
Березовка:Березовка NP-TOP ; ! ""
Березовский:Березовский NP-TOP ; ! ""
Береке:Береке NP-TOP ; ! ""
Берекет:Берекет NP-ORG ; ! ""
Берел:Берел NP-TOP ; ! ""
Берен:Берен NP-ANT-M ; ! "Beren" (Kazakh)
Бержан:Бержан NP-ANT-M ; ! "Berjan" (Kazakh)
Берзиньш:Берзиньш NP-COG-M ; !"Use/MT"
Беринг:Беринг NP-COG-M ; ! ""
Беринг:Беринг NP-TOP ; ! ""
Берингов:Берингов NP-TOP ; ! ""
Беркалиев:Беркалиев NP-COG-OB ; ! "" ! Use/MT
Берк:Берк NP-COG-MF ; !"Use/MT"
Беркли:Беркли NP-TOP ; !"Use/MT"
Беркін:Беркін NP-ANT-M ; ! "Berkin" (Kazakh)
Берқайыр:Берқайыр NP-ANT-M ; ! "Berqayır" (Kazakh)
Берлин:Берлин NP-TOP ; ! "Berlin"
Берлиоз:Берлиоз NP-COG-MF ; ! ""
Берлускони:Берлускони NP-COG-MF ; ! "" ! Use/MT 
Бермамыт:Бермамыт NP-TOP ; ! ""
Бермуд:Бермуд NP-TOP ; ! "Bermuda"
Бернар:Бернар NP-ANT-M ; ! "Bernar" (Kazakh)
Бернард:Бернард NP-ANT-M ; !"Use/MT"
Берн:Берн NP-TOP ; ! ""
Берн:Берн NP-TOP ; ! "" 
Бернд:Бернд NP-ANT-M ; !"Use/MT"
Бернштейн:Бернштейн NP-COG-MF ; ! "" ! Use/MT
Бернштейн-Коган:Бернштейн-Коган NP-COG-M ; ! ""
Берсиев:Берсиев NP-COG-OB ; ! ""
Берта:Берта NP-ANT-F ; ! "Berta" (Old High German)
Бертенсон:Бертенсон NP-COG-M ; ! ""
Бертран:Бертран NP-ANT-M ; !"Use/MT"
Бертіс:Бертіс NP-TOP ; ! ""
Берцелиус:Берцелиус NP-COG-MF ; ! ""
Бершінтөбе:Бершінтөбе NP-TOP ; ! ""
Берікбай:Берікбай NP-ANT-M ; ! "Berikbay" 
Берік:Берік NP-ANT-M ; ! "Berik" (Kazakh)
Берікқара:Берікқара NP-TOP ; ! ""
Беріктас:Беріктас NP-TOP ; ! ""
Бесағаш:Бесағаш NP-TOP ; ! ""
Бесарық:Бесарық NP-TOP ; ! ""
Бесбай:Бесбай NP-ANT-M ; ! "Besbay" (Kazakh)
Бесбалық:Бесбалық NP-TOP ; ! ""
Бесжылдық:Бесжылдық NP-TOP ; ! ""
Бесир:Бесир NP-ANT-M ; ! ""
Бескөл:Бескөл NP-TOP ; ! ""
Бесқайнар:Бесқайнар NP-TOP ; ! ""
Бесқарағай:Бесқарағай NP-TOP ; ! ""
Бесқасқа:Бесқасқа NP-TOP ; ! ""
Бесмойнақ:Бесмойнақ NP-TOP ; ! ""
Бесоба:Бесоба NP-TOP ; ! ""
Беспалов:Беспалов NP-COG-OB ; ! ""
Бессараб:Бессараб NP-TOP ; !""
Бессон:Бессон NP-COG-MF ; !"Use/MT"
Бестай:Бестай NP-COG-MF ; ! ""
Бестамақ:Бестамақ NP-TOP ; ! ""
Бестамқала:Бестамқала NP-TOP ; ! ""
Бестас:Бестас NP-TOP ; ! ""
Бестерек:Бестерек NP-TOP ; ! ""
Бестөбе:Бестөбе NP-TOP ; ! ""
Бестібаев:Бестібаев NP-COG-OB ; ! ""
Бесчасов:Бесчасов NP-COG-OB ; ! ""
Бесшатыр:Бесшатыр NP-TOP ; ! ""
Бесшоқы:Бесшоқы NP-TOP ; ! ""
Бетани:Бетани NP-ANT-F ; !"Use/MT"
Бетен:Бетен NP-TOP ; ! ""
Бетина:Бетина NP-ANT-F ; !"Use/MT"
Бетқайнар:Бетқайнар NP-TOP ; ! ""
Бетпақдала:Бетпақдала NP-TOP ; ! ""
Бетти:Бетти NP-ANT-F ; !"Use/MT"
Бетховен:Бетховен NP-COG-MF ; !"Use/MT"
Бехар:Бехар NP-COG-M ; !"Use/MT"
Бечасын:Бечасын NP-TOP ; ! ""
Биахмет:Биахмет NP-ANT-M ; ! "Bıyaxmet" (Kazakh)
Бибатыр:Бибатыр NP-ANT-M ; ! "Bıybatır" (Kazakh)
Биби:Биби NP-ANT-F ; ! "Biybiy" (Persian)
Бибигүл:Бибигүл NP-ANT-F ; ! "Biybiygül" (Persian)
Бибиғаным:Бибиғаным NP-ANT-F ; ! "Bıybıyğanım" (Kazakh)
Бибижамал:Бибижамал NP-ANT-F ; ! "Bıybıyjamal" (Persian)
Бибинұр:Бибинұр NP-ANT-F ; ! "Bıybıynur" (Persian)
Бибисара:Бибисара NP-ANT-F ; ! "Bıybıysara" (Arabic)
Бигелдинов:Бигелдинов NP-COG-OB ; ! ""
Бигиев:Бигиев NP-COG-OB ; ! ""
Биғалым:Биғалым NP-ANT-M ; ! "Bıyğalım" (Kazakh)
Биғаш:Биғаш NP-TOP ; ! ""
Бидайбек:Бидайбек NP-ANT-M ; ! "Bıydaybek" (Kazakh)
Бидайкөл:Бидайкөл NP-TOP ; ! ""
Бидайық:Бидайық NP-TOP ; ! ""
Бижан:Бижан NP-ANT-M ; ! "Bıyjan" (Persian)
Биз:Биз NP-ANT-M ; !"Use/MT"
Бизнес% нью% юроуп:Бизнес% нью% юроуп NP-ORG ; ! ""
Бик:Бик NP-ANT-M ; !"Use/MT"
Биқұм:Биқұм NP-TOP ; ! ""
Билал:Билал NP-ANT-M ; !"Use/MT"
Бил:Бил NP-ANT-M ; !"Use/MT"
Билл:Билл NP-ANT-M ; ! ""
Билли:Билли NP-ANT-M ; !"Use/MT"
Билікөл:Билікөл NP-TOP ; ! ""
Бимағамбет:Бимағамбет NP-ANT-M ; ! "Bıymağambet" (Arabic)
Биман:Биман NP-ANT-M ; ! "Bıyman" (Persian)
Бим:Бим NP-ANT-M ; !"Use/MT"
биоморфология:биоморфология NP-TOP ; !"Use/MT"
Бир:Бир NP-ANT-M ; !"Use/MT"
Бирма:Бирма NP-TOP ; ! ""
Бирн:Бирн NP-COG-MF ; !"Use/MT"
Бирюков:Бирюков NP-COG-OB ; ! ""
Бисекен:Бисекен NP-ANT-M ; ! "" 
Бисенғалиев:Бисенғалиев NP-COG-OB ; ! ""
Бискай:Бискай NP-TOP ; ! ""
Бисмарк:Бисмарк NP-COG-MF ; ! ""
Биш:Биш NP-ANT-M ; !"Use/MT"
Биікжал:Биікжал NP-TOP ; ! ""
Биіктау:Биіктау NP-TOP ; ! ""
Благовещенск:Благовещенск NP-TOP ; ! ""
Благоевград:Благоевград NP-TOP ; !"Use/MT"
Бланка:Бланка NP-ANT-F ; !"Use/MT"
Бланко:Бланко NP-COG-MF ; ! ""
Блез:Блез NP-ANT-M ; !"Use/MT"
Блейк:Блейк NP-ANT-M ; !"Use/MT"
Блинов:Блинов NP-COG-OB ; ! ""
Блохин:Блохин NP-COG-IN ; ! ""
Блохинцев:Блохинцев NP-COG-OB ; ! ""
Блумберг:Блумберг NP-ORG ; ! ""
Блэйз:Блэйз NP-ANT-M ; !"Use/MT"
Блэкджек:Блэкджек NP-ANT-M ; !
Блэклэддер:Блэклэддер NP-ANT-M ; !
Блэр:Блэр NP-COG-M ; ! ""
Блэр:Блэр NP-COG-MF ; ! ""
Бобби:Бобби NP-ANT-M ; !"Use/MT"
Боб:Боб NP-ANT-M ; !"Use/MT"
Бобков:Бобков NP-COG-OB ; ! ""
Бо:Бо NP-ANT-M ; !"Use/MT"
Бобров:Бобров NP-COG-OB ; ! ""
Бобылёв:Бобылёв NP-COG-OB ; ! ""
Богатов:Богатов NP-COG-OB ; ! ""
Богдан:Богдан NP-ANT-M ; ! ""
Богданов:Богданов NP-COG-OB ; ! ""
Богемия:Богемия NP-TOP ; !""
Боголюбов:Боголюбов NP-COG-OB ; ! ""
Боголюбский:Боголюбский NP-COG-M ; ! ""
Богота:Богота NP-TOP ; !""
Бодан:Бодан NP-ANT-M ; ! "Bodan" (Arabic)
Бодрийяр:Бодрийяр NP-TOP ; ! ""
Бодуэн:Бодуэн NP-ANT-M ; !"Use/MT"
Бозай:Бозай NP-ANT-M ; ! "Bozay" (Kazakh)
Бозақор:Бозақор NP-TOP ; ! ""
Бозанбай:Бозанбай NP-TOP ; ! ""
Бозарал:Бозарал NP-TOP ; ! ""
Бозащы:Бозащы NP-TOP ; ! ""
Боз:Боз NP-ANT-M ; !"Use/MT"
Боздақ:Боздақ NP-ANT-M ; ! "Bozdaq" (Kazakh)
Бозжиде:Бозжиде NP-TOP ; ! ""
Бозкөл:Бозкөл NP-TOP ; ! ""
Бозой:Бозой NP-TOP ; ! ""
Бозшакөл:Бозшакөл NP-TOP ; ! ""
Бозымбаев:Бозымбаев NP-COG-OB ; ! ""
Боинг:Боинг NP-ORG ; !"Use/MT"
Боинг:Боинг NP-TOP ; !"Use/MT"
Боинг:Боинг NP-TOP ; !"Use/MT"
Бок:Бок NP-COG-MF ; ! "" ! Use/MT
Боков:Боков NP-COG-OB ; ! ""
Боқаев:Боқаев NP-COG-OB ; ! ""
Боқсық:Боқсық NP-TOP ; ! ""
Боқтыбай:Боқтыбай NP-TOP ; ! ""
Боқтықарын:Боқтықарын NP-TOP ; ! ""
Болатбай:Болатбай NP-ANT-M ; ! "Bolatbay" (Latin)
Болатбек:Болатбек NP-ANT-M ; ! "Bolatbek" (Latin)
Болат:Болат NP-ANT-M ; ! "Bolat" (Latin)
Болатжан:Болатжан NP-ANT-M ; ! "Bolatjan" (Latin)
Болатқожа:Болатқожа NP-ANT-M ; ! "Bolatqoja" (Arabic)
Болаттай:Болаттай NP-ANT-M ; ! "Bolattay" (Latin)
Болаттау:Болаттау NP-TOP ; ! ""
Болатхан:Болатхан NP-ANT-M ; ! "Bolatxan" (Latin)
Болгария:Болгария NP-TOP ; ! "Bulgaria"
Болғанбай:Болғанбай NP-ANT-M ; ! "Bolğanbay" (Kazakh)
Болған:Болған NP-ANT-M ; ! "Bolğan" (Kazakh)
Болдуин:Болдуин NP-ANT-M ; ! ""
Болдырев:Болдырев NP-COG-OB ; ! ""
Боливия:Боливия NP-TOP ; ! ""
Болкиях:Болкиях NP-COG-MF ; ! ""
Болотов:Болотов NP-COG-OB ; ! ""
Болсынбек:Болсынбек NP-ANT-M ; ! "Bolsınbek" (Kazakh)
Болтаев:Болтаев NP-COG-OB ; ! ""
Большаков:Большаков NP-COG-OB ; ! ""
Бомбей:Бомбей NP-TOP ; ! ""
Бонавентура:Бонавентура NP-ANT-M ; !"Use/MT"
Бонапарт:Бонапарт NP-COG-MF ; !"Use/MT"
Бондарев:Бондарев NP-COG-OB ; ! ""
Бонн:Бонн NP-TOP ; ! "Bonn"
Боно:Боно NP-ANT-M ; !"Use/MT"
Боралдай:Боралдай NP-TOP ; ! ""
Боранбай:Боранбай NP-ANT-M ; ! "Boranbay" (Kazakh)
Боран:Боран NP-ANT-M ; ! "Boran" (Kazakh)
Боран:Боран NP-TOP ; ! ""
Боранғазы:Боранғазы NP-ANT-M ; ! "Boranğazı" (Arabic)
Боранқұл:Боранқұл NP-TOP ; ! ""
Борг:Борг NP-COG-MF ; !"Use/MT"
Борис:Борис NP-ANT-M ; ! "" 
Борисов:Борисов NP-COG-OB ; ! ""
Бормойнақ:Бормойнақ NP-TOP ; ! ""
Борнхольм:Борнхольм NP-ANT-M ; ! ""
Боровский:Боровский NP-TOP ; ! ""
Бородулиха:Бородулиха NP-TOP ; ! ""
Боронников:Боронников NP-COG-OB ; ! ""
Борохоро:Борохоро NP-TOP ; ! ""
Борусевич:Борусе NP-PAT-VICH ; ! ""
Боруссия% Дортмунд:Боруссия% Дортмунд%{☭%} NP-ORG ; ! "Borussia Dortmund"
Борхес:Борхес NP-COG-MF ; !"Use/MT"
Борщов:Борщов NP-COG-OB ; ! ""
Босния:Босния NP-TOP ; !"Use/MT"
Бостандық:Бостандық NP-TOP ; ! ""
Бостанқұм:Бостанқұм NP-TOP ; ! ""
Бостон:Бостон NP-TOP ; !"Use/MT"
Босфор:Босфор NP-TOP ; ! ""
Ботабай:Ботабай NP-ANT-M ; ! "Botabay" (Kazakh)
Бота:Бота NP-ANT-F ; ! "Bota" (Kazakh)
Ботайтөбе:Ботайтөбе NP-TOP ; ! ""
Ботакөз:Ботакөз NP-ANT-F ; ! "Botaköz" (Kazakh)
Ботақара:Ботақара NP-TOP ; ! ""
Боткөл:Боткөл NP-TOP ; ! ""
Ботсвана:Ботсвана NP-TOP ; ! ""
Боучез:Боучез NP-COG-MF ; ! ""
Бочков:Бочков NP-COG-OB ; ! ""
Бошай:Бошай NP-ANT-M ; ! "Boshay" (Old Turkic)
Бош:Бош NP-ORG ; ! "USE/MT"
Бөгембай:Бөгембай NP-TOP ; ! ""
Бөген:Бөген NP-TOP ; ! ""
Бөгетсай:Бөгетсай NP-TOP ; ! ""
Бөжей:Бөжей NP-ANT-M ; ! "Böjey"
Бөжей:Бөжей NP-ANT-M ; ! "Böjey" (Old Turkic)
Бөке:Бөке NP-TOP ; ! ""
Бөкеев:Бөкеев NP-COG-OB ; ! ""
Бөкей:Бөкей NP-ANT-M ; ! "Bökey" (Old Turkic)
Бөкей:Бөкей NP-TOP ; ! ""
Бөкейхан:Бөкейхан NP-ANT-M ; ! "" 
Бөкейхан:Бөкейхан NP-ANT-M ; ! "" 
Бөкейханов:Бөкейханов NP-COG-OB ; ! ""
Бөкейханұлы:Бөкейханұлы NP-COG-M ; ! "" ! Use/MT
Бөкенкөл:Бөкенкөл NP-TOP ; ! ""
Бөкенші:Бөкенші NP-TOP ; ! ""
Бөкенші:Бөкенші NP-TOP ; ! ""
Бөкетов:Бөкетов NP-COG-OB ; ! ""
Бөлебай:Бөлебай NP-ANT-M ; ! "Bölebay" (Kazakh)
Бөлегенов:Бөлегенов NP-COG-OB ; ! "USE/MT"
Бөлекаяқ:Бөлекаяқ NP-TOP ; ! ""
Бөлек:Бөлек NP-TOP ; ! ""
Бөлексаз:Бөлексаз NP-TOP ; ! ""
Бөпетай:Бөпетай NP-ANT-M ; ! "Böpetay" (Persian)
Бөрібай:Бөрібай NP-ANT-M ; ! "Böribay" (Kazakh)
Бөрі:Бөрі NP-ANT-M ; ! "Böri" (Kazakh)
Бөріжар:Бөріжар NP-TOP ; ! ""
Бөріқазған:Бөріқазған NP-TOP ; ! ""
Бөрілі:Бөрілі NP-TOP ; ! ""
Браво:Браво NP-COG-MF ; !"Use/MT"
Брага:Брага NP-TOP ; !"Use/MT"
Браганса:Браганса NP-TOP ; !"Use/MT"
Брагин:Брагин NP-COG-IN ; ! ""
Бразилиа:Бразилиа NP-TOP ; ! "" 
Бразилия:Бразилия NP-TOP ; ! ""
Брайан:Брайан NP-ANT-M ; !"Use/MT"
Брайант:Брайант NP-ANT-M ; !"Use/MT"
Брал:Брал NP-COG-MF ; !"Use/MT"
Бранденбург:Бранденбург NP-TOP ; !""
Бранислав:Бранислав NP-ANT-M ; ! ""
Бранко:Бранко NP-COG-MF ; ! ""
Бранку:Бранку NP-TOP ; !"Use/MT"
Братислава:Братислава NP-TOP ; ! ""
Братислава:Братислава NP-TOP ; ! "" 
Браун:Браун NP-COG-MF ; !"Use/MT"
Брахим:Брахим NP-ANT-M ; !"Use/MT"
Брежнев:Брежнев NP-COG-OB ; ! ""
Бремен:Бремен NP-TOP ; !"Use/MT"
Брендан:Брендан NP-ANT-M ; !"Use/MT"
Брендон:Брендон NP-ANT-M ; !"Use/MT"
Бреннан:Бреннан NP-ANT-M ; !"Use/MT"
Брент:Брент NP-ANT-M ; !"Use/MT"
Бриджит:Бриджит NP-ANT-F ; !"Use/MT"
Брилов:Брилов NP-COG-OB ; ! "" ! Use/MT
Бриндизи:Бриндизи NP-TOP ; !"Use/MT"
Брисбен:Брисбен NP-TOP ; !"Use/MT"
Брис:Брис NP-ANT-M ; !"Use/MT"
Британия:Британия NP-TOP ; ! "Britain"
Бритиш% Петролеум:Бритиш% Петролеум NP-ORG ; ! ""
Брно:Брно NP-TOP ; !"Use/MT"
Бродвей:Бродвей NP-TOP ; !"Use/MT"
Брок:Брок NP-ANT-M ; !"Use/MT"
Бронислав:Бронислав NP-ANT-M ; !"Use/MT"
Бронкс:Бронкс NP-TOP ; ! "Bronx"
Броуди:Броуди NP-ANT-M ; !"Use/MT"
Бруклин:Бруклин NP-ANT-F ; !"Use/MT"
Бруна:Бруна NP-ANT-F ; !"Use/MT"
Бруней:Бруней NP-TOP ; ! ""
Бруни:Бруни NP-ANT-M ; ! ""
Бруно:Бруно NP-ANT-M ; ! "Bruno" (Kazakh)
Бруно:Бруно NP-ANT-M ; !"Use/MT"
Бруссел:Бруссел NP-TOP ; ! "" 
Брэд:Брэд NP-ANT-M ; !"Use/MT"
Брэди:Брэди NP-ANT-M ; !"Use/MT"
Брэдли:Брэдли NP-ANT-M ; !"Use/MT"
Брэкстон:Брэкстон NP-ANT-M ; !"Use/MT"
Брэм:Брэм NP-ANT-M ; !"Use/MT"
Брэндон:Брэндон NP-ANT-M ; !"Use/MT"
Брюллов:Брюллов NP-COG-OB ; ! ""
Брюс:Брюс NP-ANT-M ; !"Use/MT"
Брюсов:Брюсов NP-COG-M ; ! "USE/MT"
Брюссель:Брюссель NP-TOP ; ! "Brussels"
Брянск:Брянск NP-TOP ; ! ""
Брянцев:Брянцев NP-COG-OB ; ! ""
БТА:БТА NP-ORG-ABBR ; ! "a bank in Kazakhstan"
Буа:Буа NP-TOP ; ! ""
Бу:Бу NP-ANT-M ; !"Use/MT"
Будапешт:Будапешт NP-TOP ; ! "Budapest"
Бударин:Бударин NP-TOP ; ! ""
Буз:Буз NP-ANT-M ; !"Use/MT"
Бук:Бук NP-ANT-M ; !"Use/MT"
Букер:Букер NP-COG-MF ; ! "" ! Use/MT
Букетов:Букетов NP-COG-OB ; ! ""
Булаев:Булаев NP-COG-OB ; ! ""
Булаев:Булаев NP-TOP ; ! ""
Булгаков:Булгаков NP-COG-OB ; ! ""
Бундеслига:Бундеслига NP-ORG ; ! "Bundesliga"
Бунин:Бунин NP-COG-MF ; ! ""
Бунямин:Бунямин NP-ANT-M ; ! "Benjamin"
Бурабай:Бурабай NP-ANT-M ; ! "Bıwrabay" (Kazakh)
Бурабай:Бурабай NP-TOP ; ! ""
Бура:Бура NP-ANT-M ; ! "Bıwra" (Kazakh)
Бурахан:Бурахан NP-ANT-M ; ! "Bıwraxan" (Kazakh)
Бургас:Бургас NP-TOP ; !"Use/MT"
Бургенланд:Бургенланд NP-TOP ; !"Use/MT"
Буриев:Буриев NP-COG-OB ; ! "" ! Use/MT
Буркина% Фасо:Буркина% Фасо NP-TOP ; !"Use/MT"
Бурмистров:Бурмистров NP-COG-OB ; ! ""
Буров:Буров NP-COG-OB ; ! ""
Бурсик:Бурсик NP-COG-MF ; ! ""
Бурунди:Бурунди NP-TOP ; ! ""
Бурундуков:Бурундуков NP-COG-OB ; ! ""
Бурятия:Бурятия NP-TOP ; ! ""
Буслаев:Буслаев NP-COG-OB ; ! ""
Бусленко:Бусленко NP-COG-MF ; ! ""
Бутаков:Бутаков NP-COG-OB ; ! ""
Бутан:Бутан NP-TOP ; ! ""
Бутан:Бутан NP-TOP ; ! ""
Бутенко:Бутенко NP-COG-MF ; ! ""
Буфа:Буфа NP-ANT-M ; !"Use/MT"
Бухарест:Бухарест NP-TOP ; ! "Bucharest"
Бухари:Бухари NP-COG-MF ; ! ""
Буценко:Буценко NP-COG-MF ; ! ""
Буш:Буш NP-COG-MF ; ! ""
Буэнос% Айрес:Буэнос% Айрес NP-TOP ; ! "" 
Бұғыбай:Бұғыбай NP-ANT-M ; ! "Buğıbay" (Kazakh)
Бұғылы:Бұғылы NP-TOP ; ! ""
Бұғыты:Бұғыты NP-TOP ; ! ""
Бұзанай:Бұзанай NP-TOP ; ! ""
Бұзаушы:Бұзаушы NP-ANT-M ; ! "Buzawshı" 
Бұзыққала:Бұзыққала NP-TOP ; ! ""
Бұзықтөбе:Бұзықтөбе NP-TOP ; ! ""
Бұқанов:Бұқанов NP-COG-OB ; ! "" ! Use/MT
Бұқантау:Бұқантау NP-TOP ; ! ""
Бұқарбай:Бұқарбай NP-TOP ; ! ""
Бұқар% Жырау:Бұқар% Жырау NP-TOP ; ! ""
Бұқтырма:Бұқтырма NP-TOP ; ! ""
Бұлабай:Бұлабай NP-ANT-M ; ! "Bulabay" (Kazakh)
Бұлақбай:Бұлақбай NP-ANT-M ; ! "Bulaqbay" (Kazakh)
Бұлақты:Бұлақты NP-TOP ; ! ""
Бұланбай:Бұланбай NP-ANT-M ; ! "Bulanbay" (Kazakh)
Бұлан:Бұлан NP-TOP ; ! ""
Бұланды:Бұланды NP-TOP ; ! ""
Бұланты:Бұланты NP-TOP ; ! ""
Бұлбұл:Бұлбұл NP-ANT-F ; ! "Bulbul" (Persian)
Бұлғар:Бұлғар NP-TOP ; ! ""
Бұлдыбай:Бұлдыбай NP-ANT-M ; ! "Buldıbay" (Kazakh)
Бұлдырты:Бұлдырты NP-TOP ; ! ""
Бұлқыш:Бұлқыш NP-ANT-M ; ! "Bulqısh" (Kazakh)
Бұлқышев:Бұлқышев NP-COG-OB ; ! ""
Бұрхан:Бұрхан NP-ANT-M ; ! "Burxan" (Kazakh)
Бұрыл:Бұрыл NP-TOP ; ! ""
БҰҰ:БҰҰ NP-ORG-ABBR ; ! "United Nations"
Бұхара:Бұхара NP-ANT-M ; ! "Buxara" (Arabic)
Бұхара:Бұхара NP-TOP ; ! ""
Бұхара:Бұхара NP-TOP ; ! ""
Бұхар:Бұхар NP-TOP ; ! ""
Бүби:Бүби NP-ANT-F ; ! "Bübiy" 
Бүбихан:Бүбихан NP-ANT-F ; ! "Bübiyxan" 
Бүбиш:Бүбиш NP-ANT-F ; ! "Bübiysh" 
Бүгілме:Бүгілме NP-TOP ; ! ""
Бүйен:Бүйен NP-TOP ; ! ""
Бүлдіршін:Бүлдіршін NP-ANT-F ; ! "Büldirshin" (Kazakh)
Бүркітбаев:Бүркітбаев NP-COG-OB ; ! "" ! Use/MT
Бүркітбай:Бүркітбай NP-ANT-M ; ! "Bürkitbay" (Kazakh)
Бүркітбек:Бүркітбек NP-ANT-M ; ! "Bürkitbek" (Kazakh)
Бүркіт:Бүркіт NP-ANT-M ; ! "Bürkit" (Kazakh)
Бүркітті:Бүркітті NP-TOP ; ! ""
Бүрібай:Бүрібай NP-ANT-M ; ! "Büribay" (Kazakh)
Быжы:Быжы NP-TOP ; ! ""
Быков:Быков NP-COG-OB ; ! ""
Быченков:Быченков NP-COG-OB ; ! ""
Бычков:Бычков NP-COG-OB ; ! ""
Біжікен:Біжікен NP-ANT-M ; ! "Bijiken" (Greek)
Білгір:Білгір NP-ANT-M ; ! "Bilgir" (Kazakh)
Білім:Білім NP-ANT-M ; ! "Bilim" (Kazakh)
Білісбеков:Білісбеков NP-COG-OB ; !"Use/MT"
Біржан:Біржан NP-ANT-M ; ! "Birjan" (Kazakh)
Бірлескен% араб% эмират:Бірлескен% араб% эмират N-COMPOUND-PX ; !"Use/MT"
Бірлескен% араб% эмираттары:Бірлескен% араб% эмираттар NP-TOP-COMPOUND ; ! "UAE"
Бірлестік:Бірлестік NP-TOP ; ! ""
Бірлік:Бірлік NP-ANT-M ; ! "Birlik" (Kazakh)
Бірлік:Бірлік NP-TOP ; ! ""
Бірлікүстем:Бірлікүстем NP-TOP ; ! ""
Бірсуат:Бірсуат NP-TOP ; ! ""
Біріккен% араб% эмират:Біріккен% араб% эмират N-COMPOUND-PX ; !"Use/MT"
Біріккен% араб% эмираттары:Біріккен% араб% эмираттар NP-TOP-COMPOUND ; ! "UAE"
Бірімжанов:Бірімжанов NP-COG-OB ; ! ""
Бірімқұлов:Бірімқұлов NP-COG-OB ; ! ""
Бішкек:Бішкек NP-TOP ; ! ""
Бьянка:Бьянка NP-ANT-F ; !"Use/MT"
Бэзил:Бэзил NP-ANT-M ; ! ""
Бэкон:Бэкон NP-COG-MF ; ! ""
бэнк:бэнк NP-TOP ; !"Use/MT"
Бэтмен:Бэтмен NP-ANT-M ; !"Use/MT"
Ва:Ва NP-ANT-M ; !"Use/MT"
Вавилов:Вавилов NP-COG-OB ; ! ""
Вавилон:Вавилон NP-TOP ; ! "" ! Use/MT
Ваганов:Ваганов NP-COG-OB ; ! ""
Вадим:Вадим NP-ANT-M ; ! ""
Вадуц:Вадуц NP-TOP ; ! "" 
Важоров:Важоров NP-COG-OB ; ! ""
Вайоминг:Вайоминг NP-TOP ; !"Use/MT"
Вайсбанд:Вайсбанд NP-ANT-M ; ! ""
Валеев:Валеев NP-COG-OB ; ! ""
Валенсия:Валенсия NP-ANT-F ; !"Use/MT"
Валенсия:Валенсия NP-TOP ; ! ""
Валентина:Валентина NP-ANT-F ; ! "Valentina" (Latin)
Валентин:Валентин NP-ANT-M ; ! ""
Валентино:Валентино NP-ANT-M ; !"Use/MT"
Валерий:Валерий NP-ANT-M ; ! "Valeriy" (Latin)
Валеро:Валеро NP-COG-MF ; !"Use/MT"
Валид:Валид NP-ANT-M ; !"Use/MT"
Валлетта:Валлетта NP-TOP ; ! "" 
Вальдо:Вальдо NP-ANT-M ; !"Use/MT"
Вальтер:Вальтер NP-ANT-M ; !"Use/MT"
ван% Гог:ван% Гог NP-COG-MF ; ! ""
Ванеев:Ванеев NP-COG-OB ; ! ""
Ванеса:Ванеса NP-ANT-F ; !"Use/MT"
Ванесса:Ванесса NP-ANT-F ; !"Use/MT"
Ванина:Ванина NP-ANT-F ; !"Use/MT"
Вануату:Вануату NP-TOP ; ! ""
Варвара:Варвара NP-ANT-F ; ! ""
Вардан:Вардан NP-ANT-M ; !"Use/MT"
Варшава:Варшава NP-TOP ; ! "Warsaw"
Васенко:Васенко NP-COG-MF ; ! ""
Васила:Васила NP-ANT-F ; ! "Vasila" (Arabic)
Василенко:Василенко NP-COG-MF ; ! ""
Василий:Василий NP-ANT-M ; ! "" 
Васильев:Васильев NP-COG-OB ; ! ""
Васильевич:Василье NP-PAT-VICH ; ! ""
Васильков:Васильков NP-COG-OB ; ! "" ! Use/MT
Ватзке:Ватзке NP-COG-M ; ! "Watzke (German surname)"
Ватикан:Ватикан NP-TOP ; ! ""
Вахитов:Вахитов NP-COG-OB ; ! ""
Вацлав:Вацлав NP-ANT-M ; ! "" 
Ваш:Ваш NP-ANT-M ; !"Use/MT"
Вашингтон:Вашингтон NP-ANT-M ; !"Use/MT"
Вашингтон:Вашингтон NP-TOP ; ! "Washington"
Введенка:Введенка NP-TOP ; ! ""
Вдовиченко:Вдовиченко NP-COG-MF ; ! ""
ВДР:ВДР%{а%}%{й%} NP-TOP-ABBR ; ! "Vietnam Democratic Republic"
Ве:Ве NP-ANT-M ; !"Use/MT"
Вега:Вега NP-COG-MF ; !"Use/MT"
Великанов:Великанов NP-COG-OB ; ! ""
Вена:Вена NP-TOP ; ! "Vienna"
Венансио:Венансио NP-ANT-M ; !"Use/MT"
Венгрия:Венгрия NP-TOP ; ! "Hungary"
Венди:Венди NP-ANT-F ; !"Use/MT"
Венедиктов:Венедиктов NP-COG-OB ; ! ""
Венера:Венера NP-ANT-F ; ! "Venera" (Latin)
Венесуэла:Венесуэла NP-TOP ; ! ""
Венето:Венето NP-TOP ; !"Use/MT"
Венеция:Венеция NP-TOP ; ! ""
Вениамин:Вениамин NP-ANT-M ; !"Use/MT"
Вера:Вера NP-ANT-F ; ! "Vera" (Greek)
Вергилий:Вергилий NP-ANT-M ; !"Use/MT"
Веремеенко:Веремеенко NP-COG-MF ; ! ""
Верена:Верена NP-ANT-F ; !"Use/MT"
Верендеев:Верендеев NP-COG-OB ; ! ""
Вериялов:Вериялов NP-COG-OB ; ! ""
Верликов:Верликов NP-COG-OB ; ! ""
Вермонт:Вермонт NP-TOP ; !"Use/MT"
Вернадский:Вернадский NP-COG-M ; ! ""
Вернер:Вернер NP-ANT-M ; !"Use/MT"
Верный:Верный NP-TOP ; ! ""
Верный:Верный NP-TOP ; ! "" ! Use/MT
Верона:Верона NP-ANT-F ; !"Use/MT"
Вероника:Вероника NP-ANT-F ; ! ""
Версаль:Версаль NP-TOP ; ! ""
Верхнеберезовский:Верхнеберезовский NP-TOP ; ! ""
Верхояно:Верхояно NP-TOP ; ! ""
Верхоянск:Верхоянск NP-TOP ; ! ""
Верхубинка:Верхубинка NP-TOP ; ! ""
Веселов:Веселов NP-COG-OB ; ! ""
Веселовка:Веселовка NP-TOP ; ! ""
Весна:Весна NP-ANT-F ; !"Use/MT"
Вестендорп:Вестендорп NP-COG-MF ; !"Use/MT"
Вестминстер:Вестминстер NP-TOP ; ! ""
Вестминстер:Вестминстер NP-TOP ; !"Use/MT"
Вестфалия:Вестфалия NP-TOP ; !"Use/MT"
Взгляд:Взгляд NP-AL ; ! ""
Вивальди:Вивальди NP-COG-MF ; !"Use/MT"
Вивиан:Вивиан NP-ANT-F ; !"Use/MT"
Видал:Видал NP-ANT-M ; !"Use/MT"
Византия:Византия NP-TOP ; ! ""
Визеу:Визеу NP-TOP ; !"Use/MT"
Википедия:Википедия NP-ORG ; ! "Wikipedia"
Вико:Вико NP-ANT-M ; !"Use/MT"
Виктор:Виктор NP-ANT-M ; ! "Viktor" (Latin)
Викторенко:Викторенко NP-COG-MF ; ! ""
Викторина:Викторина NP-ANT-F ; ! "Viktorina" (Latin)
Виктория:Виктория NP-ANT-F ; ! "Viktoriya" (Latin)
Викторов:Викторов NP-COG-OB ; ! ""
Викторовка:Викторовка NP-TOP ; ! ""
Вилжан:Вилжан NP-ANT-M ; ! "Viljan" (New word)
Вилинбахов:Вилинбахов NP-COG-OB ; ! ""
Вилнұр:Вилнұр NP-ANT-M ; ! "Vilnur" (New word)
Вильгельм:Вильгельм NP-ANT-M ; ! ""
Вильма:Вильма NP-ANT-F ; !"Use/MT"
Вильн:Вильн NP-ANT-M ; ! ""
Вильн:Вильн NP-TOP ; ! ""
Вильнюс:Вильнюс NP-TOP ; ! "" 
Вильярреал:Вильярреал NP-ORG ; !"Use/MT"
Вимблей:Вимблей NP-TOP ; ! "Wembley Stadium"
Винка:Винка NP-ANT-F ; !"Use/MT"
Винко:Винко NP-ANT-M ; !"Use/MT"
Виноградов:Виноградов NP-COG-OB ; ! ""
Винокуров:Винокуров NP-COG-OB ; ! ""
Винсент:Винсент NP-ANT-M ; !"Use/MT"
Винсенте:Винсенте NP-ANT-M ; !"Use/MT"
Винченцо:Винченцо NP-ANT-M ; !"Use/MT"
Виолета:Виолета NP-ANT-F ; !"Use/MT"
Виргиния:Виргиния NP-ANT-F ; !"Use/MT"
Вирджиния:Вирджиния NP-TOP ; !""
Вирхов:Вирхов NP-COG-OB ; ! ""
Вирьялов:Вирьялов NP-COG-OB ; ! ""
Висбю:Висбю NP-TOP ; ! ""
Висенте:Висенте NP-ANT-M ; !"Use/MT"
Висконсин:Висконсин NP-TOP ; ! ""
Висла:Висла NP-TOP ; ! ""
Вислинск:Вислинск NP-TOP ; ! ""
Вита:Вита NP-ANT-F ; !"Use/MT"
Виталий:Виталий NP-ANT-M ; ! "" 
Виталик:Виталик NP-ANT-M ;
Витлев:Витлев NP-COG-OB ; ! ""
Вито:Вито NP-ANT-M ; !"Use/MT"
Витренко:Витренко NP-COG-MF ; ! ""
Витторио:Витторио NP-ANT-M ; !"Use/MT"
Витус:Витус NP-ANT-M ; ! ""
Вишнев:Вишнев NP-COG-OB ; ! ""
Вишняков:Вишняков NP-COG-OB ; ! ""
Влад:Влад NP-ANT-M ; !"Use/MT"
Владивосток:Владивосток NP-TOP ; !""
Владимир:Владимир NP-ANT-M ; ! "Vladimir"
Владимир:Владимир NP-TOP ; ! ""
Владимиров:Владимиров NP-COG-OB ; ! ""
Владимирович:Владимиро NP-PAT-VICH ; ! "" ! Use/MT 
Владимировка:Владимировка NP-TOP ; ! ""
Владислав:Владислав NP-ANT-M ; ! ""
Владлен:Владлен NP-ANT-M ; ! "Vladlen" (Russian)
Владыченко:Владыченко NP-COG-MF ; ! ""
Власов:Власов NP-COG-OB ; ! ""
Воже:Воже NP-TOP ; ! ""
Возвышенка:Возвышенка NP-TOP ; ! ""
Вознесенка:Вознесенка NP-TOP ; ! ""
Возрождение:Возрождение NP-TOP ; ! ""
Волга-Балтық:Волга-Балтық NP-TOP ; ! ""
Волга:Волга NP-AL ; ! ""
Волга-Камск:Волга-Камск NP-TOP ; ! ""
Волгоград:Волгоград NP-TOP ; ! ""
Волин:Волин NP-TOP ; ! ""
Волков:Волков NP-COG-OB ; ! ""
Вологда:Вологда NP-TOP ; ! ""
Волхов:Волхов NP-COG-OB ; ! ""
Волынов:Волынов NP-COG-OB ; ! ""
Вольфганг:Вольфганг NP-ANT-M ; !"Use/MT"
Вон:Вон NP-ANT-M ; !"Use/MT"
Воробьёв:Воробьёв NP-COG-OB ; ! ""
Воронеж:Воронеж NP-TOP ; ! ""
Воронов:Воронов NP-COG-OB ; ! ""
Воронцов:Воронцов NP-COG-OB ; ! ""
Ворошилов:Ворошилов NP-COG-OB ; ! ""
Восток:Восток NP-AL ; ! ""
Востриков:Востриков NP-COG-OB ; ! ""
Востров:Востров NP-COG-OB ; ! ""
Воф:Воф NP-ANT-M ; !"Use/MT"
Врангеля:Врангеля NP-TOP ; ! ""
Враца:Враца NP-TOP ; !"Use/MT"
Время:Время NP-AL ; ! ""
Вроцлав:Вроцлав NP-TOP ; ! ""
Всеволод:Всеволод NP-ANT-M ; ! ""
Всеволодович:Всеволодович NP-COG-M ; ! ""
Вук:Вук NP-COG-MF ; !"Use/MT"
Вукичевич:Вукиче NP-PAT-VICH ; ! ""
Вуколов:Вуколов NP-COG-OB ; ! ""
Вуянович:Вуяно NP-PAT-VICH ; ! "" ! Use/MT 
Выграненко:Выграненко NP-COG-MF ; ! ""
Выдриха:Выдриха NP-TOP ; ! ""
Вьентьян:Вьентьян NP-TOP ; ! "" 
Вьетнам:Вьетнам NP-TOP ; ! "Vietnam"
Вэнс:Вэнс NP-ANT-M ; !"Use/MT"
Вюртемберг:Вюртемберг NP-TOP ; !"Use/MT"
Вячеслав:Вячеслав NP-ANT-M ; ! ""
Вячеслав:Вячеслав NP-TOP ; ! ""
Вячеславович:Вячеславо NP-PAT-VICH ; ! "" ! Use/MT 
Гаага:Гаага NP-TOP ; ! ""
Гааг:Гааг NP-TOP ; ! ""
Габ:Габ NP-ANT-M ; !"Use/MT"
Габон:Габон NP-TOP ; ! ""
Габриэла:Габриэла NP-ANT-F ; !"Use/MT"
Габриэле:Габриэле NP-ANT-M ; !"Use/MT"
Габриэль:Габриэль NP-ANT-F ; !"Use/MT"
Габриэль:Габриэль NP-ANT-M ; !"Use/MT"
Габрово:Габрово NP-TOP ; !"Use/MT"
Гавайи:Гавайи NP-TOP ; ! ""
Гаванна:Гаванна NP-TOP ; ! ""
Гаврилов:Гаврилов NP-COG-OB ; ! ""
Гагарин:Гагарин NP-TOP ; ! ""
Гад:Гад NP-ANT-M ; !"Use/MT"
Гаджиев:Гаджиев NP-COG-OB ; ! "" ! Use/MT
Газа:Газа NP-TOP ; ! ""
Газ:Газ NP-ANT-M ; !"Use/MT"
Газиантеп:Газиантеп NP-TOP ; ! ""
Газпром:Газпром NP-ORG ; ! ""
Гаити:Гаити NP-TOP-RUS ; ! ""
Гайана:Гайана NP-TOP ; ! ""
Гай:Гай NP-ANT-M ; !"Use/MT"
Гайдебуров:Гайдебуров NP-COG-OB ; ! ""
Гайнутдинов:Гайнутдинов NP-COG-OB ; ! ""
Галатасарай:Галатасарай NP-TOP ; ! ""
Гал:Гал NP-ANT-M ; !"Use/MT"
Галилей:Галилей NP-ANT-M ; !"Use/MT"
Галилей:Галилей NP-COG-MF ; ! ""
Галилео:Галилео NP-ANT-M ; ! ""
Галимзянов:Галимзянов NP-COG-OB ; ! ""
Галина:Галина NP-ANT-F ; ! "Galıyna" 
Галисия:Галисия NP-TOP ; !"Use/MT"
Галкин:Галкин NP-COG-IN ; ! ""
Галкино:Галкино NP-TOP ; ! ""
Галлахер:Галлахер NP-COG-MF ; !"Use/MT"
Галлямов:Галлямов NP-COG-OB ; ! ""
Галошев:Галошев NP-COG-OB ; ! ""
Гальяно:Гальяно NP-ANT-M ; !"Use/MT"
Галя:Галя NP-ANT-F ; !"Use/MT"
Гамаль:Гамаль NP-ANT-M ; !"Use/MT"
Гамбия:Гамбия NP-TOP ; ! ""
Гамбург:Гамбург NP-TOP ; ! "" ! Use/MT
Гамбурцев:Гамбурцев NP-COG-OB ; ! ""
Гамильтон:Гамильтон NP-COG-MF ; !"Use/MT"
Гамов:Гамов NP-COG-OB ; ! ""
Гана:Гана NP-TOP ; ! "Ghana"
Гангутск:Гангутск NP-TOP ; ! ""
Ганди:Ганди NP-COG-MF ; !"Use/MT"
Гандольфо:Гандольфо NP-ANT-M ; !"Use/MT"
Ганза:Ганза NP-TOP ; !""
Ганнибал:Ганнибал NP-ANT-M ; !"Use/MT"
Ганс:Ганс NP-ANT-M ; ! ""
Гапеев:Гапеев NP-COG-OB ; ! ""
Гапликов:Гапликов NP-COG-OB ; ! ""
Гарвард:Гарвард NP-ORG ; ! ""
Гар:Гар NP-ANT-M ; !"Use/MT"
Гардиан:Гардиан NP-ORG ; ! ""
Гарднер:Гарднер NP-COG-MF ; !"Use/MT"
Гари:Гари NP-ANT-M ; !"Use/MT"
Гарольд:Гарольд NP-ANT-M ; !"Use/MT"
Гарри:Гарри NP-ANT-M ; !"Use/MT"
Гаррисон:Гаррисон NP-ANT-M ; !"Use/MT"
Гарсия:Гарсия NP-COG-MF ; !"Use/MT"
Гаспар:Гаспар NP-ANT-M ; !"Use/MT"
Гатауов:Гатауов NP-COG-OB ; ! ""
Гаухар:Гаухар NP-ANT-F ; ! "Gawxar" (Arabic)
Гафидов:Гафидов NP-COG-OB ; ! ""
Гашпарович:Гашпаро NP-PAT-VICH ; ! "" ! Use/MT 
Гашпорович:Гашпоро NP-PAT-VICH ; ! "" ! Use/MT 
Гаэль:Гаэль NP-ANT-M ; !"Use/MT"
Гаэтано:Гаэтано NP-ANT-M ; !"Use/MT"
Гаяна:Гаяна NP-ANT-F ; ! "Gayana" (Greek)
Гвадалахара:Гвадалахара NP-TOP ; !"Use/MT"
Гвардейский:Гвардейский NP-TOP ; ! ""
Гватемала:Гватемала NP-TOP ; ! ""
Гвидо:Гвидо NP-ANT-M ; !"Use/MT"
Гвинет:Гвинет NP-ANT-F ; !"Use/MT"
Гвинея%-Бисау:Гвинея%-Бисау NP-TOP ; ! ""
Гвинея:Гвинея NP-TOP ; ! ""
Гвоздев:Гвоздев NP-COG-OB ; ! ""
Гданьск:Гданьск NP-TOP ; ! ""
Гданьск:Гданьск NP-TOP ; ! ""
Гдов:Гдов NP-COG-OB ; ! ""
Геббельс:Геббельс NP-ANT-M ; ! ""
Гег:Гег NP-ANT-M ; !"Use/MT"
Ге:Ге NP-ANT-M ; !"Use/MT"
Гедимин:Гедимин NP-COG-M ; ! ""
Гедиминович:Гедимино NP-PAT-VICH ; ! ""
Гедмин:Гедмин NP-COG-MF ; ! ""
Гейдельберг:Гейдельберг NP-TOP ; ! ""
Гейзенберг:Гейзенберг NP-COG-MF ; !"Use/MT"
Гей%-Люссак:Гей%-Люссак NP-COG-MF ; ! ""
Гейтс:Гейтс NP-COG-MF ; ! ""
Гелиос:Гелиос NP-ANT-M ; !"Use/MT"
Гельсингфорс:Гельсингфорс NP-TOP ; ! ""
Гема:Гема NP-ANT-F ; !"Use/MT"
Генко:Генко NP-COG-MF ; ! ""
Геннадий:Геннадий NP-ANT-M ; ! "" 
Генри:Генри NP-ANT-M ; !"Use/MT"
Генрих:Генрих NP-ANT-M ; ! ""
Генуя:Генуя NP-TOP ; ! ""
Георг:Георг NP-ANT-M ; !"Use/MT"
Георгиев:Георгиев NP-COG-OB ; ! ""
Георгиевск:Георгиевск NP-TOP ; ! ""
Георгий:Георгий NP-ANT-M ; ! "" 
Геральд:Геральд NP-ANT-M ; ! ""
Герасименко:Герасименко NP-COG-MF ; ! ""
Герасимов:Герасимов NP-COG-OB ; ! ""
Герат:Герат NP-TOP ; ! ""
Герберт:Герберт NP-ANT-M ; ! ""
Гер:Гер NP-ANT-M ; !"Use/MT"
Геркулес:Геркулес NP-ANT-M ; ! ""
Геркулес:Геркулес NP-TOP ; !"Use/MT"
Герман:Герман NP-ANT-M ; ! ""
Герман:Герман NP-COG-MF ; !"Use/MT"
Германия:Германия NP-TOP ; ! "Germany"
Гермезир:Гермезир NP-TOP ; ! ""
Гермес:Гермес NP-ANT-M ; !"Use/MT"
Гернси:Гернси NP-TOP ; ! ""
Геродот:Геродот NP-ANT-M ; ! ""
героев:героев NP-COG-OB ; ! ""
Герхард:Герхард NP-ANT-M ; !"Use/MT"
Герцеговина:Герцеговина NP-TOP ; !"Use/MT"
Герцен:Герцен NP-COG-MF ; ! ""
Гершель:Гершель NP-COG-MF ; !"Use/MT"
Гессен:Гессен NP-TOP ; !"Use/MT"
Гете:Гете NP-COG-MF ; ! ""
Геш:Геш NP-ANT-M ; !"Use/MT"
Гиб:Гиб NP-ANT-M ; !"Use/MT"
Гибралтар:Гибралтар NP-TOP ; !"Use/MT"
Ги:Ги NP-ANT-M ; !"Use/MT"
Гиги:Гиги NP-ANT-F ; !"Use/MT"
Гид:Гид NP-ANT-M ; !"Use/MT"
Гидденс:Гидденс NP-COG-M ; ! ""
гидра:гидра N1 ; !"Use/MT"
Гиза:Гиза NP-TOP ; !""
Гийом:Гийом NP-ANT-M ; !"Use/MT"
Гилберт:Гилберт NP-ANT-M ; ! ""
Гилберто:Гилберто NP-ANT-M ; !"Use/MT"
Гильермо:Гильермо NP-ANT-M ; !"Use/MT"
Гималай:Гималай NP-TOP ; ! ""
Гим:Гим NP-ANT-M ; !"Use/MT"
Ги% Мун:Ги% Мун NP-COG-MF ; ! ""
Гинзберг:Гинзберг NP-COG-MF ; !"Use/MT"
Гир:Гир NP-ANT-M ; !"Use/MT"
Гит:Гит NP-ANT-M ; !"Use/MT"
Гитлер:Гитлер NP-COG-MF ; ! ""
Гиффен:Гиффен NP-COG-MF ; ! ""
Гиш:Гиш NP-ANT-M ; !"Use/MT"
Гладков:Гладков NP-COG-OB ; ! ""
Глазов:Глазов NP-COG-OB ; ! ""
Глазунов:Глазунов NP-COG-OB ; ! ""
Глеб:Глеб NP-ANT-M ; ! ""
Глен:Глен NP-ANT-M ; ! ""
Глен:Глен NP-ANT-M ; ! ""
Гленн:Гленн NP-ANT-M ; !"Use/MT"
Глобал:Глобал NP-AL ; ! ""
Глория:Глория NP-ANT-F ; !"Use/MT"
Глубоков:Глубоков NP-COG-OB ; ! ""
Глубоков:Глубоков NP-TOP ; ! ""
Глубокое:Глубокое NP-TOP ; ! ""
Глубочанка:Глубочанка NP-TOP ; ! ""
Глухов:Глухов NP-COG-OB ; ! ""
Глуховка:Глуховка NP-TOP ; ! ""
Глущенко:Глущенко NP-COG-MF ; ! ""
Гнеденко:Гнеденко NP-COG-MF ; ! ""
Гниломедов:Гниломедов NP-COG-OB ; ! ""
Говард:Говард NP-ANT-M ; !"Use/MT"
Гоголь:Гоголь NP-COG-MF ; ! ""
Годдард:Годдард NP-ANT-M ; !"Use/MT"
Годунов:Годунов NP-COG-OB ; ! ""
Гой:Гой NP-ANT-M ; !"Use/MT"
Гойя:Гойя NP-COG-MF ; !"Use/MT"
Гол:Гол NP-ANT-M ; !"Use/MT"
Голдман:Голдман NP-COG-MF ; !"Use/MT"
Голенков:Голенков NP-COG-OB ; ! ""
Голландия:Голландия NP-TOP ; ! "Holland,Netherland"
Голливуд:Голливуд NP-TOP ; !"Use/MT"
Голль:Голль NP-COG-MF ; ! ""
Голос% Америки:Голос% Америки NP-ORG ; ! ""
Голоскоков:Голоскоков NP-COG-OB ; ! ""
Голубев:Голубев NP-COG-OB ; ! ""
Голубков:Голубков NP-COG-OB ; ! ""
Голубченко:Голубченко NP-COG-MF ; ! ""
Гольбах:Гольбах NP-COG-MF ; ! ""
Гомер:Гомер NP-ANT-M ; ! ""
Гондурас:Гондурас NP-TOP ; ! ""
Гонзало:Гонзало NP-ANT-M ; !"Use/MT"
Гонконг:Гонконг NP-TOP ; ! ""
Гонсалес:Гонсалес NP-COG-MF ; !"Use/MT"
Гончаров:Гончаров NP-COG-OB ; ! ""
Гораций:Гораций NP-ANT-M ; !"Use/MT"
Горбачев:Горбачев NP-COG-OB ; ! ""
Горбачёв:Горбачёв NP-COG-OB ; ! ""
Горбунов:Горбунов NP-COG-OB ; ! ""
Горган:Горган NP-TOP ; ! ""
Гор:Гор NP-COG-MF ; !"Use/MT"
Гордеев:Гордеев NP-COG-OB ; ! ""
Гордон:Гордон NP-ANT-M ; !"Use/MT"
Горленко:Горленко NP-COG-MF ; ! ""
Горный:Горный NP-TOP ; ! ""
Городовиков:Городовиков NP-COG-OB ; ! ""
Городцов:Городцов NP-COG-OB ; ! ""
Горшков:Горшков NP-COG-OB ; ! ""
Горький:Горький NP-COG-M ; ! ""
Горьковское:Горьковское NP-TOP ; ! ""
Госу:Госу NP-COG-MF ; ! ""
Готфрид:Готфрид NP-ANT-M ; !"Use/MT"
Гофман:Гофман NP-COG-M ; ! ""
Гояс:Гояс NP-TOP ; !"Use/MT"
Градов:Градов NP-COG-OB ; ! ""
Гранада:Гранада NP-TOP ; !"Use/MT"
Грант:Грант NP-ANT-M ; !"Use/MT"
Граубюнден:Граубюнден NP-TOP ; !"Use/MT"
Грачев:Грачев NP-COG-OB ; ! ""
Гребенщиков:Гребенщиков NP-COG-OB ; ! ""
Грегориус:Грегориус NP-ANT-M ; ! ""
Грей:Грей NP-COG-MF ; !"Use/MT"
Грейди:Грейди NP-ANT-M ; !"Use/MT"
Грекия:Грекия NP-TOP ; ! "Greece"
Гренада:Гренада NP-TOP ; !"Use/MT"
Гренландия:Гренландия NP-TOP ; ! "Greenland"
Грета:Грета NP-ANT-F ; !"Use/MT"
Греция:Греция NP-TOP ; ! "" ! Use/MT
Грибаускайте:Грибаускайте NP-COG-M ; ! ""
Грибоедов:Грибоедов NP-COG-OB ; ! ""
Григоренко:Григоренко NP-COG-MF ; ! ""
Григориан:Григориан NP-ANT-M ; ! ""
Григорий:Григорий NP-ANT-M ; ! ""
Григорьев:Григорьев NP-COG-OB ; ! ""
Григорьевич:Григорье NP-PAT-VICH ; ! ""
Гризобудов:Гризобудов NP-COG-OB ; ! ""
Гринвич:Гринвич NP-TOP ; ! "" ! Use/MT
Гриненко:Гриненко NP-COG-MF ; ! ""
Гринпис:Гринпис NP-ORG ; !"Use/MT"
Гриценко:Гриценко NP-COG-MF ; ! ""
Гришин:Гришин NP-COG-IN ; ! ""
Гродеков:Гродеков NP-COG-OB ; ! ""
Гродеково:Гродеково NP-TOP ; ! ""
Грозный:Грозный NP-TOP ; ! ""
Громов:Громов NP-COG-OB ; ! ""
Гросвенор:Гросвенор NP-ANT-M ; ! ""
Грос:Грос NP-TOP ; !"Use/MT"
Гротендик:Гротендик NP-COG-MF ; !"Use/MT"
Грузия:Грузия NP-TOP ; ! "Georgia"
Грызлов:Грызлов NP-COG-OB ; ! ""
Грызов:Грызов NP-COG-OB ; ! ""
Грэм:Грэм NP-ANT-M ; !"Use/MT"
Грэхем:Грэхем NP-ANT-M ; !"Use/MT"
Грязнов:Грязнов NP-COG-OB ; ! ""
Гуам:Гуам NP-TOP ; ! ""
Гуантанамо:Гуантанамо NP-TOP ; !"Use/MT"
Гуарда:Гуарда NP-TOP ; !"Use/MT"
Губанов:Губанов NP-COG-OB ; ! ""
Губарев:Губарев NP-COG-OB ; ! ""
Губерт:Губерт NP-ANT-M ; !"Use/MT"
Гу:Гу NP-ANT-M ; !"Use/MT" 
Гуд:Гуд NP-ANT-M ; !"Use/MT"
Гудзон:Гудзон NP-ANT-M ; !"Use/MT"
Гузенко:Гузенко NP-COG-MF ; ! ""
Гуйчжоу:Гуйчжоу NP-TOP ; ! "провинция на юго-западе Китая"
Гук:Гук NP-ANT-M ; !"Use/MT"
Гул:Гул NP-ANT-M ; !"Use/MT"
Гуленко:Гуленко NP-COG-MF ; ! ""
Гульельмо:Гульельмо NP-ANT-M ; !"Use/MT"
Гуляев:Гуляев NP-COG-OB ; ! ""
Гум:Гум NP-ANT-M ; !"Use/MT" 
Гумилев:Гумилев NP-COG-OB ; ! ""
Гуп:Гуп NP-ANT-M ; !"Use/MT" 
Гурбангұлы:Гурбангұлы NP-ANT-M ; ! "" 
Гурцев:Гурцев NP-COG-OB ; ! ""
Гурченко:Гурченко NP-COG-MF ; ! ""
Гуръев:Гуръев NP-COG-OB ; ! ""
Гурьев:Гурьев NP-COG-OB ; ! ""
Гурьев:Гурьев NP-TOP ; ! ""
Гусаров:Гусаров NP-COG-OB ; ! ""
Гусев:Гусев NP-COG-OB ; ! ""
Густав:Густав NP-ANT-M ; ! ""
Гутеррес:Гутеррес NP-COG-MF ; ! ""
Гущин:Гущин NP-COG-IN ; ! ""
Гұлбану:Гұлбану NP-ANT-F ; ! "Gulbanıw" (Arabic)
Гүлажар:Гүлажар NP-ANT-F ; ! "Gülajar" (Kazakh)
Гүлай:Гүлай NP-ANT-F ; ! "Gülay" 
Гүлайым:Гүлайым NP-ANT-F ; ! "Gülayım" 
Гүлбанат:Гүлбанат NP-ANT-F ; ! "Gülbanat" (Arabic)
Гүлбараш:Гүлбараш NP-ANT-F ; ! "Gülbarash" (Arabic)
Гүлбаршын:Гүлбаршын NP-ANT-F ; ! "" 
Гүлбахрам:Гүлбахрам NP-ANT-F ; ! "Gülbaxram" (Arabic)
Гүлболсын:Гүлболсын NP-ANT-F ; ! "Gülbolsın" (Kazakh)
Гүлжамал:Гүлжамал NP-ANT-F ; ! "Güljamal" (Arabic)
Гүлжамила:Гүлжамила NP-ANT-F ; ! "Güljamıyla" 
Гүлжан:Гүлжан NP-ANT-F ; ! "" 
Гүлжеңіс:Гүлжеңіс NP-ANT-F ; ! "Güljeŋis" (Kazakh)
Гүлжихан:Гүлжихан NP-ANT-F ; ! "Güljiyxan" 
Гүлзанат:Гүлзанат NP-ANT-F ; ! "" 
Гүлзара:Гүлзара NP-ANT-F ; ! "Gülzara" (Arabic)
Гүлзарифа:Гүлзарифа NP-ANT-F ; ! "Gülzarifa" (Arabic)
Гүлзипа:Гүлзипа NP-ANT-F ; ! "Gülziypa" (Arabic)
Гүлмария:Гүлмария NP-ANT-F ; ! "Gülmarıya" (Arabic)
Гүлмира:Гүлмира NP-ANT-F ; ! "Gülmiyra" (Russian)
Гүлназ:Гүлназ NP-ANT-F ; ! "Gülnaz" (Greek)
Гүлнара:Гүлнара NP-ANT-F ; ! "Gülnara" (Arabic)
Гүлнар:Гүлнар  NP-ANT-F ; ! "Gülnar " (Arabic)
Гүлнар:Гүлнар NP-ANT-F ; ! "Gülnar" (Arabic)
Гүлнафиса:Гүлнафиса NP-ANT-F ; ! "Gülnafisa" (Arabic)
Гүлнәр:Гүлнәр NP-ANT-F ; ! "" 
Гүлниса:Гүлниса NP-ANT-F ; ! "Gülniysa" (Arabic)
Гүлнұр:Гүлнұр NP-ANT-F ; ! "Gülnur" (Arabic)
Гүлсанат:Гүлсанат NP-ANT-F ; ! "Gülsanat" (Arabic)
Гүлсара:Гүлсара NP-ANT-F ; ! "Gülsara" (Arabic)
Гүлсипат:Гүлсипат NP-ANT-F ; ! "Gülsiypat" (Arabic)
Гүлшара:Гүлшара NP-ANT-F ; ! "Gülshara" (Arabic)
Гүлшар:Гүлшар NP-ANT-F ; ! "Gülshar" (Arabic)
Гүлшаруан:Гүлшаруан NP-ANT-F ; ! "Gülsharıwan" (Arabic)
Гүлшат:Гүлшат NP-ANT-F ; ! "Gülshat" (Persian)
Гүлшат:Гүлшат NP-TOP ; ! ""
Гүлшахара:Гүлшахара NP-ANT-F ; ! "Gülshaxara" (Arabic)
Гүлшаш:Гүлшаш NP-ANT-F ; ! "Gülshash" 
Гүржістан:Гүржістан NP-TOP ; ! "Georgia"
Гью:Гью NP-ANT-M ; !"Use/MT" 
Гэмпшир:Гэмпшир NP-TOP ; !"Use/MT"
Гюйгенс:Гюйгенс NP-COG-MF ; !"Use/MT"
Гюнтер:Гюнтер NP-ANT-M ; !"Use/MT"
Ғаббас:Ғаббас NP-ANT-M ; ! "Ğabbas" 
Ғаббасов:Ғаббасов NP-COG-OB ; ! ""
Ғабдіғазиз:Ғабдіғазиз NP-ANT-M ; ! "Ğabdiğazıyz" (Arabic)
Ғабидолла:Ғабидолла NP-ANT-M ; ! ""
Ғабитан:Ғабитан NP-ANT-M ; ! "Ğabıytan" (Arabic)
Ғабитбек:Ғабитбек NP-ANT-M ; ! "Ğabıytbek" (Arabic)
Ғабит:Ғабит NP-ANT-M ; ! "Ğabıyt" (Arabic)
Ғабитжан:Ғабитжан NP-ANT-M ; ! "Ğabıytjan" (Arabic)
Ғабитхан:Ғабитхан NP-ANT-M ; ! "Ğabıytxan"
Ғабитхан:Ғабитхан NP-ANT-M ; ! "Ğabıytxan" (Arabic)
ҒА:ҒА NP-ORG-LAT ; ! "Academy of Sciences"
Ғад:Ғад NP-ANT-M ; ! ""
Ғазалиев:Ғазалиев NP-COG-OB ; ! "" ! Use/MT
Ғази:Ғази NP-ANT-M ; ! "Ğazıy" (Arabic)
Ғазиза:Ғазиза NP-ANT-F ; ! "Aziza"
Ғазиза:Ғазиза NP-ANT-F ; ! "Ğazıyza" (Arabic)
Ғазизбек:Ғазизбек NP-ANT-M ; ! "Ğazıyzbek" (Kazakh)
Ғазиз:Ғазиз NP-ANT-M ; ! "Ğazıyz" 
Ғазнауи:Ғазнауи NP-TOP ; !"Use/MT"
Ғайни:Ғайни NP-ANT-F ; ! "Ğaynıy" (Arabic)
Ғайнижамал:Ғайнижамал NP-ANT-F ; ! "Ğaynıyjamal" (Arabic)
Ғайния:Ғайния NP-ANT-F ; ! "Ğaynıya" (Arabic)
Ғайса:Ғайса NP-ANT-M ; ! "Ğaysa" 
Ғайша:Ғайша NP-ANT-F ; ! "Aisha"
Ғалиақпар:Ғалиақпар NP-ANT-M ; ! "Ğalıyaqpar" (Arabic)
Ғалиасқар:Ғалиасқар NP-ANT-M ; ! "Ğalıyasqar" (Arabic)
Ғали:Ғали NP-ANT-M ; ! "Ğalıy" 
Ғалиев:Ғалиев NP-COG-OB ; ! ""
Ғалима:Ғалима NP-ANT-F ; ! "Ğalıyma" (Arabic)
Ғалия:Ғалия NP-ANT-F ; ! "Ğalıya" (Arabic)
Ғалымбек:Ғалымбек NP-ANT-M ; ! "Ğalımbek" (Kazakh)
Ғалым:Ғалым NP-ANT-M ; ! "Ğalım" (Arabic)
Ғалымжан:Ғалымжан NP-ANT-M ; ! "Ğalımjan" (Persian)
Ғамман:Ғамман NP-TOP ; ! "" 
Ғанибай:Ғанибай NP-ANT-M ; ! "Ğanıybay" (Arabic)
Ғанибек:Ғанибек NP-ANT-M ; ! "Ğanıybek" (Arabic)
Ғани:Ғани NP-ANT-M ; ! "Ğanıy" (Arabic)
Ғаппар:Ғаппар NP-ANT-M ; ! "Ğappar" (Arabic)
Ғаппас:Ғаппас NP-ANT-M ; ! "Ğappas" 
Ғарифа:Ғарифа NP-ANT-F ; ! "Ğarifa" (Arabic)
Ғарифолла:Ғарифолла NP-ANT-M ; ! "Ğarifolla" (Arabic)
Ғатауов:Ғатауов NP-COG-OB ; ! ""
Ғафар:Ғафар NP-ANT-M ; ! "Ğafar" (Arabic)
Ғафу:Ғафу NP-ANT-M ; ! "Ğafu" (Arabic)
Ғафура:Ғафура NP-ANT-F ; ! "Ğafura" (Arabic)
Ғаффар:Ғаффар NP-ANT-M ; ! "Ğaffar" (Arabic)
Ғидаят:Ғидаят NP-ANT-M ; ! "Ğıydayat" (Arabic)
Ғизат:Ғизат NP-ANT-M ; ! "Ğıyzat" (Arabic)
Ғиса:Ғиса NP-ANT-M ; ! ""
Ғұмар:Ғұмар NP-ANT-M ; ! "Ğumar" (Arabic)
Ғұрбанғұлы:Ғұрбанғұлы NP-ANT-M ; !
Ғылман:Ғылман NP-ANT-M ; ! "Ğılman" (Arabic)
Даб:Даб NP-ANT-M ; !"Use/MT" 
Дабыл:Дабыл NP-ANT-M ; ! "Dabıl" (Kazakh)
Дабыс:Дабыс NP-ANT-M ; ! "Dabıs" (Kazakh)
Дабыс:Дабыс NP-TOP ; ! ""
Дабысын:Дабысын NP-TOP ; ! ""
Давид:Давид NP-ANT-M ; ! ""
Давиде:Давиде NP-ANT-M ; !"Use/MT"
да% Винчи:да% Винчи NP-COG-MF ; ! ""
Давор:Давор NP-ANT-M ; !"Use/MT"
Давос:Давос NP-COG-MF ; !"Use/MT"
Давыденов:Давыденов NP-COG-OB ; ! ""
Давыдов:Давыдов NP-COG-OB ; ! ""
да% Гама:да% Гама NP-COG-MF ; ! ""
Дагмар:Дагмар NP-ANT-F ; !"Use/MT"
Дағанделі:Дағанделі NP-TOP ; ! ""
Дағыстан:Дағыстан NP-TOP ; ! ""
Да:Да NP-ANT-M ; ! "Da" 
Дадабай:Дадабай NP-ANT-M ; ! "Dadabay" (Kazakh)
Дайана:Дайана NP-ANT-F ; ! "Dayana" (Latin)
Дайрабай:Дайрабай NP-ANT-M ; ! "Dayrabay" (Kazakh)
Дайын:Дайын NP-ANT-M ; ! "Dayın" (Kazakh)
Дайырбек:Дайырбек NP-ANT-M ; ! "Dayırbek" (Arabic)
Дайыр:Дайыр NP-ANT-M ; ! "Dayır" (Arabic)
Дайыр:Дайыр NP-TOP ; ! ""
Дайырман:Дайырман NP-ANT-M ; ! "Dayırman" (Arabic)
Дакар:Дакар NP-TOP ; ! "" 
Дакка:Дакка NP-TOP ; ! "" 
Дакота:Дакота NP-ANT-F ; !"Use/MT"
Дакота:Дакота NP-ANT-M ; !"Use/MT"
Дакота:Дакота NP-TOP ; ! ""
Далай:Далай NP-ANT-M ; ! "" 
Далақайнар:Далақайнар NP-TOP ; ! ""
Далбағаев:Далбағаев NP-COG-OB ; ! ""
Далер:Далер NP-ANT-M ; ! "Daler" (Persian)
Дали:Дали NP-COG-MF ; !"Use/MT"
Даллас:Даллас NP-ANT-M ; !"Use/MT"
Даллас:Даллас NP-TOP ; !"Use/MT"
Даль:Даль NP-ANT-M ; !"Use/MT" 
Даля:Даля NP-ANT-F ; !"Use/MT"
Даля:Даля NP-ANT-F ; !"Use/MT"
Даля:Даля NP-TOP ; ! ""
Далянь:Далянь NP-TOP ; ! ""
Дамаск:Дамаск NP-TOP ; ! ""
Дамаск:Дамаск NP-TOP ; ! "" 
Дамиан:Дамиан NP-ANT-M ; !"Use/MT"
Дамира:Дамира NP-ANT-F ; ! "Damıyra" (New word)
Дамир:Дамир NP-ANT-M ; ! ""
Дамокл:Дамокл NP-ANT-M ; !"Use/MT"
Дамса:Дамса NP-TOP ; ! ""
Дамыл:Дамыл NP-ANT-M ; ! "Damıl" (Kazakh)
Данагүл:Данагүл NP-ANT-F ; ! "Danagül" (Persian)
Дана:Дана NP-ANT-F ; ! "Dana" (Persian)
Данай:Данай NP-ANT-M ; ! "Danay" (Persian)
Даналық:Даналық NP-ANT-M ; ! "Danalıq" (Persian)
Даная:Даная NP-ANT-F ; !"Use/MT"
Дандай:Дандай NP-ANT-M ; ! "Danday" (Kazakh)
Данеш:Данеш NP-ANT-M ; ! "Danesh" (Persian)
Даниил:Даниил NP-ANT-M ; !"Use/MT"
Данил:Данил NP-ANT-M ; ! ""
Данилов:Данилов NP-COG-OB ; ! ""
Данило:Данило NP-ANT-M ; !"Use/MT"
Данис:Данис NP-ANT-M ; !"Use/MT"
Даниэла:Даниэла NP-ANT-F ; !"Use/MT"
Даниэль:Даниэль NP-ANT-F ; !"Use/MT"
Дания:Дания NP-TOP ; ! "Denmark"
Даниял:Даниял NP-ANT-M ; ! "Danıyal" (Arabic)
Данияр:Данияр NP-ANT-M ; ! "Danıyar" (Persian)
Данко:Данко NP-COG-MF ; ! ""
Данлоп:Данлоп NP-TOP ; !"Use/MT"
Данте:Данте NP-ANT-M ; !"Use/MT"
Данченко:Данченко NP-COG-MF ; ! ""
Данышпан:Данышпан NP-ANT-M ; ! "Danıshpan" (Arabic)
Дара:Дара NP-ANT-F ; ! "Dara" (Kazakh)
Дарас:Дарас NP-ANT-M ; ! "Daras" (Kazakh)
Дар%-әл%-Харб:Дар%-әл%-Харб NP-TOP ; ! ""
Дарбаза:Дарбаза NP-TOP ; ! ""
Дарвин:Дарвин NP-ANT-M ; !"Use/MT"
Дарвин:Дарвин NP-COG-MF ; ! ""
Дар:Дар NP-ANT-M ; !"Use/MT" 
Дариға:Дариға NP-ANT-F ; ! "Darıyğa" (Persian)
Дарио:Дарио NP-ANT-M ; !"Use/MT"
Дария:Дария NP-ANT-F ; ! "Darıya" (Persian)
Даркевич:Даркевич NP-COG-M ; ! ""
Дарко:Дарко NP-ANT-M ; !"Use/MT"
Даррен:Даррен NP-ANT-M ; !"Use/MT"
Дарфур:Дарфур NP-TOP ; ! ""
Дарханбай:Дарханбай NP-ANT-M ; ! "Darxanbay" (Arabic)
Дархан:Дархан NP-ANT-M ; ! "Darxan" (Arabic)
Дарханов:Дарханов NP-COG-OB ; ! ""
Дарын:Дарын NP-ANT-M ; ! "Darın" (Kazakh)
Дарья:Дарья NP-ANT-F ; ! ""
Дасия:Дасия NP-ANT-F ; !"Use/MT"
Дастан:Дастан NP-ANT-M ; ! "Dastan" (Kazakh)
Даунинг:Даунинг NP-COG-MF ; !"Use/MT"
Даупбаев:Даупбаев NP-COG-OB ; ! ""
Даурен:Даурен NP-ANT-M ; ! "Däwren" 
Даут:Даут NP-ANT-M ; ! ""
Дауылбаев:Дауылбаев NP-COG-OB ; ! ""
Дауылбай:Дауылбай NP-ANT-M ; ! "Dawılbay" (Kazakh)
Дауылбеков:Дауылбеков NP-COG-OB ; ! ""
Дафна:Дафна NP-ANT-F ; !"Use/MT"
Даффи:Даффи NP-COG-MF ; !"Use/MT"
Дәдебаев:Дәдебаев NP-COG-OB ; ! "" ! Use/MT
Дәлбек:Дәлбек NP-ANT-M ; ! "Dälbek" (Kazakh)
Дәлила:Дәлила NP-ANT-F ; ! "Däliyla" (Arabic)
Дәмелі:Дәмелі NP-ANT-F ; ! "Dämeli" (Kazakh)
Дәмен:Дәмен NP-ANT-M ; ! "Dämen" (Arabic)
Дәмина:Дәмина NP-ANT-F ; ! "Dämiyna" (Arabic)
Дәнеш:Дәнеш NP-ANT-F ; ! "Dänesh" (Persian)
Дәркенбай:Дәркенбай NP-ANT-M ; ! "Därkenbay" (Arabic)
Дәрмен:Дәрмен NP-ANT-M ; ! "Därmen" (Persian)
Дәрмене:Дәрмене NP-TOP ; ! ""
Дәубек:Дәубек NP-ANT-M ; ! "Däwbek" (Kazakh)
Дәукенов:Дәукенов NP-COG-OB ; ! ""
Дәулен:Дәулен NP-ANT-M ; ! "Däwlen" (Kazakh)
Дәулетбай:Дәулетбай NP-ANT-M ; ! "Däwletbay" (Kazakh)
Дәулетбақ:Дәулетбақ NP-ANT-M ; ! "Däwletbaq" 
Дәулетбақов:Дәулетбақов NP-COG-OB ; ! ""
Дәулетбеков:Дәулетбеков NP-COG-OB ; ! ""
Дәулетбекұлы:Дәулетбекұлы NP-COG-M ; ! ""
Дәулет:Дәулет NP-ANT-M ; ! "Däwlet" (Arabic)
Дәулеткелді:Дәулеткелді NP-ANT-M ; ! "Däwletkeldi" (Kazakh)
Дәулетов:Дәулетов NP-COG-OB ; ! ""
Дәулетше:Дәулетше NP-ANT-M ; ! "Däwletshe" (Arabic)
Дәуренбеков:Дәуренбеков NP-COG-OB ; ! ""
Дәурен:Дәурен NP-ANT-M ; ! "Däwren" 
Дәуірбек:Дәуірбек NP-ANT-M ; ! "Däwirbek" (Kazakh)
Дәуір:Дәуір NP-ANT-M ; ! "Däwir" (Arabic)
Дәуіт:Дәуіт NP-ANT-M ; ! "Däwit" (Ancient Hebrew)
Дәуіт:Дәуіт NP-TOP ; ! ""
Дәшін:Дәшін NP-TOP ; ! ""
Двина:Двина NP-TOP ; ! ""
Дворяненко:Дворяненко NP-COG-MF ; ! ""
Двуречное:Двуречное NP-TOP ; ! ""
Деб:Деб NP-ANT-M ; !"Use/MT" 
Дебир:Дебир NP-TOP ; ! ""
Девон:Девон NP-TOP ; ! ""
Дегдар:Дегдар NP-ANT-M ; ! "Degdar" (Arabic)
Дегелең:Дегелең NP-TOP ; ! ""
Дегерес:Дегерес NP-TOP ; ! ""
Дегтярев:Дегтярев NP-COG-OB ; ! ""
Де:Де NP-ANT-M ; ! "Dе" 
Дежнев:Дежнев NP-COG-OB ; ! ""
Дежнёв:Дежнёв NP-TOP ; ! ""
Дези:Дези NP-ANT-M ; !"Use/MT"
Дейл:Дейл NP-ANT-M ; !"Use/MT"
Дейли:Дейли NP-AL ; ! ""
Декарт:Декарт NP-COG-MF ; ! ""
Деку:Деку NP-ANT-M ; !"Use/MT"
Делавэр:Делавэр NP-TOP ; !"Use/MT"
Дели:Дели NP-TOP ; ! ""
Делия:Делия NP-ANT-F ; !"Use/MT"
Дело:Дело NP-TOP ; ! "" ! Use/MT
Дельфа:Дельфа NP-TOP ; ! "" ! Use/MT
Дельфина:Дельфина NP-ANT-F ; !"Use/MT"
Демежан:Демежан NP-ANT-M ; ! "Demejan" (Kazakh)
Дементьев:Дементьев NP-COG-OB ; ! ""
Демесін:Демесін NP-ANT-M ; ! "Demesin" (Kazakh)
Демеу:Демеу NP-ANT-M ; ! "Demew" (Kazakh)
Демеуов:Демеуов NP-COG-OB ; ! ""
Деми:Деми NP-ANT-F ; !"Use/MT"
Демидов:Демидов NP-COG-OB ; ! ""
Демокрит:Демокрит NP-ANT-M ; !"Use/MT"
Демосфен:Демосфен NP-ANT-M ; !"Use/MT"
Демченко:Демченко NP-COG-MF ; ! ""
Демьянка:Демьянка NP-TOP ; ! ""
Демьянов:Демьянов NP-COG-OB ; ! ""
Денвер:Денвер NP-TOP ; !"Use/MT"
Денев:Денев NP-COG-MF ; !"Use/MT"
Дениз:Дениз NP-ANT-F ; !"Use/MT"
Дениз:Дениз NP-ANT-M ; !"Use/MT"
Дениса:Дениса NP-ANT-F ; !"Use/MT"
Денис:Денис NP-ANT-M ; ! "Deniys" (Greek)
Денисенко:Денисенко NP-COG-MF ; ! ""
Денисов:Денисов NP-COG-OB ; ! ""
Денисов:Денисов NP-TOP ; ! ""
Денисовка:Денисовка NP-TOP ; ! ""
Депардье:Депардье NP-COG-MF ; ! ""
Дербай:Дербай NP-ANT-M ; ! "Derbay" (Kazakh)
Дербісалі:Дербісалі NP-ANT-M ; ! "Derbisali" (Arabic)
Дербісәлиев:Дербісәлиев NP-COG-OB ; ! ""
Дербісек:Дербісек NP-TOP ; ! ""
Дерек:Дерек NP-ANT-M ; !"Use/MT"
Державинск:Державинск NP-TOP ; ! ""
Дерипаска:Дерипаска NP-COG-M ; !"Use/MT"
Деркөл:Деркөл NP-TOP ; ! ""
Дерлет:Дерлет NP-COG-MF ; !"Use/MT"
Деррик:Деррик NP-ANT-M ; !"Use/MT"
Десмонд:Десмонд NP-ANT-M ; !"Use/MT"
Деспина:Деспина NP-ANT-F ; !"Use/MT"
Детройт:Детройт NP-TOP ; !"Use/MT"
Дешериев:Дешериев NP-COG-OB ; ! ""
Деште%-Кевир:Деште%-Кевир NP-TOP ; ! ""
Деште%-Лух:Деште%-Лух NP-TOP ; ! ""
Джаггер:Джаггер NP-ANT-M ; !"Use/MT"
Джад:Джад NP-ANT-M ; !"Use/MT" 
Джа:Джа NP-ANT-M ; !"Use/MT" 
Джакарта:Джакарта NP-TOP ; ! "" 
Джакомо:Джакомо NP-ANT-M ; !"Use/MT"
Джалиль:Джалиль NP-ANT-M ; !"Use/MT"
Джамал:Джамал NP-ANT-M ; !"Use/MT"
Джампаоло:Джампаоло NP-ANT-M ; !"Use/MT"
Джан:Джан NP-ANT-M ; !"Use/MT"
Джанет:Джанет NP-ANT-F ; !"Use/MT"
Джанна:Джанна NP-ANT-F ; !"Use/MT"
Джанни:Джанни NP-ANT-M ; !"Use/MT"
Джансугуров:Джансугуров NP-COG-OB ; ! ""
Джанфранко:Джанфранко NP-ANT-M ; !"Use/MT"
Джастин:Джастин NP-ANT-M ; ! ""
Джаф:Джаф NP-ANT-M ; !"Use/MT" 
Джаш:Джаш NP-ANT-M ; !"Use/MT" 
Джеб:Джеб NP-ANT-M ; !"Use/MT" 
Дже:Дже NP-ANT-M ; !"Use/MT" 
Джеймс:Джеймс NP-ANT-M ; ! "James" (English)
Джейн:Джейн NP-ANT-F ; !"Use/MT"
Джейсон:Джейсон NP-ANT-M ; !"Use/MT"
Джек:Джек NP-ANT-M ; !"Use/MT"
Джексон:Джексон NP-ANT-M ; !"Use/MT"
Джексон:Джексон NP-COG-MF ; !"Use/MT"
Дженера:Дженера NP-ANT-F ; !"Use/MT"
Джени:Джени NP-ANT-F ; !"Use/MT"
Дженис:Дженис NP-ANT-F ; !"Use/MT"
Дженни:Дженни NP-ANT-F ; !"Use/MT"
Дженнифер:Дженнифер NP-ANT-F ; !"Use/MT"
Дженсен:Дженсен NP-ANT-M ; !"Use/MT"
Джеральд:Джеральд NP-ANT-M ; !"Use/MT"
Джеральдин:Джеральдин NP-ANT-F ; !"Use/MT"
Джеральдины:Джеральдины NP-ANT-F ; !"Use/MT"
Джереми:Джереми NP-ANT-M ; !"Use/MT"
Джером:Джером NP-ANT-M ; !"Use/MT"
Джерри:Джерри NP-ANT-M ; !"Use/MT"
Джерси:Джерси NP-TOP ; ! ""
Джес:Джес NP-ANT-M ; !"Use/MT" 
Джесси:Джесси NP-ANT-M ; !"Use/MT"
Джессика:Джессика NP-ANT-F ; !"Use/MT"
Джет:Джет NP-ANT-M ; !"Use/MT" 
Джеф:Джеф NP-ANT-M ; !"Use/MT" 
Джефф:Джефф NP-ANT-M ; !"Use/MT"
Джефферсон:Джефферсон NP-ANT-M ; !"Use/MT"
Джефферсон:Джефферсон NP-COG-MF ; ! ""
Джеффри:Джеффри NP-ANT-M ; ! ""
Джеш:Джеш NP-ANT-M ; !"Use/MT" 
Джиада:Джиада NP-ANT-F ; !"Use/MT"
Джибути:Джибути%{☭%} NP-TOP ; ! ""
Джид:Джид NP-ANT-M ; !"Use/MT" 
Джил:Джил NP-ANT-M ; !"Use/MT"
Джиллиан:Джиллиан NP-ANT-F ; !"Use/MT"
Джим:Джим NP-ANT-M ; !"Use/MT"
Джимми:Джимми NP-ANT-M ; !"Use/MT"
Джина:Джина NP-ANT-F ; !"Use/MT"
Джин:Джин NP-ANT-F ; !"Use/MT"
Джин:Джин NP-ANT-M ; !"Use/MT"
Джино:Джино NP-ANT-M ; !"Use/MT"
Джоаккино:Джоаккино NP-ANT-M ; !"Use/MT"
Джоанна:Джоанна NP-ANT-F ; !"Use/MT"
Джованна:Джованна NP-ANT-F ; !"Use/MT"
Джованни:Джованни NP-ANT-M ; !"Use/MT"
Джог:Джог NP-ANT-M ; !"Use/MT" 
Джо:Джо NP-ANT-M ; ! "" 
Джозеф:Джозеф NP-ANT-M ; !"Use/MT"
Джои:Джои NP-ANT-M ; !"Use/MT"
Джой:Джой NP-ANT-M ; !"Use/MT" 
Джойс:Джойс NP-ANT-F ; !"Use/MT"
Джок:Джок NP-ANT-M ; !"Use/MT" 
Джокович:Джокович NP-COG-M ; ! ""
Джоли:Джоли NP-ANT-F ; !"Use/MT"
Джона:Джона NP-ANT-M ; ! "" !"Use/MT"
Джонатан:Джонатан NP-ANT-M ; !"Use/MT"
Джон:Джон NP-ANT-M ; ! "" 
Джонни:Джонни NP-ANT-M ; !"Use/MT"
Джонс:Джонс NP-COG-MF ; !"Use/MT"
Джонсон:Джонсон NP-COG-MF ; !"Use/MT"
Джонстон:Джонстон NP-COG-MF ; !"Use/MT"
Джоп:Джоп NP-ANT-M ; !"Use/MT" 
Джордж:Джордж NP-ANT-M ; ! "" 
Джорджио:Джорджио NP-ANT-M ; !"Use/MT"
Джорджия:Джорджия NP-TOP ; ! ""
Джор:Джор NP-ANT-M ; !"Use/MT" 
Джош:Джош NP-ANT-M ; !"Use/MT"
Джошуа:Джошуа NP-ANT-M ; !"Use/MT"
Джоэл:Джоэл NP-ANT-M ; !"Use/MT"
Джуд:Джуд NP-ANT-M ; !"Use/MT"
Джудит:Джудит NP-ANT-F ; !"Use/MT"
Джузеппе:Джузеппе NP-ANT-M ; ! ""
Джулиана:Джулиана NP-ANT-F ; !"Use/MT"
Джулиан:Джулиан NP-ANT-M ; ! "" 
Джулиано:Джулиано NP-ANT-M ; !"Use/MT"
Джули:Джули NP-ANT-F ; !"Use/MT"
Джулио:Джулио NP-ANT-M ; !"Use/MT"
Джулия:Джулия NP-ANT-F ; !"Use/MT"
Джульета:Джульета NP-ANT-F ; !"Use/MT"
Джульетта:Джульетта NP-ANT-F ; ! ""
Джут:Джут NP-ANT-M ; !"Use/MT" 
Джэй:Джэй NP-ANT-M ; !"Use/MT" 
Джэймсон:Джэймсон NP-ANT-M ; !"Use/MT"
Джэт:Джэт NP-ANT-M ; !"Use/MT" 
Дзюнко:Дзюнко NP-COG-MF ; ! ""
Диана:Диана NP-ANT-F ; !"Use/MT"
Диас:Диас NP-ANT-M ; ! "Dıyas" (Arabic)
Дибай:Дибай NP-ANT-M ; ! "Dıybay" (Persian)
Диб:Диб NP-ANT-M ; !"Use/MT" 
Диваев:Диваев NP-COG-OB ; ! ""
Дидара:Дидара NP-ANT-F ; ! "Dıydara" (Persian)
Дидарбек:Дидарбек NP-ANT-M ; ! "Dıydarbek" (Persian)
Дидар:Дидар NP-ANT-M ; ! "Dıydar" (Persian)
Ди:Ди NP-ANT-M ; !"Use/MT" 
Дидро:Дидро NP-COG-MF ; ! ""
Дидье:Дидье NP-ANT-M ; !"Use/MT"
Диего:Диего NP-ANT-M ; !"Use/MT"
Дик:Дик NP-ANT-M ; ! "" 
Диқанбай:Диқанбай NP-ANT-M ; ! "Dıyqanbay" (Persian)
Дила:Дила NP-ANT-F ; ! "Dıyla" (Arabic)
Диллон:Диллон NP-ANT-M ; !"Use/MT"
Дильбегим:Дильбегим NP-ANT-M ; ! "" 
Дима:Дима NP-ANT-M ; ! ""
Дим:Дим NP-ANT-M ; !"Use/MT" 
Димитриев:Димитриев NP-COG-OB ; ! ""
Димитрий:Димитрий NP-ANT-M ; !"Use/MT"
Дина:Дина NP-ANT-F ; ! "Dina"
Дина:Дина NP-ANT-F ; ! "Dina" (Arabic)
Динамо:Динамо NP-AL ; ! ""
Динара:Динара NP-ANT-F ; ! "Dıynara" (Arabic)
Дин:Дин NP-ANT-M ; !"Use/MT"
Дино:Дино NP-ANT-M ; !"Use/MT"
Диомидов:Диомидов NP-COG-OB ; ! ""
Диор:Диор NP-COG-MF ; !"Use/MT"
Дисней:Дисней NP-COG-MF ; !"Use/MT"
Дитрих:Дитрих NP-COG-MF ; !"Use/MT"
Дифранко:Дифранко NP-COG-MF ; !"Use/MT"
Диярбек:Диярбек NP-ANT-M ; ! "Dıyarbek" (Kazakh)
Дияр:Дияр NP-ANT-M ; ! "Dıyar" (Arabic)
Дмитренко:Дмитренко NP-COG-MF ; ! ""
Дмитриев:Дмитриев NP-COG-OB ; ! ""
Дмитрий:Дмитрий NP-ANT-M ; ! "Dmitriy"
Дмитров:Дмитров NP-COG-OB ; ! ""
Днепр:Днепр NP-TOP ; ! ""
Днестр:Днестр NP-TOP ; ! ""
Добродомов:Добродомов NP-COG-OB ; ! ""
Добролюбов:Добролюбов NP-COG-OB ; ! ""
Довженко:Довженко NP-COG-MF ; ! ""
Додабай:Додабай NP-ANT-M ; ! "Dodabay" (Kazakh)
Додан:Додан NP-ANT-M ; ! "Dodan" (Kazakh)
Дойл:Дойл NP-COG-MF ; !"Use/MT"
Дойман:Дойман NP-ANT-M ; ! "Doyman" (Kazakh)
Дойче:Дойче NP-TOP ; ! "" ! Use/MT
Долган:Долган NP-TOP ; !"Use/MT"
Долган%-Ненец:Долган%-Ненец NP-TOP ; ! ""
Долгашев:Долгашев NP-COG-OB ; ! ""
Долгов:Долгов NP-COG-OB ; ! ""
Долинка:Долинка NP-TOP ; ! ""
Долорес:Долорес NP-ANT-F ; !"Use/MT"
Домалақ:Домалақ NP-TOP ; ! ""
Домбаров:Домбаров NP-COG-OB ; ! ""
Доменек:Доменек NP-COG-MF ; !"Use/MT"
Доменико:Доменико NP-ANT-M ; !"Use/MT"
Доминго:Доминго NP-ANT-M ; !"Use/MT"
Доминика:Доминика NP-ANT-F ; !"Use/MT"
Доминика:Доминика NP-TOP ; ! ""
Доминик:Доминик NP-ANT-M ; !"Use/MT"
Домогаров:Домогаров NP-COG-OB ; ! ""
Донақов:Донақов NP-COG-OB ; ! "" ! Use/MT
Дональд:Дональд NP-ANT-M ; !"Use/MT"
Донато:Донато NP-ANT-M ; !"Use/MT"
Донбасс:Донбасс NP-TOP ; ! ""
Дон:Дон NP-ANT-M ; !"Use/MT"
Дон:Дон NP-TOP ; ! ""
Донец:Донец NP-ANT-M ; ! ""
Донецк:Донецк NP-TOP ; ! ""
Доннедьё:Доннедьё NP-COG-MF ; !"Use/MT"
Донован:Донован NP-ANT-M ; !"Use/MT"
Донченко:Донченко NP-COG-MF ; ! ""
Доңызтау:Доңызтау NP-TOP ; ! ""
Дора:Дора NP-ANT-F ; ! "Dora" (Arabic)
Дор:Дор NP-ANT-M ; !"Use/MT" 
Доренко:Доренко NP-COG-MF ; ! ""
Дориан:Дориан NP-ANT-M ; !"Use/MT"
Доронин:Доронин NP-COG-IN ; ! ""
Дорофеев:Дорофеев NP-COG-OB ; ! ""
Досаев:Досаев NP-COG-OB ; ! "" ! Use/MT
Досай:Досай NP-ANT-M ; ! "Dosay" 
Досақ:Досақ NP-ANT-M ; ! "Dosaq" (Kazakh)
Досанов:Досанов NP-COG-OB ; ! "" ! Use/MT
Досәлі:Досәлі NP-ANT-M ; ! "Dosäli" (Arabic)
Досбай:Досбай NP-ANT-M ; ! "Dosbay" 
Досберген:Досберген NP-ANT-M ; ! "Dosbergen" (Kazakh)
Досбол:Досбол NP-ANT-M ; ! "Be a friend"
Досбол:Досбол NP-ANT-M ; ! "Dosbol" (Arabic)
Дос:Дос NP-ANT-M ; ! "Dos" (Arabic)
Досжан:Досжан NP-ANT-M ; ! "Dosjan" (Kazakh)
Досжанов:Досжанов NP-COG-OB ; ! ""
Досия:Досия NP-ANT-F ; ! "Dosıya" (Arabic)
Досқалиев:Досқалиев NP-COG-OB ; ! ""
Досқалиев:Досқалиев NP-COG-OB ; ! ""
Досмат:Досмат NP-ANT-M ; ! "Dosmat" (Arabic)
Досмұқан:Досмұқан NP-ANT-M ; ! "Dosmuqan" (Arabic)
Досмүхамедов:Досмүхамедов NP-COG-OB ; ! ""
Доспанов:Доспанов NP-COG-OB ; ! ""
Достоевский:Достоевский NP-COG-M ; ! ""
Достық:Достық NP-TOP ; ! ""
Досымбек:Досымбек NP-ANT-M ; ! "Dosımbek" (Kazakh)
Досым:Досым NP-ANT-M ; ! "Dosım" (Kazakh)
Досымжан:Досымжан NP-ANT-M ; ! "Dosımjan" (Arabic)
Досымжанов:Досымжанов NP-COG-OB ; ! ""
Досымов:Досымов NP-COG-OB ; ! "" ! Use/MT
Досымханов:Досымханов NP-COG-OB ; ! "" ! Use/MT
Доф:Доф NP-ANT-M ; !"Use/MT" 
Доһа:Доһа NP-TOP ; ! "" 
Доценко:Доценко NP-COG-MF ; ! ""
Доян:Доян NP-ANT-M ; ! "Doyan" 
Дөненбай:Дөненбай NP-ANT-M ; ! "Dönenbay" (Kazakh)
Дөнентаев:Дөнентаев NP-COG-OB ; ! ""
Дөңгелек:Дөңгелек NP-TOP ; ! ""
Драги:Драги NP-ANT-M ; !"Use/MT"
Драго:Драго NP-ANT-M ; !"Use/MT"
Драгомир:Драгомир NP-ANT-M ; !"Use/MT"
Дракула:Дракула NP-ANT-M ; !"Use/MT"
Дрепер:Дрепер NP-COG-MF ; ! ""
Дрина:Дрина NP-ANT-F ; !"Use/MT"
Дроздов:Дроздов NP-COG-OB ; ! ""
Дуанбай:Дуанбай NP-ANT-M ; ! "Dıwanbay" (Persian)
Дубай:Дубай NP-TOP ; ! ""
Дубай:Дубай NP-TOP ; ! "Dubai"
Дубанов:Дубанов NP-COG-OB ; ! ""
Дублин:Дублин NP-TOP ; ! "Dublin"
Дубов:Дубов NP-COG-OB ; ! ""
Дубовка:Дубовка NP-TOP ; ! ""
Дубров:Дубров NP-COG-OB ; ! ""
Дубровное:Дубровное NP-TOP ; ! ""
Дубровский:Дубровский NP-COG-M ; ! ""
Дубровченко:Дубровченко NP-COG-MF ; ! ""
Дуванов:Дуванов NP-COG-OB ; ! ""
Дуглас:Дуглас NP-ANT-M ; !"Use/MT"
Дудар:Дудар NP-ANT-M ; ! "Dıwdar" (Kazakh)
Ду:Ду NP-ANT-M ; !"Use/MT" 
Дулат:Дулат NP-ANT-M ; !"Use/MT"
Дулатов:Дулатов NP-COG-OB ; ! ""
Дулатұлы:Дулатұлы NP-COG-M ; ! "" ! Use/MT
Думан:Думан NP-ANT-M ; ! "Dıwman" (Kazakh)
Думенко:Думенко NP-COG-MF ; ! ""
Дунаев:Дунаев NP-COG-OB ; ! "" ! Use/MT
Дунай:Дунай NP-TOP ; !""
Дуния:Дуния NP-ANT-F ; ! "Dıwnıya" (Arabic)
Дункан:Дункан NP-ANT-M ; !"Use/MT"
Дурасов:Дурасов NP-COG-OB ; ! ""
Дурия:Дурия NP-ANT-F ; ! "Dıwrıya" (Persian)
Дурнев:Дурнев NP-COG-OB ; ! ""
Дусматов:Дусматов NP-COG-OB ; ! ""
Дутов:Дутов NP-COG-OB ; ! ""
Душанбе:Душанбе NP-TOP ; ! ""
Дұлатұлы:Дұлатұлы NP-COG-M ; ! "" ! Use/MT
Дүзелханов:Дүзелханов NP-COG-OB ; ! ""
Дүйсебаев:Дүйсебаев NP-COG-OB ; ! ""
Дүйсекей:Дүйсекей NP-ANT-M ; ! "Düysekey" (Persian)
Дүйсембинов:Дүйсембинов NP-COG-OB ; ! ""
Дүйсенбаев:Дүйсенбаев NP-COG-OB ; ! "USE/MT"
Дүйсенбай:Дүйсенбай NP-ANT-M ; ! "Düysenbay" (Persian)
Дүйсенбай:Дүйсенбай NP-TOP ; ! ""
Дүйсенбек:Дүйсенбек NP-ANT-M ; ! "" 
Дүйсенгали:Дүйсенгали NP-ANT-M ; ! "Düysengalıy" (Persian)
Дүйсен:Дүйсен NP-ANT-M ; ! "Düysen" (Persian)
Дүйсенов:Дүйсенов NP-COG-OB ; ! ""
Дүйсехан:Дүйсехан NP-ANT-M ; ! "Düysexan" (Persian)
Дүнгене:Дүнгене NP-TOP ; ! ""
Дүрмішев:Дүрмішев NP-COG-OB ; ! ""
Дъяков:Дъяков NP-COG-OB ; ! ""
Дыбенко:Дыбенко NP-COG-MF ; ! ""
Ділара:Ділара NP-ANT-F ; ! "Dilara" (Persian)
Ділбара:Ділбара NP-ANT-F ; ! "Dilbara" (Persian)
Ділдабай:Ділдабай NP-ANT-M ; ! "Dildabay" (Arabic)
Ділдабек:Ділдабек NP-ANT-M ; ! "Dildabek" 
Ділда:Ділда NP-ANT-F ; ! "Dilda" (Arabic)
Ділма:Ділма NP-ANT-M ; ! "Dilda" (Arabic)
Дінмұхаммед:Дінмұхаммед NP-ANT-M ; ! "" 
Дінішев:Дінішев NP-COG-OB ; ! ""
Дью:Дью NP-ANT-M ; !"Use/MT" 
Дьяченко:Дьяченко NP-COG-MF ; ! ""
Дьячков:Дьячков NP-COG-OB ; ! ""
Дэвид:Дэвид NP-ANT-M ; ! "" 
Дэвис:Дэвис NP-ANT-M ; !"Use/MT"
Дэйн:Дэйн NP-ANT-M ; !"Use/MT"
Дэйтон:Дэйтон NP-ANT-M ; !"Use/MT"
Дэмиен:Дэмиен NP-ANT-M ; !"Use/MT"
Дэн:Дэн NP-ANT-M ; !"Use/MT"
Дэнни:Дэнни NP-ANT-M ; !"Use/MT"
Дэрил:Дэрил NP-ANT-M ; !"Use/MT"
Дэстини:Дэстини NP-ANT-F ; !"Use/MT"
Дюпон:Дюпон NP-COG-MF ; !"Use/MT"
Дюрер:Дюрер NP-COG-MF ; !"Use/MT"
Дяченко:Дяченко NP-COG-MF ; ! ""
Ебейті:Ебейті NP-TOP ; ! ""
Ебінұр:Ебінұр NP-TOP ; ! ""
Ева:Ева NP-ANT-F ; ! ""
Евгениев:Евгениев NP-COG-OB ; ! "" ! Use/MT
Евгениевич:Евгение NP-PAT-VICH ; ! ""
Евгений:Евгений NP-ANT-M ; ! "Yevgeniy"
Евгения:Евгения NP-ANT-F ; ! ""
Евгеньевка:Евгеньевка NP-TOP ; ! ""
Евдокимов:Евдокимов NP-COG-OB ; ! ""
Евкуров:Евкуров NP-COG-OB ; ! "" ! Use/MT
Евланов:Евланов NP-COG-OB ; ! ""
Еврей:Еврей NP-TOP ; ! "Jew"
Евроньюс:Евроньюс NP-ORG ; ! "USE/MT"
Евсеев:Евсеев NP-COG-OB ; ! ""
Евстафьев:Евстафьев NP-COG-OB ; ! ""
Евстигнеев:Евстигнеев NP-COG-OB ; ! ""
Евтушенко:Евтушенко NP-COG-MF ; ! "" ! Use/MT
Евфрат:Евфрат NP-TOP ; ! ""
Египет:Египет NP-TOP ; ! ""
Егор:Егор NP-ANT-M ; ! "Egor" (Greek)
Егоров:Егоров NP-COG-OB ; ! ""
Егізбай:Егізбай NP-ANT-M ; ! "Egizbay" (Kazakh)
Егізбек:Егізбек NP-ANT-M ; ! "Egizbek" (Kazakh)
Егізен:Егізен NP-TOP ; ! ""
Егізқара:Егізқара NP-TOP ; ! ""
Егіндібұлақ:Егіндібұлақ NP-TOP ; ! ""
Егіндікөл:Егіндікөл NP-TOP ; ! ""
Едіге:Едіге NP-ANT-M ; ! "Edige" (Arabic)
Едіге:Едіге NP-TOP ; ! ""
Еділбай:Еділбай NP-ANT-M ; ! "Edilbay" (Old Turkic)
Еділбек:Еділбек NP-ANT-M ; ! "Edilbek" (Old Turkic)
Еділ:Еділ NP-ANT-M ; ! "Edil" (Old Turkic)
Еділ:Еділ NP-TOP ; ! ""
Еділ:Еділ NP-TOP ; ! ""
Еділ:Еділ NP-TOP ; ! "Volga river"
Еділжан:Еділжан NP-ANT-M ; ! "Ediljan" (Old Turkic)
Еділтай:Еділтай NP-ANT-M ; ! "Ediltay" (Old Turkic)
Едірей:Едірей NP-TOP ; ! ""
Еж:Еж NP-ANT-M ; !"Use/MT" 
Ежи:Ежи NP-ANT-M ; !"Use/MT"
Ежов:Ежов NP-COG-OB ; ! ""
Езекия:Езекия NP-ANT-M ; ! "Ezekial"
Екатерина:Екатерина NP-ANT-F ; ! ""
Екатеринбург:Екатеринбург NP-TOP ; ! ""
Екимов:Екимов NP-COG-OB ; ! ""
Екпінді:Екпінді NP-TOP ; ! ""
Екуадор:Екуадор NP-TOP ; ! ""
Екіаша:Екіаша NP-TOP ; ! ""
Екібастұз:Екібастұз NP-TOP ; ! ""
Екіөгіз:Екіөгіз NP-TOP ; ! ""
Елазар:Елазар NP-ANT-M ; ! ""
Еламан:Еламан NP-ANT-M ; ! "Elaman" (Kazakh)
Елбар:Елбар NP-ANT-M ; ! "Elbar" (Kazakh)
Елбас:Елбас NP-ANT-M ; ! "Elbas" (Kazakh)
Елбек:Елбек NP-ANT-M ; ! "Elbek" (Kazakh)
Елдар:Елдар NP-ANT-M ; ! "Eldar" (Kazakh)
Елдес:Елдес NP-ANT-M ; ! "Eldes" (Kazakh)
Елдос:Елдос NP-ANT-M ; ! "Eldos" (Kazakh)
Елебаев:Елебаев NP-COG-OB ; ! ""
Елебай:Елебай NP-ANT-M ; ! "Elebay" (Kazakh)
Елебеков:Елебеков NP-COG-OB ; ! ""
Елек:Елек NP-TOP ; ! ""
Елена:Елена NP-ANT-F ; ! "Elena" (Greek)
Елен:Елен NP-ANT-M ; ! "Elen" (Kazakh)
Еленовка:Еленовка NP-TOP ; ! ""
Елеубай:Елеубай NP-ANT-M ; ! "Elewbay" (Kazakh)
Елеу:Елеу NP-ANT-M ; ! "Elew" (Kazakh)
Елеуов:Елеуов NP-COG-OB ; ! ""
Елеусізов:Елеусізов NP-COG-OB ; ! ""
Елжан:Елжан NP-ANT-M ; ! "Eljan" (Kazakh)
Елжар:Елжар NP-ANT-M ; ! "Eljar" (Kazakh)
Елжасар:Елжасар NP-ANT-M ; ! "Eljasar" (Kazakh)
Елжас:Елжас NP-ANT-M ; ! "Eljas" (Kazakh)
Елизавета:Елизавета NP-ANT-F ; ! "Elizaveta" (Skandinavian)
Елимов:Елимов NP-COG-OB ; ! ""
Елисеев:Елисеев NP-COG-OB ; ! ""
Елқам:Елқам NP-ANT-M ; ! "Elqam" (Arabic)
Елмұрат:Елмұрат NP-ANT-M ; ! "Elmurat" (Arabic)
Елназар:Елназар NP-ANT-M ; ! "Elnazar" (Kazakh)
Елнар:Елнар NP-ANT-M ; ! "Elnar" (Kazakh)
Елнұр:Елнұр NP-ANT-M ; ! "Elnur" (Kazakh)
Елтай:Елтай NP-TOP ; ! ""
Елтоқ:Елтоқ NP-ANT-M ; ! "Eltoq" (Kazakh)
Елубаев:Елубаев NP-COG-OB ; ! ""
Елшібай:Елшібай NP-ANT-M ; ! "Elshibay" (Kazakh)
Елібай:Елібай NP-ANT-M ; ! "Elibay" (Kazakh)
Ельцов:Ельцов NP-COG-OB ; ! ""
Ембі:Ембі NP-TOP ; ! ""
Ембі:Ембі NP-TOP ; ! ""
Ембімұнайгаз:Ембімұнайгаз NP-TOP ; ! ""
Емел:Емел NP-ANT-M ; ! "Emel" (Arabic)
Емелжан:Емелжан NP-ANT-M ; ! "Emeljan" (Arabic)
Емелтай:Емелтай NP-ANT-M ; ! "Emeltay" (Arabic)
Емельянов:Емельянов NP-COG-OB ; ! ""
Еміл:Еміл NP-TOP ; ! ""
Ендеров:Ендеров NP-COG-OB ; ! ""
Енисей:Енисей NP-TOP ; ! ""
Енисей:Енисей NP-TOP ; ! ""
Еншіқарман:Еншіқарман NP-TOP ; ! ""
Еңбек:Еңбек NP-TOP ; ! ""
Еңбек% Ер:Еңбек% Ер N-COMPOUND-PX  ; ! ""
Еңбекші:Еңбекші NP-TOP ; ! ""
Еңбекшіқазақ:Еңбекшіқазақ NP-TOP ; ! ""
Еңбекшілдер:Еңбекшілдер NP-TOP ; ! ""
Еңлік:Еңлік NP-ANT-F ; ! "Eŋlik" (Kazakh)
Еңсебек:Еңсебек NP-ANT-M ; ! "Eŋsebek" (Kazakh)
Еңсепов:Еңсепов NP-COG-OB ; ! ""
ЕО:ЕО NP-TOP ; ! "Еуроодақ = EU"
Еразумов:Еразумов NP-COG-OB ; ! ""
Ералиев:Ералиев NP-COG-OB ; ! "" ! Use/MT
Ералы:Ералы NP-ANT-M ; ! "Eralı" (Kazakh)
Ерасыл:Ерасыл NP-ANT-M ; ! "Erasıl" (Kazakh)
Ерәлиев:Ерәлиев NP-COG-OB ; ! ""
Ерәлі:Ерәлі NP-TOP ; ! ""
Ербай:Ербай NP-ANT-M ; ! "Erbay" (Kazakh)
Ербақ:Ербақ NP-ANT-M ; ! "Erbaq" (Kazakh)
Ербала:Ербала NP-ANT-M ; ! "Erbala" (Kazakh)
Ербатыр:Ербатыр NP-ANT-M ; ! "Erbatır" (Kazakh)
Ербек:Ербек NP-ANT-M ; ! "Erbek" (Kazakh)
Ерболат:Ерболат NP-ANT-M ; ! "Erbolat" (Kazakh)
Ербол:Ербол NP-ANT-M ; ! "Erbol"
Ербол:Ербол NP-ANT-M ; ! "Erbol" (Kazakh)
Ерболсын:Ерболсын NP-ANT-M ; ! "Erbolsın" (Kazakh)
Ербосын:Ербосын NP-ANT-M ; ! "Erbosın" (Kazakh)
Ербота:Ербота NP-ANT-M ; ! "Erbota" (Kazakh)
Ергеш:Ергеш NP-ANT-M ; ! "Ergesh" (Kazakh)
Ергозы:Ергозы NP-ANT-M ; ! "Ergozı" (Kazakh)
Ерғазы:Ерғазы NP-ANT-M ; ! "Erğazı" (Arabic)
Ерғалиев:Ерғалиев NP-COG-OB ; ! ""
Ерғали:Ерғали NP-ANT-M ; ! "Erğalıy" (Arabic)
Ерғожа:Ерғожа NP-ANT-M ; ! "Erğoja" (Arabic)
Ердар:Ердар NP-ANT-M ; ! "Erdar" (Arabic)
Ердәулет:Ердәулет NP-ANT-M ; ! "Erdäwlet" (Kazakh)
Ерденай:Ерденай NP-ANT-M ; ! "Erdenay" (Kazakh)
Ерденбай:Ерденбай NP-ANT-M ; ! "Erdenbay" (Kazakh)
Ерденбек:Ерденбек NP-ANT-M ; ! "Erdenbek" (Kazakh)
Ерденғали:Ерденғали NP-ANT-M ; ! "Erdenğalıy" (Kazakh)
Ерден:Ерден NP-ANT-M ; ! "Erden" (Kazakh)
Ердоған:Ердоған NP-COG-MF ; ! ""
Ереван:Ереван NP-TOP ; ! ""
Ереван:Ереван NP-TOP ; ! "" 
Ережеп:Ережеп NP-ANT-M ; ! "Erejep" (Arabic)
Ерейментау:Ерейментау NP-TOP ; ! ""
Еремеев:Еремеев NP-COG-OB ; ! ""
Ерементау:Ерементау NP-TOP ; ! ""
Еремич:Еремич NP-COG-M ; ! ""
Еремия:Еремия NP-ANT-M ; ! ""
Ерен:Ерен NP-ANT-M ; ! "Eren" (Kazakh)
Ер:Ер NP-ANT-M ; !"Use/MT" 
Ержан:Ержан NP-ANT-M ; ! ""
Ержан:Ержан NP-ANT-M ; ! "Erjan" 
Ержан:Ержан NP-ANT-M ; ! "Erjan" (Kazakh)
Ержанов:Ержанов NP-COG-OB ; ! ""
Ержұман:Ержұман NP-ANT-M ; ! "Erjuman" (Kazakh)
Ержігіт:Ержігіт NP-ANT-M ; ! "Erjigit" (Kazakh)
Ерзада:Ерзада NP-ANT-F ; ! "Erzada" (Kazakh)
Ерзиба:Ерзиба NP-ANT-F ; ! "Erzıyba" (Kazakh)
Ерицов:Ерицов NP-COG-OB ; ! ""
Еркебай:Еркебай NP-ANT-M ; ! "Erkebay" (Kazakh)
Еркеболат:Еркеболат NP-ANT-M ; ! "Erkebolat" (Kazakh)
Еркебұлан:Еркебұлан NP-ANT-M ; ! "Erkebulan" (Kazakh)
Еркеғали:Еркеғали NP-ANT-M ; !"Use/MT"
Ерке:Ерке NP-ANT-F ; ! "Erke" (Kazakh)
Еркежан:Еркежан NP-ANT-F ; ! "Erkejan" (Kazakh)
Еркінбай:Еркінбай NP-ANT-M ; ! "Erkinbay" (Kazakh)
Еркінғали:Еркінғали NP-ANT-M ; ! "Erkinğalıy" (Kazakh)
Еркін:Еркін NP-ANT-M ; ! "Erkin" (Kazakh)
Еркін:Еркін NP-TOP ; ! ""
Еркінжан:Еркінжан NP-ANT-M ; ! "Erkinjan" (Kazakh)
Еркінқала:Еркінқала NP-TOP ; ! ""
Еркінтай:Еркінтай NP-ANT-M ; ! "Erkintay" (Kazakh)
Еркінұлы:Еркінұлы NP-ANT-M ; !
Ерқожа:Ерқожа NP-ANT-M ; ! "Erqoja" (Arabic)
Ерлан:Ерлан NP-ANT-M ; ! "Erlan"
Ерлан:Ерлан NP-ANT-M ; ! "Erlan" (Kazakh)
Ерлен:Ерлен NP-ANT-M ; ! "Erlen" (Kazakh)
Ерлік:Ерлік NP-ANT-M ; ! "Erlik" (Kazakh)
Ермак:Ермак NP-COG-M ; ! ""
Ермаков:Ермаков NP-COG-OB ; ! ""
Ерман:Ерман NP-ANT-M ; ! "Erman" (Kazakh)
Ермат:Ермат NP-ANT-M ; ! "Ermat" (Kazakh)
Ермегияев:Ермегияев NP-COG-OB ; ! "" ! Use/MT
Ермекбаев:Ермекбаев NP-COG-OB ; ! "" ! Use/MT
Ермек:Ермек NP-ANT-M ; ! "Ermek" (Kazakh)
Ермеков:Ермеков NP-COG-OB ; ! ""
Ермексу:Ермексу NP-TOP ; ! ""
Ермилов:Ермилов NP-COG-OB ; ! ""
Ермолаев:Ермолаев NP-COG-OB ; ! ""
Ермолов:Ермолов NP-COG-OB ; ! ""
Ермұхамет:Ермұхамет NP-ANT-M ; ! "" 
Ерназар:Ерназар NP-ANT-M ; ! ""
Ерназар:Ерназар NP-TOP ; ! ""
Ерназаров:Ерназаров NP-COG-OB ; ! ""
Ернар:Ернар NP-ANT-M ; ! "Ernar" (Kazakh)
Ернияз:Ернияз NP-ANT-M ; ! "Ernıyaz" (Kazakh)
Ернұр:Ернұр NP-ANT-M ; ! "Ernur" (Arabic)
Ерофеев:Ерофеев NP-COG-OB ; ! ""
Ерсайын:Ерсайын NP-ANT-M ; ! "Ersayın" (Kazakh)
Ерсұлтан:Ерсұлтан NP-ANT-M ; !"Use/MT" 
Ертаев:Ертаев NP-COG-OB ; ! "" ! Use/MT
Ертай:Ертай NP-ANT-M ; ! "Ertay" (Kazakh)
Ер% Төстік:Ер% Төстік NP-ANT-M ; ! "Er Tostik" USE/MT
Ертуған:Ертуған NP-ANT-M ; ! "Ertıwğan" (Kazakh)
Ертілес:Ертілес NP-ANT-M ; ! "Ertiles" (Kazakh)
Ертісбаев:Ертісбаев NP-COG-OB ; ! "" ! Use/MT
Ертіс:Ертіс NP-ANT-M ; ! "Ertis" (Old Turkic)
Ертіс:Ертіс NP-TOP ; ! ""
Ертіс:Ертіс NP-TOP ; ! ""
Ерубаев:Ерубаев NP-COG-OB ; ! ""
Ерубай:Ерубай NP-ANT-M ; ! "Erıwbay" (Kazakh)
Ершов:Ершов NP-COG-OB ; ! ""
Ершовка:Ершовка NP-TOP ; ! ""
Ерік:Ерік NP-ANT-M ; ! "Erik" (Kazakh)
Ерімбек:Ерімбек NP-ANT-M ; ! "Erimbek" (Kazakh)
Есбай:Есбай NP-ANT-M ; ! "Esbay" (Kazakh)
Есбатыр:Есбатыр NP-ANT-M ; ! "Esbatır" (Kazakh)
Есберген:Есберген NP-ANT-M ; ! "Esbergen" (Kazakh)
Есболай:Есболай NP-ANT-M ; ! "Esbolay" (Kazakh)
Есболайұлы:Есболайұлы NP-COG-M ; ! "USE/MT"
Есболат:Есболат NP-ANT-M ; ! "Esbolat" (Kazakh)
Есбол:Есбол NP-ANT-M ; ! "Esbol" (Kazakh)
Есболов:Есболов NP-COG-OB ; ! ""
Есдәулет:Есдәулет NP-ANT-M ; ! "Esdäwlet" (Kazakh)
Есдәулетов:Есдәулетов NP-COG-OB ; ! ""
Есдәуір:Есдәуір NP-ANT-M ; ! "Esdäwir" (Kazakh)
Есеболатов:Есеболатов NP-TOP ; ! ""
Есей:Есей NP-ANT-M ; ! "Esey" (Kazakh)
Есекеев:Есекеев NP-COG-OB ; ! "" ! Use/MT
Есенай:Есенай NP-ANT-M ; ! "Esenay" (Kazakh)
Есенаман:Есенаман NP-ANT-M ; ! "Esenaman" (Kazakh)
Есенанқаты:Есенанқаты NP-TOP ; ! ""
Есенәлі:Есенәлі NP-ANT-M ; ! "Esenäli" (Kazakh)
Есенбаев:Есенбаев NP-COG-OB ; ! ""
Есенбай:Есенбай NP-ANT-M ; ! "Esenbay" (Kazakh)
Есенбақи:Есенбақи NP-ANT-M ; ! "Esenbaqıy" (Kazakh)
Есенбек:Есенбек NP-ANT-M ; ! "" 
Есенберді:Есенберді NP-ANT-M ; ! "Esenberdi" (Kazakh)
Есенболат:Есенболат NP-ANT-M ; ! "Esenbolat" (Kazakh)
Есенбол:Есенбол NP-ANT-M ; ! "Esenbol" (Kazakh)
Есен:Есен NP-ANT-M ; ! "Esen" (Kazakh)
Есен:Есен NP-TOP ; ! ""
Есенжан:Есенжан NP-ANT-M ; ! "Esenjan" (Kazakh)
Есенкелді:Есенкелді NP-ANT-M ; ! "Esenkeldi" (Kazakh)
Есенкелді:Есенкелді NP-TOP ; ! ""
Есенқұлов:Есенқұлов NP-COG-OB ; ! ""
Есенов:Есенов NP-COG-OB ; ! ""
Есенсай:Есенсай NP-TOP ; ! ""
Есентай:Есентай NP-TOP ; ! ""
Есентемір:Есентемір NP-ANT-M ; ! "Esentemir" (Kazakh)
Ес:Ес NP-ANT-M ; ! "Es" (Kazakh)
Есет:Есет NP-ANT-M ; ! "Eset" (Persian)
Есжан:Есжан NP-ANT-M ; ! "Esjan" (Kazakh)
Есжанов:Есжанов NP-COG-OB ; ! ""
Ескелді% би:Ескелді% би NP-TOP ; ! ""
Ескелді:Ескелді NP-TOP ; ! ""
Ескендір:Ескендір NP-ANT-M ; ! "Eskendir" (Arabic)
Ескендіров:Ескендіров NP-COG-OB ; ! "Eskendir" (Arabic)
Ескене:Ескене NP-TOP ; ! ""
Ескожа:Ескожа NP-ANT-M ; ! "Eskoja" (Arabic)
Ескідариялық:Ескідариялық NP-TOP ; ! ""
Ескіжол:Ескіжол NP-TOP ; ! ""
Ескі% Иқан:Ескі% Иқан NP-TOP ; ! ""
Ескі% Шалқар:Ескі% Шалқар NP-TOP ; ! ""
Есқайыр:Есқайыр NP-ANT-M ; ! "Esqayır" (Arabic)
Есқалиев:Есқалиев NP-COG-OB ; ! ""
Есқалиев:Есқалиев NP-COG-OB ; ! ""
Есқали:Есқали NP-ANT-M ; ! "Esqalıy" (Kazakh)
Есқараев:Есқараев NP-COG-OB ; ! ""
Есмағанбетов:Есмағанбетов NP-COG-OB ; ! ""
Есмамбетов:Есмамбетов NP-COG-OB ; ! ""
Есмұрат:Есмұрат NP-ANT-M ; ! "Esmurat" (Kazakh)
Есов:Есов NP-COG-OB ; ! ""
Еспе:Еспе NP-TOP ; ! ""
Еспенбет:Еспенбет NP-TOP ; ! ""
Еспетұз:Еспетұз NP-TOP ; ! ""
Есполов:Есполов NP-COG-OB ; ! ""
Естай:Естай NP-ANT-M ; ! "" 
Естекбай:Естекбай NP-ANT-M ; ! "Estekbay" (Kazakh)
Естібай:Естібай NP-ANT-M ; ! "Estibay" (Kazakh)
Есікасу:Есікасу NP-TOP ; ! ""
Есік:Есік NP-TOP ; ! ""
Есілбай:Есілбай NP-ANT-M ; ! "Esilbay" (Kazakh)
Есіл:Есіл NP-ANT-M ; ! "Esil" (Kazakh)
Есіл:Есіл NP-TOP ; ! ""
Есімбек:Есімбек NP-ANT-M ; ! "Esimbek" (Kazakh)
Есім:Есім NP-ANT-M ; ! "Esim" (Kazakh)
Есімов:Есімов NP-COG-OB ; ! ""
Есімов:Есімов NP-COG-OB ; ! "" ! Use/MT
Есіргепов:Есіргепов NP-COG-OB ; ! ""
Есірке:Есірке NP-ANT-M ; ! "Esirke" (Kazakh)
Ет:Ет NP-ANT-M ; !"Use/MT"
Еугенио:Еугенио NP-ANT-M ; !"Use/MT"
Еултангуров:Еултангуров NP-COG-OB ; ! ""
Еуразия:Еуразия NP-TOP ; ! ""
Еуровидение:Еуровидение NP-AL ; ! ""
Еуро:Еуро NP-AL ; ! "European Championship"
Еуроодақ:Еуроодақ NP-TOP ; ! ""
еуропа:еуропа NP-TOP ; ! ""
Еуропа:Еуропа NP-TOP ; ! "Europe"
Ефатеров:Ефатеров NP-COG-OB ; ! ""
Еф:Еф NP-ANT-M ; !"Use/MT"
Ефимов:Ефимов NP-COG-OB ; ! ""
Ефрем:Ефрем NP-ANT-M ; ! ""
Ефремов:Ефремов NP-COG-OB ; ! ""
Ефремовка:Ефремовка NP-TOP ; ! ""
Ешбай:Ешбай NP-ANT-M ; ! "Eshbay" (Old Turkic)
Ешекеев:Ешекеев NP-COG-OB ; ! ""
Ешжан:Ешжан NP-ANT-M ; ! "Eshjan" (Persian)
Ешкіөлмес:Ешкіөлмес NP-TOP ; ! ""
Ешкітау:Ешкітау NP-TOP ; ! ""
Ешмұхамбет:Ешмұхамбет NP-ANT-M ; ! "Eshmuxambet" 
Ешуа:Ешуа NP-ANT-M ; ! ""
Жабағалы:Жабағалы NP-TOP ; ! ""
Жабағы:Жабағы NP-ANT-M ; ! "Jabağı" (Old Turkic)
Жабаев:Жабаев NP-COG-OB ; ! ""
Жабай:Жабай NP-ANT-M ; ! "Jabay" (Old Turkic)
Жабай:Жабай NP-TOP ; ! ""
Жабайхан:Жабайхан NP-ANT-M ; ! "Jabayxan" (Old Turkic)
Жабал:Жабал NP-ANT-M ; ! "Jabal" (Arabic)
Жабырбай:Жабырбай NP-ANT-M ; ! "Jabırbay" (Kazakh)
Жабысақ:Жабысақ NP-TOP ; ! ""
Жағабұлақ:Жағабұлақ NP-TOP ; ! ""
Жағыпар:Жағыпар NP-ANT-M ; ! "Jağıpar" (Arabic)
Жағыпаров:Жағыпаров NP-COG-OB ; ! ""
Жадай:Жадай NP-ANT-M ; ! "Jaday" (Arabic)
Жадов:Жадов NP-COG-OB ; ! ""
Жадыра:Жадыра NP-ANT-F ; ! "Jadıra" (Kazakh)
Жазғұрлы:Жазғұрлы NP-TOP ; ! ""
Жазира:Жазира NP-ANT-F ; ! "Jazıyra" (Arabic)
Жазылбек:Жазылбек NP-ANT-M ; ! "Jazılbek" (Arabic)
Жайдақкер:Жайдақкер NP-ANT-M ; !"Use/MT"
Жайдар:Жайдар NP-ANT-M ; ! "Jaydar" (Kazakh)
Жайлаубай:Жайлаубай NP-ANT-M ; ! "Jaylawbay" (Kazakh)
Жайлау:Жайлау NP-ANT-M ; ! "Jaylaw" (Kazakh)
Жайнақ:Жайнақ NP-ANT-M ; ! "Jaynaq" (Kazakh)
Жайнақов:Жайнақов NP-COG-OB ; ! ""
Жайсанбек:Жайсанбек NP-ANT-M ; ! "Jaysanbek" 
Жайық:Жайық NP-ANT-M ; ! "Jayıq" (Kazakh)
Жайық:Жайық NP-TOP ; ! "Ural river"
Жайықмұнайгаз:Жайықмұнайгаз NP-ORG ; ! ""
Жак:Жак NP-ANT-M ; ! ""
Жаклин:Жаклин NP-ANT-F ; !"Use/MT"
Жақан:Жақан NP-ANT-M ; ! "Jaqan" (Persian)
Жақанов:Жақанов NP-COG-OB ; ! "" ! Use/MT
Жақия:Жақия NP-ANT-M ; ! "Jaqıya" (Arabic)
Жақсыбай:Жақсыбай NP-ANT-M ; ! "Jaqsıbay" (Kazakh)
Жақсыбек:Жақсыбек NP-ANT-M ; ! "" 
Жақсыбеков:Жақсыбеков NP-COG-OB ; ! ""
Жақсыбеков:Жақсыбеков NP-COG-OB ; ! "" ! Use/MT
Жақсыбектов:Жақсыбектов NP-COG-OB ; ! "" ! Use/MT
Жақсылық:Жақсылық NP-ANT-M ; ! "Jaqsılıq" (Kazakh)
Жақсылықов:Жақсылықов NP-COG-OB ; ! ""
Жақсымұрат:Жақсымұрат NP-ANT-M ; ! "Jaqsımurat" (Kazakh)
Жақсытұз:Жақсытұз NP-TOP ; ! ""
Жақыпбаев:Жақыпбаев NP-COG-OB ; ! ""
Жақыпбай:Жақыпбай NP-ANT-M ; ! "Jaqıpbay" (Kazakh)
Жақыпбек:Жақыпбек NP-ANT-M ; ! "Jaqıpbek" (Kazakh)
Жақып:Жақып NP-ANT-M ; ! "Jacob"
Жақып:Жақып NP-ANT-M ; ! "Jacob"
Жақып:Жақып NP-ANT-M ; ! "Jaqıp" (Kazakh)
Жақыпжан:Жақыпжан NP-ANT-M ; ! "Jaqıpjan" (Kazakh)
Жақыпқан:Жақыпқан NP-ANT-M ; ! "Jaqıpqan" (Kazakh)
Жақыпов:Жақыпов NP-COG-OB ; ! ""
Жалал%-Абад:Жалал%-Абад NP-TOP ; ! ""
Жалал:Жалал NP-ANT-M ; ! ""
Жалғасбай:Жалғасбай NP-ANT-M ; ! "Jalğasbay" (Kazakh)
Жалғасбек:Жалғасбек NP-ANT-M ; ! "Jalğasbek" (Kazakh)
Жалғас:Жалғас NP-ANT-M ; ! "Jalğas" (Kazakh)
Жалғастұр:Жалғастұр NP-ANT-M ; ! "Jalğastur" (Kazakh)
Жалтыр:Жалтыр NP-ANT-M ; ! "Jaltır" (Kazakh)
Жалын:Жалын NP-ANT-M ; ! "Jalın" (Kazakh)
Жамал:Жамал NP-ANT-F ; ! "Jamal" (Arabic)
Жамали:Жамали NP-ANT-F ; ! "Jamalıy" (Arabic)
Жаманмұрынов:Жаманмұрынов NP-COG-OB ; ! ""
Жаманов:Жаманов NP-COG-OB ; ! ""
Жамбыл:Жамбыл NP-ANT-M ; ! "Jambıl" (Old Turkic)
Жамбыл:Жамбыл NP-TOP ; ! ""
Жамиға:Жамиға NP-ANT-F ; ! "Jamıyğa" (Arabic)
Жана:Жана NP-ANT-F ; !"Use/MT"
Жанайдар:Жанайдар NP-ANT-M ; ! "Janaydar" (Arabic)
Жанайдаров:Жанайдаров NP-COG-OB ; ! ""
Жанай:Жанай NP-ANT-M ; ! "Janay" (Kazakh)
Жанақ:Жанақ NP-ANT-M ; ! "Janaq" (Kazakh)
Жанан:Жанан NP-ANT-F ; ! "Janan" (Persian)
Жанаргүл:Жанаргүл NP-ANT-F ; ! "Janargül" (Kazakh)
Жанар:Жанар NP-ANT-F ; ! "Janar" (Kazakh)
Жанар:Жанар NP-ANT-M ; ! "Janar" (Kazakh)
Жанарыстан:Жанарыстан NP-ANT-M ; ! "Janarıstan" (Kazakh)
Жанасаев:Жанасаев NP-COG-OB ; ! ""
Жанас:Жанас NP-ANT-M ; ! "Janas" (Kazakh)
Жанатбай:Жанатбай NP-ANT-M ; ! "Janatbay" (Kazakh)
Жанатбек:Жанатбек NP-ANT-M ; ! "Janatbek" (Kazakh)
Жанат:Жанат NP-ANT-F ; ! "Janat" (Kazakh)
Жанат:Жанат NP-ANT-M ; ! "Janat" (Kazakh)
Жанатқан:Жанатқан NP-ANT-M ; ! "Janatqan" (Kazakh)
Жанахмет:Жанахмет NP-ANT-M ; ! "Janaxmet" (Arabic)
Жанәбіл:Жанәбіл NP-ANT-M ; ! "Janäbil" (Arabic)
Жанәділ:Жанәділ NP-ANT-M ; ! "Janädil" (Arabic)
Жанбай:Жанбай NP-ANT-M ; ! "Janbay" (Kazakh)
Жанбек:Жанбек NP-ANT-M ; ! "Janbek" (Kazakh)
Жанболат:Жанболат NP-ANT-M ; ! "Janbolat" (Latin)
Жанбосын:Жанбосын NP-ANT-M ; ! "Janbosın" (Kazakh)
Жанбурчинов:Жанбурчинов NP-COG-OB ; ! ""
Жанбусунов:Жанбусунов NP-COG-OB ; ! "Use/MT"
Жанғабылов:Жанғабылов NP-COG-OB ; ! ""
Жанғозы:Жанғозы NP-ANT-M ; ! "Janğozı" (Kazakh)
Жандай:Жандай NP-ANT-M ; ! "Janday" (Persian)
Жандарбек:Жандарбек NP-ANT-M ; ! "Jandarbek" (Kazakh)
Жандарбеков:Жандарбеков NP-COG-OB ; ! ""
Жандар:Жандар NP-ANT-M ; ! "Jandar" (Kazakh)
Жандархан:Жандархан NP-ANT-M ; ! "Jandarxan" (Kazakh)
Жандәулетов:Жандәулетов NP-COG-OB ; ! ""
Жандос:Жандос NP-ANT-M ; ! "Jandos" (Persian)
Жандосов:Жандосов NP-COG-OB ; ! ""
Жанжігітов:Жанжігітов NP-COG-OB ; ! ""
Жанзақов:Жанзақов NP-COG-OB ; ! ""
Жанипа:Жанипа NP-ANT-F ; ! "Janıypa" (Persian)
Жания:Жания NP-ANT-F ; ! "Janıya" (Persian)
Жанкелді:Жанкелді NP-ANT-M ; ! "Jankeldi" (Kazakh)
Жанкеұлы:Жанкеұлы NP-COG-M ; ! "USE/MT"
Жанқабыл:Жанқабыл NP-ANT-M ; ! "Janqabıl" (Arabic)
Жанқали:Жанқали NP-ANT-M ; ! "Janqalıy" (Arabic)
Жанқозы:Жанқозы NP-ANT-M ; ! "Janqozı" (Kazakh)
Жанқуат:Жанқуат NP-ANT-M ; ! "Janqıwat" (Kazakh)
Жанқұлиев:Жанқұлиев NP-COG-OB ; ! ""
Жанқұлиев:Жанқұлиев NP-COG-OB ; ! "" ! Use/MT
Жанмырза:Жанмырза NP-ANT-M ; ! "Janmırza" (Arabic)
Жанна:Жанна NP-ANT-F ; ! "Janna" (Kazakh)
Жанназар:Жанназар NP-ANT-M ; ! "Jannazar,+" (Arabic)
Жаннат:Жаннат NP-ANT-F ; ! "Jannat" (Arabic)
Жаннет:Жаннет NP-ANT-F ; !"Use/MT"
Жаннұр:Жаннұр NP-ANT-M ; ! "Jannur" (Kazakh)
Жанораз:Жанораз NP-ANT-M ; ! "Janoraz" (Kazakh)
Жанпейіс:Жанпейіс NP-ANT-M ; ! "Janpeyis" (Arabic)
Жансал:Жансал NP-ANT-M ; ! "Jansal" (Kazakh)
Жансая:Жансая NP-ANT-F ; ! "Jansaya" 
Жансейіт:Жансейіт NP-ANT-M ; ! ""
Жансұлтан:Жансұлтан NP-ANT-M ; ! "Jansultan" (Arabic)
Жансүгіров:Жансүгіров NP-COG-OB ; ! "" ! Use/MT
Жанта:Жанта NP-COG-MF ; !"Use/MT"
Жантай:Жантай NP-ANT-M ; ! "Jantay" 
жантану:жантану N1 ; !"Use/MT"
Жантас:Жантас NP-ANT-M ; ! "Jantas" (Kazakh)
Жантемір:Жантемір NP-ANT-M ; ! "Jantemir" (Kazakh)
Жантуар:Жантуар NP-ANT-M ; ! "Jantıwar" (Kazakh)
Жантуған:Жантуған NP-ANT-M ; ! "Jantıwğan" (Kazakh)
Жантілеу:Жантілеу NP-ANT-M ; ! "Jantilew" (Kazakh)
Жанұалиев:Жанұалиев NP-COG-OB ; ! ""
Жанұзақ:Жанұзақ NP-ANT-M ; ! "Januzaq" (Kazakh)
Жанұзақов:Жанұзақов NP-COG-OB ; ! ""
Жанша:Жанша NP-ANT-M ; ! "Jansha" (Arabic)
Жаңаауыл:Жаңаауыл NP-TOP ; ! ""
Жаңабаев:Жаңабаев NP-COG-OB ; ! ""
Жаңабай:Жаңабай NP-ANT-M ; ! "Jaŋabay" (Kazakh)
Жаңабергенов:Жаңабергенов NP-COG-OB ; ! "" ! Use/MT
Жаңажол:Жаңажол NP-TOP ; ! ""
Жаңа% Зеландия:Жаңа% Зеландия NP-TOP ; !"Use/MT"
Жаңақала:Жаңақала NP-TOP ; ! ""
Жаңақорған:Жаңақорған NP-TOP ; ! ""
Жаңаөзен:Жаңаөзен NP-TOP ; ! ""
Жаңатас:Жаңатас NP-TOP ; ! ""
Жаңа% Чишмә:Жаңа% Чишмә NP-TOP ; ! ""
Жапақов:Жапақов NP-COG-OB ; ! ""
Жапан:Жапан NP-ANT-M ; ! "Japan" (Kazakh)
Жапар:Жапар NP-ANT-M ; ! "Japar" (Arabic)
Жапония:Жапония NP-TOP ; ! "Japan"
Жаппасбаев:Жаппасбаев NP-COG-OB ; ! ""
Жарасбай:Жарасбай NP-ANT-M ; ! ""
Жарас:Жарас NP-ANT-M ; ! "Jaras" (Kazakh)
Жарбол:Жарбол NP-ANT-M ; ! "Jarbol" (Kazakh)
Жаркеев:Жаркеев NP-COG-OB ; ! "" ! Use/MT
Жаркент:Жаркент NP-TOP ; !"Use/MT"
Жарқайың:Жарқайың NP-TOP ; ! ""
Жарқынбай:Жарқынбай NP-ANT-M ; ! "Jarqınbay" (Kazakh)
Жарқынбек:Жарқынбек NP-ANT-M ; ! "Jarqınbek" (Kazakh)
Жарқын:Жарқын NP-ANT-M ; ! "Jarqın" (Kazakh)
Жармағамбетов:Жармағамбетов NP-COG-OB ; ! ""
Жармағанбетов:Жармағанбетов NP-COG-OB ; ! ""
Жармахан:Жармахан NP-ANT-M ; ! ""
Жармұхамбет:Жармұхамбет NP-ANT-M ; ! "Jarmuxambet" (Arabic)
Жармұхамедов:Жармұхамедов NP-COG-OB ; ! ""
Жармұхамедов:Жармұхамедов NP-COG-OB ; ! ""
Жаров:Жаров NP-COG-OB ; ! ""
Жартыбаев:Жартыбаев NP-COG-OB ; ! ""
Жарықбаев:Жарықбаев NP-COG-OB ; ! ""
Жарықпаев:Жарықпаев NP-COG-OB ; ! ""
Жарылғапов:Жарылғапов NP-COG-OB ; ! ""
Жарылқасымұлы:Жарылқасымұлы NP-COG-M ; ! "Use/MT"
Жарылқасын:Жарылқасын NP-ANT-M ; ! "Jarılqasın" (Kazakh)
Жарьқ:Жарьқ NP-ANT-M ; ! "JarʲQ" (Kazakh)
Жасамыс:Жасамыс NP-ANT-M ; ! "Jasamıs" (Kazakh)
Жасарал:Жасарал NP-TOP ; ! ""
Жас:Жас NP-ANT-M ; ! "Jas" (Kazakh)
Жасқайрат:Жасқайрат NP-ANT-M ; ! "Jasqayrat" (Kazakh)
Жасмин:Жасмин NP-ANT-F ; !"Use/MT"
Жасталап:Жасталап NP-ANT-M ; ! "Jastalap" (Kazakh)
Жасұзақ:Жасұзақ NP-ANT-M ; ! "Jasuzaq" (Kazakh)
Жасұзақов:Жасұзақов NP-COG-OB ; ! ""
Жасұлан:Жасұлан NP-ANT-M ; ! "Jasulan" (Kazakh)
Жасыбай:Жасыбай NP-ANT-M ; ! "Jasıbay" (Kazakh)
Жасыл% Өзен:Жасыл% Өзен NP-TOP ; ! ""
Жауме:Жауме NP-ANT-M ; !"Use/MT"
Жауыртау:Жауыртау NP-TOP ; ! ""
Жафаров:Жафаров NP-COG-OB ; ! ""
Жахаев:Жахаев NP-COG-OB ; ! ""
Жәдігер:Жәдігер NP-ANT-M ; ! "Jädiger" (Persian)
Жәкiшев:Жәкiшев NP-COG-OB ; ! ""
Жәкішев:Жәкішев NP-COG-OB ; ! ""
Жәлел:Жәлел NP-ANT-M ; ! "Jälel" (Arabic)
Жәлид:Жәлид NP-ANT-M ; ! "Jäliyd" (Arabic)
Жәлила:Жәлила NP-ANT-F ; ! "Jäliyla" (Arabic)
Жәмеңке:Жәмеңке NP-ANT-M ; ! ""
Жәми:Жәми NP-COG-MF ; ! ""
Жәмила:Жәмила NP-ANT-F ; ! "Jämiyla" (Arabic)
Жәмилә:Жәмилә NP-ANT-F ; ! ""
Жәмитұлы:Жәмитұлы NP-COG-M ; ! ""
Жәмихан:Жәмихан NP-ANT-M ; ! "Jämiyxan" (Arabic)
Жәмкенов:Жәмкенов NP-COG-OB ; ! "" ! Use/MT
Жәмшид:Жәмшид NP-ANT-M ; ! "Jämshiyd" (Arabic)
Жәміл:Жәміл NP-ANT-M ; ! "Jämil" (Arabic)
Жәмішев:Жәмішев NP-COG-OB ; ! "" ! Use/MT
Жәміш:Жәміш NP-ANT-M ; ! "Jamish (Kazakh)"
Жәнекешев:Жәнекешев NP-COG-OB ; ! ""
Жәния:Жәния NP-ANT-F ; ! ""
Жәнібек:Жәнібек NP-ANT-M ; ! "Jänibek" (Kazakh)
Жәнібеков:Жәнібеков NP-COG-OB ; ! ""
Жәңгір:Жәңгір NP-ANT-M ; ! "Jäŋgir" (Persian)
Жәңгіров:Жәңгіров NP-COG-OB ; ! ""
Жәрдем:Жәрдем NP-ANT-M ; ! "Järdem" (Kazakh)
Жәсия:Жәсия NP-ANT-M ; ! ""
Ждан:Ждан NP-ANT-M ; ! "Jdan" (Russian)
Жеан:Жеан NP-ANT-M ; !"Use/MT"
Жебрейіл:Жебрейіл NP-AL ; ! ""
Жезқазған:Жезқазған NP-TOP ; ! ""
Жейлітбаев:Жейлітбаев NP-COG-OB ; ! ""
Жейхун:Жейхун NP-TOP ; ! ""
Жексенбай:Жексенбай NP-ANT-M ; ! "Jeksenbay" (Kazakh)
Жексенбеков:Жексенбеков NP-COG-OB ; ! ""
Жексенбі:Жексенбі NP-ANT-M ; ! "Jeksenbi" 
Жексен:Жексен NP-ANT-M ; ! ""
Жекшен:Жекшен NP-ANT-M ; ! "Jeksen"
Желтау:Желтау NP-TOP ; ! ""
Желтов:Желтов NP-COG-OB ; ! ""
Жемісбек:Жемісбек NP-ANT-M ; ! "Jemisbek" (Kazakh)
Женева:Женева NP-ANT-F ; !"Use/MT"
Женева:Женева NP-TOP ; ! ""
Женя:Женя NP-ANT-F ; ! ""
Женя:Женя NP-ANT-M ; ! ""
Жеңісбек:Жеңісбек NP-ANT-M ; ! "Jeŋisbek" (Kazakh)
Жеңісгүл:Жеңісгүл NP-ANT-F ; ! "Jeŋisgül" 
Жеңіс:Жеңіс NP-ANT-M ; ! "Jeŋis" (Kazakh)
Жерар:Жерар NP-ANT-M ; !"Use/MT"
Жеребятиев:Жеребятиев NP-COG-OB ; ! ""
Жермен:Жермен NP-ANT-M ; !"Use/MT"
Жерорта% теңізі:Жерорта% теңіз NP-TOP ; ! "" FIXME: similar to N-COMPUND-PX cont.class for proper nouns
Жерұйық:Жерұйық NP-TOP ; ! "" 
Жетiсу:Жетiсу NP-TOP ; ! ""
Жетенов:Жетенов NP-COG-OB ; ! "" ! Use/MT
Жетес:Жетес NP-ANT-M ; ! "Jetes" (Kazakh)
Жетіасар:Жетіасар NP-TOP ; ! ""
Жетібай:Жетібай NP-ANT-M ; ! "Jetibay" (Kazakh)
Жетіген:Жетіген NP-ANT-M ; ! "Jetigen" (Kazakh)
Жетік:Жетік NP-ANT-M ; ! "Jetik" (Kazakh)
Жетіқарақшы:Жетіқарақшы NP-AL ; !"Use/MT"
Жетіқарақшы:Жетіқарақшы NP-AL ; !"Use/MT"
Жетіқарақшы:Жетіқарақшы NP-AL ; !"Use/MT"
Жетіқарақшы:Жетіқарақшы NP-AL ; !"Use/MT"
Жетіқарақшы:Жетіқарақшы NP-AL ; !"Use/MT"
Жетіқарақшы:Жетіқарақшы NP-AL ; !"Use/MT"
Жетіқарақшы:Жетіқарақшы NP-AL ; !"Use/MT"
Жетіқарақшы:Жетіқарақшы NP-TOP ; ! "" 
Жетістік:Жетістік NP-ANT-M ; ! "Jetistik" (Kazakh)
Жетісу:Жетісу NP-TOP ; ! "" 
Жжёнов:Жжёнов NP-COG-OB ; ! ""
Живоев:Живоев NP-COG-OB ; ! ""
Жигалов:Жигалов NP-COG-OB ; ! "" ! Use/MT
Жигарев:Жигарев NP-COG-OB ; ! ""
Жидебай:Жидебай NP-ANT-M ; ! "Jiydebay"
Жиекбаев:Жиекбаев NP-COG-OB ; ! ""
Жиенбаев:Жиенбаев NP-COG-OB ; ! "" ! Use/MT
Жиенқұлов:Жиенқұлов NP-COG-OB ; ! ""
Жизақ:Жизақ NP-TOP ; ! ""
Жизақ:Жизақ NP-TOP ; ! ""
Жилль:Жилль NP-ANT-M ; !"Use/MT"
Жирен:Жирен NP-ANT-M ; ! "Jiyren" 
Жиренше:Жиренше NP-ANT-M ; ! "Jiyrenşe"
Жиренше:Жиренше NP-ANT-M ; ! "Jiyrenshe" (Kazakh)
Жирнов:Жирнов NP-COG-OB ; ! ""
Жиров:Жиров NP-COG-OB ; ! ""
Жирона:Жирона NP-TOP ; !"Use/MT"
Житков:Житков NP-COG-OB ; ! ""
Жиханша:Жиханша NP-ANT-M ; ! "Jıyxansha" (Arabic)
Жовтис:Жовтис NP-COG-MF ; ! ""
Жовтис:Жовтис%{☭%} NP-COG-MF ; ! ""
Жоғаб:Жоғаб NP-ANT-M ; ! ""
Жоғары% Ослан:Жоғары% Ослан NP-TOP ; ! ""
Жоғас:Жоғас NP-ANT-M ; ! ""
Жозефина:Жозефина NP-ANT-F ; !"Use/MT"
Жолай:Жолай NP-ANT-M ; ! "Jolay" (Kazakh)
Жолан:Жолан NP-ANT-M ; ! "Jolan" (Kazakh)
Жолбарыс:Жолбарыс NP-ANT-M ; ! "Jolbarıs" (Kazakh)
Жолдасәлі:Жолдасәлі NP-ANT-M ; ! "Joldasäli" (Kazakh)
Жолдасбай:Жолдасбай NP-ANT-M ; ! "Joldasbay" (Kazakh)
Жолдасбек:Жолдасбек NP-ANT-M ; ! "Joldasbek" (Kazakh)
Жолдасбеков:Жолдасбеков NP-COG-OB ; ! ""
Жолдас:Жолдас NP-ANT-M ; ! "Joldas" (Kazakh)
Жолдасқан:Жолдасқан NP-ANT-M ; ! "Joldasqan" (Kazakh)
Жолдыбай:Жолдыбай NP-ANT-M ; ! "Joldıbay" (Kazakh)
Жолтай:Жолтай NP-ANT-M ; ! "Joltay" (Kazakh)
Жомартбай:Жомартбай NP-ANT-M ; ! "Jomartbay" (Kazakh)
Жомартбек:Жомартбек NP-ANT-M ; ! "Jomartbek" (Kazakh)
Жомарт:Жомарт NP-ANT-M ; ! "Jomart" (Kazakh)
Жонатан:Жонатан NP-ANT-M ; ! ""
Жоңғар:Жоңғар NP-AL ; ! ""
Жоңғар:Жоңғар NP-TOP ; ! ""
Жоңғария:Жоңғария NP-TOP ; ! ""
Жорабаев:Жорабаев NP-COG-OB ; ! ""
Жорабай:Жорабай NP-ANT-M ; ! "Jorabay" (Arabic)
Жора:Жора NP-ANT-M ; ! "Jora" (Arabic)
Жорам:Жорам NP-ANT-M ; ! ""
Жорахан:Жорахан NP-ANT-M ; ! "Joraxan" (Arabic)
Жорж:Жорж NP-ANT-M ; !"Use/MT"
Жортар:Жортар NP-ANT-M ; ! ""
Жосапат:Жосапат NP-ANT-M ; ! ""
Жошыбаев:Жошыбаев NP-COG-OB ; ! "" ! Use/MT
Жоэль:Жоэль NP-ANT-F ; !"Use/MT"
ЖСДП:ЖСДП NP-ORG-ABBR ; ! "Жалпыұлттық социал демократиялық партия=Гомуммилли Социаль Демократик Партия=International Social Democratic Party"
Жуалы:Жуалы NP-TOP ; ! "" 
Жуан:Жуан NP-ANT-M ; ! ""
Жуков:Жуков NP-COG-OB ; ! ""
Журавлёв:Журавлёв NP-COG-OB ; ! ""
Жұбан:Жұбан NP-ANT-M ; ! "Juban" (Kazakh)
Жұбанов:Жұбанов NP-COG-OB ; ! ""
Жұбаныш:Жұбаныш NP-ANT-M ; ! "Jubanısh" (Kazakh)
Жұлдызай:Жұлдызай NP-ANT-F ; ! "Juldızay" (Kazakh)
Жұмабаев:Жұмабаев NP-COG-OB ; ! ""
Жұмабай:Жұмабай NP-ANT-M ; ! "Jumabay"
Жұмабай:Жұмабай NP-ANT-M ; ! "Jumabay" (Arabic)
Жұмабек:Жұмабек NP-ANT-M ; ! "Jumabek" (Arabic)
Жұмабеков:Жұмабеков NP-COG-OB ; ! ""
Жұмабике:Жұмабике NP-ANT-F ; ! "Jumabıyke" (Arabic)
Жұмагүл:Жұмагүл NP-ANT-M ; ! "Jumagül" (Arabic)
Жұмағазы:Жұмағазы NP-ANT-M ; ! "Jumağazı" (Arabic)
Жұмағалиев:Жұмағалиев NP-COG-OB ; ! ""
Жұмағұл:Жұмағұл NP-ANT-M ; ! ""
Жұмағұлов:Жұмағұлов NP-COG-OB ; ! "" ! Use/MT
Жұмаділ:Жұмаділ NP-ANT-M ; ! ""
Жұмаділов:Жұмаділов NP-COG-OB ; ! ""
Жұмаділов:Жұмаділов NP-COG-OB ; ! "" ! Use/MT
Жұмаев:Жұмаев NP-COG-OB ; ! ""
Жұмажан:Жұмажан NP-ANT-M ; ! "Jumajan" (Arabic)
Жұма:Жұма NP-ANT-M ; ! "Juma" (Arabic)
Жұмакелді:Жұмакелді NP-ANT-M ; ! "Jumakeldi" (Arabic)
Жұмақан:Жұмақан NP-ANT-M ; ! "Jumaqan" (Arabic)
Жұмалиев:Жұмалиев NP-COG-MF ; !"Use/MT"
Жұманбай:Жұманбай NP-ANT-M ; ! "Jumanbay" (Kazakh)
Жұманбеков:Жұманбеков NP-COG-OB ; ! "" ! Use/MT
Жұман:Жұман NP-ANT-M ; ! "Juman" (Arabic)
Жұманияз:Жұманияз NP-ANT-M ; ! "Jumanıyaz" (Arabic)
Жұманиязов:Жұманиязов NP-COG-OB ; ! "" ! Use/MT
Жұмасқалиев:Жұмасқалиев NP-COG-OB ; ! ""
Жұматай:Жұматай NP-ANT-M ; ! "Jumatay" (Arabic)
Жұмат:Жұмат NP-ANT-M ; ! "Jumat" (Arabic)
Жұмәділ:Жұмәділ NP-ANT-M ; ! "Jumädil" (Arabic)
Жұпар:Жұпар NP-ANT-F ; ! "Jupar" (Kazakh)
Жұрынов:Жұрынов NP-COG-OB ; ! "" ! Use/MT
Жүзбай:Жүзбай NP-ANT-M ; ! "Jüzbay" (Kazakh)
Жүнісәлі:Жүнісәлі NP-ANT-M ; ! "Jünisäli" (Ancient Hebrew)
Жүнісбай:Жүнісбай NP-ANT-M ; ! "Jünisbay" (Ancient Hebrew)
Жүнісбек:Жүнісбек NP-ANT-M ; ! "Jünisbek" (Ancient Hebrew)
Жүнісжан:Жүнісжан NP-ANT-M ; ! "Jünisjan" (Ancient Hebrew)
Жүніс:Жүніс NP-ANT-M ; ! "Jünis" (Ancient Hebrew)
Жүнісов:Жүнісов NP-COG-OB ; ! ""
Жүнісов:Жүнісов NP-COG-OB ; ! ""
Жүргенов:Жүргенов NP-COG-OB ; ! ""
Жүрсін:Жүрсін NP-ANT-M ; ! "Jürsin" (Kazakh)
Жүсіпақын:Жүсіпақын NP-ANT-M ; ! "Jüsipaqın" (Arabic)
Жүсіпбай:Жүсіпбай NP-ANT-M ; ! "Jüsipbay" (Arabic)
Жүсіпбек:Жүсіпбек NP-ANT-M ; ! "Jüsipbek" (Arabic)
Жүсіпжан:Жүсіпжан NP-ANT-M ; ! "Jüsipjan" (Arabic)
Жүсіп:Жүсіп NP-ANT-M ; ! "Jüsip" (Arabic)
Жүсіпқали:Жүсіпқали NP-ANT-M ; ! "Jüsipqalıy" (Arabic)
Жүсіпқан:Жүсіпқан NP-ANT-M ; ! "Jüsipqan" (Arabic)
Жүсіпқожа:Жүсіпқожа NP-ANT-M ; ! "Jüsipqoja" (Arabic)
Жүсіпов:Жүсіпов NP-COG-OB ; ! ""
Жыланшық:Жыланшық NP-TOP ; !"Use/MT"
Жылқышиев:Жылқышиев NP-COG-OB ; ! "" ! Use/MT
Жылыой:Жылыой NP-TOP ; ! ""
Жылысбаев:Жылысбаев NP-COG-OB ; ! ""
Жібек:Жібек NP-ANT-F ; ! "Jibek" (Kazakh)
Жігер:Жігер NP-ANT-M ; ! "Jiger" (Kazakh)
Жігітек:Жігітек NP-ANT-M ; ! ""
Жіткеев:Жіткеев NP-COG-OB ; ! "" ! Use/MT
Жюльен:Жюльен NP-ANT-M ; !"Use/MT"
Жюль:Жюль NP-ANT-M ; !"Use/MT"
Жюнесса:Жюнесса NP-ANT-F ; !"Use/MT"
Жюстин:Жюстин NP-ANT-F ; !"Use/MT"
Забалета:Забалета NP-COG-MF ; ! ""
Заб:Заб NP-ANT-M ; !"Use/MT" 
Забур:Забур NP-ANT-M ; ! ""
Завала:Завала NP-COG-M ; !"Use/MT"
Завьялов:Завьялов NP-COG-OB ; ! ""
Загаев:Загаев NP-COG-OB ; ! ""
Загора:Загора NP-TOP ; !"Use/MT"
Загреб:Загреб NP-TOP ; ! "" 
Загрос:Загрос NP-TOP ; ! ""
Зағила:Зағила NP-ANT-F ; ! "Zağıyla" (Arabic)
Зағипа:Зағипа NP-ANT-F ; ! "Zağıypa" (Arabic)
Задорнов:Задорнов NP-COG-OB ; ! ""
Заев:Заев NP-COG-OB ; ! ""
За:За NP-ANT-M ; !"Use/MT" 
Заир:Заир NP-TOP ; ! ""
Зайда:Зайда NP-ANT-F ; ! "Zayda" (Persian)
Зайкенов:Зайкенов NP-COG-OB ; ! ""
Зайра:Зайра NP-ANT-F ; ! "Zayra" (Arabic)
Зайроллаұлы:Зайроллаұлы NP-COG-M ; ! "USE/MT"
Зайсан:Зайсан NP-TOP ; ! ""
Зайт:Зайт NP-ANT-M ; ! "Zayt" (Arabic)
Зайтуна:Зайтуна NP-ANT-F ; ! "Zaytıwna" (Arabic)
Зайцев:Зайцев NP-COG-OB ; ! ""
Зайыр:Зайыр NP-ANT-M ; ! "Zayır" (Arabic)
Зак:Зак NP-ANT-M ; !"Use/MT" 
Закира:Закира NP-ANT-F ; ! "Zakıyra" (Arabic)
Закиров:Закиров NP-COG-OB ; ! ""
Зал:Зал NP-ANT-M ; !"Use/MT" 
Зальцбург:Зальцбург NP-TOP ; !"Use/MT"
Заманбек:Заманбек NP-ANT-M ; ! "Zamanbek" (Kazakh)
Замбези:Замбези NP-TOP ; ! ""
Замбия:Замбия NP-TOP ; ! ""
Заменгоф:Заменгоф NP-TOP ; !""
Замзегул:Замзегул NP-ANT-F ; ! "Zamzegıwl" (Arabic)
Замилия:Замилия NP-ANT-F ; ! "Zamıylıya" (Arabic)
Замира:Замира NP-ANT-F ; ! "Zamıyra" (Arabic)
Замир:Замир NP-ANT-M ; ! "Zamıyr" (Russian)
Занзибар:Занзибар NP-TOP ; ! ""
Заңғар:Заңғар NP-ANT-M ; ! "Zaŋğar" (Kazakh)
Зарема:Зарема NP-ANT-F ; ! "Zarema" (New word)
Зар:Зар NP-ANT-M ; !"Use/MT" 
Зарина:Зарина NP-ANT-F ; ! "Zarıyna" (Arabic)
Зарипа:Зарипа NP-ANT-F ; ! "Zarıypa" (Arabic)
Зариф:Зариф NP-ANT-M ; !"Use/MT"
Заря:Заря NP-AL ; ! ""
Зат:Зат NP-ANT-M ; !"Use/MT" 
Заф:Заф NP-ANT-M ; !"Use/MT" 
Захаров:Захаров NP-COG-OB ; ! ""
Захарченко:Захарченко NP-COG-MF ; ! ""
Зәй:Зәй NP-TOP ; ! ""
Зәкария:Зәкария NP-ANT-M ; ! "Zäkarıya" (Arabic)
Зәкәрия:Зәкәрия NP-ANT-M ; ! ""
Зәкиев:Зәкиев NP-COG-OB ; ! "" ! Use/MT
Зәки:Зәки NP-ANT-M ; ! "Zäkiy" (Arabic)
Зәкия:Зәкия NP-ANT-F ; ! "Zäkıya" (Arabic)
Зәкір:Зәкір NP-ANT-M ; ! "Zäkir" (Arabic)
Зәмзамия:Зәмзамия NP-ANT-F ; ! "Zämzamıya" (Arabic)
Зәңгіров:Зәңгіров NP-COG-OB ; ! ""
Зәрия:Зәрия NP-ANT-F ; ! "Zärıya" 
Зәріп:Зәріп NP-ANT-M ; ! "Zärip" (Arabic)
Зәуре:Зәуре NP-ANT-F ; ! "Zäwre" (Arabic)
Збигнев:Збигнев NP-ANT-M ; !"Use/MT"
Зверьков:Зверьков NP-COG-OB ; ! "" ! Use/MT
Звонилов:Звонилов NP-COG-OB ; ! ""
Здоровенко:Здоровенко NP-COG-MF ; ! ""
Зеб:Зеб NP-ANT-M ; !"Use/MT" 
Зевс:Зевс NP-AL ; ! ""
Зевс:Зевс NP-ANT-M ; !"Use/MT"
Зе:Зе NP-ANT-M ; !"Use/MT" 
Зейнегүл:Зейнегүл NP-ANT-F ; ! "Zeynegül" (Arabic)
Зейнеп:Зейнеп NP-ANT-F ; ! "Zeynep" (Arabic)
Зейнеш:Зейнеш NP-ANT-F ; ! "Zeynesh" (Kazakh)
Зейнұр:Зейнұр NP-ANT-M ; ! "Zeynur" (Arabic)
Зеландия:Зеландия NP-TOP ; ! ""
Зеленов:Зеленов NP-COG-OB ; ! ""
Зеленодольск:Зеленодольск NP-TOP ; ! ""
Зел:Зел NP-ANT-M ; !"Use/MT" 
Земан:Земан NP-ANT-M ; ! "Zemfira" (Persian)
Зем:Зем NP-ANT-M ; !"Use/MT" 
Земфира:Земфира NP-ANT-F ; ! "Zemfira" (Persian)
Зенд:Зенд NP-ANT-M ; !"Use/MT"
Зенков:Зенков NP-COG-OB ; ! ""
Зеньков:Зеньков NP-COG-OB ; ! ""
Зере:Зере NP-ANT-F ; ! "Zere" (Arabic)
Зерек:Зерек NP-ANT-M ; ! "Zerek" (Arabic)
Зеренді:Зеренді NP-TOP ; ! ""
Зер:Зер NP-ANT-M ; !"Use/MT" 
Зета:Зета NP-ANT-F ; !"Use/MT"
Зею:Зею NP-ANT-M ; !"Use/MT" 
Зибагүл:Зибагүл NP-ANT-F ; ! "Zıybagül" (Arabic)
Зиба:Зиба NP-ANT-F ; ! "Zıyba" (Arabic)
Зиб:Зиб NP-ANT-M ; !"Use/MT" 
Зигмунд:Зигмунд NP-ANT-M ; !"Use/MT"
Зид:Зид NP-ANT-M ; !"Use/MT" 
Зик:Зик NP-ANT-M ; !"Use/MT" 
Зила:Зила NP-ANT-F ; ! "Zıyla" (Arabic)
Зил:Зил NP-ANT-M ; !"Use/MT" 
Зиманов:Зиманов NP-COG-OB ; ! ""
Зимбабве:Зимбабве NP-TOP ; ! ""
Зим:Зим NP-ANT-M ; !"Use/MT" 
Зимин:Зимин NP-COG-IN ; ! ""
Зинаида:Зинаида NP-ANT-F ; ! "Zıynaida" (Greek)
Зинат:Зинат NP-ANT-F ; ! "Zıynat" (Arabic)
Зинира:Зинира NP-ANT-F ; ! "Zıynıyra" (Arabic)
Зиния:Зиния NP-ANT-F ; ! "Ziynıya" (Arabic)
Зиновьев:Зиновьев NP-COG-OB ; ! ""
Зинченко:Зинченко NP-COG-MF ; ! ""
Зита:Зита NP-ANT-F ; !"Use/MT"
Зиф:Зиф NP-ANT-M ; !"Use/MT" 
Зифридович:Зифридо NP-PAT-VICH ; ! "" ! Use/MT 
Зих:Зих NP-ANT-M ; !"Use/MT" 
Зиягүл:Зиягүл NP-ANT-F ; ! "Zıyagül" (Arabic)
Зия:Зия NP-ANT-F ; ! "Zıya" (Arabic)
Зиятбек:Зиятбек NP-ANT-M ; ! "Zıyatbek" (Kazakh)
Злиха:Злиха NP-ANT-F ; ! "Zlıyxa" (Arabic)
Зов:Зов NP-COG-OB ; ! ""
Зо:Зо NP-ANT-M ; !"Use/MT" 
Зои:Зои NP-ANT-F ; !"Use/MT"
Золотницкий:Золотницкий NP-COG-OB ; ! ""
Золотов:Золотов NP-COG-OB ; ! ""
Золото:Золото NP-AL ; ! ""
Золотурн:Золотурн NP-TOP ; !"Use/MT"
Зор:Зор NP-ANT-M ; !"Use/MT" 
Зотиков:Зотиков NP-COG-OB ; ! ""
Зотов:Зотов NP-COG-OB ; ! ""
Зощенко:Зощенко NP-COG-MF ; ! ""
Зоя:Зоя NP-ANT-F ; ! "Zoya" (Greek)
Зубайда:Зубайда NP-ANT-F ; ! "Zıwbayda" (Arabic)
Зубайра:Зубайра NP-ANT-F ; ! "Zıwbayra" (Arabic)
Зубков:Зубков NP-COG-OB ; ! ""
Зуев:Зуев NP-COG-OB ; ! ""
Зу:Зу NP-ANT-M ; !"Use/MT" 
Зулькарнаев:Зулькарнаев NP-COG-OB ; ! ""
Зумрат:Зумрат NP-ANT-F ; ! "Zıwmrat" (Persian)
Зұлқарнай:Зұлқарнай NP-ANT-M ; ! "Zulqarnay" (Arabic)
Зұлпықар:Зұлпықар NP-ANT-M ; ! "Zulpıqar" (Arabic)
Зүлфия:Зүлфия NP-ANT-F ; ! "Zülfiya" (Arabic)
Зыков:Зыков NP-COG-OB ; ! ""
Зындәуітов:Зындәуітов NP-COG-OB ; ! ""
Зырянов:Зырянов NP-COG-OB ; ! ""
Зікірия:Зікірия NP-ANT-M ; ! "Zikirıya" (Arabic)
Зюганов:Зюганов NP-COG-OB ; ! ""
Иаков:Иаков NP-ANT-M ; !"Ue/MT"
Ибагүл:Ибагүл NP-ANT-F ; ! "İybagül" (Arabic)
Ибажан:Ибажан NP-ANT-F ; ! "İybajan" (Arabic)
Иба:Иба NP-ANT-F ; ! "İyba" (Arabic)
Ибар:Ибар NP-ANT-M ; !"Use/MT"
Ибат:Ибат NP-ANT-M ; ! "İybat" (Arabic)
Ибер:Ибер NP-ANT-M ; !"Use/MT"
Иберия:Иберия NP-TOP ; ! ""
Иберо:Иберо NP-ANT-M ; !"Use/MT"
Иб:Иб NP-ANT-M ; !"Use/MT" 
Ибн:Ибн NP-ANT-M ; ! ""
Ибн% Руста:Ибн% Руста NP-COG-MF ; ! ""
Ибн% Хордадбех:Ибн% Хордадбех NP-COG-MF ; ! ""
Ибрагим:Ибрагим NP-ANT-M ; ! "İybragıym" (Arabic)
Ибрагимов:Ибрагимов NP-COG-OB ; ! ""
Ибрагимов:Ибрагимов NP-COG-OB ; ! ""
Ибраев:Ибраев NP-COG-OB ; ! ""
Ибрахим:Ибрахим NP-ANT-M ; ! ""
Ибраһим:Ибраһим NP-ANT-M ; ! ""
Ибрашев:Ибрашев NP-COG-OB ; ! ""
Ибсен:Ибсен NP-COG-MF ; ! ""
Ибятов:Ибятов NP-COG-OB ; ! ""
Ивана:Ивана NP-ANT-F ; !"Use/MT"
Иван:Иван NP-ANT-M ; ! ""
Иванка:Иванка NP-ANT-F ; !"Use/MT"
Иванко:Иванко NP-COG-MF ; ! ""
Иванов:Иванов NP-COG-OB ; ! ""
Иванович:Ивано NP-PAT-VICH ; ! ""
Иваново:Иваново NP-TOP ; ! ""
Ивановский:Ивановский NP-COG-OB ; ! ""
Иванченков:Иванченков NP-COG-OB ; ! "Use/MT"
Иванченко:Иванченко NP-COG-MF ; ! ""
Ивашев:Ивашев NP-COG-OB ; ! ""
Иващенко:Иващенко NP-COG-MF ; ! ""
Ив:Ив NP-ANT-M ; !"Use/MT"
Иво:Иво NP-ANT-M ; !"Use/MT"
Ивонн:Ивонн NP-ANT-F ; !"Use/MT"
Ивченко:Ивченко NP-COG-MF ; ! ""
Игенбай:Игенбай NP-ANT-M ; ! "İygenbay" (Kazakh)
Иг:Иг NP-ANT-M ; !"Use/MT" 
Игнасио:Игнасио NP-ANT-M ; !"Use/MT"
Игнатов:Игнатов NP-COG-OB ; ! ""
Игнатьев:Игнатьев NP-COG-OB ; ! ""
Игор:Игор NP-ANT-M ; !"Use/MT"
Игорь:Игорь NP-ANT-M ; ! ""
Игратьев:Игратьев NP-COG-OB ; ! ""
Игрнатьев:Игрнатьев NP-COG-OB ; ! ""
Игілік:Игілік NP-ANT-M ; ! "İygilik" (Kazakh)
Игісін:Игісін NP-ANT-M ; ! "İygisin" (Kazakh)
Ида:Ида NP-ANT-F ; !"Use/MT"
Идаят:Идаят NP-ANT-M ; ! "İydayat" 
Ид:Ид NP-ANT-M ; !"Use/MT" 
Иегова:Иегова NP-ANT-M ; !"Use/MT"
Иез:Иез NP-ANT-M ; !"Use/MT" 
Ие:Ие NP-ANT-M ; !"Use/MT" 
Иер:Иер NP-ANT-M ; !"Use/MT" 
Иерусалим:Иерусалим%{☭%} NP-TOP ; ! "Jerusalem"
Изабель:Изабель NP-ANT-F ; !"Use/MT"
Изатбек:Изатбек NP-ANT-M ; ! "İyzatbek" (Arabic)
Изатов:Изатов NP-COG-OB ; ! ""
Изендеев:Изендеев NP-COG-OB ; ! ""
Из:Из NP-ANT-M ; !"Use/MT" 
Изотов:Изотов NP-COG-OB ; ! ""
Израил:Израил%{☭%} NP-ANT-M ; ! ""
Израиль:Израиль NP-TOP ; ! "Israel"
Иисус:Иисус NP-ANT-M ; !"Use/MT"
Икар:Икар NP-ORG ; !"Use/MT"
Ик:Ик NP-ANT-M ; !"Use/MT" 
Икрама:Икрама NP-ANT-F ; ! "İykrama" (Persian)
Икрам:Икрам NP-ANT-M ; ! "İykram" (Arabic)
Илан:Илан NP-ANT-M ; !"Use/MT"
Илеана:Илеана NP-ANT-F ; !"Use/MT"
Илиана:Илиана NP-ANT-F ; !"Use/MT"
Ил:Ил NP-AL ; ! ""
Илларионов:Илларионов NP-COG-OB ; ! ""
Иллинойс:Иллинойс NP-TOP ; !"Use/MT"
Илмар:Илмар NP-ANT-M ; ! "İylmar" (Arabic)
Илхам:Илхам NP-ANT-M ; ! ""
Ильина:Ильина NP-ANT-F ; ! ""
Ильин:Ильин NP-COG-IN ; ! ""
Ильхандар:ильхандар NP-TOP ; !"" !Use/MT
Ильющенко:Ильющенко NP-COG-MF ; ! ""
Илья:Илья NP-ANT-F ; !"Use/MT"
Илья:Илья NP-ANT-M ; ! ""
Иманақыш:Иманақыш NP-ANT-M ; ! "İymanaqısh" (Arabic)
Иманәлиев:Иманәлиев NP-COG-OB ; ! "" ! Use/MT
Иманәлі:Иманәлі NP-ANT-M ; ! "Iymanäli" (Arabic)
Иманбай:Иманбай NP-ANT-M ; ! "İymanbay" (Arabic)
Иманбек:Иманбек NP-ANT-M ; ! "İymanbek" (Arabic)
Имангелді:Имангелді NP-ANT-M ; ! "İymangeldi" (Arabic)
Иманғазиев:Иманғазиев NP-COG-OB ; ! ""
Иманғазы:Иманғазы NP-ANT-M ; ! "İymanğazı" (Arabic)
Иманғалиев:Иманғалиев NP-COG-OB ; ! "" ! Use/MT
Иманғали:Иманғали NP-ANT-M ; ! "İymanğalıy" (Arabic)
Иманжан:Иманжан NP-ANT-M ; ! "İymanjan" (Arabic)
Иман:Иман NP-ANT-M ; ! "İyman" (Arabic)
Иманқұл:Иманқұл NP-ANT-M ; ! "İymanqul" (Arabic)
Иманов:Иманов NP-COG-OB ; ! ""
Имашев:Имашев NP-COG-OB ; ! ""
Именбай:Именбай NP-ANT-M ; ! "Imenbay"
Именнов:Именнов NP-COG-OB ; ! ""
Им:Им NP-ANT-M ; !"Use/MT" 
Иммануил:Иммануил NP-ANT-M ; ! ""
Инаят:Инаят NP-ANT-M ; ! "İynayat" 
Инга:Инга NP-ANT-F ; ! "İynga" (Skandinavian)
Ингушетия:Ингушетия NP-TOP ; ! ""
Индиана:Индиана NP-ANT-F ; !"Use/MT"
Индиана:Индиана NP-TOP ; ! ""
Индира:Индира NP-ANT-F ; ! "İyndıyra" 
Индия:Индия NP-ANT-F ; !"Use/MT"
Индия:Индия NP-TOP ; ! "India" FIXME:NOTE: LR in bidix
Индонезия:Индонезия NP-TOP ; ! "Indonesia"
Индырчев:Индырчев NP-COG-OB ; ! ""
Инес:Инес NP-ANT-F ; !"Use/MT"
Инна:Инна NP-ANT-F ; !"Use/MT"
Иноходов:Иноходов NP-COG-OB ; ! ""
Инрике:Инрике NP-ANT-M ; !"Use/MT"
Интеринвестбанк:Интеринвестбанк NP-TOP ; ! "" ! Use/MT
Интернационал:Интернационал NP-AL ; ! ""
Интерфакс:Интерфакс NP-ORG ; ! ""
Иоан:Иоан NP-ANT-M  ; !"Use/MT"
Иовлев:Иовлев NP-COG-OB ; ! ""
Иоганн:Иоганн NP-ANT-M ; ! ""
Иордан:Иордан NP-TOP ; ! "Jordan" FIXME:NOTE: LR in bidix
Иордания:Иордания NP-TOP ; ! "Jordan"
Иосиф:Иосиф NP-ANT-M ; ! ""
Иохим:Иохим NP-ANT-M ; ! "Joachim (German name)"
Ирада:Ирада NP-ANT-F ; ! "İyrada" (Arabic)
Ираида:Ираида NP-ANT-F ; ! "İyraida" 
Ирақ:Ирак%{ъ%} NP-TOP ; ! "Iraq" ! Dir/LR
Ирақ:Ирақ NP-TOP ; ! "Iraq"
Иран:Иран NP-TOP ; ! "Iran"
Ирена:Ирена NP-ANT-F ; !"Use/MT"
Ирен:Ирен NP-ANT-F ; !"Use/MT"
Ирина:Ирина NP-ANT-F ; ! "İyrıyna" 
Ир:Ир NP-ANT-M ; !"Use/MT" 
Ирисметов:Ирисметов NP-COG-OB ; ! ""
Иритков:Иритков NP-COG-OB ; ! ""
Иркутск:Иркутск NP-TOP ; ! ""
Ирландия:Ирландия NP-TOP ; ! "Ireland"
Ирма:Ирма NP-ANT-F ; !"Use/MT"
Ирод:Ирод NP-ANT-M ; !"Use/MT"
Иртышъ:Иртышъ NP-TOP ; ! ""
Исаак:Исаак NP-ANT-M ; ! ""
Исабай:Исабай NP-ANT-M ; ! "İysabay" (Arabic)
Исабек:Исабек NP-ANT-M ; ! "İysabek" (Arabic)
Исағали:Исағали NP-ANT-M ; ! "İysağalıy" (Arabic)
Исаділ:Исаділ NP-ANT-M ; ! "İysadil" (Arabic)
Исаев:Исаев NP-COG-OB ; ! ""
Исажан:Исажан NP-ANT-M ; ! "İysajan" (Arabic)
Иса:Иса NP-ANT-M ; ! "İysa" (Arabic)
Иса:Иса NP-ANT-M ; ! "Jesus"
Исаков:Исаков NP-COG-OB ; ! ""
Исақан:Исақан NP-ANT-M ; ! "İysaqan" (Arabic)
Исалиев:Исалиев NP-COG-OB ; ! ""
Исалы:Исалы NP-ANT-M ; ! "İysalı" (Arabic)
Исатай:Исатай NP-ANT-M ; ! "İysatay" (Arabic)
Исаханов:Исаханов NP-COG-OB ; ! ""
Исахмет:Исахмет NP-ANT-M ; ! "İysaxmet" (Arabic)
Исекешев:Исекешев NP-COG-OB ; ! "" ! Use/MT
Исидора:Исидора NP-ANT-F ; !"Use/MT"
Исис:Исис NP-ANT-F ; !"Use/MT"
Искаков:Искаков NP-COG-OB ; ! ""
Искандеров:Искандеров NP-COG-OB ; ! ""
Искеев:Искеев NP-COG-OB ; ! ""
Искендиров:Искендиров NP-COG-OB ; ! ""
Исламабад:Исламабад NP-TOP ; !"Use/MT"
Исландия:Исландия NP-TOP ; ! "Iceland"
Исмаил:Исмаил NP-ANT-M ; ! ""
Исмаэль:Исмаэль NP-ANT-M ; !"Use/MT"
Исметов:Исметов NP-COG-OB ; ! ""
Испания:Испания NP-TOP ; ! "Spain"
Исраил:Исраил%{☭%} NP-ANT-M ; ! "Israel (as in Jacob)"
Иствуд:Иствуд NP-COG-MF ; !"Use/MT"
Исфаһан:Исфаһан NP-TOP ; ! ""
Ита:Ита NP-ANT-F ; !"Use/MT"
Италиа:Италиа NP-ANT-F ; !"Use/MT"
Италия:Италия NP-TOP ; ! "Italy"
Итало:Итало NP-ANT-M ; !"Use/MT"
Итенов:Итенов NP-COG-OB ; ! ""
Ит:Ит NP-ANT-M ; !"Use/MT" 
Иуда:Иуда NP-ANT-M ; !"Use/MT"
Иу:Иу NP-ANT-M ; !"Use/MT" 
Ифигения:Ифигения NP-ANT-F ; !"Use/MT"
Иф:Иф NP-ANT-M ; !"Use/MT" 
Их:Их NP-ANT-M ; !"Use/MT" 
Ихсаноглу:Ихсаноглу NP-COG-MF ; !"Use/MT"
Ишая:Ишая NP-ANT-M ; ! ""
Иш:Иш NP-ANT-M ; !"Use/MT" 
Ишутов:Ишутов NP-COG-OB ; ! ""
Ищанов:Ищанов NP-COG-OB ; ! ""
Йемен:Йемен NP-TOP ; ! "Yemen"
Йоав:Йоав NP-ANT-M ; !"Use/MT"
Йован:Йован NP-ANT-M ; !"Use/MT"
Йонас:Йонас NP-ANT-M ; !"Use/MT"
Йорк:Йорк NP-TOP ; ! ""
Йоркшир:Йоркшир NP-TOP ; ! ""
Йосипович:Йосипо NP-PAT-VICH ; ! "" ! Use/MT 
Йот:Йот NP-ANT-M ; !"Use/MT" 
Йохан:Йохан NP-ANT-M ; !"Use/MT"
Йоханна:Йоханна NP-ANT-F ; !"Use/MT"
Йоханнесбург:Йоханнесбург NP-TOP ; !"Use/MT"
Йоханнес:Йоханнес NP-ANT-M ; !"Use/MT"
Йоханссон:Йоханссон NP-COG-MF ; !"Use/MT"
Кабанов:Кабанов NP-COG-OB ; ! ""
Кабарда%-Балкария:Кабарда%-Балкария NP-TOP ; ! ""
Кабира:Кабира NP-ANT-F ; ! "Kabıyra" (Arabic)
Кабир:Кабир NP-ANT-M ; ! "Kabıyr" (Arabic)
Кабиров:Кабиров NP-COG-OB ; ! "" ! Use/MT
Каб:Каб NP-ANT-M ; !"Use/MT"
Кабо%-Верде:Кабо%-Верде NP-TOP ; ! ""
Кабрера:Кабрера NP-COG-MF ; !"Use/MT"
Кабул:Кабул NP-TOP ; ! "" 
Кабулов:Кабулов NP-COG-OB ; ! ""
Кавказ:Кавказ NP-TOP ; ! ""
Кавказ% маң:Кавказ% маң N-COMPOUND-PX ; ! ""
Кагул:Кагул NP-TOP ; ! ""
Каддафи:Каддафи NP-ANT-M ; !"Use/MT"
Каджар:Каджар NP-ANT-M ; ! ""
Кад:Кад NP-ANT-M ; !"Use/MT"
Кадыков:Кадыков NP-COG-OB ; ! ""
Кадышев:Кадышев NP-COG-OB ; ! ""
Казаков:Казаков NP-COG-OB ; ! ""
Казамиас:Казамиас NP-COG-MF ; !"Use/MT"
Казанова:Казанова NP-COG-MF ; !"Use/MT"
Казанов:Казанов NP-COG-OB ; ! ""
Казанцев:Казанцев NP-COG-OB ; ! ""
Казначеев:Казначеев NP-COG-OB ; ! ""
Казталов:Казталов NP-COG-OB ; ! ""
Каир:Каир NP-TOP ; ! "Cairo"
Кай:Кай NP-ANT-M ; !"Use/MT"
Кайнозой:кайнозой NP-AL ; ! ""
Ка:Ка NP-ANT-M ; !"Use/MT" 
Калаубай:Калаубай NP-ANT-M ; ! "Kalawbay" (Kazakh)
Калаужан:Калаужан NP-ANT-M ; ! "Kalawjan" (Kazakh)
Калашников:Калашников NP-COG-OB ; ! ""
Калгари:Калгари NP-TOP ; !"Use/MT"
Кале:Кале NP-ANT-M ; !"Use/MT"
Калимов:Калимов NP-COG-OB ; ! ""
Калининград:Калининград NP-TOP ; ! ""
Калинин:Калинин NP-COG-IN ; ! ""
Калифорния:Калифорния NP-TOP ; ! ""
Калкабаев:Калкабаев NP-COG-OB ; ! ""
Кал:Кал NP-ANT-M ; !"Use/MT"
Каллас:Каллас NP-COG-MF ; !"Use/MT"
Каллен:Каллен NP-ANT-M ; !"Use/MT"
Калмыков:Калмыков NP-COG-OB ; ! ""
Кальвин:Кальвин NP-COG-MF ; ! ""
Кальдерон:Кальдерон NP-COG-MF ; !"Use/MT"
Калькутта:Калькутта NP-TOP ; ! ""
Кама:Кама NP-TOP ; ! ""
Камалиев:Камалиев NP-COG-OB ; ! ""
Камал:Камал NP-ANT-M ; ! "Kamal" (Arabic)
Камалов:Камалов NP-COG-OB ; ! ""
Камбоджа:Камбоджа NP-TOP ; ! ""
Камелия:Камелия NP-ANT-F ; !"Use/MT"
Каменщиков:Каменщиков NP-COG-OB ; ! ""
Камерун:Камерун NP-TOP ; ! ""
Камиль:Камиль NP-ANT-F ; !"Use/MT"
Кампала:Кампала NP-TOP ; ! "" 
Камчатка:Камчатка NP-TOP ; ! ""
Канада:Канада NP-TOP ; ! "Canada"
Канберра:Канберра NP-TOP ; !"Use/MT"
Кандаур:Кандаур NP-TOP ; !""
Канзас:Канзас NP-TOP ; ! "Kansas"
Каннингем:Каннингем NP-COG-MF ; !"Use/MT"
Кант:Кант NP-COG-MF ; ! ""
Капа%-Верде:Капа%-Верде NP-TOP ; ! ""
Капитонов:Капитонов NP-COG-OB ; ! ""
Капсулов:Капсулов NP-COG-OB ; ! ""
Капустин:Капустин NP-COG-IN ; ! ""
Карабаев:Карабаев NP-COG-OB ; ! "" ! Use/MT
Карабасов:Карабасов NP-COG-OB ; ! "" ! Use/MT
Карабах:Карабах NP-TOP ; ! ""
Каракалпак% АССР:Каракалпак% АССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Кара:Кара NP-ANT-F ; !"Use/MT"
Каракас:Каракас NP-TOP ; ! "" 
Карамзин:Карамзин NP-COG-M ; ! ""
Карасаев:Карасаев NP-COG-OB ; ! ""
Каратаев:Каратаев NP-COG-OB ; ! ""
Карачарсков:Карачарсков NP-COG-OB ; ! ""
Карбышев:Карбышев NP-COG-OB ; ! ""
Карев:Карев NP-COG-OB ; ! ""
Карелия:Карелия NP-TOP ; ! ""
Карел:Карел NP-ANT-M ; !"Use/MT"
Каренина:Каренина NP-ANT-F ; !"Use/MT"
Каренин:Каренин NP-COG-OB ; ! ""
Карен:Карен NP-ANT-F ; !"Use/MT"
Карзай:Карзай NP-COG-MF ; ! ""
Кариб:Кариб NP-TOP ; ! ""
Карим:Карим NP-ANT-M ; !"Use/MT"
Каримов:Каримов NP-COG-OB ; ! ""
Каримов:Каримов NP-COG-OB ; ! ""
Карина:Карина NP-ANT-F ; ! "Karıyna" (Arabic)
Карин:Карин NP-ANT-F ; !"Use/MT"
Кар:Кар NP-ANT-M ; !"Use/MT"
Карла:Карла NP-ANT-F ; !"Use/MT"
Карл:Карл NP-ANT-M ; ! ""
Карл:Карл NP-ANT-M ; ! ""
Карлов:Карлов NP-COG-OB ; ! ""
Карло:Карло NP-ANT-M ; !"Use/MT"
Карлос:Карлос NP-ANT-M ; ! ""
Карлсруэ:Карлсруэ NP-TOP ; !"Use/MT"
Карлтон:Карлтон NP-ANT-M ; !"Use/MT"
Карме:Карме NP-ANT-F ; !"Use/MT"
Кармела:Кармела NP-ANT-F ; !"Use/MT"
Кармен:Кармен NP-ANT-F ; !"Use/MT"
Карнеги:Карнеги NP-COG-MF ; !"Use/MT"
Каролина:Каролина NP-ANT-F ; !"Use/MT"
Каролина:Каролина NP-TOP ; ! ""
Каролин:Каролин NP-ANT-F ; !"Use/MT"
Каролла:Каролла NP-COG-MF ; !"Use/MT"
Карпеев:Карпеев NP-COG-OB ; ! ""
Карпенко:Карпенко NP-COG-MF ; ! ""
Карпович:Карпо NP-PAT-VICH ; ! "" ! Use/MT 
Карпов:Карпов NP-COG-OB ; ! ""
Карпов:Карпов NP-COG-OB ; ! "" ! Use/MT
Картер:Картер NP-ANT-M ; !"Use/MT"
Картинтия:Картинтия NP-TOP ; !"Use/MT"
Карычев:Карычев NP-COG-OB ; ! ""
Касабланка:Касабланка NP-TOP ; !"Use/MT"
Касимов:Касимов NP-COG-OB ; ! ""
Каспаров:Каспаров NP-COG-OB ; ! ""
Каспий:Каспий NP-TOP ; ! "Caspian"
Кастро:Кастро NP-COG-MF ; ! ""
Касымжан:Касымжан NP-ANT-M ; ! "Kasımjan" (Arabic)
Катаев:Катаев NP-COG-OB ; ! ""
Каталина:Каталина NP-ANT-F ; !"Use/MT"
Каталония:Каталония NP-TOP ; !"Use/MT"
Катанов:Катанов NP-COG-OB ; ! ""
Катарина:Катарина NP-ANT-F ; !"Use/MT"
Катар:Катар NP-TOP ; ! ""
Катерина:Катерина NP-ANT-F ; ! "Katerıyna" 
Катков:Катков NP-COG-OB ; ! ""
Катманду:Катманду NP-TOP ; ! "" 
Катранов:Катранов NP-COG-OB ; ! ""
Катрина:Катрина NP-ANT-F ; !"Use/MT"
Катя:Катя NP-ANT-F ; ! ""
Кау:Кау NP-ANT-M ; !"Use/MT"
Каунас:Каунас NP-TOP ; ! ""
Кафаров:Кафаров NP-COG-OB ; ! ""
КАФ:КАФ NP-AL-ABBR ; ! ""
Каф:Каф NP-ANT-M ; !"Use/MT"
Качиньский:Качиньский NP-COG-M ; !"Use/MT"
Кашмир:Кашмир NP-TOP ; ! ""
Каштелу:Каштелу NP-TOP ; !"Use/MT"
Каюров:Каюров NP-COG-OB ; ! ""
Кая:Кая NP-ANT-F ; ! ""
Кәдірбай:Кәдірбай NP-ANT-M ; ! "Kädirbay" (Arabic)
Кәдірбек:Кәдірбек NP-ANT-M ; ! "Kädirbek" (Kazakh)
Кәдіржан:Кәдіржан NP-ANT-M ; ! "Kädirjan" (Persian)
Кәдір:Кәдір NP-ANT-M ; ! "Kädir" (Arabic)
Кәкімбек:Кәкімбек NP-ANT-M ; ! "Käkimbek" (Kazakh)
Кәкімжан:Кәкімжан NP-ANT-M ; ! "Käkimjan" (Arabic)
Кәлетаев:Кәлетаев NP-COG-OB ; ! "" ! Use/MT
Кәмалия:Кәмалия NP-ANT-F ; ! "Kämalıya" (Arabic)
Кәміл:Кәміл NP-ANT-M ; ! "Kämil" (Arabic)
Кәрима:Кәрима NP-ANT-F ; ! "Käriyma" (Arabic)
Кәрменов:Кәрменов NP-COG-OB ; ! ""
Кәрібаев:Кәрібаев NP-COG-OB ; ! ""
Кәрібжанов:Кәрібжанов NP-COG-OB ; ! "" ! Use/MT
Кәрімбаев:Кәрімбаев NP-COG-OB ; ! ""
Кәрімбаев:Кәрімбаев NP-COG-OB ; ! ""
Кәрімбаев:Кәрімбаев NP-COG-OB ; ! ""
Кәрімбай:Кәрімбай NP-ANT-M ; ! "Kärimbay" (Arabic)
Кәрімбек:Кәрімбек NP-ANT-M ; ! "Kärimbek" (Arabic)
Кәрімжан:Кәрімжан NP-ANT-M ; ! "Kärimjan" (Arabic)
Кәрім:Кәрім NP-ANT-M ; ! "Kärim" (Arabic)
Кәрімқұл:Кәрімқұл NP-ANT-M ; ! "Kärimqul" (Arabic)
Кәрімов:Кәрімов NP-COG-OB ; ! "" ! Use/MT
Кәріпжан:Кәріпжан NP-ANT-M ; ! "Käripjan" (Arabic)
Квар:Квар NP-ANT-M ; !"Use/MT"
Квасьневский:Квасьневский NP-COG-MF ; ! "" ! Use/MT 
Квентин:Квентин NP-ANT-M ; !"Use/MT"
Квинсленд:Квинсленд NP-TOP ; !"Use/MT"
Квинтон:Квинтон NP-ANT-M ; !"Use/MT"
Кебіспаев:Кебіспаев NP-COG-OB ; ! ""
Кевин:Кевин NP-ANT-M ; !"Use/MT"
Кед:Кед NP-ANT-M ; !"Use/MT"
Кедров:Кедров NP-COG-OB ; ! ""
Кей:Кей NP-ANT-M ; !"Use/MT"
Кей:Кей NP-COG-MF ; ! ""
Кейл:Кейл NP-ANT-M ; !"Use/MT"
Кейн:Кейн NP-ANT-M ; !"Use/MT"
Ке:Ке NP-ANT-M ; !"Use/MT"
Кекілбаев:Кекілбаев NP-COG-OB ; ! "" ! Use/MT
Кекіл:Кекіл NP-ANT-M ; ! "Kekil" (Kazakh)
Келат:Келат NP-TOP ; ! ""
Келбет:Келбет NP-ANT-F ; ! "Kelbet" (Kazakh)
Келес:Келес NP-TOP ; ! "Keles river" 
Келли:Келли NP-ANT-F ; !"Use/MT"
Келли:Келли NP-ANT-M ; !"Use/MT"
Келімбетов:Келімбетов NP-COG-OB ; ! "" ! Use/MT
Келімбетов:Келімбетов NP-COG-OB ; !"Use/MT"
Келіс:Келіс NP-ANT-M ; ! "Kelis" (Kazakh)
Кельмаков:Кельмаков NP-COG-OB ; ! ""
Кемелбек:Кемелбек NP-ANT-M ; ! "Kemelbek" (Arabic)
Кемелов:Кемелов NP-COG-OB ; ! ""
Кемеңгер:Кемеңгер NP-ANT-M ; ! "Kemeŋger" (Arabic)
Кемеров:Кемеров NP-COG-OB ; ! ""
Кемеров:Кемеров NP-TOP ; !"Use/MT"
Кемерово:Кемерово NP-TOP ; ! ""
Кемикл:Кемикл NP-TOP ; !"Use/MT"
Кенбаев:Кенбаев NP-COG-OB ; ! ""
Кенбай:Кенбай NP-ANT-M ; ! "Kenbay" (Kazakh)
Кендал:Кендал NP-ANT-F ; !"Use/MT"
Кенебаев:Кенебаев NP-COG-OB ; ! ""
Кенен:Кенен NP-ANT-M ; ! "Kenen" (Kazakh)
Кенесары:Кенесары NP-ANT-M ; ! "Kenesarı"
Кенешов:Кенешов NP-COG-OB ; ! ""
Кенжалы:Кенжалы NP-ANT-M ; ! ""
Кенжебаев:Кенжебаев NP-COG-OB ; ! ""
Кенжебай:Кенжебай NP-ANT-M ; ! "Kenjebay" (Kazakh)
Кенжебек:Кенжебек NP-ANT-M ; ! "Kenjebek" (Kazakh)
Кенжебеков:Кенжебеков NP-COG-OB ; ! ""
Кенжеғали:Кенжеғали NP-ANT-M ; ! "Kenjeğalıy" (Kazakh)
Кенже:Кенже NP-ANT-F ; ! "Kenje" (Kazakh)
Кенже:Кенже NP-ANT-M ; ! "Kenje" (Kazakh)
Кенжеқан:Кенжеқан NP-ANT-M ; ! "Kenjeqan" (Kazakh)
Кенжетаев:Кенжетаев NP-COG-OB ; ! ""
Кенжетай:Кенжетай NP-ANT-M ; ! "Kenjetay" (Kazakh)
Кенжетайұлы:Кенжетайұлы NP-COG-M ; ! ""
Кенжеханов:Кенжеханов NP-COG-OB ; ! ""
Кенигсберг:Кенигсберг NP-TOP ; !""
Кения:Кения NP-ANT-F ; !"Use/MT"
Кения:Кения NP-TOP ; ! ""
Кен:Кен NP-ANT-M ; !"Use/MT"
Кеннеди:Кеннеди NP-ANT-F ; !"Use/MT"
Кеннеди:Кеннеди NP-ANT-M ; !"Use/MT"
Кеннеди:Кеннеди NP-COG-MF ; !"Use/MT"
Кеннет:Кеннет NP-ANT-M ; !"Use/MT"
Кентербери:Кентербери NP-TOP ; !"Use/MT"
Кент:Кент NP-ANT-M ; !"Use/MT"
Кент:Кент NP-TOP ; ! ""
Кентукки:Кентукки NP-TOP ; ! ""
Кеңесгүл:Кеңесгүл NP-ANT-F ; ! "Keŋesgül" (Kazakh)
Кеңес:Кеңес NP-ANT-M ; ! "Keŋes" (Kazakh)
Кеңес% Одағы:Кеңес% Одақ NP-TOP-COMPOUND ; ! "Soviet Union"
Кеңшілік:Кеңшілік NP-ANT-M ; ! "Keŋshilik" (Kazakh)
Кеплер:Кеплер NP-COG-MF ; ! ""
Керей:Керей NP-ANT-M ; ! ""
Керимбала:Керимбала NP-ANT-F ; ! "Kerıymbala" (Kazakh)
Кер:Кер NP-ANT-M ; !"Use/MT"
Кермани:Кермани NP-COG-M ; !"Use/MT"
Кертис:Кертис NP-ANT-M ; !"Use/MT"
Керімәлі:Керімәлі NP-ANT-M ; ! "Kerimäli" (Arabic)
Керімбай:Керімбай NP-ANT-M ; ! "Kerimbay" (Arabic)
Керімбек:Керімбек NP-ANT-M ; ! "Kerimbek" (Arabic)
Керімбеков:Керімбеков NP-COG-OB ; ! ""
Керім:Керім NP-ANT-M ; ! "Kerim" (Arabic)
Керімқан:Керімқан NP-ANT-M ; ! "Kerimqan" (Arabic)
Керімқұлов:Керімқұлов NP-COG-OB ; ! ""
Кесов:Кесов NP-COG-OB ; ! ""
Кибивотт:Кибивотт NP-COG-M ; !"Use/MT"
Киб:Киб NP-ANT-M ; !"Use/MT"
Кивилев:Кивилев NP-COG-OB ; ! ""
Кидман:Кидман NP-COG-MF ; !"Use/MT"
Киев:Киев NP-TOP ; ! ""
Ки:Ки NP-ANT-M ; !"Use/MT"
Киликия:Киликия NP-TOP ; ! ""
Ким:Ким NP-ANT-F ; !"Use/MT"
Ким:Ким NP-ANT-M ; ! "Kiym" (New word)
КИМЭП:КИМЭП NP-ORG-ABBR ; ! "KIMEP"
Кинжалов:Кинжалов NP-COG-OB ; ! ""
Киншаса:Киншаса NP-TOP ; !""
Киприану:Киприану NP-COG-MF ; !"Use/MT"
Кипр:Кипр NP-TOP ; ! ""
Киран:Киран NP-ANT-M ; !"Use/MT"
Кирибати:Кирибати NP-TOP ; ! ""
Кирилл:Кирилл NP-ANT-M ; ! ""
Кириллов:Кириллов NP-COG-OB ; ! ""
Кир:Кир NP-ANT-M ; !"Use/MT"
Киров:Киров NP-COG-OB ; ! ""
Киров:Киров NP-TOP ; !"Use/MT"
Кирпичников:Кирпичников NP-COG-OB ; ! ""
Кирсанов:Кирсанов NP-COG-OB ; ! ""
Киселёв:Киселёв NP-COG-OB ; ! ""
Китаби%-ал%-масалик% Вал%-мамлик:Китаби%-ал%-масалик% Вал%-мамлик NP-AL ; ! "" Use/MT
Китаенко:Китаенко NP-COG-MF ; ! ""
Кит:Кит NP-ANT-M ; !"Use/MT"
Кихот:Кихот NP-COG-M ; !"Use/MT"
Кихот:Кихот NP-ORG ; !"Use/MT"
Киченсув:Киченсув NP-TOP ; ! ""
Кишинев:Кишинев NP-COG-OB ; ! ""
Киш:Киш NP-ANT-M ; !"Use/MT"
Киікбай:Киікбай NP-ANT-M ; ! "Kıyikbay" (Kazakh)
Клавдий:Клавдий NP-ANT-M ; ! ""
Клара:Клара NP-ANT-F ; ! "Klara" (Latin)
Клариса:Клариса NP-ANT-F ; !"Use/MT"
Кларк:Кларк NP-ANT-M ; !"Use/MT"
Кларк:Кларк NP-COG-MF ; !"Use/MT"
Клаудио:Клаудио NP-ANT-M ; !"Use/MT"
Клаудия:Клаудия NP-ANT-F ; !"Use/MT"
Клау:Клау NP-ANT-M ; !"Use/MT"
Клау:Клау NP-ANT-M ; !"Use/MT"
Клаус:Клаус NP-ANT-M ; ! ""
Клейн:Клейн NP-ANT-M ; !"Use/MT"
Клейтон:Клейтон NP-ANT-M ; !"Use/MT"
Кле:Кле NP-ANT-M ; !"Use/MT"
Кле:Кле NP-ANT-M ; !"Use/MT"
Клементе:Клементе NP-ANT-M ; !"Use/MT"
Клементьев:Клементьев NP-COG-OB ; ! ""
Клименко:Клименко NP-COG-MF ; ! ""
Климент:Климент NP-ANT-M ; !"Use/MT"
Клим:Клим NP-ANT-M ; ! "Kliym" (Latin)
Клинт:Клинт NP-ANT-M ; !"Use/MT"
Клинтон:Клинтон NP-ANT-M ; !"Use/MT"
Клинтон:Клинтон NP-COG-MF ; ! ""
Клиффорд:Клиффорд NP-ANT-M ; !"Use/MT"
Клод:Клод NP-ANT-M ; !"Use/MT"
Кло:Кло NP-ANT-M ; !"Use/MT"
Кло:Кло NP-ANT-M ; !"Use/MT"
Клоп:Клоп NP-COG-M ; ! "Klopp (German surmame)"
Клуни:Клуни NP-COG-MF ; !"Use/MT"
Клэр:Клэр NP-ANT-F ; !"Use/MT"
Клюни:Клюни NP-TOP ; ! ""
Ключев:Ключев NP-COG-OB ; ! ""
Ключевск:Ключевск NP-TOP ; ! ""
Кляжев:Кляжев NP-COG-OB ; ! ""
Кносс:Кносс NP-COG-MF ; ! ""
Князев:Князев NP-COG-OB ; ! ""
Кобе:Кобе NP-ANT-M ; !"Use/MT"
Кобяков:Кобяков NP-COG-OB ; ! "" ! Use/MT
Ковалев:Ковалев NP-COG-OB ; ! ""
Ковалевский:Ковалевский NP-COG-MF ; ! ""
Коваленко:Коваленко NP-COG-MF ; ! ""
Ковалёв:Ковалёв NP-COG-OB ; ! ""
Ковров:Ковров NP-COG-OB ; ! ""
Кожанов:Кожанов NP-COG-OB ; ! ""
Кожевников:Кожевников NP-COG-OB ; ! ""
Козаков:Козаков NP-COG-OB ; ! ""
Коз:Коз NP-ANT-M ; !"Use/MT"
Коз:Коз NP-ANT-M ; !"Use/MT"
Козлов:Козлов NP-COG-OB ; ! ""
Козлов:Козлов NP-COG-OB ; ! ""
Козырев:Козырев NP-COG-OB ; ! ""
Коимбра:Коимбра NP-TOP ; !"Use/MT"
Ко:Ко NP-ANT-M ; !"Use/MT"
Ко:Ко NP-ANT-M ; !"Use/MT"
Коқан:Коқан NP-TOP ; ! ""
Колби:Колби NP-ANT-M ; !"Use/MT"
Коле:Коле NP-ANT-M ; !"Use/MT"
Колесников:Колесников NP-COG-OB ; ! ""
Колесниченко:Колесниченко NP-COG-MF ; ! ""
Колин:Колин NP-ANT-M ; !"Use/MT"
Коллинз:Коллинз NP-COG-MF ; !"Use/MT"
Колл:Колл NP-COG-MF ; !"Use/MT"
Колмогоров:Колмогоров NP-COG-OB ; ! ""
Колобов:Колобов NP-COG-OB ; ! ""
Колокольников:Колокольников NP-COG-OB ; ! ""
Коломбо:Коломбо NP-ANT-M ; !"Use/MT"
Коломбо:Коломбо NP-TOP ; ! "" 
Колорадо:Колорадо NP-TOP ; ! ""
Колосков:Колосков NP-COG-OB ; ! ""
Колпаков:Колпаков NP-COG-OB ; ! ""
Колтен:Колтен NP-ANT-M ; !"Use/MT"
Колумбия:Колумбия NP-TOP ; ! ""
Колумб:Колумб NP-ANT-M ; !"Use/MT"
Колумб:Колумб NP-COG-MF ; ! ""
Кольцов:Кольцов NP-COG-OB ; ! ""
Коля:Коля NP-ANT-M ; ! ""
Комаров:Комаров NP-COG-OB ; ! ""
Коми:Коми NP-TOP ; ! ""
Комиссаров:Комиссаров NP-COG-OB ; ! ""
Комор:Комор NP-TOP ; ! ""
Коморовский:Коморовский NP-COG-M ; ! "USE/MT"
Композиторов:Композиторов NP-COG-OB ; ! ""
Конан:Конан NP-ANT-M ; !"Use/MT"
Конан:Конан NP-COG-MF ; !"Use/MT"
Конго:Конго NP-TOP ; ! ""
Кондаков:Кондаков NP-COG-OB ; ! ""
Кондолиза:Кондолиза NP-ANT-F ; !"Use/MT"
Кондратенко:Кондратенко NP-COG-MF ; ! ""
Кондратов:Кондратов NP-COG-OB ; ! ""
Кондратьев:Кондратьев NP-COG-OB ; ! ""
Кондрашов:Кондрашов NP-COG-OB ; ! ""
Конев:Конев NP-COG-OB ; ! ""
КОНМЕБОЛ:КОНМЕБОЛ NP-AL-ABBR ; ! ""
Коннектикут:Коннектикут NP-TOP ; !""
Коннектикут:Коннектикут NP-TOP ; !"Use/MT"
Коннов:Коннов NP-COG-OB ; ! ""
Коннолли:Коннолли NP-COG-MF ; !"Use/MT"
Коннор:Коннор NP-ANT-M ; !"Use/MT"
Коновалов:Коновалов NP-COG-OB ; ! ""
Кононенко:Кононенко NP-COG-MF ; ! ""
Кононов:Кононов NP-COG-OB ; ! ""
Конрад:Конрад NP-ANT-M ; !"Use/MT"
Констанинов:Констанинов NP-COG-OB ; ! ""
Констан:Констан NP-ANT-M ; ! ""
Константин:Константин NP-ANT-M ; ! ""
Константинов:Константинов NP-COG-OB ; ! ""
Константино:Константино NP-ANT-M ; !"Use/MT"
Константинополь:Константинополь NP-TOP ; ! ""
Консуэло:Консуэло NP-ANT-F ; !"Use/MT"
Контрерас:Контрерас NP-COG-MF ; !"Use/MT"
Конфуций:Конфуций NP-ANT-M ; !"Use/MT"
Копенгаген:Копенгаген NP-TOP ; ! "Copenhagen"
Копылов:Копылов NP-COG-OB ; ! ""
Кора:Кора NP-ANT-F ; !"Use/MT"
Кордильер:Кордильер NP-TOP ; ! ""
Корей:Корей NP-TOP ; !"Use/MT"
Корея:Корея NP-TOP ; ! "Korea"
Кор:Кор NP-ANT-M ; !"Use/MT"
Корнеев:Корнеев NP-COG-OB ; ! ""
Корнилов:Корнилов NP-COG-OB ; ! ""
Королев:Королев NP-COG-OB ; ! ""
Королев:Королев NP-COG-OB ; ! ""
Короленко:Короленко NP-COG-MF ; ! ""
Королёв:Королёв NP-COG-OB ; ! ""
Коротина:Коротина NP-ANT-F ; !"Use/MT"
Корреа:Корреа NP-ANT-F ; !"Use/MT"
Корсаков:Корсаков NP-COG-OB ; ! ""
Кортес:Кортес NP-ANT-M ; !"Use/MT"
Корчаков:Корчаков NP-COG-OB ; ! ""
Коршунов:Коршунов NP-COG-OB ; ! ""
Корытников:Корытников NP-COG-OB ; ! ""
Коряков:Коряков NP-COG-M ; !"Use/MT"
Косарев:Косарев NP-COG-OB ; ! ""
Косачев:Косачев NP-COG-OB ; ! ""
Космосбек:Космосбек NP-ANT-M ; ! "Kosmosbek" (New word)
Косово:Косово NP-TOP ; ! ""
Коста%-Рика:Коста%-Рика NP-TOP ; ! ""
Костин:Костин NP-COG-IN ; ! ""
Костуника:Костуника NP-COG-MF ; !"Use/MT"
Костя:Костя NP-ANT-M ; ! ""
Косырев:Косырев NP-COG-OB ; ! ""
Кот%-д%'Ивуар:Кот%-д%'Ивуар NP-TOP ; ! ""
Кот%-д%-Ивуар:Кот%-д%-Ивуар NP-TOP ; ! ""
Котельников:Котельников NP-COG-OB ; ! ""
Котил:Котил NP-COG-M ; ! "Kotil (Turkish surname)"
Котович:Кото NP-PAT-VICH ; ! "" ! Use/MT 
Котов:Котов NP-COG-OB ; ! ""
Коул:Коул NP-ANT-M ; !"Use/MT"
Коулман:Коулман NP-ANT-M ; !"Use/MT"
Коуэн:Коуэн NP-COG-MF ; !"Use/MT"
Кочетков:Кочетков NP-COG-OB ; ! ""
Кочкарев:Кочкарев NP-COG-OB ; ! ""
Кошелев:Кошелев NP-COG-OB ; ! ""
Коэн:Коэн NP-COG-MF ; !"Use/MT"
Көбдіков:Көбдіков NP-COG-OB ; ! ""
Көбеген:Көбеген NP-ANT-M ; ! "Köbegen" (Old Turkic)
Көбеев:Көбеев NP-COG-OB ; ! ""
Көбей:Көбей NP-ANT-M ; ! "Köbey" (Kazakh)
Көбейсін:Көбейсін NP-ANT-M ; ! "Köbeysin" (Kazakh)
Көбенқұлов:Көбенқұлов NP-COG-OB ; ! ""
Көдек:Көдек NP-ANT-M ; ! "Ködek" (Arabic)
Көжеков:Көжеков NP-COG-OB ; ! ""
Көкбай:Көкбай NP-ANT-M ; ! "Kökbay" (Kazakh)
Көксерек:Көксерек NP-ANT-M ; ! "Kökserek" (the name of a wolf that's a character in a story)
Көксу:Көксу NP-TOP ; ! ""
Көктөбе:Көктөбе NP-TOP ; ! ""
Көкше:Көкше NP-TOP ; ! ""
Көкшетау:Көкшетау NP-TOP ; ! ""
Көлбаев:Көлбаев NP-COG-OB ; ! ""
Көмекбаев:Көмекбаев NP-COG-OB ; ! ""
Көпбай:Көпбай NP-ANT-M ; ! "Köpbay" (Kazakh)
Көпеев:Көпеев NP-COG-OB ; ! ""
Көпешов:Көпешов NP-COG-OB ; ! ""
Көпжан:Көпжан NP-ANT-M ; ! "Köpjan" (Kazakh)
Көпжасар:Көпжасар NP-ANT-M ; ! "Köpjasar" (Kazakh)
Көтібақ:Көтібақ NP-ANT-M ; ! ""
Көтібаров:Көтібаров NP-COG-OB ; ! ""
Көшағаш:Көшағаш NP-TOP ; ! ""
Көшербаев:Көшербаев NP-COG-OB ; ! ""
Көшімбай:Көшімбай NP-ANT-M ; ! "Köshimbay" 
Кравков:Кравков NP-COG-OB ; ! ""
Кравченко:Кравченко NP-COG-MF ; ! ""
Краков:Краков NP-TOP ; ! ""
Крамаренко:Крамаренко NP-COG-MF ; ! ""
Крамдсбанк:Крамдсбанк NP-TOP ; ! "" ! Use/MT
Красильников:Красильников NP-COG-OB ; ! ""
Краснов:Краснов NP-COG-OB ; ! ""
Краснояр:Краснояр NP-TOP ; !"Use/MT"
Красноярск:Красноярск NP-TOP ; ! ""
Красноярск:Красноярск NP-TOP ; !"Use/MT"
Крашенинников:Крашенинников NP-COG-OB ; ! ""
Крганов:Крганов NP-COG-OB ; ! ""
Крейг:Крейг NP-ANT-M ; !"Use/MT"
Кремль:Кремль NP-TOP ; !"Use/MT"
Крестьянович:Крестьяно NP-PAT-VICH ; ! "" ! Use/MT 
Кривоносов:Кривоносов NP-COG-OB ; ! ""
Криков:Криков NP-COG-OB ; ! ""
Крис:Крис NP-ANT-M ; !"Use/MT"
Кристиана:Кристиана NP-ANT-F ; !"Use/MT"
Кристиан:Кристиан NP-ANT-M ; !"Use/MT"
Кристина:Кристина NP-ANT-F ; ! ""
Кристин:Кристин NP-ANT-F ; !"Use/MT"
Кристофер:Кристофер NP-ANT-M ; !"Use/MT"
Кристоф:Кристоф NP-ANT-M ; !"Use/MT"
Крит:Крит NP-TOP ; ! ""
Крлыков:Крлыков NP-COG-OB ; ! ""
Крокт:Крокт NP-ANT-M ; !"Use/MT"
Кросс:Кросс NP-ANT-M ; !"Use/MT"
Кротов:Кротов NP-COG-OB ; ! ""
Кроули:Кроули NP-COG-MF ; !"Use/MT"
Крутов:Крутов NP-COG-OB ; ! ""
Крыленко:Крыленко NP-COG-MF ; ! ""
Крылов:Крылов NP-COG-OB ; ! ""
Крюков:Крюков NP-COG-OB ; ! ""
Крючков:Крючков NP-COG-OB ; ! ""
Ксавье:Ксавье NP-ANT-M ; !"Use/MT"
Ксения:Ксения NP-ANT-F ; ! ""
Ксенофонтов:Ксенофонтов NP-COG-OB ; ! ""
КСРО:КСРО%{а%} NP-TOP-ABBR ; ! "USSR"
Куба:Куба NP-TOP ; ! "Cuba"
Куваев:Куваев NP-COG-OB ; ! ""
Кувейт:Кувейт NP-TOP ; ! ""
Кувшинов:Кувшинов NP-COG-OB ; ! ""
Кудаков:Кудаков NP-COG-OB ; ! ""
Кудрявцев:Кудрявцев NP-COG-OB ; ! ""
Кудряшов:Кудряшов NP-COG-OB ; ! ""
Кузнецк:Кузнецк NP-TOP ; ! ""
Кузнецов:Кузнецов NP-COG-OB ; ! ""
Кузьменков:Кузьменков NP-COG-OB ; ! ""
Кузьменко:Кузьменко NP-COG-MF ; ! ""
Кузьмин:Кузьмин NP-COG-IN ; ! ""
Кузяев:Кузяев NP-COG-OB ; ! "" ! Use/MT
Кузя:Кузя NP-ANT-M ;
Куйбышев:Куйбышев NP-COG-OB ; ! ""
Кукси:Кукси NP-COG-MF ; !"Use/MT"
Ку:Ку NP-ANT-M ; !"Use/MT"
Ку:Ку NP-ANT-M ; !"Use/MT"
Кулагин:Кулагин NP-COG-IN ; ! ""
Кулаков:Кулаков NP-COG-OB ; ! ""
Куленов:Куленов NP-COG-M ; !"Use/MT"
Куленов:Куленов NP-COG-OB ; ! ""
Кулешов:Кулешов NP-COG-OB ; ! "" ! Use/MT
Куликов:Куликов NP-COG-OB ; ! ""
Кулов:Кулов NP-COG-OB ; ! ""
Кульев:Кульев NP-COG-OB ; ! ""
Купер:Купер NP-ANT-M ; !"Use/MT"
Кура%-Аракс:Кура%-Аракс NP-TOP ; ! ""
Куравлев:Куравлев NP-COG-OB ; ! ""
Кураков:Кураков NP-COG-OB ; ! ""
Курамшин:Курамшин NP-COG-IN ; ! ""
Курбатов:Курбатов NP-COG-OB ; ! ""
Курганцев:Курганцев NP-COG-OB ; ! "" ! Use/MT
Кургат:Кургат NP-ANT-M ; !"Use/MT"
Курдистан:Курдистан NP-TOP ; !"Use/MT"
Курдстан:Курдстан NP-TOP ; ! ""
Курзенко:Курзенко NP-COG-MF ; ! ""
Куросава:Куросава NP-COG-MF ; !"Use/MT"
Курск:Курск NP-TOP ; ! ""
Курт:Курт NP-ANT-M ; !"Use/MT"
Курчатов:Курчатов NP-COG-OB ; ! ""
Кустиков:Кустиков NP-COG-OB ; ! ""
Кухруд:Кухруд NP-TOP-RUS ; ! ""
Кучеренко:Кучеренко NP-COG-MF ; ! ""
Кушаев:Кушаев NP-COG-OB ; ! ""
Кушан:Кушан NP-COG-MF ; !"Use/MT"
Кушнаренко:Кушнаренко NP-COG-MF ; ! ""
Куэльяр:Куэльяр NP-COG-MF ; !"Use/MT"
Куэнка:Куэнка NP-TOP ; !"Use/MT"
Кұдайберді:Кұдайберді NP-ANT-M ; ! ""
Күбара:Күбара NP-ANT-F ; ! "Kübara" (Arabic)
Күзембаев:Күзембаев NP-COG-OB ; ! ""
Күлай:Күлай NP-ANT-F ; ! "Külay" (Kazakh)
Күлайхан:Күлайхан NP-ANT-F ; ! "Külayxan" (Kazakh)
Күлән:Күлән NP-ANT-F ; ! "Külän" (Kazakh)
Күләш:Күләш NP-ANT-F ; ! 
Күлбағира:Күлбағира NP-ANT-F ; ! "Külbağıyra" (Arabic)
Күлғайша:Күлғайша NP-ANT-F ; ! "Külğaysha" 
Күлеев:Күлеев NP-COG-OB ; ! ""
Күлтегін:Күлтегін NP-ANT-M ; ! "Kultegin" 
Күлтегін:Күлтегін NP-ANT-M ; !"Use/MT"
Күлянда:Күлянда NP-ANT-F ; ! "Külyanda" (Greek)
Күмісай:Күмісай NP-ANT-F ; ! "Kümisay" (Kazakh)
Күмісбаев:Күмісбаев NP-COG-OB ; ! "" ! Use/MT
Күмісбеков:Күмісбеков NP-COG-OB ; ! ""
Күнай:Күнай NP-ANT-F ; ! "Künay" (Kazakh)
Күнғалиев:Күнғалиев NP-COG-OB ; ! "" ! Use/MT
Күнке:Күнке NP-ANT-M ; ! ""
Күнсұлу:Күнсұлу NP-ANT-F ; ! "Künsulıw" (Kazakh)
Күнту:Күнту NP-ANT-M ; ! "Küntiw" (Kazakh)
Күңке:Күңке NP-ANT-F ; ! ""
Күрдер:Күрдер NP-TOP ; ! ""
Күрішбаев:Күрішбаев NP-COG-OB ; ! "" ! Use/MT
Күшікбай:Күшікбай NP-ANT-M ; ! "Küşikbay"
Кырджали:Кырджали NP-TOP ; !"Use/MT"
Кэл:Кэл NP-ANT-M ; !"Use/MT"
Кэмерон:Кэмерон NP-COG-MF ; ! ""
Кэмпбелл:Кэмпбелл NP-COG-MF ; !"Use/MT"
Кэролайн:Кэролайн NP-ANT-F ; !"Use/MT"
Кэрол:Кэрол NP-ANT-F ; !"Use/MT"
Кэрри:Кэрри NP-ANT-F ; !"Use/MT"
Кэти:Кэти NP-ANT-F ; !"Use/MT"
Кэтлин:Кэтлин NP-ANT-F ; !"Use/MT"
Кэтрин:Кэтрин NP-ANT-F ; !"Use/MT"
Кюракао:Кюракао NP-TOP ; ! ""
Кюри:Кюри NP-COG-MF ; !"Use/MT"
Кюстендил:Кюстендил NP-TOP ; !"Use/MT"
Қабай:Қабай NP-ANT-M ; ! "Qabay" (Old Turkic)
Қабашев:Қабашев NP-COG-OB ; ! ""
Қабдолданов:Қабдолданов NP-COG-OB ; ! "" ! Use/MT
Қабдолов:Қабдолов NP-COG-OB ; ! "" ! Use/MT
Қабиб:Қабиб NP-ANT-M ; ! "Qabıyb" (Arabic)
Қаби:Қаби NP-ANT-M ; ! "Qabıy" (Arabic)
Қабимолдаев:Қабимолдаев NP-COG-OB ; ! ""
Қабыкенов:Қабыкенов NP-COG-OB ; ! ""
Қабылбаев:Қабылбаев NP-COG-OB ; ! ""
Қабылдин:Қабылдин NP-COG-IN ; ! ""
Қабыл:Қабыл NP-ANT-M ; ! "Qabıl" (Arabic)
Қабылов:Қабылов NP-COG-OB ; ! ""
Қабілет:Қабілет NP-ANT-M ; ! "Qabilet" (Arabic)
Қағба:Қағба NP-AL ; ! ""
Қадыралы:Қадыралы NP-ANT-M ; ! "Qadıralı" (Arabic)
Қадырбаев:Қадырбаев NP-COG-OB ; ! ""
Қадырбай:Қадырбай NP-ANT-M ; ! "Qadırbay" (Kazakh)
Қадырбек:Қадырбек NP-ANT-M ; ! "Qadırbek" (Kazakh)
Қадырбеков:Қадырбеков NP-COG-OB ; ! ""
Қадыржан:Қадыржан NP-ANT-M ; ! "Qadırjan" (Arabic)
Қадыржанов:Қадыржанов NP-COG-OB ; ! "" ! Use/MT
Қадыр:Қадыр NP-ANT-M ; ! "Qadır" (Arabic)
Қадыр% Мырза% Әли:Қадыр% Мырза% Әли NP-ANT-M ; ! "" (Arabic)
Қадыров:Қадыров NP-COG-OB ; ! "" ! Use/MT
Қадырхан:Қадырхан NP-ANT-M ; ! "Qadırxan" (Kazakh)
Қажығалиев:Қажығалиев NP-COG-OB ; ! ""
Қажығали:Қажығали NP-ANT-M ; ! "Qajığalıy" (Arabic)
Қажыкен:Қажыкен NP-ANT-M ; ! "Qajıken" (Arabic)
Қажы:Қажы NP-ANT-M ; ! "Qajı" (Arabic)
Қажым:Қажым NP-ANT-M ; ! "Qajım" (Arabic)
Қажымқан:Қажымқан NP-ANT-M ; ! ""
Қажымұрат:Қажымұрат NP-ANT-M ; ! "Qajımurat" (Arabic)
Қазақ% АССР:Қазақ% АССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Қазақбаев:Қазақбаев NP-COG-OB ; ! ""
Қазақбай:Қазақбай NP-ANT-M ; ! "Qazaqbay" (Kazakh)
Қазақгейт:Қазақгейт NP-AL ; ! ""
Қазақ% КСР:Қазақ% КСР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Қазақмыс:Қазақмыс NP-AL ; ! ""
Қазақ% ССР:Қазақ% ССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Қазақстан:Қазақстан NP-TOP ; ! ""
Қазақстан:Қазақстан NP-TOP ; ! "Kazakhstan" 
Қазақтелеком:Қазақтелеком NP-ORG ; ! ""
Қазақфильм:Қазақфильм NP-AL ; ! ""
Қазан:Қазан NP-TOP ; ! "Kazan"
Қазансев:Қазансев NP-COG-OB ; ! ""
Қазиза:Қазиза NP-ANT-F ; ! "Qazıyza" 
Қази:Қази NP-ANT-M ; ! "Qazıy" (Arabic)
Қазила:Қазила NP-ANT-F ; ! "Qazıyla" (Kazakh)
Қазкоммерцбанк:Қазкоммерцбанк NP-TOP ; ! "" ! Use/MT
Қазмырыш:Қазмырыш NP-ORG ; ! ""
Қазна:Қазна NP-ANT-F ; ! "Qazna" (Kazakh)
Қазпочта:Қазпочта NP-ORG ; ! ""
Қазпошта:Қазпошта NP-ORG ; ! ""
Қазталов:Қазталов NP-COG-OB ; ! ""
Қазтуған:Қазтуған NP-ANT-M ; ! "Qaztıwğan" (Kazakh)
Қазыбаев:Қазыбаев NP-COG-OB ; ! ""
Қазыбай:Қазыбай NP-ANT-M ; ! "Qazıbay" (Arabic)
Қазыбек:Қазыбек NP-ANT-M ; ! "Qazıbek" (Arabic)
Қазығали:Қазығали NP-ANT-M ; ! "Qazığalıy" (Arabic)
Қазығұрт:Қазығұрт NP-TOP ; ! ""
Қазықан:Қазықан NP-ANT-M ; ! "Qazıqan" (Arabic)
Қазыханов:Қазыханов NP-COG-OB ; ! "" ! Use/MT
Қайбыч:Қайбыч NP-TOP ; ! ""
Қайдар:Қайдар NP-ANT-M ; ! "Qaydar" (Arabic)
Қайдаров:Қайдаров NP-COG-OB ; ! ""
Қайдос:Қайдос NP-ANT-M ; !
Қайрат:Қайрат NP-ANT-M ; ! "Qayrat" (Arabic)
Қайсар:Қайсар NP-ANT-M ; ! "Qaysar" (Arabic)
Қайсен:Қайсен NP-ANT-M ; ! "Qaysen" (Kazakh)
Қайсенов:Қайсенов NP-COG-OB ; ! "" ! Use/MT
Қайым:Қайым NP-ANT-M ; ! "Qayım" (Arabic)
Қайып:Қайып NP-ANT-M ; ! "Qayıp" (Arabic)
Қайырбай:Қайырбай NP-ANT-M ; ! "Qayırbay" (Arabic)
Қайырбек:Қайырбек NP-ANT-M ; ! "Qayırbek" (Arabic)
Қайырбеков:Қайырбеков NP-COG-OB ; ! ""
Қайырбекұлы:Қайырбекұлы NP-COG-M ; ! "Use/MT"
Қайырболат:Қайырболат NP-ANT-M ; ! "Qayırbolat" (Arabic)
Қайыргелді:Қайыргелді NP-ANT-M ; ! ""
Қайырғали:Қайырғали NP-ANT-M ; ! "Qayırğalıy" (Arabic)
Қайыржан:Қайыржан NP-ANT-M ; ! "Qayırjan" (Arabic)
Қайыр:Қайыр NP-ANT-M ; ! "Qayır" (Arabic)
Қайырлы:Қайырлы NP-ANT-M ; ! "Qayırlı" (Arabic)
Қайыров:Қайыров NP-COG-OB ; ! "" ! Use/MT
Қақназар:Қақназар NP-ANT-M ; ! "Qaqnazar" (Arabic)
Қаламбаев:Қаламбаев NP-COG-OB ; ! ""
Қаламқас:Қаламқас NP-ANT-F ; ! "Qalamqas" (Kazakh)
Қалау:Қалау NP-ANT-M ; ! "Qalaw" (Kazakh)
Қалауов:Қалауов NP-COG-OB ; ! ""
Қалауша:Қалауша NP-ANT-M ; ! "Qalawsha" (Kazakh)
Қалбиби:Қалбиби NP-ANT-F ; ! "Qalbıybıy" 
Қалдарбек:Қалдарбек NP-ANT-M ; ! "Qaldarbek" (Arabic)
Қалдаяқов:Қалдаяқов NP-COG-OB ; ! ""
Қалдыбай:Қалдыбай NP-ANT-M ; ! "Qaldıbay" (Arabic)
Қалдыбек:Қалдыбек NP-ANT-M ; ! "Qaldıbek" (Arabic)
Қалиев:Қалиев NP-COG-OB ; ! ""
Қалижанов:Қалижанов NP-COG-OB ; ! "" ! Use/MT
Қали:Қали NP-ANT-M ; ! "Qalıy" 
Қалия:Қалия NP-ANT-F ; ! "Qalıya" 
Қалқа:Қалқа NP-ANT-M ; ! "Qalqa" (Kazakh)
Қалқаман:Қалқаман NP-ANT-M ; ! "Qalqaman" (Kazakh)
Қалқұтан:Қалқұтан NP-TOP ; ! ""
Қалмұхамбет:Қалмұхамбет NP-ANT-M ; ! "Qalmuxambet" (Persian)
Қалмырзаев:Қалмырзаев NP-COG-OB ; ! "" ! Use/MT
Қалтай:Қалтай NP-ANT-M ; ! ""
Қалтұрган:Қалтұрган NP-ANT-F ; ! "Qalturgan" 
Қалуа:Қалуа NP-ANT-F ; ! "Qalıwa" 
Қалуан:Қалуан NP-ANT-F ; ! "Qalıwan" 
Қалыбай:Қалыбай NP-ANT-M ; ! "Qalıbay" (Arabic)
Қалыбек:Қалыбек NP-ANT-M ; ! "Qalıbek" (Arabic)
Қалымбаев:Қалымбаев NP-COG-OB ; ! ""
Қалібекұлы:Қалібекұлы NP-COG-M ; ! ""
Қамария:Қамария NP-ANT-F ; ! "Qamarıya" (Arabic)
Қамар:Қамар NP-ANT-M ; ! "Qamar" (Arabic)
Қама% Сағасы:Қама% Сағасы NP-TOP ; ! ""
Қамбар:Қамбар NP-ANT-M ; ! "Qambar" (Arabic)
Қамбаров:Қамбаров NP-COG-OB ; ! ""
Қамбышев:Қамбышев NP-COG-OB ; ! ""
Қамқа:Қамқа NP-ANT-F ; ! "Qamqa" (Kazakh)
Қамысбай:Қамысбай NP-ANT-M ; ! ""
Қамыскөл:Қамыскөл NP-TOP ; ! ""
Қамыскөл:Қамыскөл NP-TOP ; ! ""
Қамысқала:Қамысқала NP-TOP ; ! ""
Қанағат:Қанағат NP-ANT-M ; ! "Qanağat" (Arabic)
Қанапиянов:Қанапиянов NP-COG-OB ; ! "" ! Use/MT
Қанат:Қанат NP-ANT-M ; ! "Qanat" (Kazakh)
Қанахан:Қанахан NP-ANT-M ; ! ""
Қаниса:Қаниса NP-ANT-M ; !"Use/MT"
Қаныш:Қаныш NP-ANT-M ; ! ""
Қанышқызы:Қанышқызы NP-COG-MF ; ! "" !Use/MT
Қапбас:Қапбас NP-ANT-M ; ! "Qapbas" 
Қапиза:Қапиза NP-ANT-F ; ! "Qapıyza" (Arabic)
Қаппаров:Қаппаров NP-COG-OB ; ! "" ! Use/MT
Қаппас:Қаппас NP-ANT-M ; ! "Qappas" 
Қапыз:Қапыз NP-ANT-M ; ! "Qapız" 
Қарабақ:Қарабақ NP-TOP ; ! ""
Қарабас:Қарабас NP-TOP ; ! ""
Қарабұлақ:Қарабұлақ NP-TOP ; ! ""
Қарағандыкөмір:Қарағандыкөмір NP-TOP ; ! ""
Қарағанды:Қарағанда NP-TOP ; ! "Qarağanda"  ! Dir/LR
Қарағанды:Қарағанды NP-TOP ; ! "Qarağanda"
Қаражігітов:Қаражігітов NP-COG-OB ; ! ""
Қаракөз:Қаракөз NP-ANT-F ; ! "Qaraköz" (Kazakh)
Қаракөл:Қаракөл NP-TOP ; ! ""
Қарақорым:Қарақорым NP-TOP ; ! ""
Қарақұм:Қарақұм NP-TOP ; ! ""
Қаралаев:Қаралаев NP-COG-OB ; ! ""
Қараман:Қараман NP-ANT-M ; ! "Qaraman" (Kazakh)
Қараой:Қараой NP-TOP ; ! ""
Қарасай:Қарасай NP-ANT-M ; ! "Qarasay" (Kazakh)
Қарасай:Қарасай NP-TOP ; !""
Қаратаев:Қаратаев NP-COG-OB ; ! ""
Қаратай:Қаратай NP-ANT-M ; ! "Qaratay" (Kazakh)
Қаратал:Қаратал NP-TOP ; ! "" 
Қаратау:Қаратау NP-TOP ; ! ""
Қара% теңіз:Қара% теңіз NP-TOP ; ! ""
Қарахан:Қарахан NP-TOP ; !"Use/MT"
Қарашоқы:Қарашоқы NP-TOP ; ! ""
Қарашығанақ:Қарашығанақ NP-TOP ; ! ""
Қарқаралы:Қарқаралы NP-TOP ; ! ""
Қарлығаш:Қарлығаш NP-ANT-F ; ! "Qarlığash" (Kazakh)
Қармыс:Қармыс NP-ANT-M ; ! "Qarmıs" (Arabic)
Қарсақбаев:Қарсақбаев NP-COG-OB ; ! ""
Қарсақбай:Қарсақбай NP-TOP ; ! "" 
Қартбаев:Қартбаев NP-COG-OB ; ! ""
Қаршыға:Қаршыға NP-ANT-F ; ! "Qarshığa"
Қаршыға:Қаршыға NP-ANT-M ; ! "Qarshığa"
Қасанов:Қасанов NP-COG-OB ; ! ""
Қасейінов:Қасейінов NP-COG-OB ; ! "" ! Use/MT
Қасен:Қасен NP-ANT-M ; ! "Qasen"
Қасиман:Қасиман NP-ANT-M ; ! "Qasıyman" (Arabic)
Қасиманов:Қасиманов NP-COG-OB ; ! ""
Қасқабасов:Қасқабасов NP-COG-OB ; ! "" ! Use/MT
Қастеев:Қастеев NP-COG-OB ; ! ""
Қасымали:Қасымали NP-ANT-M ; ! "Qasımalıy" (Arabic)
Қасымбай:Қасымбай NP-ANT-M ; ! "Qasımbay" (Arabic)
Қасымбек:Қасымбек NP-ANT-M ; ! "Qasımbek" (Arabic)
Қасымбеков:Қасымбеков NP-COG-OB ; ! ""
Қасымқан:Қасымқан NP-ANT-M ; ! "Qasımqan" (Arabic)
Қасым:Қасым NP-ANT-M ; ! "Qasım" (Arabic)
Қасымқұл:Қасымқұл NP-ANT-M ; ! "Qasımqul" (Arabic)
Қасымов:Қасымов NP-COG-OB ; ! ""
Қасымтай:Қасымтай NP-ANT-M ; ! "Qasımtay" (Arabic)
Қатира:Қатира NP-ANT-F ; ! "Qatıyra" (Arabic)
Қатиф:Қатиф NP-ANT-M ; ! "Qatif" (Arabic)
Қатша:Қатша NP-ANT-F ; ! ""
Қаһарман:Қаһарман NP-ANT-M ; ! "Qaharman" (Arabic)
Қашаубаев:Қашаубаев NP-COG-OB ; ! ""
Қирабаев:Қирабаев NP-COG-OB ; ! ""
Қирабай:Қирабай NP-ANT-M ; ! "Qıyrabay" (Arabic)
Қиса:Қиса NP-ANT-M ; ! "Qıysa" 
Қисан:Қисан NP-ANT-M ; ! "Qıysan" (Arabic)
Қияс:Қияс NP-ANT-M ; ! "Qıyas" (Arabic)
ҚМЭБИ:ҚМЭБИ NP-ORG-ABBR ; ! "KIMEP"
Қобдабай:Қобдабай NP-ANT-M ; ! "Qobdabay" (Kazakh)
Қобыланды:Қобыланды NP-ANT-M ; ! "Qobılandı" (Kazakh)
Қоғамбай:Қоғамбай NP-ANT-M ; ! "Qoğambay" (Kazakh)
Қодар:Қодар NP-ANT-M ; ! ""
Қожабай:Қожабай NP-ANT-M ; ! "Qojabay" (Arabic)
Қожабек:Қожабек NP-ANT-M ; ! "Qojabek" (Arabic)
Қожагелді:Қожагелді NP-ANT-M ; ! "Qojageldi" (Arabic)
Қожақ:Қожақ NP-ANT-M ; ! "Qojaq" (Greek)
Қожа:Қожа NP-ANT-M ; ! "Qoja" (Arabic)
Қожамжаров:Қожамжаров NP-COG-OB ; ! ""
Қожамқұлов:Қожамқұлов NP-COG-OB ; ! ""
Қожанасыр:Қожанасыр NP-ANT-M ; ! ""
Қожанияз:Қожанияз NP-ANT-M ; ! "Qojanıyaz" (Arabic)
Қожанов:Қожанов NP-COG-OB ; ! ""
Қожатай:Қожатай NP-ANT-M ; ! "Qojatay" (Arabic)
Қожахмет:Қожахмет NP-ANT-M ; ! "Qojaxmet" (Arabic)
Қожбан:Қожбан NP-ANT-M ; ! "Qojban" (Greek)
Қожеке:Қожеке NP-ANT-M ; ! "Qojeke" (version of Qoja)
Қожықов:Қожықов NP-COG-OB ; ! ""
Қозбағаров:Қозбағаров NP-COG-OB ; ! ""
Қозыбағар:Қозыбағар NP-ANT-M ; ! "Qozıbağar" (Kazakh)
Қозыбай:Қозыбай NP-ANT-M ; ! "Qozıbay" (Kazakh)
Қозыбақ:Қозыбақ NP-ANT-M ; ! ""
Қозыбасы:Қозыбасы NP-TOP ; ! ""
Қозы:Қозы NP-ANT-M ; ! "Qozı" (Kazakh)
Қойгелдиев:Қойгелдиев NP-COG-OB ; ! ""
Қойлыбаев:Қойлыбаев NP-COG-M ; !"Use/MT"
Қойлыбай:Қойлыбай NP-ANT-M ; ! "Qoylıbay" (Kazakh)
Қойлышев:Қойлышев NP-COG-OB ; ! ""
Қоқан:Қоқан NP-TOP ; !"Use/MT"
Қолдас:Қолдас NP-ANT-M ; ! "Qoldas" (Kazakh)
Қонаев:Қонаев NP-COG-OB ; ! ""
Қонақай:Қонақай NP-ANT-M ; ! ""
Қонақбаев:Қонақбаев NP-COG-OB ; ! "" ! Use/MT
Қонақбай:Қонақбай NP-ANT-M ; ! "Qonaqbay" 
Қоңыр%-көкше:Қоңыр%-көкше NP-TOP ; ! ""
Қорабаев:Қорабаев NP-COG-OB ; ! ""
Қоразбаев:Қоразбаев NP-COG-OB ; ! "" ! Use/MT
Қоразбаев:Қоразбаев NP-COG-OB ; ! "" ! Use/MT
Қоран:Қоран NP-ORG ; ! ""
Қорғанбек:Қорғанбек NP-ANT-M ; ! "Qorğanbek" (Kazakh)
Қорқытов:Қорқытов NP-COG-M ; !"Use/MT"
Қорлан:Қорлан NP-ANT-F ; ! "Qorlan" (Arabic)
Қосай:Қосай NP-ANT-M ; ! "Qosay" (Kazakh)
Қосанов:Қосанов NP-COG-OB ; ! ""
Қосанов:Қосанов NP-COG-OB ; ! ""
Қосман:Қосман NP-ANT-M ; ! ""
Қостанай:Қостанай NP-TOP ; ! ""
Қосымбаев:Қосымбаев NP-COG-OB ; ! ""
Қосынбай:Қосынбай NP-ANT-M ; ! "Qosınbay" 
Қосын:Қосын NP-ANT-M ; ! "Qosın" (Arabic)
Қошқарбаев:Қошқарбаев NP-COG-OB ; ! ""
Қошқарбай:Қошқарбай NP-ANT-M ; ! "Qoshqarbay" (Kazakh)
ҚР:ҚР NP-TOP-ABBR ; ! "Republic of Kazakhstan"
Қуандықов:Қуандықов NP-COG-OB ; ! ""
Қуан:Қуан NP-ANT-M ; ! "Qıwan" (Kazakh)
Қуанышбаев:Қуанышбаев NP-COG-OB ; ! ""
Қуанышбек:Қуанышбек NP-ANT-M ; ! "" ! Use/MT
Қуанышев:Қуанышев NP-COG-OB ; ! "" ! Use/MT
Қуат:Қуат NP-ANT-M ; ! "Qıwat" (Kazakh)
Қуатұлы:Қуатұлы NP-COG-M ; ! "USE/MT"
Қуқмара:Қуқмара NP-TOP ; ! ""
Құбажон:Құбажон NP-ANT-M ; !"Use/MT"
Құбайжан:Құбайжан NP-ANT-M ; ! "Qubayjan" (Kazakh)
Құбан:Құбан NP-ANT-M ; ! "Quban" (Persian)
Құдабаев:Құдабаев NP-COG-OB ; ! ""
Құдайберген:Құдайберген NP-ANT-M ; ! "Qudaybergen" (Kazakh)
Құдайбергенов:Құдайбергенов NP-COG-OB ; ! ""
Құдайберді:Құдайберді NP-ANT-M ; ! "Qudayberdi"
Құдайқұл:Құдайқұл NP-ANT-M ; ! "Qudayqul" (Kazakh)
Құдайқұлов:Құдайқұлов NP-COG-OB ; ! ""
Құдайменде:Құдайменде NP-ANT-M ; ! "Qudaymende" (Kazakh)
Құдси:Құдси NP-ANT-M ; !"Use/MT"
Құдыс:Құдыс NP-TOP ; ! ""
Құзыкейұлы:Құзыкейұлы NP-COG-M ; !"Use/MT"
Құланов:Құланов NP-COG-OB ; ! ""
Құлахметов:Құлахметов NP-COG-OB ; ! "" ! Use/MT
Құлбаев:Құлбаев NP-COG-OB ; ! "" ! Use/MT
Құлекеев:Құлекеев NP-COG-OB ; ! "" ! Use/MT
Құлжа:Құлжа NP-TOP ; ! ""
Құлманбаев:Құлманбаев NP-COG-OB ; ! ""
Құлманов:Құлманов NP-COG-OB ; ! ""
Құлов:Құлов NP-COG-OB ; ! ""
Құлсариев:Құлсариев NP-COG-OB ; ! ""
Құлсары:Құлсары NP-TOP ; ! ""
Құлыбаев:Құлыбаев NP-COG-OB ; ! ""
Құлыншақ:Құлыншақ NP-ANT-M ; ! "Qulınshaq" (Kazakh)
Құлышев:Құлышев NP-COG-OB ; ! ""
Құмар:Құмар NP-ANT-M ; ! "Qumar" (Arabic)
Құнанбаев:Құнанбаев NP-COG-OB ; ! ""
Құнанбай:Құнанбай NP-ANT-M ; ! "Qunanbay"
Құнанбай:Құнанбай NP-COG-MF ; ! "Qunanbay"
Құнанбайұлы:Құнанбайұлы NP-COG-M ; ! ""
Құндыз:Құндыз NP-ANT-F ; ! "Qundız" (Kazakh)
Құныскерей:Құныскерей NP-ANT-M ; ! ""
Құралай:Құралай NP-ANT-F ; ! "Quralay" (Kazakh)
Құралбек:Құралбек NP-ANT-M ; ! "Quralbek" (Kazakh)
Құрманәлі:Құрманәлі NP-ANT-M ; ! "Qurmanäli" (Arabic)
Құрманбаев:Құрманбаев NP-COG-OB ; ! ""
Құрманбай:Құрманбай NP-ANT-M ; ! "Qurmanbay" (Arabic)
Құрманбек:Құрманбек NP-ANT-M ; ! "Qurmanbek" (Arabic)
Құрманбеков:Құрманбеков NP-COG-OB ; ! ""
Құрманғазы:Құрманғазы NP-ANT-M ; ! "Qurmanğazı" (Arabic)
Құрманғалиев:Құрманғалиев NP-COG-OB ; ! ""
Құрманғали:Құрманғали NP-ANT-M ; ! "Qurmanğalıy" (Arabic)
Құрманқұлов:Құрманқұлов NP-COG-OB ; ! ""
Құрман:Құрман NP-ANT-M ; ! "Qurman" (Arabic)
Құрмансейіт:Құрмансейіт NP-ANT-M ; ! "Qurmanseyit" (Arabic)
Құрмантай:Құрмантай NP-ANT-M ; ! "Qurmantay" (Arabic)
Құрмаш:Құрмаш NP-ANT-F ; ! "Qurmaş"  (sometimes M?)
Құрмет:Құрмет NP-ANT-M ; ! "Qurmet" (Arabic)
Құрмыш:Құрмыш NP-ANT-M ; !"Use/MT"
Құрышжан:Құрышжан NP-ANT-M ; ! "Qurıshjan" (Arabic)
Құрышжанов:Құрышжанов NP-COG-OB ; ! ""
Құсаинов:Құсаинов NP-COG-OB ; ! ""
Құсайын:Құсайын NP-ANT-M ; ! "Qusayın" (Arabic)
Құсайынов:Құсайынов NP-COG-OB ; ! ""
Құсайынов:Құсайынов NP-COG-OB ; ! ""
Құтел:Құтел NP-ANT-M ; ! "Qutel" (Arabic)
Құтлык:Құтлык NP-ANT-M ; !"Use/MT"
Құтлық:Құтлық NP-ANT-M ; !"Use/MT"
Құттыбай:Құттыбай NP-ANT-M ; ! "Quttıbay" (Arabic)
Құттымұхамбет:Құттымұхамбет NP-ANT-M ; ! ""
ҚХР:ҚХР%{а%} NP-TOP-ABBR ; ! "PRC"
Қыдырәлі:Қыдырәлі NP-ANT-M ; ! "Qıdıräli" (Arabic)
Қыдырбай:Қыдырбай NP-ANT-M ; ! "Qıdırbay" (Arabic)
Қыдырбек:Қыдырбек NP-ANT-M ; ! "Qıdırbek" (Arabic)
Қыдырбеков:Қыдырбеков NP-COG-OB ; ! ""
Қыдырғали:Қыдырғали NP-ANT-M ; ! "Qıdırğalıy" (Arabic)
Қыдыр:Қыдыр NP-ANT-M ; ! "Qıdır" (Arabic)
Қызғалдақ:Қызғалдақ NP-ANT-F ; ! "Qızğaldaq" (Kazakh)
Қызылқұм:Қызылқұм NP-TOP ; ! ""
Қызылқұм:Қызылқұм NP-TOP ; !"Use/MT"
Қызылорда:Қызылорда NP-TOP ; ! ""
Қызылту:Қызылту NP-TOP ; ! ""
Қымбат:Қымбат NP-ANT-F ; ! "Qımbat" (Kazakh)
Қырғыз% АССР:Қырғыз% АССР%{э%}%{й%} NP-TOP-ASSR ;
Қырғыз% КСР:Қырғыз% КСР%{э%}%{й%} NP-TOP-ASSR ;
Қырғыз% ССР:Қырғыз% ССР%{э%}%{й%} NP-TOP-ASSR ;
Қырғызстан:Қырғызстан NP-TOP ; ! "Kyrgyzstan" 
Қырмызы:Қырмызы NP-ANT-F ; ! "Qırmızı" (Kazakh)
Қырықбаев:Қырықбаев NP-COG-OB ; ! ""
Қырымбек:Қырымбек NP-ANT-M ; !"Use/MT"
Қытай:Қытай NP-TOP ; ! "China"
Лoрбрульгруд:Лoрбрульгруд NP-TOP ; ! "Lorbrwlgrwd"
Лаврентьев:Лаврентьев NP-COG-OB ; ! ""
Лавриненко:Лавриненко NP-COG-MF ; ! ""
Лавров:Лавров NP-COG-OB ; ! ""
Лавуазье:Лавуазье NP-COG-MF ; !"Use/MT"
Лада:Лада NP-AL ; ! ""
Ладога:Ладога NP-TOP ; ! ""
Ладыков:Ладыков NP-COG-OB ; ! ""
Лазарев:Лазарев NP-COG-OB ; ! ""
Лазаренко:Лазаренко NP-COG-MF ; ! ""
Лаз:Лаз NP-ANT-M ; !"Use/MT"
Лазым:Лазым NP-ANT-F ; ! "Lazım" (Arabic)
Лайд:Лайд NP-ANT-M ; !"Use/MT"
Лайс:Лайс NP-ANT-M ; !"Use/MT"
Лайыш:Лайыш NP-TOP ; ! ""
Лакачев:Лакачев NP-COG-OB ; ! ""
Лакачов:Лакачов NP-COG-OB ; ! ""
Лак:Лак NP-ANT-M ; !"Use/MT"
Ла:Ла NP-ANT-M ; !"Use/MT"
Лала:Лала NP-ANT-F ; ! "Lala" (Arabic)
Ламаев:Ламаев NP-COG-OB ; ! ""
Ла%-Манш:Ла%-Манш NP-TOP ; ! "English Channel"
Ламберт:Ламберт NP-COG-MF ; !"Use/MT"
Ламберто:Ламберто NP-ANT-M ; !"Use/MT"
Ламмерт:Ламмерт NP-ANT-M ; !"Use/MT"
Лана:Лана NP-ANT-F ; !"Use/MT"
Ланкастер:Ланкастер NP-TOP ; ! ""
Лаос:Лаос NP-TOP ; ! ""
Лапаев:Лапаев NP-COG-OB ; ! ""
Лапин:Лапин NP-COG-IN ; ! ""
Ла%-Плата:Ла%-Плата NP-TOP ; ! ""
Лаптев:Лаптев NP-COG-OB ; ! ""
Лара:Лара NP-ANT-F ; !"Use/MT"
Ларин:Ларин NP-COG-IN ; ! ""
Ларионов:Ларионов NP-COG-OB ; ! ""
Лариса:Лариса NP-ANT-F ; ! "Larıysa" (Greek)
Ларри:Ларри NP-ANT-M ; !"Use/MT"
Ларс:Ларс NP-ANT-M ; !"Use/MT"
Латвия:Латвия NP-TOP ; ! "Latvia"
Латифа:Латифа NP-ANT-F ; ! "Latifa" (Arabic)
Латиф:Латиф NP-ANT-M ; ! "Latif" (Arabic)
Латыпов:Латыпов NP-COG-OB ; ! ""
Лаула:Лаула NP-ANT-M ; ! "Lawla" (Kazakh)
Лаура:Лаура NP-ANT-F ; !"Use/MT"
Лацио:Лацио NP-TOP ; !"Use/MT"
Лашын:Лашын NP-ANT-F ; ! "Lashın" (Kazakh)
Ләбиба:Ләбиба NP-ANT-F ; ! "Läbiyba" (Arabic)
Ләззат:Ләззат NP-ANT-F ; ! "Läzzat" (Arabic)
Ләзиза:Ләзиза NP-ANT-F ; ! "Läziyza" (Arabic)
Ләйла:Ләйла NP-ANT-F ; ! "Läyla" (Arabic)
Ләшкербек:Ләшкербек NP-ANT-M ; ! "Läshkerbek" (Kazakh)
Ләшкер:Ләшкер NP-ANT-M ; ! "Läshker" (Arabic)
Леандра:Леандра NP-ANT-F ; !"Use/MT"
Лебедев:Лебедев NP-COG-OB ; ! ""
Леб:Леб NP-ANT-M ; !"Use/MT"
Леви:Леви NP-ANT-M ; !"Use/MT"
Левин:Левин NP-COG-IN ; ! ""
Лев:Лев NP-ANT-M ; ! ""
Левчаев:Левчаев NP-COG-OB ; ! ""
Левченко:Левченко NP-COG-MF ; ! ""
Ледо:Ледо NP-COG-MF ; !"Use/MT"
Лейла:Лейла NP-ANT-F ; !"Use/MT"
Лейпциг:Лейпциг NP-TOP ; ! ""
Лека:Лека NP-ANT-F ; !"Use/MT"
Леканов:Леканов NP-COG-OB ; ! ""
Лекер:Лекер NP-ANT-M ; ! "Leker" (Persian)
Лекеров:Лекеров NP-COG-OB ; ! ""
Лексус:Лексус NP-ANT-F ; !"Use/MT"
Ле:Ле NP-ANT-M ; !"Use/MT"
Лелюшенко:Лелюшенко NP-COG-MF ; ! ""
Лем:Лем NP-ANT-M ; !"Use/MT"
Лена:Лена NP-ANT-F ; ! ""
Ленар:Ленар NP-ANT-M ; ! "Lenar" (Arabic)
Ленинград:Ленинград NP-TOP ; ! ""
Ленин:Ленин NP-COG-IN ; ! ""
Лениногорск:Лениногорск NP-TOP ; ! ""
Ленкоран:Ленкоран NP-TOP ; ! ""
Леннон:Леннон NP-COG-MF ; !"Use/MT"
Леньков:Леньков NP-COG-OB ; ! ""
Лео:Лео NP-ANT-M ; !"Use/MT"
Леонард:Леонард NP-ANT-M ; !"Use/MT"
Леонардо:Леонардо NP-ANT-M ; ! ""
Леоне:Леоне NP-ANT-M ; !"Use/MT"
Леонид:Леонид NP-ANT-M ; ! ""
Леон:Леон NP-ANT-M ; !"Use/MT"
Леонов:Леонов NP-COG-OB ; ! ""
Леонора:Леонора NP-ANT-F ; !"Use/MT"
Леонтьев:Леонтьев NP-COG-OB ; ! ""
Леопольд:Леопольд NP-ANT-M ; ! ""
Лермонтов:Лермонтов NP-COG-OB ; ! ""
Лесков:Лесков NP-COG-OB ; ! ""
Лесото:Лесото NP-TOP ; ! ""
Лессинг:Лессинг NP-COG-MF ; ! ""
Летавка:Летавка NP-TOP ; ! ""
Лех:Лех NP-ANT-M ; ! ""
Лещенко:Лещенко NP-COG-MF ; ! ""
Лиана:Лиана NP-ANT-F ; !"Use/MT"
Либерия:Либерия NP-ANT-F ; !"Use/MT"
Либерия:Либерия NP-TOP ; ! ""
Либерман:Либерман NP-COG-M ; ! ""
Либия:Либия NP-TOP ; ! ""
Либ:Либ NP-ANT-M ; !"Use/MT"
Ливан:Ливан NP-TOP ; ! ""
Ливенворт:Ливенворт NP-TOP ; !"Use/MT"
Ливия:Ливия NP-TOP ; !"Use/MT"
Ливония:Ливония NP-TOP ; ! ""
Лигурия:Лигурия NP-TOP ; !"Use/MT"
Лидия:Лидия NP-ANT-F ; ! ""
Лидс:Лидс NP-TOP ; ! ""
Лиза:Лиза NP-ANT-F ; !"Use/MT"
Лила:Лила NP-ANT-F ; !"Use/MT"
Ли:Ли NP-ANT-M ; !"Use/MT"
Ли:Ли NP-COG-MF ; ! ""
Лилиана:Лилиана NP-ANT-F ; !"Use/MT"
Лилиан:Лилиан NP-ANT-F ; ! ""
Лилия:Лилия NP-ANT-F ; ! ""
Лилли:Лилли NP-ANT-F ; !"Use/MT"
Лима:Лима NP-TOP ; ! "" 
Лина:Лина NP-ANT-F ; !"Use/MT"
Линда:Линда NP-ANT-F ; !"Use/MT"
Линдсей:Линдсей NP-ANT-F ; !"Use/MT"
Линдси:Линдси NP-ANT-F ; !"Use/MT"
Линкольн:Линкольн NP-ANT-M ; !"Use/MT"
Линкольн:Линкольн NP-COG-MF ; ! ""
Линней:Линней NP-COG-MF ; ! ""
Лино:Лино NP-ANT-M ; !"Use/MT"
Линч:Линч NP-COG-MF ; !"Use/MT"
Лионель:Лионель NP-ANT-M ; !"Use/MT"
Лион:Лион NP-COG-MF ; !"Use/MT"
Лисаков:Лисаков NP-COG-OB ; ! ""
Лисбет:Лисбет NP-ANT-F ; !"Use/MT"
Лиссабон:Лиссабон NP-TOP ; ! "" 
Литбел:Литбел NP-ANT-M ; !"Use/MT"
Литва:Литва NP-TOP ; ! "Lithuania"
Литвинов:Литвинов NP-COG-OB ; ! ""
Лихарев:Лихарев NP-COG-OB ; ! ""
Лихачёв:Лихачёв NP-COG-OB ; ! ""
Лихтенштейн:Лихтенштейн NP-TOP ; ! ""
Лицина:Лицина NP-ANT-F ; !"Use/MT"
Лия:Лия NP-ANT-F ; !"Use/MT"
Ллоидз:Ллоидз NP-TOP ; !"Use/MT"
Ллойд:Ллойд NP-COG-MF ; !"Use/MT"
Лобамба:Лобамба NP-TOP ; ! "" 
Лобанов:Лобанов NP-COG-OB ; ! ""
Лобачев:Лобачев NP-COG-OB ; ! ""
Лобков:Лобков NP-COG-OB ; ! ""
Лованов:Лованов NP-COG-OB ; ! ""
Логвиненко:Логвиненко NP-COG-MF ; ! ""
Логинов:Логинов NP-COG-OB ; ! ""
Логунов:Логунов NP-COG-OB ; ! ""
Лоис:Лоис NP-ANT-M ; !"Use/MT"
Лойола:Лойола NP-COG-MF ; !"Use/MT"
Локк:Локк NP-COG-MF ; ! ""
Локтев:Локтев NP-COG-OB ; ! ""
Лола:Лола NP-ANT-F ; !"Use/MT"
Лолита:Лолита NP-ANT-F ; !"Use/MT"
Ло:Ло NP-COG-MF ; !"Use/MT"
Ломбарди:Ломбарди NP-COG-MF ; ! "USE/MT"
Ломбардия:Ломбардия NP-TOP ; !"Use/MT"
Ломов:Ломов NP-COG-OB ; ! ""
Ломоносов:Ломоносов NP-COG-OB ; ! ""
Лондиниум:Лондиниум NP-TOP ; !""
Лондон:Лондон NP-TOP ; ! "London"
Лопе:Лопе NP-ANT-M ; ! ""
Лопес:Лопес NP-COG-MF ; !"Use/MT"
Лора:Лора NP-ANT-F ; ! "Lora" (New word)
Лоран:Лоран NP-ANT-M ; !"Use/MT"
Лорена:Лорена NP-ANT-F ; !"Use/MT"
Лорен:Лорен NP-ANT-F ; !"Use/MT"
Лоренс:Лоренс NP-ANT-M ; !"Use/MT"
Лоренцо:Лоренцо NP-ANT-M ; !"Use/MT"
Лос%-Анджелес:Лос%-Анджелес NP-TOP ; ! ""
Лосманов:Лосманов NP-COG-OB ; ! ""
Лотар:Лотар NP-ANT-M ; !"Use/MT"
Лотышев:Лотышев NP-COG-OB ; ! ""
Луанда:Луанда NP-TOP ; !""
Луара:Луара NP-ANT-F ; ! ""
Луара:Луара NP-TOP ; ! ""
Лувр:Лувр NP-ORG ; !"Use/MT"
Лужков:Лужков NP-COG-OB ; ! "" ! Use/MT
Луиджи:Луиджи NP-ANT-M ; !"Use/MT"
Луиза:Луиза NP-ANT-F ; ! ""
Луиза:Луиза NP-TOP ; ! ""
Луизиана:Луизиана NP-TOP ; ! ""
Луи:Луи NP-ANT-M ; ! ""
Луис:Луис NP-ANT-M ; ! ""
Лука:Лука NP-ANT-M ; !"Use/MT"
Лукас:Лукас NP-ANT-M ; !"Use/MT"
Лукашенко:Лукашенко NP-COG-MF ; ! ""
Лукиан:Лукиан NP-ANT-M ; !"Use/MT"
Лукианов:Лукианов NP-COG-OB ; ! ""
Лукин:Лукин NP-COG-IN ; ! ""
Лукиянов:Лукиянов NP-COG-OB ; ! ""
Лукойл:Лукойл NP-ORG ; ! ""
Лукоянов:Лукоянов NP-COG-OB ; ! ""
Лукьяненко:Лукьяненко NP-COG-MF ; ! ""
Лукьянов:Лукьянов NP-COG-OB ; ! ""
Лула:Лула NP-ANT-F ; !"Use/MT"
Лу:Лу NP-ANT-M ; !"Use/MT"
Луначарский:Луначарский NP-COG-M ; ! ""
Лупов:Лупов NP-COG-OB ; ! ""
Лупо:Лупо NP-ANT-M ; !"Use/MT"
Лурдес:Лурдес NP-ANT-F ; !"Use/MT"
Лусака:Лусака NP-TOP ; ! "" 
Лут:Лут NP-ANT-M ; ! ""
Луферов:Луферов NP-COG-OB ; ! ""
Луценко:Луценко NP-COG-MF ; ! ""
Лучано:Лучано NP-ANT-M ; !"Use/MT"
Лұқпан:Лұқпан NP-ANT-M ; ! "Luqpan" (Arabic)
Лұт:Лұт NP-ANT-M ; ! ""
Лхаса:Лхаса NP-TOP ; !""
Лысенко:Лысенко NP-COG-MF ; ! ""
Лыткин:Лыткин NP-COG-IN ; ! ""
Львов:Львов NP-TOP ; ! ""
Льгов:Льгов NP-COG-OB ; ! ""
Льюис:Льюис NP-ANT-M ; ! ""
Лэйла:Лэйла NP-ANT-F ; !"Use/MT"
Лэйн:Лэйн NP-ANT-M ; !"Use/MT"
Лэндон:Лэндон NP-ANT-M ; !"Use/MT"
Люба:Люба NP-ANT-F ; ! ""
Любезнов:Любезнов NP-COG-OB ; ! ""
Любимов:Любимов NP-COG-OB ; ! ""
Люблин:Люблин NP-TOP ; ! ""
Любляна:Любляна NP-TOP ; ! "" 
Любовь:Любовь NP-ANT-F ; ! ""
Людмила:Людмила NP-ANT-F ; ! ""
Людовик:Людовик NP-ANT-M ; ! ""
Людовь:Людовь NP-ANT-F ; ! ""
Люк:Люк NP-ANT-M ; !"Use/MT"
Люксембург:Люксембург NP-TOP ; ! ""
Лю:Лю NP-COG-MF ; ! ""
Люси:Люси NP-ANT-F ; !"Use/MT"
Люсия:Люсия NP-ANT-F ; !"Use/MT"
Люсье:Люсье NP-ANT-F ; !"Use/MT"
Люсьен:Люсьен NP-ANT-M ; !"Use/MT"
Ля:Ля NP-ANT-M ; !"Use/MT"
Ляньюньган:Ляньюньган NP-TOP ; ! ""
Ляпунов:Ляпунов NP-COG-OB ; ! ""
Ляхов:Ляхов NP-COG-OB ; ! ""
Мавлоний:Мавлоний NP-COG-MF ; ! ""
Маврикий:Маврикий NP-TOP ; !"Use/MT"
Мавритания:Мавритания NP-TOP ; ! ""
Магадан:Магадан NP-TOP ; ! ""
Магдалена:Магдалена NP-ANT-F ; !"Use/MT"
Магда:Магда NP-ANT-F ; !"Use/MT"
Магдиев:Магдиев NP-COG-OB ; ! ""
Магеллан:Магеллан NP-COG-MF ; ! ""
Магомаев:Магомаев NP-COG-OB ; ! ""
Магомаев:Магомаев NP-COG-OB ; ! ""
Магриба:Магриба NP-TOP ; !"Use/MT"
Мағаз:Мағаз NP-ANT-M ; ! "Mağaz" (Arabic)
Мағауия:Мағауия NP-ANT-M ; ! "Mağawıya" (Arabic)
Мағжан:Мағжан NP-ANT-M ; ! ""
Мағзұм:Мағзұм NP-ANT-M ; ! "Mağzum" (Arabic)
Мағрипа:Мағрипа NP-ANT-F ; ! "Mağrıypa" (Arabic)
Мағруф:Мағруф NP-ANT-M ; ! "Mağruf" (Arabic)
Мадагаскар:Мадагаскар NP-TOP ; ! ""
Мадейра:Мадейра NP-TOP ; ! ""
Мадина:Мадина NP-ANT-F ; !"Use/MT"
Мадлен:Мадлен NP-ANT-F ; !"Use/MT"
Мадов:Мадов NP-COG-OB ; ! ""
Мадона:Мадона NP-ANT-F ; !"Use/MT"
Мадонна:Мадонна NP-ANT-F ; !"Use/MT"
Мадрид:Мадрид NP-TOP ; ! "Madrid"
Мадумаров:Мадумаров NP-COG-OB ; ! ""
Мадуров:Мадуров NP-COG-OB ; ! ""
Мадхат:Мадхат NP-ANT-M ; ! "Madxat" (Arabic)
Мажарстан:Мажарстан NP-TOP ; ! "Hungary"
Майами:Майами NP-TOP ; !"Use/MT" 
Майбасар:Майбасар NP-ANT-M ; ! ""
Майдан:Майдан NP-ANT-M ; ! "Maydan" (Arabic)
Майкл:Майкл NP-ANT-M ; ! ""
Майк:Майк NP-ANT-M ; !"Use/MT"
Майков:Майков NP-COG-OB ; ! ""
Майкрософт:Майкрософт NP-ORG ; ! ""
Майқайын:Майқайын NP-TOP ; ! ""
Майлыбаев:Майлыбаев NP-COG-OB ; ! ""
Майлы:Майлы NP-ANT-M ; ! "Maylı" (Kazakh)
Майна:Майна NP-ANT-F ; ! "Mayna" (Persian)
Майнц:Майнц NP-TOP ; !"Use/MT"
Майорка:Майорка NP-TOP ; !"Use/MT"
Майор:Майор NP-COG-MF ; !"Use/MT"
Майра:Майра NP-ANT-F ; !"Use/MT"
Майра:Майра NP-ANT-F ; !"Use/MT"
Майса:Майса NP-ANT-F ; ! "Maysa" (Persian)
Майте:Майте NP-ANT-F ; !"Use/MT"
Майя:Майя NP-ANT-F ; !"Use/MT"
Макалис:Макалис NP-COG-MF ; !"Use/MT"
Макао:Макао NP-TOP ; !""
Макаревич:Макаревич NP-COG-M ; ! ""
Макаренко:Макаренко NP-COG-MF ; ! ""
Макаров:Макаров NP-COG-OB ; ! ""
Макау:Макау NP-TOP ; ! ""
Макашарипов:Макашарипов NP-COG-OB ; ! ""
Макашұлы:Макашұлы NP-COG-M ; ! "Use/MT"
Македония:Македония NP-TOP ; ! ""
Македонский:Македонский NP-COG-MF ; ! ""
Макеев:Макеев NP-COG-OB ; ! ""
Маки:Маки NP-ANT-F ; !"Use/MT"
Макинск:Макинск NP-TOP ; ! ""
Маккейн:Маккейн NP-COG-MF ; !"Use/MT"
Мак:Мак NP-ANT-M ; !"Use/MT"
Максименко:Максименко NP-COG-MF ; ! ""
Максим:Максим NP-ANT-M ; ! ""
Максимов:Максимов NP-COG-OB ; ! ""
Максимус:Максимус NP-ANT-M ; !"Use/MT"
Макс:Макс NP-ANT-M ; ! ""
Максутов:Максутов NP-COG-OB ; ! ""
Мақабыл:Мақабыл NP-ANT-M ; ! "Maqabıl" (Arabic)
Мақатаев:Мақатаев NP-COG-OB ; ! ""
Мақат:Мақат NP-TOP ; ! ""
Мақашұлы:Мақашұлы NP-COG-M ; ! "Use/MT"
Мақпал:Мақпал NP-ANT-F ; ! "Maqpal" (Arabic)
Мақсат:Мақсат NP-ANT-M ; ! "Maqsat" (Arabic)
Мақсуди:Мақсуди NP-COG-MF ; ! ""
Мақсұд:Мақсұд NP-ANT-M ; ! ""
Мақсұм:Мақсұм NP-ANT-M ; ! "Maqsum" (Arabic)
Мақсұт:Мақсұт NP-ANT-M ; ! "" ! Use/MT
Мақсұтұлы:Мақсұтұлы NP-COG-M ; ! "USE/MT"
Мақтымқұлы:Мақтымқұлы NP-COG-M ; !"Use/MT"
Мақыш:Мақыш NP-ANT-M ; ! ""
Малабо:Малабо NP-TOP ; ! "" 
Малави:Малави NP-TOP ; !"Use/MT"
Малага:Малага NP-TOP ; !"Use/MT"
Малайзия:Малайзия NP-TOP ; ! ""
Малайзия:Малайзия NP-TOP ; ! ""
Малакка:Малакка NP-TOP ; ! ""
Малахов:Малахов NP-COG-OB ; ! ""
Малбай:Малбай NP-ANT-M ; ! "Malbay" (Kazakh)
Малдыбаев:Малдыбаев NP-COG-OB ; ! ""
Малеев:Малеев NP-COG-OB ; ! ""
Малжан:Малжан NP-ANT-M ; ! "Maljan" (Kazakh)
Мали:Мали NP-TOP ; ! ""
Малкольм:Малкольм NP-ANT-M ; !"Use/MT"
Мал:Мал NP-ANT-M ; !"Use/MT"
Малов:Малов NP-COG-OB ; ! ""
Малышев:Малышев NP-COG-OB ; ! ""
Мальдивалар:Мальдивалар NP-TOP ; ! ""
Мальдив% аралдары:Мальдив% аралдар NP-TOP-COMPOUND ; ! ""
Мальдив% арал:Мальдив% арал N-COMPOUND-PX ; ! "Maldive Islands"
Мальта:Мальта NP-TOP ; ! ""
Мальцев:Мальцев NP-COG-OB ; ! ""
Ма:Ма NP-ANT-M ; !"Use/MT"
Мамадыш:Мамадыш NP-TOP ; ! ""
Мамаев:Мамаев NP-COG-OB ; ! ""
Мамажан:Мамажан NP-ANT-M ; ! "Mamajan" (Kazakh)
Мамай:Мамай NP-ANT-M ; ! ""
Маман:Маман NP-ANT-M ; ! "Maman" (Kazakh)
Мамасадық:Мамасадық NP-ANT-M ; ! "Mamasadıq" (Arabic)
Мамат:Мамат NP-ANT-M ; ! "Mamat" (Arabic)
Мамашев:Мамашев NP-COG-OB ; ! "" ! Use/MT
Мамаш:Мамаш NP-ANT-M ; !"Use/MT"
Мамбетказиев:Мамбетказиев NP-COG-M ; !"Use/MT"
Мамедов:Мамедов NP-COG-OB ; ! ""
Мам:Мам NP-ANT-M ; !"Use/MT"
Мамонтов:Мамонтов NP-COG-OB ; ! ""
Мамуржон:Мамуржон NP-ANT-M ; ! ""
Мамыраев:Мамыраев NP-COG-OB ; ! ""
Мамырқазов:Мамырқазов NP-COG-OB ; ! ""
Мамыр:Мамыр NP-ANT-M ; ! "Mamır" (Kazakh)
Мамырханов:Мамырханов NP-COG-OB ; ! ""
Мамытбеков:Мамытбеков NP-COG-OB ; ! ""
Мамыт:Мамыт NP-ANT-M ; ! "Mamıt" (Arabic)
Манагуа:Манагуа NP-TOP ; ! "" 
Манан:Манан NP-ANT-M ; ! "Manan" (Arabic)
Манап:Манап NP-ANT-M ; ! "Manap" (Arabic)
Манапов:Манапов NP-COG-OB ; ! ""
Манар:Манар NP-ANT-M ; ! "Manar" (Arabic)
Манаса:Манаса NP-ANT-M ; ! ""
Манасбай:Манасбай NP-ANT-M ; ! "Manasbay" (Kazakh)
Манас:Манас NP-ANT-M ; ! ""
Манат:Манат NP-ANT-F ; ! "Manat" (Kazakh)
Мандела:Мандела NP-COG-MF ; !"Use/MT"
Манзура:Манзура NP-ANT-F ; ! "Manzıwra" (Arabic)
Манила:Манила NP-TOP ; !"Use/MT"
Манко:Манко NP-COG-MF ; ! ""
Манола:Манола NP-ANT-F ; !"Use/MT"
Маноло:Маноло NP-ANT-M ; !"Use/MT"
Мансур:Мансур NP-ANT-M ; ! ""
Мансуров:Мансуров NP-COG-OB ; ! ""
Мансұр:Мансұр NP-ANT-M ; ! "Mansur" (Arabic)
Мансұров:Мансұров NP-COG-OB ; ! "" ! Use/MT
Ману:Ману NP-ANT-M ; !"Use/MT"
Мануэла:Мануэла NP-ANT-F ; !"Use/MT"
Мануэль:Мануэль NP-ANT-M ; !"Use/MT"
Манхэттен:Манхэттен NP-TOP ; !"Use/MT"
Манчестер:Манчестер NP-TOP ; !"Use/MT"
Манчестер% Юнайтед:Манчестер% Юнайтед NP-ORG ; ! "Manchester United Football Club"
Маньчжурия:Маньчжурия NP-TOP ; !""
Маңғыстау:Маңғыстау NP-TOP ; ! ""
Мао:Мао NP-ANT-M ; !"Use/MT"
Мапуту:Мапуту NP-TOP ; ! "" 
Марабай:Марабай NP-ANT-M ; ! "Marabay" (Kazakh)
Марадона:Марадона NP-COG-IN ; ! ""
Маракана:Маракана NP-AL ; ! ""
Маралбай:Маралбай NP-ANT-M ; ! "Maralbay" (Kazakh)
Мара:Мара NP-ANT-F ; !"Use/MT"
Мараньян:Мараньян NP-TOP ; !"Use/MT"
Марат:Марат NP-ANT-M ; ! "Marat" (Arabic)
Маргарет:Маргарет NP-ANT-F ; !"Use/MT"
Маргарита:Маргарита NP-ANT-F ; ! "Margarıyta" 
Маргерит:Маргерит NP-ANT-F ; !"Use/MT"
Марго:Марго NP-ANT-F ; !"Use/MT"
Марғұлан:Марғұлан NP-COG-MF ; ! ""
Мардан:Мардан NP-ANT-M ; ! "Mardan" (Arabic)
Маресьев:Маресьев NP-COG-OB ; ! ""
Маржан:Маржан NP-ANT-F ; ! "Marjan" (Kazakh)
Марзия:Марзия NP-ANT-F ; ! "Marzıya" (Arabic)
Мариана:Мариана NP-ANT-F ; !"Use/MT"
Мариан:Мариан NP-ANT-F ; !"Use/MT"
Марианна:Марианна NP-ANT-F ; !"Use/MT"
Мариано:Мариано NP-ANT-M ; !"Use/MT"
Мариетта:Мариетта NP-ANT-F ; !"Use/MT"
Мари:Мари NP-ANT-F ; ! ""
Марина:Марина NP-ANT-F ; ! "Marıyna" (Latin)
Марин:Марин NP-ANT-M ; !"Use/MT"
Марин:Марин NP-ORG ; ! ""
Марино:Марино NP-ANT-M ; !"Use/MT"
Марино:Марино NP-ANT-M ; !"Use/MT"
Марино:Марино NP-TOP ; !""
Марио:Марио NP-ANT-M ; !"Use/MT"
Марион:Марион NP-ANT-F ; !"Use/MT"
Марита:Марита NP-ANT-F ; !"Use/MT"
Мариус:Мариус NP-ANT-M ; ! ""
Марица:Марица NP-ANT-F ; !"Use/MT"
Мария:Мария NP-ANT-F ; ! "Marıya" (Arabic)
Маркварт:Маркварт NP-ANT-M ; !"Use/MT"
Маркелов:Маркелов NP-COG-OB ; ! ""
Марке:Марке NP-TOP ; !"Use/MT"
Маркес:Маркес NP-ANT-M ; ! ""
Маркес:Маркес NP-COG-MF ; !"Use/MT"
Маркиянов:Маркиянов NP-COG-OB ; ! ""
Марк:Марк NP-ANT-M ; ! ""
Марков:Марков NP-COG-OB ; ! ""
Марко:Марко NP-ANT-M ; !"Use/MT"
Маркос:Маркос NP-ANT-M ; !"Use/MT"
Маркс:Маркс NP-COG-M ; ! ""
Маркус:Маркус NP-ANT-M ; !"Use/MT"
Марқай:Марқай NP-ANT-M ; ! "Marqay" (Kazakh)
Марлен:Марлен NP-ANT-F ; !"Use/MT"
Марлен:Марлен NP-ANT-M ; ! "Marlen" (Arabic)
Марлон:Марлон NP-ANT-M ; !"Use/MT"
Мар:Мар NP-ANT-F ; !"Use/MT"
Марна:Марна NP-TOP ; ! ""
Марокко:Марокко NP-TOP ; ! ""
Марсело:Марсело NP-ANT-M ; !"Use/MT"
Марсель:Марсель NP-ANT-M ; ! ""
Марсель:Марсель NP-TOP ; !"Use/MT"
Марсио:Марсио NP-ANT-M ; !"Use/MT"
Марс:Марс NP-ANT-M ; ! "Mars" (Arabic)
Марс:Марс NP-TOP ; !"Use/MT"
Марта:Марта NP-ANT-F ; ! ""
Мартинес:Мартинес NP-COG-MF ; !"Use/MT"
Мартин:Мартин NP-ANT-M ; ! ""
Мартов:Мартов NP-COG-OB ; ! ""
Мартынов:Мартынов NP-COG-OB ; ! ""
Маруан:Маруан NP-ANT-M ; !"Use/MT"
Маруф:Маруф NP-ANT-M ; ! ""
Марфа:Марфа NP-ANT-F ; !"Use/MT"
Марфуға:Марфуға NP-ANT-F ; ! "Marfuğa" (Arabic)
Мархаба:Мархаба NP-ANT-F ; ! "Marxaba" (Arabic)
Мархабат:Мархабат NP-ANT-M ; ! "Marxabat" (Arabic)
Марчелло:Марчелло NP-ANT-M ; !"Use/MT"
Марченко:Марченко NP-COG-MF ; ! ""
Маршалл% Аралдары:Маршалл% Аралдар NP-TOP-COMPOUND ; ! "Marshall Islands" 
Маршалл:Маршалл NP-ANT-M ; !"Use/MT" "Marshall"
Маршалл:Маршалл NP-TOP ; ! ""
Маршал:Маршал NP-ANT-M ; ! "Marshal" (Arabic)
Маршал:Маршал NP-COG-MF ; ! ""
Масқұт:Масқұт NP-ANT-M ; ! "Masqut" (Arabic)
Масленников:Масленников NP-COG-OB ; ! ""
Маслов:Маслов NP-COG-OB ; ! ""
Масляков:Масляков NP-COG-OB ; ! ""
Мас:Мас NP-ANT-M ; !"Use/MT"
Массачусетс:Массачусетс NP-TOP ; ! ""
Массимо:Массимо NP-ANT-M ; !"Use/MT"
Мастура:Мастура NP-ANT-F ; ! "Mastıwra" (Arabic)
Масуд:Масуд NP-COG-MF ; !"Use/MT"
Масудов:Масудов NP-COG-OB ; ! ""
Матвеевич:Матвее NP-PAT-VICH ; ! ""
Матвеев:Матвеев NP-COG-OB ; ! ""
Матвеенко:Матвеенко NP-COG-MF ; ! ""
Матвиенко:Матвиенко NP-COG-MF ; ! ""
Матиас:Матиас NP-ANT-M ; !"Use/MT"
Матлуба:Матлуба NP-ANT-F ; ! "Matlıwba" (Arabic)
Матросов:Матросов NP-COG-OB ; ! ""
Маттео:Маттео NP-ANT-M ; !"Use/MT"
Маттиас:Маттиас NP-ANT-M ; !"Use/MT"
Маурисио:Маурисио NP-ANT-M ; !"Use/MT"
Маурицио:Маурицио NP-ANT-M ; !"Use/MT"
Мауро:Мауро NP-ANT-M ; !"Use/MT"
Маусымбай:Маусымбай NP-ANT-M ; ! "Mawsımbay" (Kazakh)
Маусымжан:Маусымжан NP-ANT-F ; ! "Mawsımjan" (Arabic)
Махамбет:Махамбет NP-ANT-M ; ! "Maxambet" (Arabic)
Маханбетәжиев:Маханбетәжиев NP-COG-OB ; ! "" ! Use/MT
Махбара:Махбара NP-ANT-F ; ! "Maxbara" (Arabic)
Махбуби:Махбуби NP-ANT-F ; ! "Maxbıwbıy" (Arabic)
Махинур:Махинур NP-ANT-F ; ! "Maxıynıwr" (Persian)
Мах:Мах NP-ANT-M ; !"Use/MT"
Махмет:Махмет NP-ANT-M ; ! "Maxmet" (Arabic)
Махмуд:Махмуд NP-ANT-M ; ! ""
Махмуд:Махмуд NP-COG-MF ; !"Use/MT"
Махмутов:Махмутов NP-COG-OB ; ! ""
Махмұд:Махмұд NP-ANT-M ; ! ""
Махмұт:Махмұт NP-ANT-M ; ! "Maxmut" (Arabic)
Махмұтов:Махмұтов NP-COG-OB ; ! ""
Маху:Маху NP-ANT-M ; ! "Maxıw" (Persian)
Махы:Махы NP-ANT-M ; ! "Maxı" (Persian)
Машанов:Машанов NP-COG-OB ; ! ""
Машеев:Машеев NP-COG-OB ; ! ""
Машкевич:Машкевич NP-COG-MF ; ! ""
Машков:Машков NP-COG-OB ; ! ""
Машрап:Машрап NP-ANT-M ; ! "Mashrap" (Arabic)
Маяков:Маяков NP-COG-OB ; ! ""
Мәдел:Мәдел NP-ANT-M ; ! "Mädel" (Old Turkic)
Мәди:Мәди NP-ANT-M ; ! "Mädiy" (Arabic)
Мәдина:Мәдина NP-ANT-F ; ! "Mädiyna" (Arabic)
Мәдит:Мәдит NP-ANT-M ; ! "Mädiyt" (Arabic)
Мәдхан:Мәдхан NP-ANT-M ; ! "Mädxan" (Arabic)
Мәжгүл:Мәжгүл NP-ANT-M ; ! "Mäjgül" (Persian)
Мәжен:Мәжен NP-ANT-M ; ! "Mäjen" (Arabic)
Мәжит:Мәжит NP-ANT-M ; ! "Mäjiyt" (Arabic)
Мәлика:Мәлика NP-ANT-F ; ! "Mäliyka" (Arabic)
Мәлік:Мәлік NP-ANT-M ; ! "Mälik" (Arabic)
Мәліков:Мәліков NP-COG-OB ; ! "" ! Use/MT
Мәліқайдар:Мәліқайдар NP-ANT-M ; ! "Mäliqaydar" (Arabic)
Мәлім:Мәлім NP-ANT-M ; ! "Mälim" (Arabic)
Мәмбет:Мәмбет NP-ANT-M ; ! "Mämbet" (Arabic)
Мәмбетов:Мәмбетов NP-COG-OB ; ! ""
Мәмбетов:Мәмбетов NP-COG-OB ; ! ""
Мәметов:Мәметов NP-COG-OB ; ! "" ! Use/MT
Мәнзила:Мәнзила NP-ANT-F ; ! "Mänziyla" (Arabic)
Мәнсура:Мәнсура NP-ANT-F ; ! "Mänsiwra" (Arabic)
Мәншүк:Мәншүк NP-ANT-M ; ! ""
Мәриям:Мәриям NP-ANT-F ; ! ""
Мәрмара:Мәрмара NP-ANT-F ; ! "Märmara" (Arabic)
Мәрсеков:Мәрсеков NP-COG-OB ; ! ""
Мәскеу:Мәскеу NP-TOP ; ! "Moscow"
Мәсімбай:Мәсімбай NP-ANT-M ; ! "Mäsimbay" (Arabic)
Мәсімов:Мәсімов NP-COG-M ; !"Use/MT"
Мәсімов:Мәсімов NP-COG-OB ; ! ""
Мәсімов:Мәсімов NP-COG-OB ; ! ""
Мәсімов:Мәсімов NP-COG-OB ; ! "" ! Use/MT
Мәсіх:Мәсіх NP-ANT-M ; ! "Moses"  ! FIXME: check
Мәтжан:Мәтжан NP-ANT-M ; ! "Mätjan" (Arabic)
Мәткәрім:Мәткәрім NP-ANT-M ; ! "Mätkärim" (Arabic)
Мәтқасым:Мәтқасым NP-ANT-M ; ! "Mätqasım" (Arabic)
Мәтхалық:Мәтхалық NP-ANT-M ; ! "Mätxalıq" (Arabic)
Мәтхалық:Мәтхалық NP-ANT-M ; ! "Mätxalıq" (Arabic)
Мәтібәлі:Мәтібәлі NP-ANT-M ; ! "Mätibäli" (Arabic)
Мәтіби:Мәтіби NP-ANT-M ; ! "Mätibiy" (Arabic)
Мәуе:Мәуе NP-ANT-F ; ! "Mäwe" (Arabic)
Мәулен:Мәулен NP-ANT-M ; ! "Mäwlen" (Arabic)
Мәуленов:Мәуленов  NP-COG-OB ; ! "" ! Use/MT
Мәһди:Мәһди NP-ANT-M ; ! ""
Мәшһур:Мәшһур NP-ANT-M ; ! "Mäshhiwr" (Arabic)
Меган:Меган NP-ANT-F ; !"Use/MT"
Мег:Мег NP-ANT-F ; !"Use/MT"
Медведев:Медведев NP-COG-OB ; ! "Medvedev"
Медведьев:Медведьев NP-COG-OB ; ! ""
Меделбек:Меделбек NP-ANT-M ; ! ""
Медетбай:Медетбай NP-ANT-M ; ! "Medetbay" (Arabic)
Медетбек:Медетбек NP-ANT-M ; ! "Medetbek" (Arabic)
Медет:Медет NP-ANT-M ; ! "Medet" (Arabic)
Медетнар:Медетнар NP-ANT-M ; ! "Medetnar" (Kazakh)
Медетхан:Медетхан NP-ANT-M ; ! "Medetxan" (Kazakh)
Медеу:Медеу NP-ANT-M ; ! "Medew" (Arabic)
Медеу:Медеу NP-TOP ; ! "Medew"
Мед:Мед NP-ANT-M ; !"Use/MT"
Медхат:Медхат NP-ANT-M ; ! "Medxat" (Arabic)
Мейер:Мейер NP-ANT-F ; !"Use/MT"
Мейер:Мейер NP-ANT-M ; !"Use/MT"
Мейер:Мейер NP-COG-MF ; ! ""
Мейман:Мейман NP-ANT-M ; ! "Meyman" (Arabic)
Мей:Мей NP-COG-MF ; !"Use/MT"
Мейрамгүл:Мейрамгүл NP-ANT-F ; ! "Meyramgül" (Kazakh)
Мейрам:Мейрам NP-ANT-M ; ! "Meyram" (Kazakh)
Мейржан:Мейржан NP-ANT-F ; ! "Meyrjan" (Kazakh)
Мейсон:Мейсон NP-ANT-M ; !"Use/MT"
Мейіз:Мейіз NP-ANT-F ; ! "Meyiz" (Kazakh)
Мейірбек:Мейірбек NP-ANT-M ; ! "Meyirbek" (Kazakh)
Мейірман:Мейірман NP-ANT-M ; ! "Meyirman" (Arabic)
Мейір:Мейір NP-ANT-M ; ! "Meyir" (Persian)
Мекемтас:Мекемтас NP-ANT-M ; ! "Mekemtas" (Kazakh)
Мекке:Мекке NP-TOP ; ! ""
Мекке:Мекке NP-TOP ; ! ""
Мекран:Мекран NP-TOP ; ! ""
Мексика:Мексика NP-TOP ; ! "Mexico"
Мексико:Мексико NP-TOP ; !"Use/MT"
Мелвилл:Мелвилл NP-COG-MF ; !"Use/MT"
Мелина:Мелина NP-ANT-F ; !"Use/MT"
Мелинда:Мелинда NP-ANT-F ; !"Use/MT"
Мелисса:Мелисса NP-ANT-F ; !"Use/MT"
Мельников:Мельников NP-COG-OB ; ! ""
Мем:Мем NP-ANT-M ; !"Use/MT"
Мемфис:Мемфис NP-TOP ; ! ""
Менделеев:Менделеев NP-COG-OB ; ! ""
Менделеевск:Менделеевск NP-TOP ; ! ""
Мендель:Мендель NP-COG-MF ; ! ""
Мендельсон:Мендельсон NP-COG-MF ; ! ""
Меңгерей:Меңгерей NP-ANT-M ; ! "Meŋgerey" (Kazakh)
Меңдалиев:Меңдалиев NP-COG-OB ; ! ""
Меңдіахмет:Меңдіахмет NP-ANT-M ; ! "Meŋdiaxmet" (Arabic)
Меңдібай:Меңдібай NP-ANT-M ; ! "Meŋdibay" (Kazakh)
Меңешев:Меңешев NP-COG-OB ; ! ""
Меңліахмет:Меңліахмет NP-ANT-M ; ! "Meŋliaxmet" (Arabic)
Меңлібай:Меңлібай NP-ANT-M ; ! "Meŋlibay" (Kazakh)
Мең:Мең NP-ANT-M ; ! "Meŋ" (Kazakh)
Меңсұлу:Меңсұлу  NP-ANT-M ; ! "Meŋsulıw " (Kazakh)
Мергенбаев:Мергенбаев NP-COG-OB ; ! ""
Мерген:Мерген NP-ANT-M ; ! "Mergen" (Kazakh)
Мердеев:Мердеев NP-COG-OB ; ! ""
Мердок:Мердок NP-COG-MF ; !"Use/MT"
Мередит:Мередит NP-ANT-F ; !"Use/MT"
Мерей:Мерей NP-ANT-M ; ! "Merey" (Kazakh)
Мереке:Мереке NP-ANT-M ; ! "Mereke" (Kazakh)
Мерецков:Мерецков NP-COG-OB ; ! ""
Мерзляков:Мерзляков NP-COG-OB ; ! ""
Мерил:Мерил NP-ANT-F ; !"Use/MT"
Меркель:Меркель NP-ANT-F ; !"Use/MT"
Меркель:Меркель NP-COG-MF ; ! ""
Мерке:Мерке NP-TOP ; ! "" 
Меркулов:Меркулов NP-COG-OB ; ! ""
Меркурий:Меркурий NP-AL ; ! ""
Меркурьев:Меркурьев NP-COG-OB ; ! ""
Меркушев:Меркушев NP-COG-OB ; ! ""
Мер:Мер NP-ANT-M ; !"Use/MT"
Мерседес:Мерседес NP-ANT-F ; !"Use/MT"
Меруерт:Меруерт NP-ANT-F ; ! "Merwert" (Greek)
Мерфи:Мерфи NP-COG-MF ; !"Use/MT"
Месахаб:Месахаб NP-ANT-M ; ! ""
Месси:Месси NP-COG-M ; ! ""
Метлов:Метлов NP-COG-OB ; ! ""
Мет:Мет NP-ANT-M ; !"Use/MT"
Меф:Меф NP-ANT-M ; !"Use/MT"
Мефодьев:Мефодьев NP-COG-OB ; ! ""
Мехико:Мехико NP-TOP ; ! "Mexico"
Меценатов:Меценатов NP-COG-OB ; ! ""
Мечников:Мечников NP-COG-OB ; ! ""
Мешков:Мешков NP-COG-OB ; ! ""
Мешхед:Мешхед NP-TOP ; ! ""
Миа:Миа NP-ANT-F ; !"Use/MT"
Мианма:Мианма NP-TOP ; ! ""
Миб:Миб NP-ANT-M ; !"Use/MT"
Мигель:Мигель NP-ANT-M ; !"Use/MT"
Миг:Миг NP-ANT-M ; !"Use/MT"
Мидж:Мидж NP-ANT-M ; !"Use/MT"
Мидлэнд:Мидлэнд NP-TOP ; !"Use/MT"
Мид:Мид NP-ANT-M ; !"Use/MT"
Мидуков:Мидуков NP-COG-OB ; ! ""
Мизам:Мизам NP-ANT-M ; ! "Mıyzam" (Arabic)
Миз:Миз NP-ANT-M ; !"Use/MT"
Микаэла:Микаэла NP-ANT-F ; !"Use/MT"
Микеланджело:Микеланджело NP-ANT-M ; !"Use/MT"
Микки:Микки NP-ANT-M ; !"Use/MT"
Мик:Мик NP-ANT-M ; !"Use/MT"
Микронезия:Микронезия NP-TOP ; !"Use/MT"
Милад:Милад NP-ANT-M ; !"Use/MT"
Мила:Мила NP-ANT-F ; !"Use/MT"
Милана:Милана NP-ANT-F ; !"Use/MT"
Милан:Милан NP-TOP ; ! ""
Милена:Милена NP-ANT-F ; !"Use/MT"
Миллер:Миллер NP-COG-MF ; !"Use/MT"
Миллер:Миллер NP-COG-M ; !"Use/MT"
Миллионов:Миллионов NP-COG-OB ; ! ""
Миллионщиков:Миллионщиков NP-COG-OB ; ! ""
Мил:Мил NP-ANT-M ; !"Use/MT"
Мило:Мило NP-ANT-M ; !"Use/MT"
Милорад:Милорад NP-ANT-M ; !"Use/MT"
Милосевиц:Милосевиц NP-COG-M ; ! ""
Милос:Милос NP-COG-MF ; !"Use/MT"
Милтон:Милтон NP-ANT-M ; !"Use/MT"
Ми:Ми NP-ANT-M ; !"Use/MT"
Мими:Мими NP-ANT-F ; !"Use/MT"
Мина:Мина NP-ANT-F ; !"Use/MT"
Миначев:Миначев NP-COG-OB ; ! ""
Мингалев:Мингалев NP-COG-OB ; ! ""
Миндовг:Миндовг NP-ANT-M ; ! ""
Минерва:Минерва NP-ANT-F ; !"Use/MT"
Минзәлә:Минзәлә NP-TOP ; ! ""
Минковский:Минковский NP-COG-M ; !"Use/MT"
Мин:Мин NP-ANT-M ; !"Use/MT"
Мин:Мин NP-ANT-M ; !"Use/MT"
Миннеаполис:Миннеаполис NP-TOP ; !"Use/MT"
Миннегулов:Миннегулов NP-COG-OB ; ! ""
Миннесота:Миннесота NP-TOP ; !"Use/MT"
Минниханов:Минниханов NP-COG-OB ; ! ""
Минск:Минск NP-TOP ; ! ""
Минск:Минск%{і%} NP-TOP ; ! "" ! Dir/LR
Минтимер:Минтимер NP-ANT-M ; ! ""
Миңнеханов:Миңнеханов NP-COG-OB ; ! ""
Мирабо:Мирабо NP-ANT-M ; !"Use/MT"
Мира:Мира NP-ANT-F ; ! ""
Мира:Мира NP-ANT-F ; ! "Mıyra" (New word)
Мирас:Мирас NP-ANT-M ; ! "Mıyras" (Arabic)
Мирболат:Мирболат NP-ANT-M ; ! "Mıyrbolat" (Kazakh)
Мирғали:Мирғали NP-ANT-M ; ! "Mıyrğalıy" (Arabic)
Мирзиёев:Мирзиёев NP-COG-OB ; ! "" ! Use/MT
Мирко:Мирко NP-ANT-M ; !"Use/MT"
Мир:Мир NP-AL ; ! ""
Мир:Мир NP-ANT-M ; !"Use/MT"
Мироваев:Мироваев NP-COG-OB ; ! ""
Миров:Миров NP-COG-OB ; ! ""
Мирон:Мирон NP-ANT-M ; ! "Mıyron" (Greek)
Миронов:Миронов NP-COG-OB ; ! ""
Мирослав:Мирослав NP-ANT-M ; ! ""
Мирошниченко:Мирошниченко NP-COG-MF ; ! ""
Миршанов:Миршанов NP-COG-OB ; ! ""
Мисагие:Мисагие NP-COG-MF ; !"Use/MT"
Мис:Мис NP-ANT-M ; !"Use/MT"
Мисрата:Мисрата NP-TOP ; !"Use/MT"
Миссисипи:Миссисипи NP-TOP ; !"Use/MT"
Миссиссипи:Миссиссипи NP-TOP ; !"Use/MT"
Миссури:Миссури NP-TOP ; !"Use/MT"
Мит:Мит NP-ANT-M ; !"Use/MT"
Митрофанов:Митрофанов NP-COG-OB ; ! ""
Миттеран:Миттеран NP-COG-MF ; !"Use/MT"
Миттов:Миттов NP-COG-OB ; ! ""
Митчелл:Митчелл NP-ANT-M ; !"Use/MT"
Митчел:Митчел NP-ANT-M ; !"Use/MT"
Мифтахутдинов:Мифтахутдинов NP-COG-OB ; ! ""
Михаил:Михаил NP-ANT-M ; ! ""
Михайленко:Михайленко NP-COG-MF ; ! ""
Михайлович:Михайло NP-PAT-VICH ; ! ""
Михайловка:Михайловка NP-TOP ; ! ""
Михайлов:Михайлов NP-COG-OB ; ! ""
Михалков:Михалков NP-COG-OB ; ! ""
Михаэлис:Михаэлис NP-ANT-M ; ! ""
Михеев:Михеев NP-COG-OB ; ! ""
Мицубиси:Мицубиси NP-TOP ; !"Use/MT"
Мичиган:Мичиган NP-TOP ; ! ""
Мич:Мич NP-ANT-M ; !"Use/MT"
Мишель:Мишель NP-ANT-F ; !"Use/MT"
Мишель:Мишель NP-ANT-M ; ! ""
Мишель:Мишель NP-ANT-M ; ! ""
Мишин:Мишин NP-COG-IN ; ! ""
Мишлен:Мишлен NP-COG-MF ; !"Use/MT"
Мишлин:Мишлин NP-ANT-F ; !"Use/MT"
Миш:Миш NP-ANT-M ; !"Use/MT"
Мият:Мият NP-ANT-M ; ! "Mıyat" (Kazakh)
Моаб:Моаб NP-TOP ; ! ""
Могилев:Могилев NP-COG-OB ; ! ""
Моғолстан:Моғолстан NP-TOP ; ! ""
Мозамбик:Мозамбик NP-TOP ; !"Use/MT"
Моисеев:Моисеев NP-COG-OB ; ! ""
Мойзес:Мойзес NP-ANT-M ; !"Use/MT"
Мокеев:Мокеев NP-COG-OB ; ! ""
Мокиенко:Мокиенко NP-COG-MF ; ! ""
Молбай:Молбай NP-ANT-M ; ! "Molbay" (Arabic)
Молдабеков:Молдабеков NP-COG-OB ; ! ""
Молдавия:Молдавия NP-TOP ; ! "Moldavia, Moldova"
Молдағалиев:Молдағалиев NP-COG-OB ; ! ""
Молдағұлов:Молдағұлов NP-COG-OB ; ! ""
Молдакәрімов:Молдакәрімов NP-COG-OB ; ! ""
Молдан:Молдан NP-ANT-M ; ! "Moldan" (Kazakh)
Молдова:Молдова NP-TOP ; ! "Moldavia, Moldova"
Молдоқов:Молдоқов NP-COG-OB ; ! ""
Молоков:Молоков NP-COG-OB ; ! ""
Молотков:Молотков NP-COG-OB ; ! ""
Молотов:Молотов NP-COG-OB ; ! ""
Молчанов:Молчанов NP-COG-OB ; ! ""
Мольер:Мольер NP-COG-MF ; ! ""
Моляков:Моляков NP-COG-OB ; ! ""
Момбек:Момбек NP-ANT-M ; ! "Mombek" (Kazakh)
Момбеков:Момбеков NP-COG-OB ; ! ""
Мо:Мо NP-ANT-M ; !"Use/MT"
Момынәлиев:Момынәлиев NP-COG-OB ; ! "" ! Use/MT
Момынәлі:Момынәлі NP-ANT-M ; ! "Momınäli" (Kazakh)
Монако:Монако NP-TOP ; ! ""
Мона:Мона NP-ANT-F ; !"Use/MT"
Моне:Моне NP-COG-MF ; !"Use/MT"
Моника:Моника NP-ANT-F ; !"Use/MT"
Мономах:Мономах NP-COG-MF ; ! ""
Монпелье:Монпелье NP-TOP ; !"Use/MT"
Монреаль:Монреаль NP-TOP ; !"Use/MT"
Монсанто:Монсанто NP-TOP ; !"Use/MT"
Монтана:Монтана NP-ANT-F ; !"Use/MT"
Монтгомери:Монтгомери NP-ANT-M ; !"Use/MT"
Монтень:Монтень NP-COG-MF ; ! ""
Монтсеррат:Монтсеррат NP-TOP ; !"Use/MT"
Моңғолия:Моңғолия NP-TOP ; ! "Mongolia"
Моран:Моран NP-COG-MF ; !"Use/MT"
Моратинос:Моратинос NP-COG-MF ; !"Use/MT"
Морган:Морган NP-ANT-F ; !"Use/MT"
Морган:Морган NP-ANT-M ; !"Use/MT"
Мордашев:Мордашев NP-COG-OB ; ! "" ! Use/MT
Мордвинцев:Мордвинцев NP-COG-OB ; ! ""
Мордовия:Мордовия NP-TOP ; ! ""
Мордовия:Мордовия NP-TOP ; !"Use/MT"
Мордяков:Мордяков NP-COG-OB ; ! ""
Морено:Морено NP-COG-MF ; !"Use/MT"
Мориарти:Мориарти NP-ANT-M ; !"Use/MT"
Морин:Морин NP-ANT-F ; !"Use/MT"
Морис:Морис NP-ANT-M ; !"Use/MT"
Мор:Мор NP-ANT-M ; !"Use/MT"
Мороз:Мороз NP-ANT-M ; !"Use/MT"
Морозов:Морозов NP-COG-OB ; ! ""
Мосаддық:Мосаддық NP-COG-MF ; !"Use/MT"
Москаленко:Москаленко NP-COG-MF ; ! ""
Мостар:Мостар NP-TOP ; !"Use/MT"
Моуринью:Моуринью NP-COG-MF ; !"Use/MT"
Мохамед:Мохамед NP-ANT-M ; !"Use/MT"
Мохаммад:Мохаммад NP-ANT-M ; ! ""
Мохаммед:Мохаммед NP-ANT-M ; !"Use/MT"
Мохсени:Мохсени NP-COG-MF ; !"Use/MT"
Мохтасем:Мохтасем NP-COG-MF ; !"Use/MT"
Моцарт:Моцарт NP-COG-MF ; !"Use/MT"
Мочалов:Мочалов NP-COG-OB ; ! ""
Моше:Моше NP-ANT-M ; !"Use/MT"
Мояз:Мояз NP-ANT-M ; ! "Moyaz" (Arabic)
Моя:Моя NP-COG-MF ; !"Use/MT"
Мөңке:Мөңке NP-ANT-M ; ! "Möŋke" (Old Turkic)
Мөслим:Мөслим NP-TOP ; ! ""
Мөхәммәтшин:Мөхәммәтшин NP-COG-IN ; ! ""
Мстислав:Мстислав NP-ANT-M ; ! ""
Мубарак:Мубарак NP-COG-OB ; ! ""
Мугалжар:Мугалжар NP-TOP ; ! ""
Мукаррама:Мукаррама NP-ANT-F ; ! "Mıwkarrama" (Arabic)
Муллаев:Муллаев NP-COG-OB ; ! ""
Муллан:Муллан NP-COG-MF ; !"Use/MT"
Мулюков:Мулюков NP-COG-OB ; ! ""
Мулянов:Мулянов NP-COG-OB ; ! ""
Мумбай:Мумбай NP-TOP ; ! ""
Му:Му NP-ANT-M ; !"Use/MT"
Мундус:Мундус NP-ORG ; ! ""
Муп:Муп NP-ANT-M ; !"Use/MT"
Муравленко:Муравленко NP-COG-MF ; ! ""
Муравьев:Муравьев NP-COG-OB ; ! ""
Муравьёв:Муравьёв NP-COG-OB ; ! ""
Мурат:Мурат NP-ANT-M ; ! "" ()
Мурауин:Мурауин NP-COG-IN ; ! ""
Мурзуков:Мурзуков NP-COG-OB ; ! ""
Мурманск:Мурманск NP-TOP ; ! ""
Мур:Мур NP-COG-MF ; !"Use/MT"
Мусатов:Мусатов NP-COG-OB ; ! ""
Мусин:Мусин NP-COG-IN ; ! ""
Мусинов:Мусинов NP-COG-OB ; ! ""
Муссолини:Муссолини NP-COG-MF ; !"Use/MT"
Мустаев:Мустаев NP-COG-OB ; ! ""
Мустафа:Мустафа NP-ANT-M ; !"Use/MT"
Мухамбетәзіз:Мухамбетәзіз NP-ANT-M ; ! "Mıwxambetäziz" (Arabic)
Мухамет:Мухамет NP-ANT-M ; ! ""
Мухаммед:Мухаммед NP-ANT-M ; !"Use/MT"
Мухин:Мухин NP-COG-IN ; ! ""
Мушарраф:Мушарраф NP-ANT-M ; !"Use/MT"
Мушкетов:Мушкетов NP-COG-OB ; ! ""
Мұғалжар:Мұғалжар NP-TOP ; !""
Мұзафар:Мұзафар NP-ANT-M ; ! "Muzafar" (Arabic)
Мұқағали:Мұқағали NP-ANT-M ; ! "Muqağalıy" (Persian)
Мұқаділ:Мұқаділ NP-ANT-M ; ! "Muqadil" (Arabic)
Мұқали:Мұқали NP-ANT-M ; ! "Muqalıy" (Arabic)
Мұқамәди:Мұқамәди NP-ANT-M ; ! "Muqamädiy" (Arabic)
Мұқамбетбай:Мұқамбетбай NP-ANT-M ; ! "Muqambetbay" (Arabic)
Мұқамбетбек:Мұқамбетбек NP-ANT-M ; ! "Muqambetbek" (Arabic)
Мұқамбетғали:Мұқамбетғали NP-ANT-M ; ! "Muqambetğalıy" (Arabic)
Мұқамбетжан:Мұқамбетжан NP-ANT-M ; ! "Muqambetjan" (Arabic)
Мұқамбеткәрім:Мұқамбеткәрім NP-ANT-M ; ! "Muqambetkärim" (Arabic)
Мұқамбетқожа:Мұқамбетқожа NP-ANT-M ; ! "Muqambetqoja" (Arabic)
Мұқамбет:Мұқамбет NP-ANT-M ; ! "Muqambet" (Arabic)
Мұқамбетназар:Мұқамбетназар NP-ANT-M ; ! "Muqambetnazar" (Arabic)
Мұқамедияр:Мұқамедияр NP-ANT-M ; ! "Muqamedıyar" (Arabic)
Мұқамет:Мұқамет NP-ANT-M ; ! "Muqamet" (Arabic)
Мұқа:Мұқа NP-ANT-M ; ! "Muqa" (Old Turkic)
Мұқан:Мұқан NP-ANT-M ; ! 
Мұқанов:Мұқанов NP-COG-OB ; ! ""
Мұқашев:Мұқашев NP-COG-OB ; ! "" ! Use/MT
Мұқым:Мұқым NP-ANT-M ; ! "Muqım" (Arabic)
Мұңайтпасов:Мұңайтпасов NP-COG-OB ; ! ""
Мұрабай:Мұрабай NP-ANT-M ; ! "Murabay" (Kazakh)
Мұратәлі:Мұратәлі NP-ANT-M ; ! "Muratäli" (Arabic)
Мұратбаев:Мұратбаев NP-COG-OB ; ! ""
Мұратбай:Мұратбай NP-ANT-M ; ! "Muratbay" (Arabic)
Мұратбек:Мұратбек NP-ANT-M ; ! "Muratbek" (Arabic)
Мұратбеков:Мұратбеков NP-COG-OB ; ! ""
Мұратберді:Мұратберді NP-ANT-M ; ! "Muratberdi" (Arabic)
Мұратғали:Мұратғали NP-ANT-M ; ! "Muratğalıy" (Arabic)
Мұратжан:Мұратжан NP-ANT-M ; ! "Muratjan" (Arabic)
Мұратқан:Мұратқан NP-ANT-M ; ! "Muratqan" (Arabic)
Мұратқожа:Мұратқожа NP-ANT-M ; ! "Muratqoja" (Arabic)
Мұрат:Мұрат NP-ANT-M ; ! "Murat"
Мұрат:Мұрат NP-ANT-M ; ! "Murat" (Arabic)
Мұсабаев:Мұсабаев NP-COG-OB ; ! ""
Мұсабек:Мұсабек NP-ANT-M ; ! "Musabek" (Arabic)
Мұсағали:Мұсағали NP-ANT-M ; ! "Musağalıy" (Arabic)
Мұсажан:Мұсажан NP-ANT-M ; ! "Musajan" (Arabic)
Мұсайынов:Мұсайынов NP-COG-OB ; ! ""
Мұсақожаев:Мұсақожаев NP-COG-OB ; ! ""
Мұсақожаев:Мұсақожаев NP-COG-OB ; ! ""
Мұсақыл:Мұсақыл NP-ANT-M ; ! "Musaqul"
Мұса:Мұса NP-ANT-M ; ! "Musa" (Arabic)
Мұсатай:Мұсатай NP-ANT-M ; ! "Musatay" (Arabic)
Мұсахан:Мұсахан NP-ANT-M ; ! "Musaxan" (Arabic)
Мұсаханов:Мұсаханов NP-COG-OB ; !"Use/MT"
Мұсаходжаев:Мұсаходжаев NP-COG-OB ; ! ""
Мұстанбаев:Мұстанбаев NP-COG-OB ; ! ""
Мұстапаев:Мұстапаев NP-COG-OB ; ! ""
Мұстафа:Мұстафа NP-ANT-M ; ! "Mustafa" (Arabic)
Мұстахиб:Мұстахиб NP-ANT-M ; ! "Mustaxıyb" (Arabic)
Мұфтахов:Мұфтахов NP-COG-OB ; ! ""
Мұфтолла:Мұфтолла NP-ANT-M ; ! ""
Мұхамәди:Мұхамәди NP-ANT-M ; ! "Muxamädiy" (Arabic)
Мұхамбет:Мұхамбет NP-ANT-M ; ! "Muxambet" (Arabic)
Мұхамбетов:Мұхамбетов NP-COG-OB ; ! "" ! Use/MT
Мұхамеджанов:Мұхамеджанов NP-COG-OB ; ! ""
Мұхамедиұлы:Мұхамедиұлы NP-COG-M ; ! ""
Мұхаметжанов:Мұхаметжанов NP-COG-OB ; ! ""
Мұхаметқалиев:Мұхаметқалиев NP-COG-OB ; ! ""
Мұхаммед:Мұхаммед NP-ANT-M ; ! "Mohammed"
Мұхитан:Мұхитан NP-ANT-M ; ! "Muxıytan" (Persian)
Мұхит:Мұхит NP-ANT-M ; ! "Muxıyt" (Arabic)
Мұхтарбай:Мұхтарбай NP-ANT-M ; ! "" ! Use/MT
Мұхтарима:Мұхтарима NP-ANT-F ; ! "Muxtarıyma" (Arabic)
Мұхтар:Мұхтар NP-ANT-M ; ! "Muxtar" (Arabic)
Мүбарак:Мүбарак NP-COG-M ; ! ""
Мүбәрак:Мүбәрак NP-ANT-M ; ! "Mübärak" (Arabic)
Мүлипа:Мүлипа NP-ANT-F ; ! "Müliypa" (Arabic)
Мүнира:Мүнира NP-ANT-F ; ! "Müniyra" (Arabic)
Мүрсейіт:Мүрсейіт NP-ANT-M ; ! "Mürseyit" (Arabic)
Мүсiрепов:Мүсiрепов NP-COG-OB ; ! ""
Мүсабаев:Мүсабаев NP-COG-OB ; ! ""
Мүсілім:Мүсілім NP-ANT-M ; ! ""
Мүсірәлі:Мүсірәлі NP-ANT-M ; ! "Müsiräli" (Arabic)
Мүсіреп:Мүсіреп NP-ANT-M ; ! "Müsirep" (Arabic)
Мүсірепов:Мүсірепов NP-COG-OB ; ! ""
Мүтәліп:Мүтәліп NP-ANT-M ; ! "Mütälip" (Arabic)
Мүткенов:Мүткенов NP-COG-OB ; ! ""
Мүштәри:Мүштәри NP-ANT-F ; ! "Müshtäriy" (Arabic)
Мығым:Мығым NP-ANT-M ; ! "Mığım" (Kazakh)
Мықтыбаев:Мықтыбаев NP-COG-OB ; ! ""
Мынбаев:Мынбаев NP-COG-OB ; ! ""
Мыңбаев:Мыңбаев NP-COG-OB ; ! ""
Мыңбаев:Мыңбаев NP-COG-OB ; ! ""
Мыңбай:Мыңбай NP-ANT-M ; ! "Mıŋbay" (Kazakh)
Мыңжасар:Мыңжасар NP-ANT-M ; ! "Mıŋjasar" (Kazakh)
Мыңтуар:Мыңтуар NP-ANT-M ; ! "Mıŋtıwar" (Kazakh)
Мырзабай:Мырзабай NP-ANT-M ; ! "Mırzabay" (Arabic)
Мырзабеков:Мырзабеков NP-COG-OB ; ! ""
Мырзаберген:Мырзаберген NP-ANT-M ; ! "Mırzabergen" (Arabic)
Мырзаболат:Мырзаболат NP-ANT-M ; ! "Mırzabolat" (Arabic)
Мырзагелді:Мырзагелді NP-ANT-M ; ! "Mırzageldi" (Arabic)
Мырзагул:Мырзагул NP-ANT-M ; ! "Mırzagıwl" (Arabic)
Мырзакәрім:Мырзакәрім NP-ANT-M ; ! "Mırzakärim" (Arabic)
Мырзалиев:Мырзалиев NP-COG-OB ; ! "" ! Use/MT
Мырзамадиев:Мырзамадиев NP-COG-OB ; ! "" ! Use/MT
Мырзамұрат:Мырзамұрат NP-ANT-M ; ! "Mırzamurat" (Arabic)
Мырзатай:Мырзатай NP-ANT-M ; ! ""
Мырзахан:Мырзахан NP-ANT-M ; ! "Mırzaqan"  (sometimes M?)
Мырзахан:Мырзахан NP-ANT-M ; ! "Mırzaxan" (Arabic)
Мырзахмет:Мырзахмет NP-ANT-M ; ! "Mırzaxmet" (Arabic)
Мырзахметов:Мырзахметов NP-COG-M ; ! "USE/MT"
Мырзахметов:Мырзахметов NP-COG-OB ; ! "" ! Use/MT
Мырқасым:Мырқасым NP-ANT-M ; ! "Mırqasım" (Arabic)
Мырхайдар:Мырхайдар NP-ANT-M ; ! "Mırxaydar" (Arabic)
Мыс:Мыс NP-ANT-M ; !"Use/MT"
Мысыр:Мысыр NP-TOP ; ! "Egypt"
Мышбай:Мышбай NP-ANT-M ; ! "Mıshbay" (Old Turkic)
Мышырбай:Мышырбай NP-ANT-M ; ! "Mıshırbay" (Arabic)
Мінайдаров:Мінайдаров NP-COG-OB ; ! ""
Мірқадыров:Мірқадыров NP-COG-OB ; ! ""
Мьянма:Мьянма NP-TOP ; ! ""
Мэган:Мэган NP-ANT-F ; !"Use/MT"
Мэг:Мэг NP-ANT-F ; !"Use/MT"
Мэдисон:Мэдисон NP-ANT-F ; !"Use/MT"
Мэй:Мэй NP-ANT-F ; !"Use/MT"
Мэн:Мэн NP-TOP ; !"Use/MT"
Мэриленд:Мэриленд NP-TOP ; !"Use/MT"
Мэрилин:Мэрилин NP-ANT-F ; !"Use/MT"
Мэри:Мэри NP-ANT-F ; ! "Mary" (English)
Мэтью:Мэтью NP-ANT-M ; ! ""
Мюллер:Мюллер NP-COG-MF ; !"Use/MT"
Мюнхен:Мюнхен NP-TOP ; ! ""
Мюриэль:Мюриэль NP-ANT-F ; !"Use/MT"
Мюррей:Мюррей NP-COG-MF ; !"Use/MT"
Мясищев:Мясищев NP-COG-OB ; ! ""
Мясников:Мясников NP-COG-OB ; ! ""
Наарбаев:Наарбаев NP-COG-OB ; ! "" ! Use/MT
Набиль:Набиль NP-ANT-M ; !"Use/MT"
Набуходоносор:Набуходоносор NP-ANT-M ; ! ""
Наварро:Наварро NP-COG-MF ; !"Use/MT"
навигациялық:навигациялық A4 ; !"Use/MT"
Наг:Наг NP-ANT-M ; !"Use/MT"
Нагорнов:Нагорнов NP-COG-OB ; ! ""
Нағиза:Нағиза NP-ANT-F ; ! "Nağıyza" (Persian)
Нағима:Нағима NP-ANT-F ; ! "Nağıyma" (Arabic)
Нағи:Нағи NP-ANT-M ; ! "Nağıy" (Arabic)
Нағымбек:Нағымбек NP-ANT-M ; ! "Nağımbek" (Kazakh)
Нағым:Нағым NP-ANT-M ; ! "Nağım" (Arabic)
Надаль:Надаль NP-COG-MF ; !"Use/MT"
Надежда:Надежда NP-ANT-F ; ! "" 
Надер:Надер NP-ANT-M ; !"Use/MT"
Надин:Надин NP-ANT-F ; !"Use/MT"
Над:Над NP-TOP ; ! ""
Надя:Надя NP-ANT-F ; ! ""
Назарбаев:Назарбаев NP-COG-OB ; ! "Nazarbayev"
Назарбай:Назарбай NP-ANT-M ; ! "Nazarbay" (Kazakh)
Назарбеков:Назарбеков NP-COG-OB ; ! ""
Назар:Назар NP-ANT-M ; ! "Nazar" (Arabic)
Назаров:Назаров NP-COG-OB ; ! ""
Назгүл:Назгүл NP-ANT-F ; ! "Nazgül" 
Наз:Наз NP-ANT-M ; !"Use/MT"
Назырбаев:Назырбаев NP-COG-OB ; ! ""
Наим:Наим NP-ANT-M ; !"Use/MT"
Найджел:Найджел NP-ANT-M ; !"Use/MT"
Найманбай:Найманбай NP-ANT-M ; ! ""
Най:Най NP-ANT-M ; !"Use/MT"
Найпидау:Найпидау NP-TOP ; ! "" 
Найра:Найра NP-ANT-F ; ! "Nayra" (Arabic)
Найроби:Найроби NP-TOP ; ! ""
Найроби:Найроби NP-TOP ; ! "" 
Нақып:Нақып NP-ANT-M ; ! "Naqıp" (Arabic)
Нақысбек:Нақысбек NP-ANT-M ; ! "Naqısbek" (Kazakh)
Намибия:Намибия NP-TOP ; ! ""
На:На NP-ANT-M ; !"Use/MT"
Нансен:Нансен NP-COG-IN ; ! ""
Наполеон:Наполеон NP-ANT-M ; ! ""
Нарбай:Нарбай NP-ANT-M ; ! "Narbay" (Kazakh)
Нарбек:Нарбек NP-ANT-M ; ! "Narbek" (Kazakh)
Нарбол:Нарбол NP-ANT-M ; ! "Narbol" (Kazakh)
Нариман:Нариман NP-ANT-M ; ! "Narıyman" (Persian)
Нариманов:Нариманов NP-COG-OB ; ! ""
Наркес:Наркес NP-ANT-M ; ! "Narkes" (Kazakh)
Нармагамбет:Нармагамбет NP-ANT-M ; ! "Narmagambet" (Kazakh)
Нармұхамбет:Нармұхамбет NP-ANT-M ; ! "Narmuxambet" (Kazakh)
Народецкий:Народецкий NP-COG-OB ; ! "" FIXME another ending for feminine
Нартау:Нартау NP-ANT-M ; ! "Nartaw" (Kazakh)
Нарту:Нарту NP-ANT-M ; ! "Nartıw" (Kazakh)
Нарша:Нарша NP-ANT-F ; ! "Narsha" (Kazakh)
Нарымбаев:Нарымбаев NP-COG-OB ; ! ""
Нарымбетов:Нарымбетов NP-COG-OB ; ! ""
Нарымжан:Нарымжан NP-ANT-M ; ! "Narımjan" (Kazakh)
Нарын:Нарын NP-TOP ; ! ""
Насимов:Насимов NP-COG-OB ; ! ""
Насиров:Насиров NP-COG-OB ; ! ""
Насх:Насх NP-ANT-M ; !"Use/MT"
Насырбай:Насырбай NP-ANT-M ; ! "Nasırbay" (Arabic)
Насыр:Насыр NP-ANT-M ; ! "Nasır" (Arabic)
Насырулла:Насырулла NP-ANT-M ; ! "Nasırıwlla" (Arabic)
Натали:Натали NP-ANT-F ; !"Use/MT"
Наталия:Наталия NP-ANT-F ; ! ""
Наталья:Наталья NP-ANT-F ; ! "" 
Наташа:Наташа NP-ANT-F ; ! ""
НАТО:НАТО NP-ORG-ABBR ; ! ""
Науаи:Науаи NP-COG-MF ; ! ""
Науат:Науат NP-ANT-F ; ! "Nawat" (Arabic)
Наум:Наум NP-ANT-M ; !"Use/MT"
Наумов:Наумов NP-COG-OB ; ! ""
Науру:Науру NP-TOP ; ! ""
Наурызбаев:Наурызбаев NP-COG-OB ; ! ""
Наурыз:Наурыз NP-ANT-M ; ! "Nawrız" (Arabic)
Науша:Науша NP-ANT-F ; ! "Nawsha" (Persian)
Наф:Наф NP-ANT-M ; !"Use/MT"
Нахименко:Нахименко NP-COG-MF ; ! ""
Нах:Нах NP-ANT-M ; !"Use/MT"
Нахчыван:Нахчыван NP-TOP ; ! ""
Нәбибулла:Нәбибулла NP-ANT-M ; ! "Näbiybiwlla" (Arabic)
Нәби:Нәби NP-ANT-M ; ! "Näbiy" (Arabic)
Нәбира:Нәбира NP-ANT-F ; ! "Näbiyra" (Arabic)
Нәдира:Нәдира NP-ANT-F ; ! "Nädiyra" (Arabic)
Нәдірбай:Нәдірбай NP-ANT-M ; ! "Nädirbay" (Kazakh)
Нәдірбек:Нәдірбек NP-ANT-M ; ! "Nädirbek" (Kazakh)
Нәдір:Нәдір NP-ANT-M ; ! "Nädir" (Arabic)
Нәжимеденов:Нәжимеденов NP-COG-OB ; ! ""
Нәжімеденов:Нәжімеденов NP-COG-OB ; ! ""
Нәжіп:Нәжіп NP-ANT-M ; ! "Näjip" (Arabic)
Нәзипа:Нәзипа NP-ANT-F ; ! "Näziypa" (Arabic)
Нәзірбек:Нәзірбек NP-ANT-M ; ! "Näzirbek" (Kazakh)
Нәзір:Нәзір NP-ANT-M ; ! "Näzir" (Arabic)
Нәйля:Нәйля NP-ANT-F ; ! "Näylya" (Arabic)
Нәргүл:Нәргүл NP-ANT-F ; ! "Närgül" (Arabic)
Нәрима:Нәрима NP-ANT-F ; ! "Näriyma" (Arabic)
Нәрікбаев:Нәрікбаев NP-COG-OB ; ! "" ! Use/MT
Нәрік:Нәрік NP-ANT-M ; ! "Närik" (Arabic)
Нәсима:Нәсима NP-ANT-F ; ! "Näsiyma" (Arabic)
Нәсірдің:Нәсірдің NP-COG-MF ; !"Use/MT"
Нәфиса:Нәфиса NP-ANT-F ; ! "Näfisa" (Arabic)
Неаполь:Неаполь NP-TOP ; !"Use/MT"
Небраска:Небраска NP-TOP ; ! ""
Невада:Невада NP-TOP ; ! ""
Нева:Нева NP-TOP ; !""
Невский:Невский NP-ANT-M ; ! ""
Нег:Нег NP-ANT-M ; !"Use/MT"
Нейман:Нейман NP-ANT-M ; !"Use/MT"
Некрасов:Некрасов NP-COG-OB ; ! ""
Нельсон:Нельсон NP-ANT-M ; ! ""
Немилов:Немилов NP-COG-OB ; ! ""
Нем:Нем NP-ANT-M ; !"Use/MT"
Нем:Нем NP-ANT-M ; !"Use/MT"
Немцев:Немцев NP-COG-OB ; ! ""
Не:Не NP-ANT-M ; !"Use/MT"
Ненецк:Ненецк NP-TOP ; !"Use/MT"
Непал:Непал NP-TOP ; ! ""
Непенин:Непенин NP-COG-IN ; ! ""
Неплюев:Неплюев NP-COG-OB ; ! ""
Нептун:Нептун NP-AL ; ! ""
Нер:Нер NP-ANT-M ; !"Use/MT"
Неро:Неро NP-ANT-M ; !"Use/MT"
Нестеров:Нестеров NP-COG-OB ; ! ""
Нестор:Нестор NP-ANT-M ; !"Use/MT"
Несіпбаев:Несіпбаев NP-COG-OB ; ! ""
Несіпбай:Несіпбай NP-ANT-M ; ! "Nesipbay" (Kazakh)
Несіпжан:Несіпжан NP-ANT-F ; ! "Nesipjan" (Arabic)
Несіп:Несіп NP-ANT-M ; ! "Nesip" (Arabic)
Неуструев:Неуструев NP-COG-OB ; ! ""
Неф:Неф NP-ANT-M ; !"Use/MT"
Нехорошев:Нехорошев NP-COG-OB ; ! "" ! Use/MT
Нечаев:Нечаев NP-COG-OB ; ! ""
Ниа:Ниа NP-ANT-F ; !"Use/MT"
Ниб:Ниб NP-ANT-M ; !"Use/MT"
Нигерия:Нигерия NP-TOP ; ! ""
Нигер:Нигер NP-TOP ; ! ""
Нигматуллин:Нигматуллин NP-COG-IN ; ! ""
Нидерланд:Нидерланд NP-TOP ; ! ""
Нидерланд:Нидерланд NP-TOP ; ! ""
Нидерландтық% антил% аралдар:Нидерландтық% антил% аралдар NP-TOP ; ! ""
Нидерландылар:Нидерландылар NP-TOP ; ! ""
Ниетқали:Ниетқали NP-ANT-M ; ! "Nıyetqalıy" (Arabic)
Нижегород:Нижегород NP-TOP ; !"Use/MT"
Нижегородов:Нижегородов NP-COG-OB ; ! ""
Низами:Низами NP-COG-MF ; ! ""
Низам:Низам NP-ANT-M ; ! "Nıyzam" (Arabic)
Никаноров:Никаноров NP-COG-OB ; ! ""
Никарагуа:Никарагуа NP-TOP ; ! ""
Никита:Никита NP-ANT-M ; ! ""
Никитенко:Никитенко NP-COG-MF ; ! ""
Никитин:Никитин NP-COG-IN ; ! ""
Никитченко:Никитченко NP-COG-MF ; ! ""
Никифоров:Никифоров NP-COG-OB ; ! ""
Ник:Ник NP-ANT-M ; !"Use/MT"
Николаевич:Николае NP-PAT-VICH ; ! ""
Николаевка:Николаевка NP-ANT-F ; !"Use/MT"
Николаевка:Николаевка NP-TOP ; !""
Николаев:Николаев NP-COG-OB ; ! ""
Николае:Николае NP-ANT-M ; !"Use/MT"
Николаенко:Николаенко NP-COG-MF ; ! ""
Николай:Николай NP-ANT-M ; ! ""
Никола:Никола NP-ANT-M ; !"Use/MT"
Николас:Николас NP-ANT-M ; !"Use/MT"
Николсон:Николсон NP-COG-MF ; !"Use/MT"
Николь:Николь NP-ANT-F ; !"Use/MT"
Николя:Николя NP-ANT-M ; ! ""
Нико:Нико NP-ANT-M ; !"Use/MT"
Никонов:Никонов NP-COG-OB ; ! ""
Никоноров:Никоноров NP-COG-OB ; ! ""
Нил:Нил NP-ANT-M ; !"Use/MT"
Нил:Нил NP-TOP ; ! ""
Нильс:Нильс NP-ANT-M ; !"Use/MT"
Ним:Ним NP-ANT-M ; !"Use/MT"
Ним:Ним NP-ANT-M ; !"Use/MT"
Нина:Нина NP-ANT-F ; ! "" 
Ни:Ни NP-ANT-M ; !"Use/MT"
Ни:Ни NP-COG-MF ; ! ""
Нин:Нин NP-ANT-M ; !"Use/MT"
Нино:Нино NP-ANT-M ; !"Use/MT"
Нита:Нита NP-ANT-F ; !"Use/MT"
Ничиков:Ничиков NP-COG-OB ; ! ""
Нишапур:Нишапур NP-TOP ; ! ""
Ниш:Ниш NP-ANT-M ; !"Use/MT"
Ниязбек:Ниязбек NP-ANT-M ; ! "Nıyazbek" (Arabic)
Ниязбеков:Ниязбеков NP-COG-OB ; ! ""
Ниязғали:Ниязғали NP-ANT-M ; ! "Nıyazğalıy" (Arabic)
Ниязмұхамбет:Ниязмұхамбет NP-ANT-M ; ! "Nıyazmuxambet" (Arabic)
Нияз:Нияз NP-ANT-M ; ! "Nıyaz" (Arabic)
Ниязов:Ниязов NP-COG-OB ; ! ""
Ниязымбетов:Ниязымбетов NP-COG-OB ; ! "" ! Use/MT
Ноа:Ноа NP-ANT-F ; !"Use/MT"
Нобелев:Нобелев NP-COG-OB ; ! ""
Нобель:Нобель NP-ANT-M ; !"Use/MT"
Нобель:Нобель NP-COG-MF ; ! ""
Новгород:Новгород NP-TOP ; ! ""
Новиков:Новиков NP-COG-OB ; ! ""
Новозывков:Новозывков NP-COG-OB ; ! ""
Новомихайлов:Новомихайлов NP-COG-OB ; ! ""
Новоселов:Новоселов NP-COG-OB ; ! ""
Новосибирск:Новосибирск NP-TOP ; ! ""
Ноғаев:Ноғаев NP-COG-OB ; ! ""
Ноғайбай:Ноғайбай NP-ANT-M ; ! "Noğaybay" (Kazakh)
Ноздряков:Ноздряков NP-COG-OB ; ! ""
Ноиль:Ноиль NP-ANT-M ; ! "Noilʲ" (Arabic)
Ной:Ной NP-ANT-M ; !"Use/MT"
Нора:Нора NP-ANT-F ; !"Use/MT"
Норберт:Норберт NP-ANT-M ; !"Use/MT"
Норвегия:Норвегия NP-TOP ; ! "Norway"
Нормандия:Нормандия NP-TOP ; !"Use/MT"
Норман:Норман NP-ANT-M ; !"Use/MT"
Носков:Носков NP-COG-OB ; ! ""
Носов:Носов NP-COG-OB ; ! ""
Ноэль:Ноэль NP-ANT-M ; ! "Noelʲ" (Arabic)
Ноянбай:Ноянбай NP-ANT-M ; ! "Noyanbay" (Arabic)
Ноянбек:Ноянбек NP-ANT-M ; ! "Noyanbek" (Arabic)
Ноян:Ноян NP-ANT-M ; ! "Noyan" (Arabic)
Нөкенұлы:Нөкенұлы NP-COG-M ; ! ""
Нөкербек:Нөкербек NP-ANT-M ; ! "Nökerbek" (Arabic)
Нөсербаев:Нөсербаев NP-COG-OB ; ! ""
Нуакшот:Нуакшот NP-TOP ; ! "" 
Нунцио:Нунцио NP-ANT-M ; !"Use/MT"
Нурақын:Нурақын NP-ANT-M ; ! "Nıwraqın" (Persian)
Нургалиев:Нургалиев NP-COG-OB ; ! ""
Нури:Нури NP-ANT-F ; !"Use/MT"
Нурлат:Нурлат NP-TOP ; ! ""
Нурутдинов:Нурутдинов NP-COG-OB ; ! ""
Нұақын:Нұақын NP-ANT-M ; ! "Nuaqın" (Arabic)
Нұғман:Нұғман NP-ANT-M ; ! "Nuğman" 
Нұрабаев:Нұрабаев NP-COG-OB ; ! ""
Нұрабай:Нұрабай NP-ANT-M ; ! "Nurabay" 
Нұрайдар:Нұрайдар NP-ANT-M ; ! "Nuraydar" (Arabic)
Нұрай:Нұрай NP-ANT-F ; ! "Nuray" 
Нұрайша:Нұрайша NP-ANT-F ; ! "Nuraysha" (Arabic)
Нұрақов:Нұрақов NP-COG-OB ; ! ""
Нұралы:Нұралы NP-ANT-M ; ! "Nuralı" (Arabic)
Нұра:Нұра NP-TOP ; ! ""
Нұрахметов:Нұрахметов NP-COG-OB ; ! ""
Нұрәділ:Нұрәділ NP-ANT-M ; ! "Nurädil" (Arabic)
Нұрәлінов:Нұрәлінов NP-COG-OB ; ! "" ! Use/MT
Нұрбағила:Нұрбағила NP-ANT-F ; ! "Nurbağıyla" (Arabic)
Нұрбаев:Нұрбаев NP-COG-OB ; ! ""
Нұрбай:Нұрбай NP-ANT-M ; ! "Nurbay" (Arabic)
Нұрбақ:Нұрбақ NP-ANT-M ; ! "Nurbaq" (Arabic)
Нұрбапа:Нұрбапа NP-ANT-M ; ! "Nurbapa" (Kazakh)
Нұрбек:Нұрбек NP-ANT-M ; ! "Nurbek" (Kazakh)
Нұрбиби:Нұрбиби NP-ANT-F ; ! "Nurbıybıy" (Arabic)
Нұрболат:Нұрболат NP-ANT-M ; ! "Nurbolat" (Kazakh)
Нұрбол:Нұрбол NP-ANT-M ; ! "Nurbol" (Kazakh)
Нұргелді:Нұргелді NP-ANT-M ; ! "Nurgeldi" (Kazakh)
Нұрғазы:Нұрғазы NP-ANT-M ; ! "Nurğazı" (Arabic)
Нұрғалиев:Нұрғалиев NP-COG-OB ; ! ""
Нұрғалым:Нұрғалым NP-ANT-M ; ! "Nurğalım" (Arabic)
Нұрғалы:Нұрғалы NP-ANT-M ; ! "Nurğalı" (Arabic)
Нұрғаным:Нұрғаным NP-ANT-F ; ! ""
Нұрғиса:Нұрғиса NP-ANT-M ; ! "Nurğıysa" (Arabic)
Нұрғожа:Нұрғожа NP-ANT-M ; ! "Nurğoja" (Arabic)
Нұрдан:Нұрдан NP-ANT-M ; ! "Nurdan" (Kazakh)
Нұржамал:Нұржамал NP-ANT-F ; ! "Nurjamal" (Arabic)
Нұржан:Нұржан NP-ANT-M ; ! "Nurjan" (Persian)
Нұржанов:Нұржанов NP-COG-OB ; ! ""
Нұржау:Нұржау NP-ANT-M ; ! "Nurjaw" (Kazakh)
Нұржахан:Нұржахан NP-ANT-M ; ! "Nurjaxan" (Persian)
Нұржәмила:Нұржәмила NP-ANT-F ; ! "Nurjämiyla" (Arabic)
Нұржекеев:Нұржекеев NP-COG-OB ; ! "" ! Use/MT
Нұржігіт:Нұржігіт NP-ANT-M ; ! "Nurjigit" (Kazakh)
Нұрзада:Нұрзада NP-ANT-M ; ! "Nurzada" (Persian)
Нұрзерек:Нұрзерек NP-ANT-M ; ! "Nurzerek" (Kazakh)
Нұрзила:Нұрзила NP-ANT-F ; ! "Nurzıyla" (Arabic)
Нұрзия:Нұрзия NP-ANT-F ; ! "Nurzıya" (Arabic)
Нұрикамал:Нұрикамал NP-ANT-F ; ! "Nurıykamal" (Arabic)
Нұрила:Нұрила NP-ANT-F ; ! "Nurıyla" (Arabic)
Нұри:Нұри NP-ANT-M ; ! "Nurıy" (Arabic)
Нұрипа:Нұрипа NP-ANT-F ; ! "Nurıypa" (Arabic)
Нұрия:Нұрия NP-ANT-F ; ! "Nurıya" (Arabic)
Нұрқасым:Нұрқасым NP-ANT-M ; ! "Nurqasım" 
Нұрлай:Нұрлай NP-ANT-M ; ! "Nurlay" (Kazakh)
Нұрлан:Нұрлан NP-ANT-M ; ! "Nurlan" (Arabic)
Нұрлат:Нұрлат NP-ANT-M ; ! "Nurlat" (Arabic)
Нұрлыбаев:Нұрлыбаев NP-COG-OB ; ! ""
Нұрлыбай:Нұрлыбай NP-ANT-M ; ! "Nurlıbay" (Arabic)
Нұрлыбек:Нұрлыбек NP-ANT-M ; ! "Nurlıbek" (Arabic)
Нұрлыгүл:Нұрлыгүл NP-ANT-F ; ! "Nurlıgül" 
Нұрлы:Нұрлы NP-ANT-M ; ! "Nurlı" (Arabic)
Нұрмағамбетов:Нұрмағамбетов NP-COG-OB ; ! "" ! Use/MT
Нұрмаханбетов:Нұрмаханбетов NP-COG-OB ; ! ""
Нұрмаханов:Нұрмаханов NP-COG-OB ; ! ""
Нұрмұхамбет:Нұрмұхамбет NP-ANT-M ; ! "Nurmuxambet" (Arabic)
Нұрмұхамедов:Нұрмұхамедов NP-COG-OB ; ! ""
Нұрмұхаметов:Нұрмұхаметов NP-COG-OB ; ! ""
Нұрмыхан:Нұрмыхан NP-ANT-M ; !"Use/MT"
Нұрнияз:Нұрнияз NP-ANT-M ; ! "Nurnıyaz" (Arabic)
Нұр:Нұр NP-ANT-M ; ! "Nur" (Arabic)
Нұр% Отан:Нұр% Отан NP-ORG ; ! ""
Нұрпейсов:Нұрпейсов NP-COG-M ; !"Use/MT"
Нұрпейіс:Нұрпейіс NP-ANT-M ; ! "Nurpeyis" (Persian)
Нұрпейісов:Нұрпейісов NP-COG-OB ; ! ""
Нұрпейісов:Нұрпейісов NP-COG-OB ; ! "" ! Use/MT
Нұрсапа:Нұрсапа NP-ANT-M ; ! "Nursapa" 
Нұрсейіт:Нұрсейіт NP-ANT-M ; ! "Nurseyit" (Arabic)
Нұрсовет:Нұрсовет NP-ANT-M ; ! "Nursovet" (New word)
Нұрсұлтан:Нұрсұлтан NP-ANT-M ; ! "Nursultan" (Arabic)
Нұрсұлу:Нұрсұлу NP-ANT-F ; ! "Nursulıw" (Arabic)
Нұртаев:Нұртаев NP-COG-OB ; ! ""
Нұртай:Нұртай NP-ANT-M ; ! ""
Нұртайұлы:Нұртайұлы NP-ANT-M ; !
Нұрталап:Нұрталап NP-ANT-M ; ! "Nurtalap" (Kazakh)
Нұртас:Нұртас NP-ANT-M ; ! "Nurtas" (Kazakh)
Нұрхан:Нұрхан NP-ANT-F ; ! "Nurxan" 
Нұрхан:Нұрхан NP-ANT-M ; ! "Nurxan" (Arabic)
Нұршайықов:Нұршайықов NP-COG-OB ; ! "" ! Use/MT
Нұршайым:Нұршайым NP-ANT-F ; ! "Nurshayım" (Arabic)
Нұрша:Нұрша NP-ANT-F ; ! "Nursha" (Arabic)
Нұршара:Нұршара NP-ANT-F ; ! "Nurshara" (Arabic)
Нұрым:Нұрым NP-ANT-M ; ! "Nurım" (Arabic)
Нұрышев:Нұрышев NP-COG-OB ; ! ""
Нұсқабай:Нұсқабай NP-ANT-M ; ! "Nusqabay" (Arabic)
Нұсқар:Нұсқар NP-ANT-M ; ! "Nusqar" (Kazakh)
Нұх:Нұх NP-ANT-M ; ! ""
Нүкенов:Нүкенов NP-COG-OB ; ! ""
Нүсіпжанов:Нүсіпжанов NP-COG-OB ; ! ""
Нүсіп:Нүсіп NP-ANT-M ; ! "Nüsip" 
Нығматулин:Нығматулин NP-COG-IN ; ! ""
Нығметжан:Нығметжан NP-ANT-M ; ! "Nığmetjan" (Arabic)
Нығметқали:Нығметқали NP-ANT-M ; ! "Nığmetqalıy" (Arabic)
Нығмет:Нығмет NP-ANT-M ; ! "Nığmet" (Arabic)
Нығметов:Нығметов NP-COG-OB ; ! ""
Нығметолла:Нығметолла NP-ANT-M ; ! "Nığmetolla" (Arabic)
Нысанбаев:Нысанбаев NP-COG-OB ; ! ""
Нысанғалиев:Нысанғалиев NP-COG-OB ; ! ""
Нью%-Дели:Нью%-Дели NP-TOP ; ! ""
Нью% Йорк:Нью% Йорк NP-TOP ; !""
Нью%-Йорк:Нью%-Йорк NP-TOP ; ! "New York"
Нью%-Йорк% таймс:Нью%-Йорк% таймс NP-ORG ; ! ""
Нью%-Йорк% таймс:Нью%-Йорк% Таймс NP-ORG ; ! "" Dir/LR
Нью%-Йорк% таймс:Нью%-Йорк% таймс NP-ORG ; ! "New York Times"
Ньютон:Ньютон NP-COG-MF ; ! ""
Нэлли:Нэлли NP-ANT-F ; !"Use/MT"
Нэнси:Нэнси NP-ANT-F ; !"Use/MT"
Нэшнл:Нэшнл NP-TOP ; !"Use/MT"
Нэш:Нэш NP-ANT-M ; !"Use/MT"
Нюрнберг:Нюрнберг NP-TOP ; ! "" ! Use/MT
Няриса:Няриса NP-TOP ; ! ""
Обаев:Обаев NP-COG-OB ; ! ""
Обай:Обай NP-ANT-M ; ! "Obay" 
Обама:Обама NP-COG-MF ; ! "Obama"
Обогрелов:Обогрелов NP-COG-OB ; ! ""
Образцов:Образцов NP-COG-OB ; ! ""
Обручев:Обручев NP-COG-OB ; ! ""
Обухов:Обухов NP-COG-OB ; ! ""
Обь:Обь NP-TOP ; ! "Ob river"
Овельев:Овельев NP-COG-OB ; ! ""
Овсеенко:Овсеенко NP-COG-MF ; ! ""
Овчинников:Овчинников NP-COG-OB ; ! ""
Огайо:Огайо NP-TOP ; !"Use/MT"
Огишев:Огишев NP-COG-OB ; ! ""
Огурцов:Огурцов NP-COG-OB ; ! ""
Одақ:Одақ NP-ANT-M ; ! "Odaq" (Kazakh)
Одер:Одер NP-TOP ; ! ""
Одесса:Одесса NP-TOP ; ! ""
Одинцов:Одинцов NP-COG-OB ; ! ""
Одоев:Одоев NP-COG-OB ; ! ""
Одри:Одри NP-ANT-F ; !"Use/MT"
Одюков:Одюков NP-COG-OB ; ! ""
Ожарбай:Ожарбай NP-ANT-M ; ! "Ojarbay" (Kazakh)
Озарбек:Озарбек NP-ANT-M ; ! "Ozarbek" (Kazakh)
Озария:Озария NP-ANT-F ; ! "Ozarıya" (Persian)
Озат:Озат NP-ANT-M ; ! "Ozat" (Kazakh)
Озғанбаев:Озғанбаев NP-COG-OB ; ! "" ! Use/MT
Оз:Оз NP-ANT-M ; !"Use/MT"
Ойлы:Ойлы NP-ANT-M ; ! "Oylı" (Kazakh)
Ойрат:Ойрат NP-ANT-M ; ! "Oyrat" (Arabic)
Океания:Океания NP-TOP ; !"Use/MT"
Оклахома:Оклахома NP-TOP ; !"Use/MT"
Оксана:Оксана NP-ANT-F ; ! "Oksana" (Greek)
Оксфорд:Оксфорд NP-TOP ; ! ""
Октябрина:Октябрина NP-ANT-F ; ! "Oktyabrıyna" (New word)
Октябрь:Октябрь NP-ANT-M ; ! "Oktyabrʲ" 
Окунев:Окунев NP-COG-OB ; ! "" ! Use/MT
Оқпаев:Оқпаев NP-COG-OB ; ! ""
Олаф:Олаф NP-ANT-M ; !"Use/MT"
Олегович:Олего NP-PAT-VICH ; ! "" ! Use/MT 
Олег:Олег NP-ANT-M ; ! "Oleg" (Skandinavian)
Олжабай:Олжабай NP-ANT-M ; ! "Oljabay" (Kazakh)
Олжай:Олжай NP-ANT-M ; ! ""
Олжас:Олжас NP-ANT-M ; ! "Oljas" (Kazakh)
Олзоев:Олзоев NP-COG-OB ; ! ""
Оливанов:Оливанов NP-COG-OB ; ! ""
Оливер:Оливер NP-ANT-M ; !"Use/MT"
Олимпия:Олимпия NP-AL ; ! ""
Олимп:Олимп NP-AL ; ! ""
Олли:Олли NP-ANT-M ; !"Use/MT"
Ольга:Ольга NP-ANT-F ; ! "OlʲGa" (Skandinavian)
Оман:Оман NP-TOP ; ! ""
Омарбаев:Омарбаев NP-COG-OB ; ! ""
Омарбай:Омарбай NP-ANT-M ; ! "Omarbay" (Arabic)
Омарбек:Омарбек NP-ANT-M ; ! "Omarbek" (Arabic)
Омарғали:Омарғали NP-ANT-M ; ! "Omarğalıy" (Arabic)
Омари:Омари NP-ANT-M ; !"Use/MT"
Омаров:Омаров NP-COG-OB ; ! ""
Омаров:Омаров NP-COG-OB ; ! ""
Омар:Омар NP-ANT-M ; ! "Omar" (Arabic)
Омархан:Омархан NP-ANT-M ; ! "Omarxan" (Arabic)
Омбы:Омбы NP-TOP ; ! ""
Омега:Омега NP-ANT-M ; !"Use/MT"
Омер:Омер NP-ANT-M ; !"Use/MT"
Омичъ:Омичъ NP-AL ; ! ""
Ом:Ом NP-ANT-M ; !"Use/MT"
Омск:Омск NP-TOP ; ! ""
Омуралиев:Омуралиев NP-COG-OB ; ! "" ! Use/MT
Она:Она NP-ANT-F ; !"Use/MT"
Онбай:Онбай NP-ANT-M ; ! "Onbay" (Kazakh)
Онегин:Онегин NP-COG-OB ; ! ""
Онищенко:Онищенко NP-COG-MF ; ! ""
Онуфриенко:Онуфриенко NP-COG-MF ; ! ""
Оңалбай:Оңалбай NP-ANT-M ; ! "Oŋalbay" (Kazakh)
Оңалбек:Оңалбек NP-ANT-M ; ! "Oŋalbek" (Kazakh)
Оңғарбай:Оңғарбай NP-ANT-M ; ! "Oŋğarbay" (Kazakh)
Оңғар:Оңғар NP-ANT-M ; ! "Oŋğar" (Kazakh)
Оңдасынов:Оңдасынов NP-COG-OB ; ! ""
Оңдасын:Оңдасын NP-ANT-M ; ! "Oŋdasın" (Kazakh)
Оңтүстік% Америка:Оңтүстік% Америка NP-TOP ; ! "South America"
Оңтүстік% Африка% Республика:Оңтүстік% Африка% Республика N-COMPOUND-PX ; !"Use/MT"
Оңтүстік% Корея:Оңтүстік% Корея NP-TOP ; !"Use/MT"
Оңтүстік% Тарава:Оңтүстік% Тарава NP-TOP ; ! "" 
Опабек:Опабек NP-ANT-M ; ! "Opabek" (Kazakh)
Опажан:Опажан NP-ANT-F ; ! "Opajan" (Arabic)
Оразақ:Оразақ NP-ANT-M ; ! "Orazaq" (Arabic)
Оразақын:Оразақын NP-ANT-M ; ! "Orazaqın" (Persian)
Оразалинов:Оразалинов NP-COG-OB ; ! "" ! Use/MT
Оразали:Оразали NP-ANT-M ; ! "Orazalıy" (Arabic)
Оразалы:Оразалы NP-ANT-M ; ! ""
Оразбаев:Оразбаев NP-COG-OB ; ! ""
Оразбай:Оразбай NP-ANT-M ; ! "Orazbay" (Kazakh)
Оразбақов:Оразбақов NP-COG-OB ; ! ""
Оразбек:Оразбек NP-ANT-M ; ! "Orazbek" (Kazakh)
Оразбекұлы:Оразбекұлы NP-COG-M ; ! ""
Оразгелді:Оразгелді NP-ANT-M ; ! "Orazgeldi" (Kazakh)
Оразгүл:Оразгүл NP-ANT-F ; ! "Orazgül" (Persian)
Оразғалиев:Оразғалиев NP-COG-OB ; ! "" ! Use/MT
Оразғұл:Оразғұл NP-ANT-M ; ! "Orazğul" (Kazakh)
Оразмұхамбет:Оразмұхамбет NP-ANT-M ; ! "Orazmuxambet" (Kazakh)
Ораз:Ораз NP-ANT-M ; ! "Oraz" (Old Turkic)
Орақбай:Орақбай NP-ANT-M ; ! "Oraqbay" (Kazakh)
Орақ:Орақ NP-ANT-M ; ! "Oraq" (Old Turkic)
Оралбаев:Оралбаев NP-COG-OB ; ! ""
Оралбай:Оралбай NP-ANT-M ; ! "Oralbay" (Kazakh)
Оралбек:Оралбек NP-ANT-M ; ! "Oralbek" (Kazakh)
Орал:Орал NP-ANT-F ; ! "Oral" (Kazakh)
Орал:Орал NP-ANT-M ; ! "Oral" (Kazakh)
Орал:Орал NP-TOP ; ! "Uralsk"
Орал:Орал NP-TOP ; ! "Oral"
Оралтай:Оралтай NP-ANT-M ; ! "Oraltay" (Kazakh)
Оралұлы:Оралұлы NP-COG-M ; ! "" ! Use/MT
Оралхан:Оралхан NP-ANT-M ; ! "Oralxan" (Kazakh)
Оран:Оран NP-TOP ; !"Use/MT"
Орасан:Орасан NP-ANT-M ; ! "Orasan" (Kazakh)
Ордабаев:Ордабаев NP-COG-OB ; ! "" ! Use/MT
Ордабай:Ордабай NP-ANT-M ; ! "Ordabay" (Old Turkic)
Ордабек:Ордабек NP-ANT-M ; ! "Ordabek" (Old Turkic)
Ордағали:Ордағали NP-ANT-M ; ! "Ordağalıy" (Old Turkic)
Ордалы:Ордалы NP-ANT-M ; ! "Ordalı" (Old Turkic)
Орегон:Орегон NP-TOP ; !"Use/MT"
Орест:Орест NP-ANT-M ; !"Use/MT"
Орехов:Орехов NP-COG-OB ; ! ""
Орешников:Орешников NP-COG-OB ; ! ""
Оринов:Оринов NP-COG-OB ; ! ""
Ори:Ори NP-ANT-F ; !"Use/MT"
Орков:Орков NP-COG-OB ; ! ""
Орландо:Орландо NP-ANT-M ; !"Use/MT"
Орлеан:Орлеан NP-TOP ; ! ""
Орловка:Орловка NP-TOP ; ! ""
Орлов:Орлов NP-COG-OB ; ! ""
Орманбаев:Орманбаев NP-COG-OB ; ! "" ! Use/MT
Орманбай:Орманбай NP-ANT-M ; ! "Ormanbay" (Kazakh)
Орманшы:Орманшы NP-ANT-M ; ! "Ormanshı" (Kazakh)
Орнела:Орнела NP-ANT-F ; !"Use/MT"
Орсариев:Орсариев NP-COG-OB ; ! "" ! Use/MT
Орск:Орск NP-TOP ; ! ""
Орсон:Орсон NP-ANT-M ; !"Use/MT"
Орталық% Африка% Республика:Орталық% Африка% Республика N-COMPOUND-PX ; !"Use/MT"
Орталық% Африка% Республикасы:Орталық% Африка% Республикасы NP-TOP ; !"Use/MT"
Орфей:Орфей NP-ANT-M ; ! ""
Орхон:Орхон NP-TOP ; ! ""
Орхус:Орхус NP-TOP ; ! ""
Орымбаев:Орымбаев NP-COG-OB ; ! ""
Орымбетов:Орымбетов NP-COG-OB ; ! ""
Орынбаев:Орынбаев NP-COG-OB ; ! "" ! Use/MT
Орынбай:Орынбай NP-ANT-M ; ! "Orınbay" (Kazakh)
Орынбеков:Орынбеков NP-COG-OB ; ! ""
Орынбек:Орынбек NP-ANT-M ; ! "Orınbek" (Kazakh)
Орынбор:Оренбург NP-TOP ; ! "" Dir/LR
Орынбор:Орынбор NP-TOP ; ! ""
Орынғали:Орынғали NP-ANT-M ; ! "Orınğalıy" (Kazakh)
Орынша:Орынша NP-ANT-F ; ! "Orınsha" (Kazakh)
Орыс:Орыс NP-TOP ; ! "Russia"
Осака:Осака NP-TOP ; !"Use/MT"
Осакаров:Осакаров NP-COG-OB ; ! ""
Освенцим:Освенцим NP-TOP ; ! "" 
Осетия:Осетия NP-TOP ; ! ""
Осипов:Осипов NP-COG-OB ; ! ""
Оскар:Оскар NP-ANT-M ; !"Use/MT"
Осло:Осло NP-TOP ; ! ""
Осман:Осман NP-TOP ; ! ""
Основьяненко:Основьяненко NP-COG-MF ; ! ""
Оспанәлі:Оспанәлі NP-ANT-M ; ! "Ospanäli" (Arabic)
Оспанбаев:Оспанбаев NP-COG-OB ; ! ""
Оспанбай:Оспанбай NP-ANT-M ; ! "Ospanbay" (Arabic)
Оспанбек:Оспанбек NP-ANT-M ; ! "Ospanbek" (Arabic)
Оспанбет:Оспанбет NP-ANT-M ; ! "Ospanbet" (Arabic)
Оспанғали:Оспанғали NP-ANT-M ; ! "Ospanğalıy" (Arabic)
Оспанов:Оспанов NP-COG-OB ; ! ""
Оспан:Оспан NP-ANT-M ; ! "Osman"
Оспан:Оспан NP-ANT-M ; ! "Ospan" (Arabic)
Оспанхан:Оспанхан NP-ANT-M ; ! "Ospanxan" (Arabic)
Остин:Остин NP-ANT-M ; !"Use/MT"
Остров:Остров NP-AL ; ! ""
Островский:Островский NP-COG-M ; ! ""
Отан:Отан NP-ANT-M ; ! "Otan" (Kazakh)
Отарбай:Отарбай NP-ANT-M ; ! "Otarbay" (Kazakh)
Отарбеков:Отарбеков NP-COG-OB ; ! ""
Отар:Отар NP-ANT-M ; ! "Otar" (Kazakh)
Отелло:Отелло NP-ANT-M ; ! ""
Отеро:Отеро NP-COG-MF ; !"Use/MT"
Отилия:Отилия NP-ANT-F ; !"Use/MT
От:От NP-ANT-M ; !"Use/MT
Отрыванов:Отрыванов NP-COG-OB ; ! ""
Оттон:Оттон NP-ANT-M ; ! ""
Отто:Отто NP-ANT-M ; ! ""
Отунбаев:Отунбаев NP-COG-OB ; ! ""
Отынбаев:Отынбаев NP-COG-OB ; ! ""
Оуэн:Оуэн NP-ANT-M ; !"Use/MT"
Оф:Оф NP-ANT-M ; !"Use/MT"
Охотников:Охотников NP-COG-OB ; ! ""
Ошақбай:Ошақбай NP-ANT-M ; ! "Oshaqbay" (Kazakh)
Ош:Ош NP-TOP ; ! ""
Өжет:Өжет NP-ANT-M ; ! "Öjet" (Kazakh)
Өзбекәлі:Өзбекәлі NP-ANT-M ; ! "Özbekäli" (Kazakh)
Өзбекбай:Өзбекбай NP-ANT-M ; ! "Özbekbay" (Kazakh)
Өзбеков:Өзбеков NP-COG-OB ; ! ""
Өзбек:Өзбек NP-ANT-M ; ! "Özbek" (Kazakh)
Өзбек% ССР:Өзбек% ССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Өзбекстан:Өзбекстан NP-TOP ; ! "Uzbekistan" 
Өзенбай:Өзенбай NP-ANT-M ; ! "Özenbay" (Kazakh)
ӨИҚ:ӨИҚ NP-ORG ; ! "Өзбекстанның ислам қозғалысы - IMU=Islamic Movement of Uzbekistan"
Өксікбаев:Өксікбаев NP-COG-OB ; ! "" ! Use/MT
Өктем:Өктем NP-ANT-M ; ! "Öktem" (Kazakh)
Өкім:Өкім NP-ANT-M ; ! "Ökim" (Arabic)
Өлкебай:Өлкебай NP-ANT-M ; ! "Ölkebay" (Kazakh)
Өліппе:Өліппе NP-TOP ; ! "ABC"
Өмірбаев:Өмірбаев NP-COG-OB ; ! ""
Өмірбай:Өмірбай NP-ANT-M ; ! "Ömirbay" (Kazakh)
Өміргүл:Өміргүл NP-ANT-F ; ! "Ömirgül" (Arabic)
Өмірғазы:Өмірғазы NP-ANT-M ; ! "Ömirğazı" (Arabic)
Өмірғали:Өмірғали NP-ANT-M ; ! "Ömirğalıy" (Arabic)
Өмірзақ:Өмірзақ NP-ANT-M ; ! "Ömirzaq" (Kazakh)
Өмір:Өмір NP-ANT-M ; ! "Ömir" (Arabic)
Өмірше:Өмірше NP-ANT-F ; ! "Ömirshe" (Kazakh)
Өндіріс:Өндіріс NP-ANT-M ; ! "Öndiris" (Kazakh)
Өнеге:Өнеге NP-ANT-F ; ! "Önege" (Kazakh)
Өнербек:Өнербек NP-ANT-M ; ! "Önerbek" (Kazakh)
Өрбике:Өрбике NP-ANT-F ; ! "Örbiyke" (Kazakh)
Өрекен:Өрекен NP-ANT-M ; ! "Öreken" (Old Turkic)
Өрен:Өрен NP-ANT-M ; ! "Ören" (Kazakh)
Өрзия:Өрзия NP-ANT-F ; ! "Örzıya" 
Өрсариев:Өрсариев NP-COG-OB ; ! "" ! Use/MT
Өрім:Өрім NP-ANT-M ; ! "Örim" (Kazakh)
Өсер:Өсер NP-ANT-M ; ! "Öser" (Kazakh)
Өскемен:Өскемен NP-TOP ; ! "Ust-Kamenogorsk"
Өскемен:Усть-Каменогорск NP-TOP ; ! "Ust-Kamenogorsk" ! Dir/LR
Өскенбаев:Өскенбаев NP-COG-OB ; ! ""
Өскенбай:Өскенбай NP-ANT-M ; ! "Öskenbay" (Kazakh)
Өскенбайұлы:Өскенбайұлы NP-COG-M ; ! ""
Өтебаев:Өтебаев NP-COG-OB ; ! "" ! Use/MT
Өтебай:Өтебай NP-ANT-M ; ! "Ötebay" (Kazakh)
Өтеген:Өтеген NP-ANT-M ; ! "Ötegen" (Kazakh)
Өтеғали:Өтеғали NP-ANT-M ; ! "Öteğalıy" (Kazakh)
Өтеғұлов:Өтеғұлов NP-COG-OB ; ! "" ! Use/MT
Өтекешев:Өтекешев NP-COG-OB ; ! ""
Өтелбаев:Өтелбаев NP-COG-OB ; ! "" ! Use/MT
Өтемағамбет:Өтемағамбет NP-ANT-M ; ! "Ötemağambet" (Kazakh)
Өтемұратов:Өтемұратов NP-COG-OB ; ! "" ! Use/MT
Өтемісов:Өтемісов NP-COG-OB ; ! ""
Өтетілеу:Өтетілеу NP-ANT-M ; ! "Ötetilew" (Kazakh)
Өтеулиев:Өтеулиев NP-COG-OB ; ! ""
Өтешев:Өтешев NP-COG-OB ; ! ""
Пабло:Пабло NP-ANT-M ; !"Use/MT"
Павел:Павел NP-ANT-M ; ! ""
Павлович:Павло NP-PAT-VICH ; ! ""
Павлов:Павлов NP-COG-OB ; ! ""
Павлов:Павлов NP-COG-OB ; ! ""
Павлодар:Павлодар NP-TOP ; ! ""
Пазарджик:Пазарджик NP-TOP ; !"Use/MT"
Пазылбек:Пазылбек NP-ANT-M ; ! "Pazılbek" (Arabic)
Пазыл:Пазыл NP-ANT-M ; ! "Pazıl" 
Пайгусов:Пайгусов NP-COG-OB ; ! ""
Пайпер:Пайпер NP-ANT-F ; !"Use/MT"
Палау:Палау NP-TOP ; ! ""
Палермо:Палермо NP-TOP ; !"Use/MT"
Палестина:Палестина NP-TOP ; ! ""
Палестин:Палестин NP-TOP ; !""
Палласов:Палласов NP-COG-OB ; ! ""
Палмер:Палмер NP-ANT-M ; !"Use/MT"
Палуанғали:Палуанғали NP-ANT-M ; ! "Palıwanğalıy" (Arabic)
Палуан:Палуан NP-ANT-M ; ! "Palıwan" (Arabic)
Памела:Памела NP-ANT-F ; !"Use/MT"
Памир:Памир NP-TOP ; ! ""
Панабек:Панабек NP-ANT-M ; ! "Panabek" (Persian)
Панама:Панама NP-TOP ; ! ""
Пана:Пана NP-ANT-F ; ! "Pana" (Persian)
Пандора:Пандора NP-ANT-F ; !"Use/MT"
Панев:Панев NP-COG-OB ; ! ""
Панеке:Панеке NP-COG-MF ; ! ""
Панов:Панов NP-COG-OB ; ! ""
Пан:Пан NP-ANT-M ; ! "Pan"
Пантелеймонов:Пантелеймонов NP-COG-OB ; ! ""
Панфилов:Панфилов NP-COG-OB ; ! ""
Панюков:Панюков NP-COG-OB ; ! ""
Паола:Паола NP-ANT-F ; !"Use/MT"
Паоло:Паоло NP-ANT-M ; !"Use/MT"
Па:Па NP-ANT-M ; !"Use/MT"
Папанов:Папанов NP-COG-OB ; ! ""
Папуа% Жаңа% Гвинея:Папуа% Жаңа% Гвинея NP-TOP ; !"Use/MT"
Папуа:Папуа NP-TOP ; ! ""
Парагвай:Парагвай NP-TOP ; ! ""
Параиба:Параиба NP-TOP ; !"Use/MT"
Парасат:Парасат NP-ANT-F ; ! "Parasat" (Arabic)
Парасат:Парасат NP-ANT-M ; ! "Parasat" (Arabic)
Парзан:Парзан NP-ANT-F ; ! "Parzan" (Persian)
Париж:Париж NP-TOP ; ! "Paris"
Паркер:Паркер NP-ANT-F ; !"Use/MT"
Паркер:Паркер NP-ANT-M ; !"Use/MT"
Паркер:Паркер NP-COG-MF ; !"Use/MT"
Пар:Пар NP-ANT-M ; !"Use/MT"
Парсонс:Парсонс NP-TOP ; !"Use/MT"
Парфенов:Парфенов NP-COG-OB ; ! ""
Парфия:Парфия NP-TOP ; !"Use/MT"
Паскаль:Паскаль NP-COG-MF ; ! ""
Пас:Пас NP-ANT-F ; !"Use/MT"
Пастер:Пастер NP-COG-MF ; !"Use/MT"
Пастор:Пастор NP-ANT-M ; !"Use/MT"
Пасха:Пасха NP-ORG ; !"Use/MT"
Патрик:Патрик NP-ANT-M ; !"Use/MT"
Патрисия:Патрисия NP-ANT-F ; !"Use/MT"
Патриция:Патриция NP-ANT-F ; !"Use/MT"
Паула:Паула NP-ANT-F ; !"Use/MT"
Паули:Паули NP-ANT-F ; !"Use/MT"
Паулу:Паулу NP-ANT-M ; !"Use/MT"
Пау:Пау NP-ANT-M ; !"Use/MT"
Пауыл:Пауль NP-ANT-M ; ! "Paul" Dir/LR
Пауыл:Пауыл NP-ANT-M ; ! "Paul"
Пахоменко:Пахоменко NP-COG-MF ; ! ""
Пахомов:Пахомов NP-COG-OB ; ! ""
Паш:Паш NP-ANT-M ; !"Use/MT"
Пәкістан:Пәкістан NP-TOP ; ! "Pakistan" 
Педро:Педро NP-ANT-M ; ! ""
Пейдж:Пейдж NP-ANT-F ; !"Use/MT"
Пекин:Пекин NP-TOP ; ! ""
Пелеев:Пелеев NP-COG-OB ; ! ""
Пеленган:Пеленган NP-TOP ; ! ""
Пелла:Пелла NP-COG-MF ; !"Use/MT"
Пелоси:Пелоси NP-COG-MF ; !"Use/MT"
Пел:Пел NP-ANT-M ; !"Use/MT"
Пена:Пена NP-COG-MF ; ! ""
Пенелопа:Пенелопа NP-ANT-F ; !"Use/MT"
Пенза:Пенза NP-TOP ; ! ""
Пенсильвания:Пенсильвания NP-TOP ; !"Use/MT"
Пентагон:Пентагон NP-ORG ; ! ""
Пепе:Пепе NP-ANT-M ; !"Use/MT"
Перғауын:Перғауын NP-TOP ; ! ""
Пердебек:Пердебек NP-ANT-M ; ! "Perdebek" (Arabic)
Пердью:Пердью NP-TOP ; !"Use/MT"
Передвижников:Передвижников NP-COG-OB ; ! ""
Перейра:Перейра NP-TOP ; !"Use/MT"
Пере:Пере NP-ANT-M ; !"Use/MT"
Перес:Перес NP-COG-MF ; ! ""
Перизат:Перизат NP-ANT-F ; ! "Perıyzat" 
Перикл:Перикл NP-ANT-M ; !"Use/MT"
Перла:Перла NP-ANT-F ; !"Use/MT"
Пермь:Пермь NP-TOP ; ! ""
Пернамбуку:Пернамбуку NP-TOP ; !"Use/MT"
Пернешұлы:Пернешұлы NP-COG-M ; ! ""
Перник:Перник NP-TOP ; !"Use/MT"
Перов:Перов NP-COG-OB ; ! ""
Перрин:Перрин NP-ANT-M ; !"Use/MT"
Перри:Перри NP-ANT-M ; !"Use/MT"
Персей:Персей NP-ANT-M ; ! ""
Персиваль:Персиваль NP-ANT-M ; !"Use/MT"
Перу:Перу NP-TOP ; ! ""
Перцев:Перцев NP-COG-OB ; ! ""
Перцов:Перцов NP-COG-OB ; ! ""
Пестов:Пестов NP-COG-OB ; ! ""
Петербург:Петербург NP-TOP ; ! ""
Петерсон:Петерсон NP-COG-MF ; ! ""
Петляков:Петляков NP-COG-OB ; ! ""
Петра:Петра NP-ANT-F ; !"Use/MT"
Петреус:Петреус NP-COG-MF ; ! ""
Петрович:Петро NP-PAT-VICH ; ! ""
Петров:Петров NP-COG-OB ; ! ""
Петроград:Петроград NP-TOP ; !""
Петроков:Петроков NP-COG-OB ; ! ""
Петропавловск:Петропавловск NP-TOP ; ! ""
Петропавл:Петропавл NP-TOP ; ! ""
Петр:Петр NP-ANT-M ; ! ""
Петухов:Петухов NP-COG-OB ; ! ""
Петір:Петір NP-ANT-M ; ! "Peter"
Петя:Петя NP-ANT-M ; ! ""
Пехлеви:Пехлеви NP-COG-MF ; !"Use/MT"
Печников:Печников NP-COG-OB ; ! ""
Печора:Печора NP-TOP ; ! ""
Пётр:Пётр NP-ANT-M ; ! ""
Пиауи:Пиауи NP-TOP ; !"Use/MT"
Пиебалгс:Пиебалгс NP-COG-MF ; !"Use/MT"
Пикассо:Пикассо NP-COG-MF ; !"Use/MT"
Пил:Пил NP-ANT-M ; !"Use/MT"
Пино:Пино NP-ANT-M ; !"Use/MT"
Пио:Пио NP-ANT-M ; !"Use/MT"
Пи:Пи NP-ANT-M ; !"Use/MT"
Пи:Пи NP-ANT-M ; !"Use/MT"
Пиреней:Пиреней NP-TOP ; ! ""
Пирогов:Пирогов NP-COG-OB ; ! ""
Пир:Пир NP-ANT-M ; !"Use/MT"
Пис:Пис NP-ANT-M ; !"Use/MT"
Питер:Питер NP-ANT-M ; !"Use/MT"
Питер:Питер NP-TOP ; ! ""
Питлев:Питлев NP-COG-OB ; ! ""
Питрәч:Питрәч NP-TOP ; ! ""
Питт:Питт NP-COG-MF ; !"Use/MT"
Пифагор:Пифагор NP-ANT-M ; ! ""
Планка:Планка NP-COG-MF ; !"Use/MT"
Планк:Планк NP-COG-MF ; ! ""
Пласидо:Пласидо NP-ANT-M ; !"Use/MT"
Платини:Платини NP-COG-MF ; ! "Platini"
Платонов:Платонов NP-COG-OB ; ! ""
Платон:Платон NP-ANT-M ; ! ""
Плевен:Плевен NP-TOP ; !"Use/MT"
Плетнев:Плетнев NP-COG-OB ; ! ""
Плеханов:Плеханов NP-COG-OB ; ! ""
Плешаков:Плешаков NP-COG-OB ; ! ""
Плинио:Плинио NP-ANT-M ; !"Use/MT"
Пловдив:Пловдив NP-TOP ; !"Use/MT"
Плотников:Плотников NP-COG-OB ; ! ""
Плугов:Плугов NP-COG-OB ; ! ""
Плутон:Плутон NP-AL ; ! ""
Победоносцев:Победоносцев NP-COG-OB ; ! ""
Поведа:Поведа NP-COG-MF ; !"Use/MT"
Погодаев:Погодаев NP-COG-OB ; ! ""
Подворков:Подворков NP-COG-OB ; ! ""
Подосинов:Подосинов NP-COG-OB ; ! ""
Познань:Познань NP-TOP ; ! ""
Полански:Полански NP-COG-MF ; !"Use/MT"
Полежаев:Полежаев NP-COG-OB ; ! "" ! Use/MT
Поливанов:Поливанов NP-COG-OB ; ! ""
полиграфия:полиграфия N1 ; !"Use/MT"
Поликарпов:Поликарпов NP-COG-OB ; ! ""
Полина:Полина NP-ANT-F ; ! ""
Полинезия:Полинезия NP-TOP ; !""
Поло:Поло NP-ANT-M ; !"Use/MT"
Полоруссов:Полоруссов NP-COG-OB ; ! ""
Пол:Пол NP-ANT-M ; !"Use/MT"
Полтавченко:Полтавченко NP-COG-MF ; ! ""
Полугрудов:Полугрудов NP-COG-OB ; ! ""
Польша:Польша NP-TOP ; ! "Poland"
Поляков:Поляков NP-COG-OB ; ! ""
Поляк:Поляк NP-TOP ; ! "Pole"
Поминов:Поминов NP-COG-OB ; ! ""
Пономарев:Пономарев NP-COG-OB ; ! ""
Пономарёв:Пономарёв NP-COG-OB ; ! ""
Пон:Пон NP-ANT-M ; !"Use/MT"
По:По NP-ANT-M ; !"Use/MT"
Попов:Попов NP-COG-OB ; ! ""
Порошенко:Порошенко NP-COG-MF ; ! ""
Портнов:Портнов NP-COG-OB ; ! ""
Португалия:Португалия NP-TOP ; ! "Portugal"
Порту:Порту NP-TOP ; !"Use/MT"
Поручиков:Поручиков NP-COG-OB ; ! ""
Порфирьев:Порфирьев NP-COG-OB ; ! ""
Поспелов:Поспелов NP-COG-OB ; ! "" ! Use/MT
Потапов:Потапов NP-COG-OB ; ! ""
Поттеринг:Поттеринг NP-COG-MF ; !"Use/MT"
Поттер:Поттер NP-COG-MF ; !"Use/MT"
Пояндаев:Пояндаев NP-COG-OB ; ! ""
Правда:Правда NP-AL ; ! ""
Прага:Прага NP-TOP ; ! "Prague"
Преваль:Преваль NP-COG-MF ; ! "" ! Use/MT 
Преголя:Преголя NP-TOP ; !""
Пресервед:Пресервед NP-TOP ; !"Use/MT"
Преснов:Преснов NP-COG-OB ; ! ""
Пресногорьков:Пресногорьков NP-COG-OB ; ! ""
Претория:Претория NP-TOP ; !"Use/MT"
Прибылов:Прибылов NP-COG-OB ; ! ""
Прието:Прието NP-COG-MF ; !"Use/MT"
Примаков:Примаков NP-COG-OB ; ! ""
Принцесса:Принцесса NP-ANT-F ; !"Use/MT"
Принц:Принц NP-ANT-M ; !"Use/MT"
Прис:Прис NP-ANT-M ; !"Use/MT"
Пришепенко:Пришепенко NP-COG-MF ; ! ""
Приштина:Приштина NP-TOP ; ! "" 
Проди:Проди NP-COG-MF ; ! ""
Прокопьев:Прокопьев NP-COG-OB ; ! ""
Прокофьев:Прокофьев NP-COG-OB ; ! ""
Проктер:Проктер NP-COG-MF ; !"Use/MT"
Прорва:Прорва NP-TOP ; ! ""
Прохоренко:Прохоренко NP-COG-MF ; ! ""
Прохоров:Прохоров NP-COG-OB ; ! ""
Проч:Проч NP-ANT-M ; !"Use/MT"
Пруст:Пруст NP-COG-MF ; !"Use/MT"
Птолемей:Птолемей NP-ANT-M ; ! ""
Пуанкаре:Пуанкаре NP-COG-IN ; ! ""
Пуаро:Пуаро NP-COG-MF ; !"Use/MT"
Пугачев:Пугачев NP-COG-OB ; ! ""
Пугачев:Пугачев NP-COG-OB ; ! ""
Пулков:Пулков NP-COG-OB ; ! ""
Пупорев:Пупорев NP-COG-OB ; ! ""
Пу:Пу NP-ANT-M ; !"Use/MT"
Пустовойтенко:Пустовойтенко NP-COG-MF ; ! ""
Путеров:Путеров NP-COG-OB ; ! ""
Путилов:Путилов NP-COG-OB ; ! ""
Путин:Путин NP-COG-IN ; ! "Putin"
Пуч:Пуч NP-COG-MF ; !"Use/MT"
Пушкин:Пушкин NP-COG-IN ; ! ""
Пуэрто%-Рико:Пуэрто%-Рико NP-TOP ; ! ""
Пхеньян:Пхеньян NP-TOP ; ! ""
Пірәлиев:Пірәлиев NP-COG-OB ; ! "" ! Use/MT
Пірәлі:Пірәлі NP-ANT-M ; ! "Piräli" (Arabic)
Пірімгүл:Пірімгүл NP-ANT-M ; ! "Pirimgül" (Kazakh)
Пішенбаев:Пішенбаев NP-COG-OB ; ! ""
Пьедонт:Пьедонт NP-TOP ; !"Use/MT"
Пьемонт:Пьемонт NP-TOP ; ! ""
Пьеро:Пьеро NP-ANT-M ; !"Use/MT"
Пьер:Пьер NP-ANT-M ; ! ""
Пьетро:Пьетро NP-ANT-M ; !"Use/MT"
Пэрис:Пэрис NP-ANT-F ; !"Use/MT"
Пэт:Пэт NP-ANT-M ; !"Use/MT"
Пятиков:Пятиков NP-COG-OB ; ! ""
Рабат:Рабат NP-ANT-M ; ! "Rabat" (Arabic)
Рабия:Рабия NP-ANT-F ; ! "Rabıya" (Arabic)
Раб:Раб NP-ANT-M ; !"Use/MT"
Радаев:Радаев NP-COG-OB ; ! ""
Радж:Радж NP-ANT-M ; ! "Radj" (New word)
Радий:Радий NP-ANT-M ; ! "Radıy" (New word)
Радищев:Радищев NP-COG-OB ; ! ""
Радлов:Радлов NP-COG-OB ; ! ""
Рад:Рад NP-ANT-M ; !"Use/MT"
Радченко:Радченко NP-COG-MF ; ! ""
Разақ:Разақ NP-ANT-M ; ! "Razaq" (Arabic)
Разумов:Разумов NP-COG-OB ; ! ""
Разумов:Разумов NP-COG-OB ; ! "" ! Use/MT
Раил:Раил NP-ANT-M ; ! "Rail" (Arabic)
Раиса:Раиса NP-ANT-F ; ! "Raisa" 
Райан:Райан NP-ANT-F ; !"Use/MT"
Райан:Райан NP-ANT-M ; !"Use/MT"
Райбаев:Райбаев NP-COG-OB ; ! ""
Райбай:Райбай NP-ANT-M ; ! "Raybay" (Kazakh)
Райбек:Райбек NP-ANT-M ; ! "Raybek" (Arabic)
Райгүл:Райгүл NP-ANT-F ; ! "Raygül" (Arabic)
Райд:Райд NP-COG-MF ; !"Use/MT"
Раймонд:Раймонд NP-ANT-M ; !"Use/MT"
Райна:Райна NP-ANT-F ; !"Use/MT"
Райнер:Райнер NP-ANT-M ; !"Use/MT"
Райнұр:Райнұр NP-ANT-M ; ! "Raynur" (Arabic)
Рай:Рай NP-ANT-M ; ! "Ray" (Arabic)
Райхан:Райхан NP-ANT-F ; ! "Rayxan" (Arabic)
Райымбек:Райымбек NP-ANT-M ; ! "Rayımbek" (Arabic)
Райыс:Райыс NP-ANT-M ; ! "Rayıs" (Arabic)
Раков:Раков NP-COG-OB ; ! ""
Рак:Рак NP-ANT-M ; !"Use/MT"
Рақап:Рақап NP-ANT-M ; ! "Raqap" (Arabic)
Рақымов:Рақымов NP-COG-OB ; ! ""
Рақым:Рақым NP-ANT-M ; ! "Raqım" (Arabic)
Рақышев:Рақышев NP-COG-OB ; ! "" ! Use/MT
Ральф:Ральф NP-ANT-M ; !"Use/MT"
Рамазанов:Рамазанов NP-COG-OB ; ! ""
Рамазан:Рамазан NP-ANT-M ; ! "Ramazan" (Arabic)
Раманқұлов:Раманқұлов NP-COG-OB ; ! "" ! Use/MT
Рамиз:Рамиз NP-ANT-M ; ! "Ramıyz" (Arabic)
Рамирес:Рамирес NP-COG-MF ; !"Use/MT"
Рамон:Рамон NP-ANT-M ; !"Use/MT"
Рамсфелд:Рамсфелд NP-COG-MF ; !"Use/MT"
Ранис:Ранис NP-ANT-M ; ! ""
Ра:Ра NP-ANT-M ; !"Use/MT"
Расима:Расима NP-ANT-F ; ! "Rasıyma" (Arabic)
Расков:Расков NP-COG-OB ; ! ""
Расо:Расо NP-ANT-M ; ! 
Распутин:Распутин NP-COG-IN ; ! ""
Рассел:Рассел NP-ANT-M ; !"Use/MT"
Расул:Расул NP-ANT-M ; ! "Rasıwl" (Arabic)
Ратманов:Ратманов NP-TOP ; ! ""
Ратцингер:Ратцингер NP-COG-MF ; !"Use/MT"
Ратьков:Ратьков NP-COG-OB ; ! ""
Рауан:Рауан NP-ANT-M ; ! "Rawan" (Persian)
Рауза:Рауза NP-ANT-F ; ! "Rawza" (Arabic)
Рауль:Рауль NP-ANT-M ; !"Use/MT"
Раушан:Раушан NP-ANT-F ; ! "Rawshan" (Persian)
Раушан:Раушан NP-ANT-M ; ! ""
Рафаель:Рафаель NP-ANT-M ; ! ""
Рафаэль:Рафаэль NP-ANT-M ; ! "Rafaelʲ" (Ancient Hebrew)
Рафель:Рафель NP-ANT-M ; !"Use/MT"
Рафика:Рафика NP-ANT-F ; ! "Rafika" (Arabic)
Рафиков:Рафиков NP-COG-OB ; ! ""
Рафик:Рафик NP-ANT-M ; ! "Rafik" (Arabic)
Рафил:Рафил NP-ANT-M ; ! ""
Рафих:Рафих NP-ANT-M ; ! "Rafix" (Arabic)
Рафия:Рафия NP-COG-MF ; !"Use/MT"
Рафсанджани:Рафсанджани NP-COG-MF ; !"Use/MT"
Рахат:Рахат NP-ANT-M ; ! "Raxat" (Arabic)
Рахила:Рахила NP-ANT-F ; ! "Raxıyla" (Arabic)
Рахима:Рахима NP-ANT-F ; ! "Raxıyma" (Arabic)
Рахимов:Рахимов NP-COG-OB ; ! ""
Рахия:Рахия NP-ANT-F ; ! "Raxıya" (Arabic)
Рахмадиев:Рахмадиев NP-COG-OB ; ! ""
Рахмадиұлы:Рахмадиұлы NP-COG-M ; ! ""
Рахманинов:Рахманинов NP-COG-OB ; ! ""
Рахманов:Рахманов NP-COG-OB ; ! ""
Рахман:Рахман NP-ANT-M ; ! "Raxman" (Arabic)
Рахметолла:Рахметолла NP-ANT-M ; ! "Raxmetolla" (Arabic)
Рахметоллаұлы:Рахметоллаұлы NP-COG-M ; ! ""
Рахмет:Рахмет NP-ANT-M ; ! "Raxmet" (Arabic)
Рахмонов:Рахмонов NP-COG-OB ; ! "" ! Use/MT
Рахмон:Рахмон NP-COG-MF ; ! ""
Рахов:Рахов NP-COG-OB ; ! ""
Рахой:Рахой NP-ANT-M ; !"Use/MT"
Рахымбаев:Рахымбаев NP-COG-OB ; ! "" ! Use/MT
Рахымқанұлы:Рахымқанұлы NP-COG-M ; ! "USE/MT"
Рашида:Рашида NP-ANT-F ; ! "Rashıyda" (Arabic)
Рашид:Рашид NP-ANT-M ; ! ""
Рашид:Рашид NP-COG-M ; !"Use/MT"
Рашит:Рашит NP-ANT-M ; ! "Rashıyt" (Arabic)
Рәбиға:Рәбиға NP-ANT-F ; ! "Räbiyğa" (Arabic)
Рәзиев:Рәзиев NP-COG-OB ; ! ""
Рәзия:Рәзия NP-ANT-F ; ! "Räzıya" (Arabic)
Рдюков:Рдюков NP-COG-OB ; ! ""
Реал% Мадрид:Реал% Мадрид%{☭%} NP-ORG ; ! "Real Madrid Club de Fútbol"
Ребекка:Ребекка NP-ANT-F ; !"Use/MT"
Реболло:Реболло NP-COG-MF ; !"Use/MT"
Ребров:Ребров NP-COG-OB ; ! ""
Ревенко:Ревенко NP-COG-MF ; ! ""
Ревмир:Ревмир NP-ANT-M ; ! "Revmir" (New word)
Регина:Регина NP-ANT-F ; !"Use/MT"
Редгрейв:Редгрейв NP-COG-MF ; !"Use/MT"
Режеп:Режеп NP-ANT-M ; ! ""
Реза:Реза NP-ANT-M ; ! ""
Резников:Резников NP-COG-OB ; ! ""
Рейд:Рейд NP-ANT-M ; !"Use/MT"
Рейкиявик:Рейкиявик NP-TOP ; ! ""
Рейкиявик:Рейкиявик NP-TOP ; ! "" 
Рейкьявик:Рейкьявик NP-TOP ; !"Use/MT"
Рей:Рей NP-ANT-M ; !"Use/MT"
Рейтер:Рейтер NP-ORG ; ! ""
Рекеев:Рекеев NP-COG-OB ; ! ""
Рембрандт:Рембрандт NP-ANT-M ; !"Use/MT"
Ремезов:Ремезов NP-COG-OB ; ! ""
Ремиджио:Ремиджио NP-ANT-M ; !"Use/MT"
Ремингтон:Ремингтон NP-ANT-M ; !"Use/MT"
Ремчуков:Ремчуков NP-COG-OB ; ! ""
Рената:Рената NP-ANT-F ; ! "Renata" (New word)
Ренато:Ренато NP-ANT-M ; !"Use/MT"
Ренат:Ренат NP-ANT-M ; ! "Renat" (New word)
Рене:Рене NP-ANT-F ; !"Use/MT"
Рене:Рене NP-ANT-M ; ! "" ! Use/MT
Рено:Рено NP-AL ; ! ""
Рено:Рено NP-ANT-M ; !"Use/MT"
Рено:Рено NP-COG-MF ; ! ""
Ренцо:Ренцо NP-ANT-M ; !"Use/MT"
Репин:Репин NP-COG-M ; ! ""
Ре:Ре NP-ANT-M ; !"Use/MT"
Ре:Ре NP-ANT-M ; !"Use/MT"
Ресей:Ресей NP-TOP ; ! "Russia"
Реутерс:Реутерс NP-ORG ; ! ""
Реутов:Реутов NP-COG-OB ; ! ""
Рехн:Рехн NP-COG-M ; ! ""
Ржанов:Ржанов NP-COG-OB ; ! ""
Рзақұл:Рзақұл NP-ANT-M ; !"Use/MT"
Риа:Риа NP-ANT-F ; !"Use/MT"
Рива:Рива NP-ANT-F ; !"Use/MT"
Ривера:Ривера NP-COG-MF ; !"Use/MT"
Рига:Рига NP-TOP ; ! ""
Рига:Рига NP-TOP ; ! "" 
Ригов:Ригов NP-COG-OB ; ! ""
Рид:Рид NP-ANT-M ; !"Use/MT"
Риз:Риз NP-ANT-M ; !"Use/MT"
Рикарда:Рикарда NP-ANT-F ; !"Use/MT"
Рикардо:Рикардо NP-ANT-M ; !"Use/MT"
Рикельме:Рикельме NP-COG-MF ; !"Use/MT"
Рико:Рико NP-ANT-M ; !"Use/MT"
Рико:Рико NP-COG-MF ; !"Use/MT"
Рик:Рик NP-ANT-M ; !"Use/MT"
Римов:Римов NP-COG-OB ; ! ""
Рим:Рим NP-TOP ; ! ""
Рим:Рим NP-TOP ; ! "Rome"
Римский%-Корсаков:Римский%-Корсаков NP-COG-M ; ! ""
Рино:Рино NP-ANT-M ; !"Use/MT"
Рин:Рин NP-ANT-M ; !"Use/MT"
Рио%-де%-Жанейро:Рио%-де%-Жанейро NP-TOP ; ! ""
Риоха:Риоха NP-TOP ; !"Use/MT"
Ри:Ри NP-ANT-M ; !"Use/MT"
Рис:Рис NP-ANT-M ; !"Use/MT"
Рита:Рита NP-ANT-F ; !"Use/MT"
Рит:Рит NP-ANT-M ; !"Use/MT"
Рихтер:Рихтер NP-COG-MF ; !"Use/MT"
Ричард:Ричард NP-ANT-M ; ! ""
Ришат:Ришат NP-ANT-M ; ! "Rıyshat" (Arabic)
Роберта:Роберта NP-ANT-F ; !"Use/MT"
Роберто:Роберто NP-ANT-M ; ! "" ! Use/MT
Роберт:Роберт NP-ANT-M ; ! ""
Робертс:Робертс NP-COG-MF ; !"Use/MT"
Робин:Робин NP-ANT-F ; !"Use/MT"
Робинсон:Робинсон NP-COG-MF ; !"Use/MT"
Робиньо:Робиньо NP-ANT-M ; !"Use/MT"
Ровена:Ровена NP-ANT-F ; !"Use/MT"
Рогов:Рогов NP-COG-OB ; ! ""
Рогун:Рогун NP-TOP ; ! ""
Родезия:Родезия NP-TOP ; !"Use/MT"
Роден:Роден NP-COG-MF ; !"Use/MT"
Родерик:Родерик NP-ANT-M ; !"Use/MT"
Роджер:Роджер NP-ANT-M ; !"Use/MT"
Родзянко:Родзянко NP-COG-MF ; ! ""
Родионов:Родионов NP-COG-OB ; ! ""
Родригес:Родригес NP-COG-MF ; !"Use/MT"
Родченко:Родченко NP-COG-MF ; ! ""
Рожков:Рожков NP-COG-OB ; ! ""
Розана:Розана NP-ANT-F ; !"Use/MT"
Розанна:Розанна NP-ANT-F ; !"Use/MT"
Розария:Розария NP-ANT-F ; !"Use/MT"
Роза:Роза NP-ANT-F ; ! "Roza" 
Розлана:Розлана NP-ANT-F ; ! "" 
Розмарин:Розмарин NP-ANT-F ; !"Use/MT"
Розов:Розов NP-COG-OB ; ! ""
Розыбақиев:Розыбақиев NP-COG-OB ; ! "" ! Use/MT
Рой:Рой NP-ANT-M ; !"Use/MT"
Рокко:Рокко NP-ANT-M ; !"Use/MT"
Роксана:Роксана NP-ANT-F ; !"Use/MT"
Рокфеллер:Рокфеллер NP-COG-MF ; !"Use/MT"
Роландо:Роландо NP-ANT-M ; !"Use/MT"
Роланд:Роланд NP-ANT-M ; !"Use/MT"
Романа:Романа NP-ANT-F ; !"Use/MT"
Романенко:Романенко NP-COG-MF ; ! ""
Романов:Романов NP-COG-OB ; ! ""
Романо:Романо NP-ANT-M ; !"Use/MT"
Роман:Роман NP-ANT-M ; ! ""
Романья:Романья NP-TOP ; !"Use/MT"
Ромео:Ромео NP-ANT-M ; ! ""
Ромеро:Ромеро NP-COG-MF ; !"Use/MT"
Ромни:Ромни NP-COG-MF ; ! ""
Ромула:Ромула NP-TOP ; ! "" 
Роналду:Роналду NP-ANT-M ; !"Use/MT"
Рональд:Рональд NP-ANT-M ; !"Use/MT"
Ронан:Ронан NP-ANT-M ; !"Use/MT"
Ронко:Ронко NP-COG-MF ; ! ""
Рон:Рон NP-ANT-M ; !"Use/MT"
Рори:Рори NP-ANT-M ; !"Use/MT"
Ро:Ро NP-ANT-M ; !"Use/MT"
Ро:Ро NP-ANT-M ; !"Use/MT"
Росковшенко:Росковшенко NP-COG-MF ; ! ""
Росси:Росси NP-COG-MF ; !"Use/MT"
Россия:Россия NP-TOP ; ! "Russia"
Росс:Росс NP-ANT-M ; !"Use/MT"
Ростов:Ростов NP-COG-OB ; ! ""
Роттердам:Роттердам NP-TOP ; !""
Рөстәм:Рөстәм NP-ANT-M ; ! ""
РСФКР:РСФКР NP-TOP-ABBR ; ! ""
Руанда:Руанда NP-TOP ; ! ""
Рубалькаба:Рубалькаба NP-COG-MF ; !"Use/MT"
Рубинов:Рубинов NP-COG-OB ; ! ""
Рубцов:Рубцов NP-COG-OB ; ! ""
Руденко:Руденко NP-COG-MF ; ! ""
Рудольф:Рудольф NP-ANT-M ; !"Use/MT"
Рудольф:Рудольф NP-TOP ; ! ""
Рузвельт:Рузвельт NP-COG-M ; ! ""
Руис:Руис NP-COG-MF ; !"Use/MT"
Румыния:Румыния NP-TOP ; ! "Romania"
Румянцев:Румянцев NP-COG-OB ; ! ""
Рурк:Рурк NP-COG-MF ; !"Use/MT"
Ру:Ру NP-ANT-M ; !"Use/MT"
Русаков:Русаков NP-COG-OB ; ! ""
Русанов:Русанов NP-COG-OB ; ! ""
Русинов:Русинов NP-COG-OB ; ! ""
Руслана:Руслана NP-ANT-F ; ! "Rıwslana" (Russian)
Руслан:Руслан NP-ANT-M ; ! "Rıwslan" (Arabic)
Руссефф:Руссефф NP-COG-MF ; ! ""
Руссо:Руссо NP-COG-MF ; ! ""
Рустам:Рустам NP-ANT-M ; ! ""
Русь:Русь NP-TOP ; ! "" ! Use/MT
Рут:Рут NP-ANT-F ; !"Use/MT"
Руфия:Руфия NP-ANT-F ; ! "Rufiya" (Persian)
Рухия:Рухия NP-ANT-F ; ! "Rıwxıya" (Persian)
Рүстемов:Рүстемов NP-COG-OB ; ! ""
Рүстем:Рүстем NP-ANT-M ; ! "Rüstem" (Persian)
РФ:РФ NP-TOP-ABBR ; ! ""
Рыбаков:Рыбаков NP-COG-OB ; ! ""
Рыбак:Рыбак NP-COG-MF ; ! ""
Рыженко:Рыженко NP-COG-MF ; ! ""
Рыжков:Рыжков NP-COG-OB ; ! "" ! Use/MT
Рыков:Рыков NP-COG-OB ; ! ""
Рымбаев:Рымбаев NP-COG-OB ; ! ""
Рымтайұлы:Рымтайұлы NP-ANT-M ; !
Рынков:Рынков NP-COG-OB ; ! ""
Рысбек:Рысбек NP-ANT-M ; ! "Rısbek" 
Рысқалиев:Рысқалиев NP-COG-OB ; ! "" ! Use/MT
Рысқұлбекұлы:Рысқұлбекұлы NP-COG-M ; ! ""
Рысқұлов:Рысқұлов NP-COG-OB ; ! ""
Рычков:Рычков NP-COG-OB ; ! ""
Рью:Рью NP-ANT-M ; !"Use/MT"
Рэйчел:Рэйчел NP-ANT-F ; !"Use/MT"
Рэнди:Рэнди NP-ANT-M ; !"Use/MT"
Рэпх:Рэпх NP-ANT-M ; !"Use/MT"
Рюрикович:Рюрикович NP-COG-M ; ! ""
Рюрик:Рюрик NP-ANT-M ; ! ""
Рюрик:Рюрик NP-ANT-M ; ! ""
Рябов:Рябов NP-COG-OB ; ! ""
Рябцев:Рябцев NP-COG-OB ; ! "" ! Use/MT
Рябышев:Рябышев NP-COG-OB ; ! ""
Рязанов:Рязанов NP-COG-OB ; ! ""
Рязань:Рязань NP-TOP ; ! ""
Сəтбаев:Сəтбаев NP-COG-OB ; ! ""
Сааведра:Сааведра NP-COG-MF ; ! ""
Сааданбеков:Сааданбеков NP-COG-OB ; ! "" ! Use/MT
Сабаз:Сабаз NP-ANT-M ; ! "Sabaz" (Persian)
Сабаншы:Сабаншы NP-ANT-M ; ! "Sabanshı" (Arabic)
Саба:Саба NP-TOP ; ! ""
Сабатаев:Сабатаев NP-COG-OB ; ! ""
Сабатай:Сабатай NP-ANT-M ; ! "Sabatay" (Kazakh)
Сабато:Сабато NP-ANT-M ; !"Use/MT"
Сабдалин:Сабдалин NP-COG-IN ; ! ""
Сабиғат:Сабиғат NP-ANT-M ; ! "Sabıyğat" (Arabic)
Сабильянов:Сабильянов NP-COG-OB ; ! ""
Сабина:Сабина NP-ANT-F ; !"Use/MT"
Сабиров:Сабиров NP-COG-MF ; ! ""
Сабит:Сабит NP-ANT-M ; ! ""
Сабрина:Сабрина NP-ANT-F ; !"Use/MT"
Саб:Саб NP-ANT-M ; !"Use/MT"
Сабыржан:Сабыржан NP-ANT-M ; ! "Sabırjan" (Arabic)
Сабыр:Сабыр NP-ANT-M ; ! "Sabır" (Arabic)
Саванков:Саванков NP-COG-OB ; ! "" ! Use/MT
Саванна:Саванна NP-ANT-F ; !"Use/MT"
Савельев:Савельев NP-COG-OB ; ! ""
Савёлов:Савёлов NP-COG-OB ; ! ""
Савимби:Савимби NP-COG-M ; ! ""
Савин:Савин NP-COG-IN ; ! ""
Савуков:Савуков NP-COG-OB ; ! ""
Савченко:Савченко NP-COG-MF ; ! ""
Сағадат:Сағадат NP-ANT-M ; ! "Sağadat" (Arabic)
Сағадиев:Сағадиев NP-COG-OB ; ! "" ! Use/MT
Сағат:Сағат NP-ANT-M ; ! "Sağat" (Arabic)
Сағдиев:Сағдиев NP-COG-OB ; ! "" ! Use/MT
Сағди:Сағди NP-COG-MF ; ! ""
Сағида:Сағида NP-ANT-F ; ! "Sağıyda" (Arabic)
Сағира:Сағира NP-ANT-F ; ! "Sağıyra" (Arabic)
Сағи:Сағи NP-ANT-M ; ! "Sağıy" (Arabic)
Сағит:Сағит NP-ANT-M ; ! "Sağıyt" (Arabic)
Сағия:Сағия NP-ANT-F ; ! "Sağıya" (Arabic)
Сағымбай:Сағымбай NP-ANT-M ; ! "Sağımbay" (Old Turkic)
Сағымбек:Сағымбек NP-ANT-M ; ! "Sağımbek" (Old Turkic)
Сағым:Сағым NP-ANT-M ; ! "Sağım" (Old Turkic)
Сағынай:Сағынай NP-ANT-F ; ! ""
Сағынбай:Сағынбай NP-ANT-M ; ! "Sağınbay" (Kazakh)
Сағынғали:Сағынғали NP-ANT-M ; ! "Sağınğalıy" (Kazakh)
Сағындықов:Сағындықов NP-COG-OB ; ! ""
Сағындық:Сағындық NP-ANT-M ; ! "Sağındıq" (Kazakh)
Сағын:Сағын NP-ANT-M ; ! "Sağın" (Kazakh)
Сағынтаев:Сағынтаев NP-COG-OB ; ! ""
Сағынтай:Сағынтай NP-ANT-M ; ! "Sağıntay" (Kazakh)
Сағырбаев:Сағырбаев NP-COG-OB ; ! ""
Сағыр:Сағыр NP-ANT-M ; ! "Sağır" (Arabic)
Саданов:Саданов NP-COG-OB ; ! ""
Садан:Садан NP-ANT-M ; ! "Sadan" 
Саддам:Саддам NP-ANT-M ; ! ""
Садида:Садида NP-ANT-F ; ! "Sadıyda" (Arabic)
Садовников:Садовников NP-COG-OB ; ! ""
Садықов:Садықов NP-COG-OB ; ! ""
Садық:Садық NP-ANT-M ; ! "Sadıq" (Arabic)
Садықұлов:Садықұлов NP-COG-OB ; ! ""
Садыралы:Садыралы NP-ANT-M ; ! "Sadıralı" (Arabic)
Садырбаев:Садырбаев NP-COG-OB ; ! ""
Садырбай:Садырбай NP-ANT-M ; ! "Sadırbay"
Садырбай:Садырбай NP-ANT-M ; ! "Sadırbay" (Arabic)
Садырбек:Садырбек NP-ANT-M ; ! "Sadırbek" (Arabic)
Садыркан:Садыркан NP-ANT-M ; ! "Sadırkan" (Arabic)
Садыр:Садыр NP-ANT-M ; ! "Sadır" (Arabic)
Садюков:Садюков NP-COG-OB ; ! ""
Саеров:Саеров NP-COG-OB ; ! ""
Сажида:Сажида NP-ANT-F ; ! "Sajıyda" (Arabic)
Сазерленд:Сазерленд NP-COG-MF ; !"Use/MT"
Сазонов:Сазонов NP-COG-OB ; ! ""
Саида:Саида NP-ANT-F ; ! "Saida" (Arabic)
Сайбақ:Сайбақ NP-ANT-M ; ! "Saybaq" (Arabic)
Сайгон:Сайгон NP-TOP ; ! "Saigon"
Сайдар:Сайдар NP-ANT-M ; ! "Saydar" (Arabic)
Сайдашев:Сайдашев NP-COG-OB ; ! ""
Сайденов:Сайденов NP-COG-OB ; ! ""
Сайлаугүл:Сайлаугүл NP-ANT-F ; ! "Saylawgül" (Kazakh)
Сайлау:Сайлау NP-ANT-M ; ! "Saylaw" (Arabic)
Саймасай:Саймасай NP-ANT-M ; ! "Saymasay" (Kazakh)
Саймон:Саймон NP-ANT-M ; !"Use/MT"
Сайрамбай:Сайрамбай NP-ANT-M ; ! "Sayrambay" 
Сайран:Сайран NP-ANT-M ; ! "Sayran" (Kazakh)
Сайра:Сайра NP-ANT-F ; ! "Sayra" (Arabic)
Сай:Сай NP-ANT-M ; !"Use/MT"
Сайтиев:Сайтиев NP-COG-OB ; ! ""
Сайын:Сайын NP-ANT-M ; ! "Sayın" (Old Turkic)
Сакмаров:Сакмаров NP-COG-OB ; ! ""
Сак:Сак NP-ANT-M ; !"Use/MT"
Саксония:Саксония NP-TOP ; !"Use/MT"
Сакс:Сакс NP-COG-MF ; !"Use/MT"
Сақабеков:Сақабеков NP-COG-OB ; ! ""
Сақбалдыұлы:Сақбалдыұлы NP-COG-M ; ! ""
Сақтар:Сақтар NP-ANT-M ; ! "Saqtar" (Kazakh)
Сақыпжамал:Сақыпжамал NP-ANT-F ; ! "Saqıpjamal" (Arabic)
Сақып:Сақып NP-ANT-F ; ! "Saqıp" (Arabic)
Сақып:Сақып NP-ANT-M ; ! "Saqıp" (Arabic)
салалық:салалық A4 ; !"Use/MT"
Саламанка:Саламанка NP-TOP ; !"Use/MT"
Саламат:Саламат NP-ANT-M ; ! "Salamat" (Arabic)
Салауат:Салауат NP-ANT-M ; ! "Salawat" (Arabic)
Салидат:Салидат NP-ANT-M ; ! ""
Салимов:Салимов NP-COG-OB ; ! ""
Салихов:Салихов NP-COG-OB ; ! ""
Салих:Салих NP-ANT-M ; ! ""
Салливан:Салливан NP-ANT-M ; !"Use/MT"
Салоники:Салоники NP-TOP ; !"Use/MT"
Салтанат:Салтанат NP-ANT-F ; ! "Saltanat" (Arabic)
Салтыков:Салтыков NP-COG-OB ; ! ""
Салық:Салық NP-ANT-M ; ! "Salıq" (Arabic)
Сальвадор:Сальвадор NP-ANT-M ; !"Use/MT"
Сальвадор:Сальвадор NP-TOP ; ! ""
Сальваторе:Сальваторе NP-ANT-M ; !"Use/MT"
Сальгадо:Сальгадо NP-COG-MF ; !"Use/MT"
Сальников:Сальников NP-COG-OB ; ! ""
Самара:Самара NP-TOP ; ! ""
Самарқан:Самарқан NP-TOP ; ! ""
Самар:Самар NP-TOP ; ! "" 
Саматов:Саматов NP-COG-OB ; ! ""
Самат:Самат NP-ANT-M ; ! "Samat" (Arabic)
Самашев:Самашев NP-COG-OB ; ! ""
Самға:Самға NP-ANT-M ; ! "Samğa" (Kazakh)
Самир:Самир NP-ANT-M ; !"Use/MT"
Сами:Сами NP-ANT-M ; !"Use/MT"
Самоа:Самоа NP-TOP ; ! ""
Самодуров:Самодуров NP-COG-OB ; ! ""
Самойлов:Самойлов NP-COG-OB ; ! ""
Само:Само NP-TOP ; ! "" ! Use/MT
Самсонов:Самсонов NP-COG-OB ; ! ""
Самуил:Самуил NP-ANT-M ; ! "Samuel"
Самұрат:Самұрат NP-ANT-M ; ! "Samurat" (Arabic)
Самұрық%-қазына:Самұрық%-қазына NP-ORG ; ! "USE/MT"
Самұрық:Самұрық NP-AL ; ! ""
Сана:Сана NP-ANT-M ; ! "Sana" (Kazakh)
Сана:Сана NP-TOP ; ! ""
Санат:Санат NP-ANT-M ; ! "Sanat" (Arabic)
Санбаев:Санбаев NP-COG-OB ; ! ""
Сандимиров:Сандимиров NP-COG-OB ; ! ""
Сандра:Сандра NP-ANT-F ; !"Use/MT"
Сандуғаш:Сандуғаш NP-ANT-F ; ! "Sandıwğash" (Old Turkic)
Сандыбаев:Сандыбаев NP-COG-OB ; ! ""
Сандыбай:Сандыбай NP-ANT-M ; ! "Sandıbay" (Kazakh)
Санжар:Санжар NP-ANT-M ; ! "Sanjar" (Arabic)
Санко:Санко NP-COG-MF ; ! ""
Санкт%-Петербург:Санкт%-Петербург NP-TOP ; ! ""
Сан%-Паулу:Сан%-Паулу NP-TOP ; ! ""
Сан:Сан NP-ANT-M ; !"Use/MT"
Сансызбай:Сансызбай NP-ANT-M ; ! "Sansızbay" (Arabic)
Сантарен:Сантарен NP-TOP ; !"Use/MT"
Санта:Санта NP-ANT-F ; !"Use/MT"
Сантино:Сантино NP-ANT-M ; !"Use/MT"
Сан%-Томе% және% Принсипи:Сан%-Томе% және% Принсипи NP-TOP ; ! ""
Сан%-Томе:Сан%-Томе NP-TOP ; ! ""
Сантос:Сантос NP-ANT-M ; !"Use/MT"
Сантьяго:Сантьяго NP-ANT-M ; !"Use/MT"
Сантьяго:Сантьяго NP-TOP ; ! "Santiago"
Сан%-Франциско:Сан%-Франциско NP-TOP ; ! ""
Санчес:Санчес NP-COG-MF ; !"Use/MT"
Сапабек:Сапабек NP-ANT-M ; ! "Sapabek" (Arabic)
Сапарбаев:Сапарбаев NP-COG-OB ; ! "" ! Use/MT
Сапарбай:Сапарбай NP-ANT-M ; ! "Saparbay" (Arabic)
Сапарбек:Сапарбек NP-ANT-M ; ! "Saparbek" (Arabic)
Сапарбекұлы:Сапарбекұлы NP-COG-M ; ! ""
Сапарғалиев:Сапарғалиев NP-COG-OB ; ! "" ! Use/MT
Сапарғали:Сапарғали NP-ANT-M ; ! ""
Сапарғали:Сапарғали NP-ANT-M ; ! "Saparğalıy" (Arabic)
Сапармұрат:Сапармұрат NP-ANT-M ; ! ""
Сапар:Сапар NP-ANT-M ; ! "Sapar" (Arabic)
Сапа:Сапа NP-ANT-M ; ! "Sapa" (Arabic)
Сапатеро:Сапатеро NP-COG-MF ; !"Use/MT"
Сапи:Сапи NP-ANT-M ; ! "Sapıy" (Arabic)
Сапожников:Сапожников NP-COG-OB ; ! ""
Сапуұлы:Сапуұлы NP-COG-M ; ! "USE/MT"
Сарагоса:Сарагоса NP-TOP ; !"Use/MT"
Сараево:Сараево NP-TOP ; ! "Sarajevo"
Сараев:Сараев NP-COG-OB ; ! ""
Сарайшық:Сарайшык NP-TOP ; ! ""
Саран:Саран NP-TOP ; ! ""
Сара:Сара NP-ANT-F ; ! ""
Сара:Сара NP-ANT-F ; ! "Sara" (Arabic)
Сарасенбаев:Сарасенбаев NP-COG-OB ; ! ""
Саратов:Саратов NP-COG-OB ; ! ""
Саратов:Саратов NP-TOP ; ! ""
Сарбалаев:Сарбалаев NP-COG-OB ; ! "" ! Use/MT
Сарбасов:Сарбасов NP-COG-OB ; ! ""
Сардар:Сардар NP-ANT-M ; ! "Sardar" (Arabic)
Сардиния:Сардиния NP-TOP ; !"Use/MT"
Сарина:Сарина NP-ANT-F ; !"Use/MT"
Сари:Сари NP-ANT-M ; ! "Sarıy" (Arabic)
Сарита:Сарита NP-ANT-F ; !"Use/MT"
Сария:Сария NP-ANT-F ; ! "Sarıya" (Arabic)
Саркизов:Саркизов NP-COG-OB ; ! ""
Саркози:Саркози NP-COG-MF ; ! ""
Сарқұлов:Сарқұлов NP-COG-OB ; ! ""
Сарманов:Сарманов NP-COG-OB ; ! ""
Сарман:Сарман NP-ANT-M ; ! "Sarman" (Arabic)
Сарман:Сарман NP-TOP ; ! ""
Сар:Сар NP-ANT-M ; !"Use/MT"
Сарсенбай:Сарсенбай NP-ANT-M ; ! USE/MT
Сарханов:Сарханов NP-COG-OB ; ! ""
Сарыағаш:Сарыағаш NP-TOP ; ! ""
Сарыарқа:Сарыарқа NP-TOP ; ! ""
Сарыбаев:Сарыбаев NP-COG-OB ; ! ""
Сарыбай:Сарыбай NP-ANT-M ; ! "Sarıbay" (Kazakh)
Сарыбас:Сарыбас NP-ANT-M ; ! "Sarıbas" (Kazakh)
Сарын:Сарын NP-ANT-M ; ! "Sarın" (Kazakh)
Сары:Сары NP-ANT-M ; ! "Sarı" (Kazakh)
Сарысу:Сарысу NP-TOP ; ! "" 
Сарышығанақ:Сарышығанақ NP-TOP ; ! ""
Сарьян:Сарьян NP-ANT-M ; ! ""
Са:Са NP-ANT-M ; !"Use/MT"
Сасани:Сасани NP-TOP ; !""
Сасан:Сасан NP-ANT-M ; ! "Sasan" (Persian)
Сасықов:Сасықов NP-COG-OB ; ! ""
Сатаев:Сатаев NP-COG-OB ; ! ""
Сатай:Сатай NP-ANT-M ; ! "Satay" 
Сатпаев:Сатпаев NP-COG-OB ; ! ""
Саттаров:Саттаров NP-COG-OB ; ! ""
Саттар:Саттар NP-ANT-M ; ! "Sattar" (Arabic)
Саттарханов:Саттарханов NP-COG-OB ; ! "" ! Use/MT
Сатурн:Сатурн NP-AL ; ! ""
Сатурн:Сатурн NP-TOP ; !"Use/MT"
Сатыбалдиев:Сатыбалдиев NP-COG-OB ; ! ""
Сатыбалдинов:Сатыбалдинов NP-COG-OB ; ! ""
Сатыбалды:Сатыбалды NP-COG-MF ; ! ""
Сатыбалдыұлы:Сатыбалдыұлы NP-COG-M ; ! ""
Сауат:Сауат NP-ANT-M ; ! ""
Саудабаев:Саудабаев NP-COG-OB ; ! ""
Сауд% Арабия:Сауд% Арабия N-COMPOUND-PX ; ! ""
Сауд% Арабиясы:Сауд% Арабиясы NP-TOP ; ! ""
Сауле:Сауле NP-ANT-F ; !"Use/MT"
Саул:Саул NP-ANT-M ; ! "Saul"
Сауранбаев:Сауранбаев NP-COG-OB ; ! ""
Сауранбай:Сауранбай NP-ANT-M ; ! "Sawranbay" (Kazakh)
Сауранбек:Сауранбек NP-ANT-M ; ! "Sawranbek" (Kazakh)
Сауран:Сауран NP-ANT-M ;
Сауран:Сауран NP-ANT-M ; ! "Sawran" (Kazakh)
Сауран:Сауран NP-TOP ; ! "" 
Саурық:Саурық NP-ANT-M ; ! "Sawrıq" (Kazakh)
Саутуорк:Саутуорк NP-ANT-M ; ! ""
Сауық:Сауық NP-ANT-M ; ! "Sawıq" (Arabic)
Сауытбай:Сауытбай NP-ANT-M ; ! "Sawıtbay" (Kazakh)
Сауыт:Сауыт NP-ANT-M ; ! "Sawıt" (Kazakh)
Сафаргалиев:Сафаргалиев NP-COG-OB ; ! ""
Сафа:Сафа NP-ANT-M ; ! "Safa" (Arabic)
Сафиев:Сафиев NP-COG-OB ; ! ""
Сафин:Сафин NP-COG-IN ; ! ""
Сафи:Сафи NP-ANT-M ; ! "Safi" (Arabic)
Сафонов:Сафонов NP-COG-OB ; ! ""
Сафронов:Сафронов NP-COG-OB ; ! ""
Сафуан:Сафуан NP-ANT-M ; ! "Safuan" (Arabic)
Сафура:Сафура NP-ANT-F ; ! "Safura" (Arabic)
Сахалин:Сахалин NP-TOP ; !""
Сахара:Сахара NP-TOP ; ! ""
Сахаров:Сахаров NP-COG-OB ; ! ""
Сахи:Сахи NP-ANT-M ; ! "Saxıy" (Arabic)
Саша:Саша NP-ANT-F ; ! ""
Саша:Саша NP-ANT-M ; ! ""
Саюдис:Саюдис NP-TOP ; ! ""
Саяно:Саяно NP-TOP ; ! ""
Саян:Саян NP-ANT-M ; ! "Sayan" (Kazakh)
Сая:Сая NP-ANT-M ; ! "Saya" (Persian)
Саятқан:Саятқан NP-ANT-M ; ! "Sayatqan" (Arabic)
Саят%-Нова:Саят%-Нова NP-ANT-M ; ! ""
Саят:Саят NP-ANT-M ; ! "" !"Use/MT"
Саят:Саят NP-TOP ; ! ""
Сәбиға:Сәбиға NP-ANT-F ; ! "Säbiyğa" (Arabic)
Сәбиғат:Сәбиғат NP-ANT-M ; ! "Säbiyğat" (Arabic)
Сәбила:Сәбила NP-ANT-F ; ! "Säbiyla" (Arabic)
Сәбира:Сәбира NP-ANT-F ; ! "Säbiyra" (Arabic)
Сәбит:Сәбит NP-ANT-M ; ! "Säbiyt" (Arabic)
Сәғди:Сәғди NP-COG-MF ; ! ""
Сәденұлы:Сәденұлы NP-COG-M ; ! ""
Сәдуақасов:Сәдуақасов NP-COG-OB ; ! ""
Сәдуақас:Сәдуақас NP-ANT-M ; ! "Sädiwaqas" (Arabic)
Сәдуақасұлы:Сәдуақасұлы NP-COG-M ; ! ""
Сәйденов:Сәйденов NP-COG-OB ; ! ""
Сәйденов:Сәйденов NP-COG-OB ; ! "" ! Use/MT
Сәкен:Сәкен NP-ANT-M ; ! "" ()
Сәкиев:Сәкиев NP-COG-OB ; ! ""
Сәлима:Сәлима NP-ANT-F ; ! "Säliyma" (Arabic)
Сәлмен:Сәлмен NP-ANT-M ; ! "Sälmen" (Persian)
Сәлім:Сәлім NP-ANT-M ; ! "Sälim" (Arabic)
Сәлімұлы:Сәлімұлы NP-ANT-M ; !
Сәмединов:Сәмединов NP-COG-OB ; ! ""
Сәмен:Сәмен NP-ANT-M ; ! "Sämen" (Persian)
Сәмет:Сәмет NP-ANT-M ; ! "Sämet" (Arabic)
Сәми:Сәми NP-ANT-M ; ! "Sämiy" (Arabic)
Сәмүд:Сәмүд NP-TOP ; ! ""
Сәндігүл:Сәндігүл NP-ANT-F ; ! "Sändigül" (Kazakh)
Сәния:Сәния NP-ANT-F ; ! "Sänıya" (Arabic)
Сәнімай:Сәнімай NP-ANT-F ; ! "Sänimay" (Kazakh)
Сәпиев:Сәпиев NP-COG-OB ; ! ""
Сәпина:Сәпина NP-ANT-F ; ! "Säpiyna" (Arabic)
Сәрсекбаев:Сәрсекбаев NP-COG-OB ; ! "" ! Use/MT
Сәрсеков:Сәрсеков NP-COG-OB ; ! ""
Сәрсеков:Сәрсеков NP-COG-OB ; ! "" ! Use/MT
Сәрсембаев:Сәрсембаев NP-COG-OB ; ! ""
Сәрсенәлі:Сәрсенәлі NP-ANT-M ; ! "Särsenäli" (Arabic)
Сәрсенбаев:Сәрсенбаев NP-COG-OB ; ! ""
Сәрсенбаев:Сәрсенбаев NP-COG-OB ; ! ""
Сәрсенбай:Сәрсенбай NP-ANT-M ; ! "Särsenbay" (Arabic)
Сәрсенбайұлы:Сәрсенбайұлы NP-COG-M ; ! ""
Сәрсенбек:Сәрсенбек NP-ANT-M ; ! "Särsenbek" (Arabic)
Сәрсенбі:Сәрсенбі NP-ANT-M ; ! "Särsenbi" (Arabic)
Сәрсенқан:Сәрсенқан NP-ANT-M ; ! "Särsenqan" (Arabic)
Сәрсенов:Сәрсенов NP-COG-OB ; ! ""
Сәрсен:Сәрсен NP-ANT-M ; ! ""
Сәруар:Сәруар NP-ANT-F ; ! "Säriwar" (Persian)
Сәрінжіпов:Сәрінжіпов NP-COG-OB ; ! "" ! Use/MT
Сәтбаев:Сәтбаев NP-COG-OB ; ! ""
Сәтбаев:Сәтбаев NP-COG-OB ; ! ""
Сәтбай:Сәтбай NP-ANT-M ; ! "Sätbay" (Kazakh)
Сәтжан:Сәтжан NP-ANT-M ; ! "Sätjan" (Arabic)
Сәтқалиев:Сәтқалиев NP-COG-OB ; ! "" ! Use/MT
Сәтпаев:Сәтпаев NP-COG-OB ; ! ""
Сәт:Сәт NP-ANT-M ; ! "Sät" (Arabic)
Сәтім:Сәтім NP-ANT-M ; ! "Sätim" (Kazakh)
Сәуле:Сәуле NP-ANT-F ; ! "Säwle" (Kazakh)
Сәулет:Сәулет NP-ANT-F ; ! "Säwlet" (Arabic)
Сәуірбек:Сәуірбек NP-ANT-M ; ! "Säwirbek" (Arabic)
Сәуірқұл:Сәуірқұл NP-ANT-M ; ! "Säwirqul" (Arabic)
Сәуір:Сәуір NP-ANT-M ; ! "Säwir" (Arabic)
Сваев:Сваев NP-COG-OB ; ! ""
Свазиленд:Свазиленд NP-TOP ; !"Use/MT"
Свен:Свен NP-ANT-M ; !"Use/MT"
Свердлов:Свердлов NP-COG-OB ; ! ""
Свердловск:Свердловск NP-TOP ; ! ""
Сверчков:Сверчков NP-COG-OB ; ! ""
Светлана:Светлана NP-ANT-F ; ! "" 
Свиридов:Свиридов NP-COG-OB ; ! ""
Свислочь:Свислочь NP-TOP ; !""
Свистков:Свистков NP-COG-OB ; ! ""
Свобода:Свобода NP-AL ; ! ""
Свобода:Свобода NP-COG-MF ; !"Use/MT"
Святенко:Святенко NP-COG-MF ; ! ""
Святославич:Святославич NP-COG-M ; ! ""
Святослав:Святослав NP-ANT-M ; ! ""
Сеара:Сеара NP-TOP ; !"Use/MT"
Себастьян:Себастьян NP-ANT-M ; ! ""
Севериано:Севериано NP-ANT-M ; !"Use/MT"
Северина:Северина NP-ANT-F ; !"Use/MT"
Северцов:Северцов NP-COG-OB ; ! ""
Севилья:Севилья NP-TOP ; !"Use/MT"
Сегізбаев:Сегізбаев NP-COG-OB ; ! ""
Седов:Седов NP-COG-OB ; ! "" ! Use/MT
Сеитов:Сеитов NP-COG-OB ; ! ""
Сейдағалиев:Сейдағалиев NP-COG-OB ; ! "" ! Use/MT
Сейдахмет:Сейдахмет NP-ANT-M ; ! "Seydaxmet" (Arabic)
Сейдәлі:Сейдәлі NP-ANT-M ; ! "Seydäli" (Arabic)
Сейдолла:Сейдолла NP-ANT-M ; ! "Seydolla" (Arabic)
Сейд:Сейд NP-ANT-M ; !"Use/MT"
Сейдуәли:Сейдуәли NP-ANT-M ; ! "Seydiwäliy" (Arabic)
Сейділдә:Сейділдә NP-ANT-M ; ! "Seydildä" (Arabic)
Сейед:Сейед NP-ANT-M ; ! ""
Сейнт% Питер% Порт:Сейнт% Питер% Порт NP-TOP ; ! "" 
Сейсенбай:Сейсенбай NP-ANT-M ; ! "Seysenbay" (Arabic)
Сейсенбек:Сейсенбек NP-ANT-M ; ! "Seysenbek" (Arabic)
Сейсенбі:Сейсенбі NP-ANT-M ; ! "Seysenbi" (Arabic)
Сейсенғали:Сейсенғали NP-ANT-M ; ! "Seysenğalıy" (Arabic)
Сейсенқан:Сейсенқан NP-ANT-M ; ! "Seysenqan" (Arabic)
Сейфолла:Сейфолла NP-ANT-M ; ! "Seyfolla" (Arabic)
Сейшель% аралдары:Сейшель% аралдары NP-TOP ; !"Use/MT"
Сейіл:Сейіл NP-ANT-M ; ! "Seyil" (Arabic)
Сейітбаттал:Сейітбаттал NP-ANT-M ; ! "Seyitbattal" (Arabic)
Сейітбек:Сейітбек NP-ANT-M ; ! "Seyitbek" (Arabic)
Сейітжан:Сейітжан NP-ANT-M ; ! "Seyitjan" (Arabic)
Сейіткерей:Сейіткерей NP-ANT-M ; ! "Seyitkerey" (Arabic)
Сейіткожа:Сейіткожа NP-ANT-M ; ! "Seyitkoja" (Arabic)
Сейітқали:Сейітқали NP-ANT-M ; ! "Seyitqalıy" (Arabic)
Сейітмәмет:Сейітмәмет NP-ANT-M ; ! "Seyitmämet" (Arabic)
Сейітмет:Сейітмет NP-ANT-M ; ! "Seyitmet" (Arabic)
Сейітмұрат:Сейітмұрат NP-ANT-M ; ! "Seyitmurat" (Arabic)
Сейітмұхамбет:Сейітмұхамбет NP-ANT-M ; ! "Seyitmuxambet" (Arabic)
Сейітов:Сейітов NP-COG-OB ; ! ""
Сейіт:Сейіт NP-ANT-M ; ! "Seyit" (Arabic)
Сейітхан:Сейітхан NP-ANT-M ; ! "" ! Use/MT
Секерхан:Секерхан NP-ANT-F ; ! "Sekerxan" (Arabic)
Сексенбай:Сексенбай NP-ANT-M ; ! "Seksenbay" (Kazakh)
Селезнев:Селезнев NP-COG-OB ; ! ""
Селезнёв:Селезнёв NP-COG-OB ; ! ""
Селенга:Селенга NP-TOP ; ! ""
Селиванов:Селиванов NP-COG-OB ; ! ""
Селиверстов:Селиверстов NP-COG-OB ; ! ""
Селивестров:Селивестров NP-COG-OB ; ! ""
Селим:Селим NP-ANT-M ; !"Use/MT"
Селия:Селия NP-ANT-F ; !"Use/MT"
Сельма:Сельма NP-ANT-F ; !"Use/MT"
Сембаев:Сембаев NP-COG-OB ; ! ""
Сембай:Сембай NP-ANT-M ; ! "Sembay" (Persian)
Семби:Семби NP-ANT-M ; ! "Sembiy" (Persian)
Семей:Семей NP-TOP ; ! "Semipalatinsk"
Семей:Семипалатинск NP-TOP ; ! "Semipalatinsk"  ! Dir/LR
Семененко:Семененко NP-COG-MF ; ! ""
Семенов:Семенов NP-COG-OB ; ! ""
Семенов:Семенов NP-COG-OB ; ! ""
Семёнов:Семёнов NP-COG-OB ; ! ""
Сена:Сена NP-TOP ; ! ""
Сенегал:Сенегал NP-TOP ; ! ""
Сенкаку:Сенкаку NP-TOP ; ! ""
Сент%-Винсент% пен% Гренадиндер:Сент%-Винсент% пен% Гренадиндер	NP-TOP ; !""
Сент%-Люсия:Сент%-Люсия NP-TOP ; ! ""
Сенім:Сенім NP-ANT-M ; ! "Senim" (Kazakh)
Сеңгірбаев:Сеңгірбаев NP-COG-OB ; ! ""
Сеңгірбай:Сеңгірбай NP-ANT-M ; ! "Seŋgirbay" (Old Turkic)
Серафимович:Серафимович NP-COG-M ; ! ""
Серафимов:Серафимов NP-COG-OB ; ! ""
Серәлі:Серәлі NP-ANT-M ; ! "Seräli" (Persian)
Сербия:Сербия NP-TOP ; ! ""
Сервантес:Сервантес NP-COG-MF ; !"Use/MT"
Сергачев:Сергачев NP-COG-OB ; ! ""
Сергеевич:Сергее NP-PAT-VICH ; ! ""
Сергеев:Сергеев NP-COG-OB ; ! ""
Сергей:Сергей NP-ANT-M ; ! "Sergey"
Сергиев:Сергиев NP-COG-OB ; ! ""
Серги:Серги NP-ANT-M ; !"Use/MT"
Серғазиев:Серғазиев NP-COG-OB ; ! ""
Серғазы:Серғазы NP-ANT-M ; ! "Serğazı" (Arabic)
Серғали:Серғали NP-ANT-M ; ! "Serğalıy" (Arabic)
Серғалиұлы:Серғалиұлы NP-COG-M ; ! "USE/MT"
Сердюков:Сердюков NP-COG-OB ; ! "" ! Use/MT
Серебренников:Серебренников NP-COG-OB ; ! ""
Серебряков:Серебряков NP-COG-OB ; ! ""
Серена:Серена NP-ANT-F ; !"Use/MT"
Серж:Серж NP-ANT-M ; !"Use/MT"
Серкебаев:Серкебаев NP-COG-OB ; ! ""
Серке:Серке NP-ANT-M ; ! "Serke" (Kazakh)
Серов:Серов NP-COG-OB ; ! ""
Серпухов:Серпухов NP-COG-OB ; ! ""
Сер:Сер NP-ANT-M ; !"Use/MT"
Серхед:Серхед NP-TOP-RUS ; ! ""
Серхетяка:Серхетяка NP-ORG ; ! ""
Серхио:Серхио NP-ANT-M ; !"Use/MT"
Серікбай:Серікбай NP-ANT-M ; ! "Serikbay"
Серікбай:Серікбай NP-ANT-M ; ! "Serikbay" (Arabic)
Серікбек:Серікбек NP-ANT-M ; ! "Serikbek" (Arabic)
Серікбол:Серікбол NP-ANT-M ; ! "Serikbol" (Kazakh)
Серікболсын:Серікболсын NP-ANT-M ; ! ""
Серікқалиев:Серікқалиев NP-COG-OB ; ! ""
Серікқали:Серікқали NP-ANT-M ; ! "Serikqalıy" (Arabic)
Серік:Серік NP-ANT-M ; ! ""
Серік:Серік NP-ANT-M ; ! "Serik" (Arabic)
Серіктұр:Серіктұр NP-ANT-M ; ! "Seriktur" (Arabic)
Се:Се NP-ANT-M ; !"Use/MT"
Се:Се NP-ANT-M ; !"Use/MT"
Сесилия:Сесилия NP-ANT-F ; !"Use/MT"
Сестрорецк:Сестрорецк NP-TOP ; !""
Сетнеров:Сетнеров NP-COG-OB ; ! ""
Сетов:Сетов NP-COG-OB ; ! ""
Сет:Сет NP-ANT-M ; !"Use/MT"
Сеул:Сеул NP-TOP ; ! ""
Сеченев:Сеченев NP-COG-OB ; ! ""
Сеченов:Сеченов NP-COG-OB ; ! ""
Сиб:Сиб NP-ANT-M ; !"Use/MT"
Сиваш:Сиваш NP-TOP ; !""
Сигизмунд:Сигизмунд NP-ANT-M ; !"Use/MT"
Сигнал% Идуна% Парк:Сигнал% Идуна% Парк NP-TOP ; ! "Signal Iduna Park football stadium"
Сигнал% Идуна% Парк:Сигнал% Идуна% Парк%{ъ%} NP-TOP ; ! "Signal Iduna Park football stadium" ! Dir/LR
Сигтуна:Сигтуна NP-TOP ; !""
Сиданко:Сиданко NP-COG-MF ; ! ""
Сидней:Сидней NP-ANT-F ; !"Use/MT"
Сидней:Сидней NP-TOP ; ! ""
Сидоренко:Сидоренко NP-COG-MF ; ! ""
Сидоров:Сидоров NP-COG-OB ; ! ""
Сидорочев:Сидорочев NP-COG-OB ; ! ""
Сид:Сид NP-ANT-M ; !"Use/MT"
Сидяков:Сидяков NP-COG-OB ; ! ""
Силезия:Силезия NP-TOP ; !""
Силенко:Силенко NP-COG-MF ; ! ""
Силин:Силин NP-COG-IN ; ! ""
Силистра:Силистра NP-TOP ; !"Use/MT"
Сил:Сил NP-ANT-M ; !"Use/MT"
Сильвана:Сильвана NP-ANT-F ; !"Use/MT"
Сильвано:Сильвано NP-ANT-M ; !"Use/MT"
Сильва:Сильва NP-COG-MF ; !"Use/MT"
Сильвестр:Сильвестр NP-ANT-M ; !"Use/MT"
Сильвио:Сильвио NP-ANT-M ; !"Use/MT"
Сильви:Сильви NP-ANT-F ; !"Use/MT"
Сильвия:Сильвия NP-ANT-F ; ! ""
Сильвэйн:Сильвэйн NP-ANT-M ; !"Use/MT"
Сильнов:Сильнов NP-COG-OB ; ! ""
Сименс:Сименс NP-AL ; ! ""
Сименс:Сименс NP-ORG ; ! "USE/MT"
Симона:Симона NP-ANT-F ; !"Use/MT"
Симонетта:Симонетта NP-ANT-F ; !"Use/MT"
Симонов:Симонов NP-COG-OB ; ! ""
Симон:Симон NP-ANT-M ; ! ""
Сим:Сим NP-ANT-M ; !"Use/MT"
Сим:Сим NP-ANT-M ; !"Use/MT"
Симус:Симус NP-ANT-M ; !"Use/MT"
Симьянов:Симьянов NP-COG-OB ; ! ""
Сингапур:Сингапур NP-TOP ; ! ""
Синельников:Синельников NP-COG-OB ; ! ""
Синтия:Синтия NP-ANT-F ; !"Use/MT"
Синьхуа:Синьхуа NP-TOP ; ! ""
Сион:Сион NP-ANT-M ; !"Use/MT"
Сион:Сион NP-TOP ; ! ""
Сиприен:Сиприен NP-ANT-M ; !"Use/MT"
Сираж:Сираж NP-ANT-M ; ! "Sıyraj" (Arabic)
Сирин:Сирин NP-COG-MF ; !"Use/MT"
Сирия:Сирия NP-TOP ; ! ""
Сиро:Сиро NP-ANT-M ; !"Use/MT"
Си:Си NP-ANT-M ; !"Use/MT"
Сис:Сис NP-ANT-M ; !"Use/MT"
Систан:Систан NP-TOP ; ! ""
Ситников:Ситников NP-COG-OB ; ! ""
Ситх:Ситх NP-ANT-M ; !"Use/MT"
Сихотэ-Алинь:Сихотэ-Алинь NP-TOP ; ! ""
Сицилия:Сицилия NP-TOP ; !"Use/MT"
сиыршы:сиыршы N1 ; !"Use/MT"
Сиэтл:Сиэтл NP-TOP ; !"Use/MT"
Сиязбек:Сиязбек NP-ANT-M ; ! "" ! Use/MT
Сиякух:Сиякух NP-TOP ; ! ""
Скагеррак:Скагеррак NP-TOP ; !""
Скай:Скай NP-ANT-F ; !"Use/MT"
Скала:Скала NP-COG-MF ; !"Use/MT"
Скандинавия:Скандинавия NP-TOP ; ! ""
Скатов:Скатов NP-COG-OB ; ! ""
Скворцов:Скворцов NP-COG-OB ; ! ""
Скиф:Скиф NP-ANT-M ; !"Use/MT"
Скотт:Скотт NP-ANT-M ; !"Use/MT"
Скоша:Скоша NP-TOP ; !""
Славен:Славен NP-ANT-M ; !"Use/MT"
Славо%-Сербия:Славо%-Сербия NP-TOP ; !""
Славян:Славян NP-TOP ; ! "Slav"
Сладков:Сладков NP-COG-OB ; ! ""
Сланов:Сланов NP-COG-OB ; ! ""
Слатнов:Слатнов NP-COG-OB ; ! ""
Сләмқұлов:Сләмқұлов NP-COG-OB ; ! ""
Слободан:Слободан NP-ANT-M ; !"Use/MT"
Слободан:Слободан NP-COG-IN ; !"Use/MT"
Словакия:Словакия NP-TOP ; ! ""
Словения:Словения NP-TOP ; ! ""
Слонов:Слонов NP-COG-OB ; ! ""
Смағұлов:Смағұлов NP-COG-OB ; ! "" ! Use/MT
Смағұл:Смағұл NP-ANT-M ; ! "'Ismanqul'"
Смағұлұлы:Смағұлұлы NP-COG-M ; ! "'Ismanqul'"
Смаилов:Смаилов NP-COG-OB ; ! "" ! Use/MT
Смайылов:Смайылов NP-COG-OB ; ! "" ! Use/MT
Смақов:Смақов NP-COG-OB ; ! ""
Смирнов:Смирнов NP-COG-OB ; ! ""
Смит:Смит NP-COG-MF ; !"Use/MT"
Смоленск:Смоленск NP-TOP ; ! ""
Смолов:Смолов NP-COG-OB ; ! ""
Смолян:Смолян NP-TOP ; !"Use/MT"
Смородинов:Смородинов NP-COG-OB ; ! ""
Смыр:Смыр NP-ANT-M ; !"Use/MT"
Смятон:Смятон NP-COG-IN ; ! ""
Сни:Сни NP-ANT-M ; !"Use/MT"
Снонетта:Снонетта NP-COG-MF ; ! ""
Соболев:Соболев NP-COG-OB ; ! ""
Собчак:Собчак NP-COG-OB ; ! ""
Советбек:Советбек NP-ANT-M ; ! "Sovetbek" (New word)
Советжан:Советжан NP-ANT-M ; ! "Sovetjan" (New word)
Советқан:Советқан NP-ANT-M ; ! "Sovetqan" (New word)
Совет:Совет NP-ANT-M ; ! "Sovet" (New word)
Сод:Сод NP-ANT-M ; !"Use/MT"
Созақбаев:Созақбаев NP-COG-OB ; ! ""
Соза:Соза NP-COG-MF ; ! ""
Соколов:Соколов NP-COG-OB ; ! ""
Сократ:Сократ NP-ANT-M ; ! "Sokrat" (Arabic)
Соқпақбаев:Соқпақбаев NP-COG-OB ; ! ""
Солана:Солана NP-ANT-F ; !"Use/MT"
Солдатбек:Солдатбек NP-ANT-M ; ! "Soldatbek" (Kazakh)
Солженицын:Солженицын NP-COG-M ; !"Use/MT"
Солис:Солис NP-ORG ; !"Use/MT"
Соловьёв:Соловьёв NP-COG-OB ; ! ""
Соловьёв:Соловьёв NP-COG-OB ; ! ""
Соломон:Соломон NP-ANT-M ; !"Use/MT"
Солтүстік% және% Оңтүстік% Америка:Солтүстік% және% Оңтүстік% Америка NP-TOP ; !"Use/MT"
Солтүстік% Корея:Солтүстік% Корея NP-TOP ; !"Use/MT"
Солтүстік:Солтүстік NP-TOP ; ! "North"
Солтыбай:Солтыбай NP-ANT-M ; ! "Soltıbay" (Arabic)
Соль:Соль NP-ANT-F ; !"Use/MT"
Сомали:Сомали NP-TOP ; ! ""
Сомбел:Сомбел NP-ANT-M ; ! "Sombel" (Kazakh)
Сомма:Сомма NP-TOP ; !""
Сонко:Сонко NP-COG-MF ; ! ""
Соня:Соня NP-ANT-F ; !"Use/MT"
Соп:Соп NP-ANT-M ; !"Use/MT"
Сория:Сория NP-TOP ; !"Use/MT"
Сорокин:Сорокин NP-COG-IN ; ! ""
Сос:Сос NP-ANT-M ; !"Use/MT"
Софи:Софи NP-ANT-F ; !"Use/MT"
София:София NP-TOP ; ! ""
София:София NP-TOP ; ! "" 
Софокл:Софокл NP-ANT-M ; !"Use/MT"
Софронов:Софронов NP-COG-OB ; ! ""
Соф:Соф NP-ANT-M ; !"Use/MT"
Софья:Софья NP-ANT-F ; ! "Sofia" (Greek)
Социал:Социал NP-ANT-M ; ! "Socıyal" (New word)
Сөмединов:Сөмединов NP-COG-OB ; ! ""
Спамбетов:Спамбетов NP-COG-OB ; ! ""
Спандияр:Спандияр NP-ANT-M ; ! "Spandıyar" (Persian)
Спартак:Спартак NP-AL ; ! ""
Спартак:Спартак NP-ANT-M ; ! ""
Спас:Спас NP-TOP ; ! ""
Спенсер:Спенсер NP-ANT-M ; !"Use/MT"
Спиваков:Спиваков NP-COG-OB ; ! "" ! Use/MT
Спиноза:Спиноза NP-COG-MF ; ! ""
Спиноза:Спиноза NP-COG-MF ; !"Use/MT"
Спиридович:Спиридович NP-COG-M ; ! ""
Спиридонов:Спиридонов NP-COG-OB ; ! ""
Спиридон:Спиридон NP-ANT-M ; ! ""
ССР:ССР%{э%}%{й%} NP-TOP ; ! ""
Ставрополь:Ставрополь NP-TOP ; ! ""
Ставрос:Ставрос NP-ANT-M ; !"Use/MT"
Сталбек:Сталбек NP-ANT-M ; ! ""
Сталинград:Сталинград NP-TOP ; ! ""
Сталин:Сталин NP-COG-IN ; ! ""
Сталлоне:Сталлоне NP-COG-MF ; ! ""
Стамбул:Стамбул NP-TOP ; ! ""
Стамбұл:Стамбұл NP-TOP ; !"Use/MT"
Станимир:Станимир NP-ANT-M ; ! "" ! Use/MT
Станиславский:Станиславский NP-COG-M ; ! ""
Станислав:Станислав NP-ANT-M ; ! ""
Станислас:Станислас NP-ANT-M ; !"Use/MT"
Стародубцев:Стародубцев NP-COG-OB ; ! ""
Старченко:Старченко NP-COG-MF ; ! ""
Старшов:Старшов NP-COG-OB ; ! ""
Стасов:Стасов NP-COG-OB ; ! ""
Стас:Стас NP-ANT-M ; ! ""
Ста:Ста NP-ANT-M ; !"Use/MT"
Стаханов:Стаханов NP-COG-OB ; ! ""
Стеблов:Стеблов NP-COG-OB ; ! ""
Стелла:Стелла NP-ANT-F ; !"Use/MT"
Степанович:Степано NP-PAT-VICH ; ! "" ! Use/MT 
Степанов:Степанов NP-COG-OB ; ! ""
Степан:Степан NP-ANT-M ; ! ""
Степняк:Степняк NP-TOP ; ! ""
Стеттиниус:Стеттиниус NP-ANT-M ; !"Use/MT"
Стефани:Стефани NP-ANT-F ; !"Use/MT"
Стефания:Стефания NP-ANT-F ; !"Use/MT"
Стефано:Стефано NP-ANT-M ; !"Use/MT"
Стефан:Стефан NP-ANT-M ; !"Use/MT"
Стеф:Стеф NP-ANT-M ; !"Use/MT"
Стивен:Стивен NP-ANT-M ; !"Use/MT"
Стив:Стив NP-ANT-M ; !"Use/MT"
Стирия:Стирия NP-TOP ; ! ""
Стифонов:Стифонов NP-COG-OB ; ! ""
Стокгольм:Стокгольм NP-TOP ; ! ""
Стокгольм:Стокгольм NP-TOP ; ! ""
Столыпин:Столыпин NP-TOP ; !""
Столяров:Столяров NP-COG-OB ; ! ""
Сто:Сто NP-ANT-M ; !"Use/MT"
Страсбург:Страсбург NP-TOP ; !"Use/MT" 
Стратонов:Стратонов NP-COG-OB ; ! ""
Страхов:Страхов NP-COG-OB ; ! ""
Стрекозов:Стрекозов NP-COG-OB ; ! "" ! Use/MT
Стрелков:Стрелков NP-COG-OB ; ! ""
Стрельников:Стрельников NP-COG-OB ; ! ""
Стрельцов:Стрельцов NP-COG-OB ; ! ""
Стрип:Стрип NP-COG-MF ; !"Use/MT"
Строганов:Строганов NP-COG-OB ; ! ""
Стросс:Стросс NP-COG-MF ; !"Use/MT"
Струнников:Струнников NP-COG-OB ; ! ""
Стручков:Стручков NP-COG-OB ; ! ""
Студеница:Студеница NP-TOP ; !""
Студенцов:Студенцов NP-COG-OB ; ! ""
Стэнли:Стэнли NP-ANT-M ; !"Use/MT"
Стэнли:Стэнли NP-COG-MF ; ! ""
Стэн:Стэн NP-ANT-M ; !"Use/MT"
Стюарт:Стюарт NP-ANT-M ; !"Use/MT"
Стюарт:Стюарт NP-COG-MF ; !"Use/MT"
Суазиланд:Суазиланд NP-TOP ; ! ""
Субанов:Субанов NP-COG-OB ; ! ""
Субботин:Субботин NP-COG-IN ; ! ""
субрегион:субрегион N1 ; !"Use/MT"
Суворов:Суворов NP-COG-OB ; ! ""
Судаков:Судаков NP-COG-OB ; ! ""
Судан:Судан NP-TOP ; !"Use/MT"
Суйін:Суйін NP-ANT-M ; ! "Sıwyin" (Kazakh)
Сукре:Сукре NP-TOP ; ! "" 
Сук:Сук NP-ANT-M ; !"Use/MT"
Сук:Сук NP-ANT-M ; !"Use/MT"
Сулеймен:Сулеймен NP-ANT-M ; ! "Sıwleymen" (Ancient Hebrew)
Сулиманов:Сулиманов NP-COG-OB ; ! ""
Сумитомо:Сумитомо NP-TOP ; !"Use/MT"
Сундарбан:Сундарбан NP-TOP ; ! "" 
Сундетов:Сундетов NP-COG-OB ; ! "" ! Use/MT
Суриков:Суриков NP-COG-OB ; ! ""
Суринам:Суринам NP-TOP ; !"Use/MT"
Сусанино:Сусанино NP-TOP ; !""
Суслов:Суслов NP-COG-OB ; ! ""
Су:Су NP-ANT-M ; !"Use/MT"
Суханов:Суханов NP-COG-OB ; ! ""
Суховерхов:Суховерхов NP-COG-OB ; ! ""
Сухомлинов:Сухомлинов NP-COG-OB ; ! ""
Сухум:Сухум NP-TOP ; ! "" 
Сушков:Сушков NP-COG-OB ; ! "" ! Use/MT
Сұдан:Сұдан NP-TOP ; ! ""
Сұзы:Сұзы NP-TOP ; !"" !Use/MT
Сұлтанбаев:Сұлтанбаев NP-COG-OB ; ! ""
Сұлтанбаев:Сұлтанбаев NP-COG-OB ; ! ""
Сұлтанбай:Сұлтанбай NP-ANT-M ; ! "Sultanbay" (Arabic)
Сұлтанбек:Сұлтанбек NP-ANT-M ; ! "Sultanbek" (Arabic)
Сұлтангелді:Сұлтангелді NP-ANT-M ; ! "Sultangeldi" (Kazakh)
Сұлтанғали:Сұлтанғали NP-ANT-M ; ! "Sultanğalıy" (Arabic)
Сұлтанов:Сұлтанов NP-COG-OB ; ! ""
Сұлтан:Сұлтан NP-ANT-M ; ! "Sultan" (Arabic)
Сұлуке:Сұлуке NP-ANT-F ; ! "Sulıwke" (Kazakh)
Сұлу:Сұлу NP-ANT-F ; ! "Sulıw" (Kazakh)
Сұлухан:Сұлухан NP-ANT-F ; ! "Sulıwxan" (Kazakh)
Сұлушаш:Сұлушаш NP-ANT-F ; ! "Sulıwshash" (Kazakh)
Сұңқар:Сұңқар NP-ANT-M ; ! "Suŋqar" (Kazakh)
Сүгірбаев:Сүгірбаев NP-COG-OB ; ! ""
Сүгірбай:Сүгірбай NP-ANT-M ; ! "Sügirbay" (Arabic)
Сүгір:Сүгір NP-ANT-M ; ! "Sügir" (Old Turkic)
Сүйеу:Сүйеу NP-ANT-M ; ! "Süyew" (Kazakh)
Сүйменбаев:Сүйменбаев NP-COG-OB ; ! ""
Сүйімхан:Сүйімхан NP-ANT-F ; ! "Süyimxan" (Kazakh)
Сүйінбай:Сүйінбай NP-ANT-M ; ! "Süyinbay" (Kazakh)
Сүйінбала:Сүйінбала NP-ANT-F ; ! "Süyinbala" (Kazakh)
Сүйіндік:Сүйіндік NP-ANT-M ; ! "Süyindik"
Сүлеев:Сүлеев NP-COG-OB ; ! ""
Сүлейменов:Сүлейменов NP-COG-OB ; ! ""
Сүлейменов:Сүлейменов NP-COG-OB ; ! "" ! Use/MT
Сүлеймен:Сүлеймен NP-ANT-M ; ! "Solomon"
Сүмбебай:Сүмбебай NP-ANT-M ; ! "Sümbebay" (Persian)
Сүндетов:Сүндетов NP-COG-OB ; ! "" ! Use/MT
Сүндет:Сүндет NP-ANT-M ; ! "Sündet" (Arabic)
Сүттібай:Сүттібай NP-ANT-M ; ! "Süttibay" (Kazakh)
Сыбанқұл:Сыбанқұл NP-ANT-M ; ! "Sıbanqul" (Arabic)
Сыдықов:Сыдықов NP-COG-OB ; ! "" ! Use/MT
Сыдық:Сыдық NP-ANT-M ; ! "Sıdıq" (Arabic)
Сыздықов:Сыздықов NP-COG-OB ; ! ""
Сызрань:Сызрань NP-TOP ; ! ""
Сылан:Сылан NP-ANT-M ; ! "Sılan" (Old Turkic)
Сымақ:Сымақ NP-ANT-M ; ! "Sımaq" (Persian)
Сымбат:Сымбат NP-ANT-F ; ! "Sımbat" (Kazakh)
Сындыбай:Сындыбай NP-ANT-M ; ! "Sındıbay" (Kazakh)
Сындыбала:Сындыбала NP-ANT-F ; ! "Sındıbala" (Kazakh)
Сын:Сын NP-ANT-M ; !"Use/MT"
Сыпатаев:Сыпатаев NP-COG-OB ; ! ""
Сырбай:Сырбай NP-ANT-M ; ! "Sırbay" (Kazakh)
Сырбар:Сырбар NP-ORG ; ! ""
Сырғабай:Сырғабай NP-ANT-M ; ! "Sırğabay" (Kazakh)
Сырғабек:Сырғабек NP-ANT-M ; ! "Sırğabek" (Kazakh)
Сырдария:Сырдария NP-TOP ; ! ""
Сырмұхамбет:Сырмұхамбет NP-ANT-M ; ! "Sırmuxambet" (Arabic)
Сыромятников:Сыромятников NP-COG-OB ; ! ""
Сыр:Сыр NP-ANT-M ; !"Use/MT"
Сырттанов:Сырттанов NP-COG-OB ; ! ""
Сырымбетов:Сырымбетов NP-COG-OB ; ! ""
Сырымбет:Сырымбет NP-ANT-M ; ! "Sirimbet"
Сырым:Сырым NP-ANT-M ; ! "Sırım" (Old Turkic)
Сысоев:Сысоев NP-COG-OB ; ! ""
Сычев:Сычев NP-COG-OB ; ! ""
Сычуань:Сычуань NP-TOP ; ! ""
Сібір:Сібір NP-TOP ; ! "Siberia"
Сьван:Сьван NP-TOP ; ! "Gypsy"   ! FIXME: check
Сьерра% Леоне:Сьерра% Леоне NP-TOP ; !"Use/MT"
Сьерра:Сьерра NP-ANT-F ; !"Use/MT"
Сьюдад%-пердида:Сьюдад%-пердида NP-TOP ; ! ""
Сьюдад%-Хуарес:Сьюдад%-Хуарес NP-TOP ; !"Use/MT"
Сьюзен:Сьюзен NP-ANT-F ; !"Use/MT"
Сэлинджер:Сэлинджер NP-COG-M ; ! ""
Сэл:Сэл NP-ANT-M ; !"Use/MT"
Сэм:Сэм NP-ANT-M ; !"Use/MT"
Сэмюэл:Сэмюэл NP-ANT-M ; !"Use/MT"
Сэсто:Сэсто NP-ANT-M ; !"Use/MT"
Сюзанна:Сюзанна NP-ANT-F ; !"Use/MT"
Сюй:Сюй NP-COG-MF ; ! ""
Сю:Сю NP-ANT-M ; !"Use/MT"
Табар:Табар NP-ANT-M ; ! "Tabar" (Kazakh)
Табылған:Табылған NP-ANT-M ; ! "Tabılğan" (Kazakh)
Табылдиев:Табылдиев NP-COG-OB ; ! ""
Табыс:Табыс NP-ANT-M ; ! "Tabıs" (Kazakh)
Тагильцев:Тагильцев NP-COG-OB ; ! ""
Таған:Таған NP-ANT-M ; ! "Tağan" (Arabic)
Тадеуш:Тадеуш NP-ANT-M ; !"Use/MT"
Тажик% ССР:Тажик% ССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Тазабеков:Тазабеков NP-COG-OB ; ! ""
Тазабек:Тазабек NP-ANT-M ; ! "Tazabek" (Kazakh)
Таиланд:Таиланд NP-TOP ; ! ""
Тайбек:Тайбек NP-ANT-M ; ! "Taybek" (Kazakh)
Тайвань:Тайвань NP-TOP ; ! "Taiwan"
Тайлақ:Тайлақ NP-ANT-M ; ! "Taylaq" (Kazakh)
Тайланд:Тайланд NP-TOP ; ! ""
Таймағамбетов:Таймағамбетов NP-COG-OB ; ! ""
Тайманов:Тайманов NP-COG-OB ; ! ""
Таймасов:Таймасов NP-COG-OB ; ! ""
таймс:таймс NP-AL ; ! ""
Таймс:Таймс NP-AL ; ! ""
Таймыр:Таймыр NP-TOP ; ! ""
Таймыр:Таймыр NP-TOP ; ! ""
Тайуан:Тайуан NP-TOP ; ! ""
Тайшапов:Тайшапов NP-COG-OB ; ! ""
Тайшық:Тайшық NP-ANT-M ; ! "Tayshıq" (Arabic)
Тайыр:Тайыр NP-ANT-M ; ! "Tayır" (Arabic)
Тақыр:Тақыр NP-ANT-M ; ! "Taqır" (Arabic)
Таланцев:Таланцев NP-COG-OB ; ! ""
Талап:Талап NP-ANT-M ; ! "Talap" (Arabic)
Талас:Талас NP-TOP ; ! "Talas river" 
Талғат:Талғат NP-ANT-M ; ! "Talğat" (Arabic)
Талдықорған:Талдықорған NP-TOP ; ! ""
Талжанов:Талжанов NP-COG-OB ; ! ""
Талибан:Талибан NP-AL ; ! ""
Талиб:Талиб NP-ANT-M ; ! ""
Талиев:Талиев NP-COG-OB ; ! ""
Тали:Тали NP-ANT-F ; !"Use/MT"
Талқа:Талқа NP-ANT-M ; ! "Talqa" (Arabic)
Таллин:Таллин NP-TOP ; ! ""
Таллин:Таллин NP-TOP ; ! "" 
Талмас:Талмас NP-ANT-M ; ! "Talmas" (Kazakh)
Талов:Талов NP-COG-OB ; ! ""
Талшыбық:Талшыбық NP-ANT-F ; ! "Talshıbıq" (Kazakh)
Таль:Таль NP-ANT-M ; !"Use/MT"
Тамабаев:Тамабаев NP-COG-OB ; ! ""
Тамара:Тамара NP-ANT-F ; ! "Tamara" (Arabic)
Тамар:Тамар NP-ANT-M ; !"Use/MT"
Тамбов:Тамбов NP-TOP ; ! ""
Тамбукан:Тамбукан NP-TOP ; !""
Тамм:Тамм NP-ANT-M ; !"Use/MT"
Там:Там NP-ANT-M ; !"Use/MT"
Танабай:Танабай NP-ANT-M ; ! "Tanabay" (Kazakh)
Тана:Тана NP-ANT-F ; ! "Tana" (Kazakh)
Танзания:Танзания NP-TOP ; ! ""
Таныкөк:Таныкөк NP-ANT-M ; ! "Tonykok"
Таня:Таня NP-ANT-F ; !"Use/MT"
Таңатаров:Таңатаров NP-COG-OB ; ! ""
Таңат:Таңат NP-ANT-M ; ! "Taŋat" (Kazakh)
Таңбол:Таңбол NP-ANT-M ; ! "Taŋbol" (Kazakh)
Таңсұлу:Таңсұлу NP-ANT-F ; ! "Taŋsulıw" (Kazakh)
Таңсык:Таңсык NP-ANT-F ; ! "Taŋsık" (Kazakh)
Таңсықбай:Таңсықбай NP-ANT-M ; ! "Taŋsıqbay" (Kazakh)
Таңшолпан:Таңшолпан NP-ANT-F ; ! "Taŋsholpan" (Kazakh)
Тапцырев:Тапцырев NP-COG-OB ; ! ""
Тараз:Тараз NP-TOP ; ! ""
Таранов:Таранов NP-COG-OB ; ! ""
Тарантино:Тарантино NP-COG-MF ; !"Use/MT"
Тарасов:Тарасов NP-COG-OB ; ! ""
Тарас:Тарас NP-ANT-M ; ! "Taras" (Greek)
Тара:Тара NP-ANT-F ; !"Use/MT"
Тарбағатай:Тарбағатай NP-TOP ; ! "mountain"
Тарғын:Тарғын NP-ANT-M ; ! "Tarğın" (Arabic)
Тариба:Тариба NP-TOP ; ! ""
Тарик:Тарик NP-ANT-M ; !"Use/MT"
Тарковский:Тарковский NP-COG-M ; !"Use/MT"
Тарпищев:Тарпищев NP-COG-OB ; ! "" ! Use/MT
Таррагона:Таррагона NP-TOP ; !"Use/MT"
Тар:Тар NP-ANT-M ; !"Use/MT"
Тар:Тар NP-ANT-M ; !"Use/MT"
Тархан:Тархан NP-ANT-M ; ! ""
Тарчизио:Тарчизио NP-ANT-M ; !"Use/MT"
Тасболатов:Тасболатов NP-COG-OB ; ! ""
Тасболат:Тасболат NP-ANT-M ; ! "Tasbolat" (Latin)
Тасемен:Тасемен NP-ANT-M ; ! "Tasemen" (Kazakh)
Тасқайрат:Тасқайрат NP-ANT-M ; ! "Tasqayrat" (Kazakh)
Тасқын:Тасқын NP-ANT-M ; ! "Tasqın" (Kazakh)
Тасмағамбетов:Тасмағамбетов NP-COG-OB ; ! ""
Тасмағамбетов:Тасмағамбетов NP-COG-OB ; ! "" ! Use/MT
Тасмания:Тасмания NP-TOP ; !"Use/MT"
Тасман:Тасман NP-TOP ; ! ""
Тастайбеков:Тастайбеков NP-COG-OB ; ! ""
Тастайбек:Тастайбек NP-ANT-M ; ! "Tastaybek" (Kazakh)
Тастанбеков:Тастанбеков NP-COG-OB ; ! ""
Тастемір:Тастемір NP-ANT-M ; ! "Tastemir" (Kazakh)
Тастүлек:Тастүлек NP-ANT-M ; ! "Tastülek" 
Та:Та NP-ANT-M ; !"Use/MT"
Татанов:Татанов NP-COG-OB ; ! ""
Татар% АССР:Татар% АССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Татар%-информ:Татар%-информ NP-ORG ; ! ""
Татарстан:Татарстан NP-TOP ; ! "Tatarstan" 
Татарханов:Татарханов NP-COG-OB ; ! ""
Тат:Тат NP-ANT-M ; !"Use/MT"
Татубай:Татубай NP-ANT-M ; ! "Tatıwbay" (Kazakh)
Татьяна:Татьяна NP-ANT-F ; ! "" 
Тауасат:Тауасат NP-ANT-M ; ! "Tawasat" (Kazakh)
Таубай:Таубай NP-ANT-M ; ! "Tawbay" (Kazakh)
Таужан:Таужан NP-ANT-M ; ! "Tawjan" (Kazakh)
Таукин:Таукин NP-COG-IN ; ! ""
Тауман:Тауман NP-ANT-M ; ! "Tawman" (Kazakh)
Тау:Тау NP-ANT-M ; ! "Taw" (Kazakh)
Тауфих:Тауфих NP-ANT-M ; ! "Taufix" (Arabic)
Тауэр:Тауэр NP-TOP ; !""
Тафаев:Тафаев NP-COG-OB ; ! ""
Тахауи:Тахауи NP-ANT-M ; ! "Taxawıy" (Arabic)
Тахиржан:Тахиржан NP-ANT-M ; ! ""
Тахир:Тахир NP-ANT-M ; ! ""
Тах:Тах NP-ANT-M ; !"Use/MT"
Ташкент:Ташкент NP-TOP ; ! "Tashkent"
Ташмұхамедов:Ташмұхамедов NP-COG-OB ; ! ""
Таяу% Шығыс:Таяу% Шығыс NP-TOP ; ! ""
Тәбиба:Тәбиба NP-ANT-F ; ! "Täbiyba" (Arabic)
Тәбила:Тәбила NP-ANT-F ; ! "Täbiyla" (Arabic)
Тәбия:Тәбия NP-ANT-F ; ! "Täbıya" (Arabic)
Тәжигүл:Тәжигүл NP-ANT-F ; ! "Täjiygül" (Persian)
Тәжиев:Тәжиев NP-COG-OB ; ! "" ! Use/MT
Тәжин:Тәжин NP-COG-IN ; ! ""
Тәжібаев:Тәжібаев NP-COG-OB ; ! ""
Тәжібай:Тәжібай NP-ANT-M ; ! "Täjibay" (Persian)
Тәжікстан:Тәжікстан NP-TOP ; ! "Tajikistan" 
Тәжімұрат:Тәжімұрат NP-ANT-M ; ! "Täjimurat" (Persian)
Тәжін:Тәжін NP-COG-IN ; ! ""
Тәжі:Тәжі NP-ANT-M ; ! "Täji" (Persian)
Тәкежан:Тәкежан NP-ANT-M ; ! ""
Тәкібаев:Тәкібаев NP-COG-OB ; ! ""
Тәкім:Тәкім NP-ANT-M ; ! "Täkim" (Arabic)
Тәлипа:Тәлипа NP-ANT-F ; ! "Täliypa" (Arabic)
Тәліп:Тәліп NP-ANT-M ; ! "Tälip" (Arabic)
Тәмила:Тәмила NP-ANT-F ; ! "Tämiyla" (Arabic)
Тәнзила:Тәнзила NP-ANT-F ; ! "Tänziyla" (Kazakh)
Тәнзия:Тәнзия NP-ANT-F ; ! "Tänzıya" (Arabic)
Тәңірбергенов:Тәңірбергенов NP-COG-OB ; ! ""
Тәңірберді:Тәңірберді NP-ANT-M ; ! ""
Тәрбие:Тәрбие NP-ANT-F ; ! "Tärbiye" (Kazakh)
Тәтеш:Тәтеш NP-TOP ; ! ""
Тәттібай:Тәттібай NP-ANT-M ; ! "Tättibay" (Kazakh)
Тәттібала:Тәттібала NP-ANT-F ; ! "Tättibala" (Kazakh)
Тәуекел:Тәуекел NP-ANT-M ; ! "Täwekel" (Kazakh)
Тәуекелұлы:Тәуекелұлы NP-COG-M ; ! "USE/MT"
Тәуке:Тәуке NP-ANT-M ; ! ""
Тәурат:Таурат NP-AL ; ! "" Dir/LR
Тәурат:Тәурат NP-AL ; ! ""
Тәуір:Тәуір NP-ANT-M ; ! "Täwir" (Kazakh)
Тәшенев:Тәшенев NP-COG-OB ; ! ""
Тәшиев:Тәшиев NP-COG-OB ; ! "" ! Use/MT
Тверь:Тверь NP-TOP ; !""
Тебриз:Тебриз NP-TOP-RUS ; ! ""
Тевкелев:Тевкелев NP-COG-OB ; ! ""
Тевмесс:Тевмесс NP-TOP ; !""
Тегель:Тегель NP-TOP ; !""
Тегеран:Тегеран NP-TOP ; ! ""
Тегусигальпа:Тегусигальпа NP-TOP ; ! "" 
Тейлор:Тейлор NP-ANT-F ; !"Use/MT"
Тейлор:Тейлор NP-ANT-M ; !"Use/MT"
Тейт:Тейт NP-TOP ; !""
Текеш:Текеш NP-ANT-M ; !"Use/MT"
Тектіғұл:Тектіғұл NP-ANT-M ; ! ""
Теләче:Теләче NP-TOP ; ! ""
Телесинко:Телесинко NP-COG-MF ; ! ""
Телжан:Телжан NP-ANT-M ; ! "Teljan" (Kazakh)
Теличенко:Теличенко NP-COG-MF ; ! ""
Телма:Телма NP-ANT-F ; !"Use/MT"
Темелин:Темелин NP-ORG ; ! ""
Темел:Темел NP-ANT-M ; ! "Temel (Turkish name)"
Теменов:Теменов NP-COG-OB ; ! ""
Темза:Темза NP-TOP ; !""
Темпель:Темпель NP-TOP ; !"Use/MT"
Темірәлі:Темірәлі NP-ANT-M ; ! "Temiräli" (Kazakh)
Темірбаев:Темірбаев NP-COG-OB ; ! "" ! Use/MT
Темірбеков:Темірбеков NP-COG-OB ; ! ""
Темірбек:Темірбек NP-ANT-M ; ! "Temirbek" 
Темірболат:Темірболат NP-ANT-M ; ! "Temirbolat" (Kazakh)
Темірғали:Темірғали NP-ANT-M ; ! "Temirğalıy" (Kazakh)
Теміржанов:Теміржанов NP-COG-OB ; ! ""
Темірқан:Темірқан NP-ANT-M ; ! "Temirqan" (Kazakh)
Темірлан:Темірлан NP-ANT-M ; ! "Temirlan" 
Теміров:Теміров NP-COG-OB ; ! ""
Теміртау:Теміртау NP-TOP ; ! ""
Темір:Темір NP-ANT-M ; ! "Temir" (Kazakh)
Тенгиз:Тенгиз NP-ORG ; ! "Tengiz" !Use/MT
Тенерифе:Тенерифе NP-TOP ; !"Use/MT"
Теннесси:Теннесси NP-TOP ; !"Use/MT"
Теннис:Теннис NP-COG-MF ; !"Use/MT"
Тенюшев:Тенюшев NP-COG-OB ; ! ""
Теңдік:Теңдік NP-ANT-F ; ! "Teŋdik" (Kazakh)
Теодор:Теодор NP-ANT-M ; ! ""
Тео:Тео NP-ANT-M ; !"Use/MT"
Теофилос:Теофилос NP-ANT-M ; !"Use/MT"
Теплов:Теплов NP-COG-OB ; ! ""
Теплоухов:Теплоухов NP-COG-OB ; ! ""
Терасия:Терасия NP-ANT-F ; !"Use/MT"
Тера:Тера NP-ANT-F ; !"Use/MT"
Тереғұлов:Тереғұлов NP-COG-OB ; ! ""
Тереза:Тереза NP-ANT-F ; !"Use/MT"
Терек:Терек NP-TOP ; !""
Терентьев:Терентьев NP-COG-OB ; ! ""
Тереньтев:Тереньтев NP-COG-OB ; ! ""
терминологиялық:терминологиялық A3 ; !"Use/MT"
Тернер:Тернер NP-ANT-M ; !"Use/MT"
Терренс:Терренс NP-ANT-M ; !"Use/MT"
Терри:Терри NP-ANT-M ; !"Use/MT"
Тер:Тер NP-ANT-M ; !"Use/MT"
Теруэль:Теруэль NP-TOP ; !"Use/MT"
Тесса:Тесса NP-ANT-F ; !"Use/MT"
Те:Те NP-ANT-M ; !"Use/MT"
Тетерин:Тетерин NP-COG-IN ; ! ""
Техас:Техас NP-TOP ; ! ""
Теһран:Теһран NP-TOP ; ! ""
Тённер:Тённер NP-ANT-M ; !"Use/MT"
Тиберий:Тиберий NP-TOP ; ! ""
Тибет:Тибет NP-TOP ; ! ""
Тибор:Тибор NP-ANT-M ; !"Use/MT"
Тиб:Тиб NP-ANT-M ; !"Use/MT"
Тигиев:Тигиев NP-COG-OB ; ! "" ! Use/MT
Тигр:Тигр NP-TOP ; ! ""
Тиг:Тиг NP-ANT-M ; !"Use/MT"
Тиесов:Тиесов NP-COG-OB ; ! ""
Тиесов:Тиесов NP-COG-OB ; ! ""
Тик:Тик NP-ANT-M ; !"Use/MT"
Тиль:Тиль NP-ANT-M ; !"Use/MT"
Тимаев:Тимаев NP-COG-OB ; ! ""
Тимирязев:Тимирязев NP-COG-OB ; ! ""
Тимор%-Лесте:Тимор%-Лесте NP-TOP ; ! ""
Тимор:Тимор NP-TOP ; ! ""
Тимофеевич:Тимофее NP-PAT-VICH ; ! ""
Тимофеев:Тимофеев NP-COG-OB ; ! ""
Тимофей:Тимофей NP-ANT-M ; !"Use/MT"
Тимошенко:Тимошенко NP-COG-MF ; ! ""
Тим:Тим NP-ANT-M ; !"Use/MT"
Тимурлыбаев:Тимурлыбаев NP-COG-OB ; ! "" ! Use/MT
Тимур:Тимур NP-ANT-M ; ! ""
Тина:Тина NP-ANT-F ; !"Use/MT"
Тирана:Тирана NP-TOP ; ! ""
Тирана:Тирана NP-TOP ; ! "" 
Тиргартен:Тиргартен NP-TOP ; !""
Тирмизи:Тирмизи NP-COG-MF ; ! ""
Тироль:Тироль NP-TOP ; !"Use/MT"
Тир:Тир NP-ANT-M ; !"Use/MT"
Тир:Тир NP-ANT-M ; !"Use/MT"
Титаренко:Титаренко NP-COG-MF ; ! ""
Ти:Ти NP-ANT-M ; !"Use/MT"
Титов:Титов NP-COG-OB ; ! ""
Тито:Тито NP-COG-MF ; !"Use/MT"
Тит:Тит NP-ANT-M ; !"Use/MT"
Тифлис:Тифлис NP-TOP ; !""
Тиф:Тиф NP-ANT-M ; !"Use/MT"
Тихвин:Тихвин NP-TOP ; !""
Тихонов:Тихонов NP-COG-OB ; ! ""
Тихон:Тихон NP-ANT-M ; !"Use/MT"
Тихорецк:Тихорецк NP-TOP ; !""
Тия:Тия NP-ANT-F ; !"Use/MT"
Ткаченко:Ткаченко NP-COG-MF ; ! ""
Тобиас:Тобиас NP-ANT-M ; !"Use/MT"
Тоби:Тоби NP-ANT-M ; !"Use/MT"
Тобольск:Тобольск NP-TOP ; ! ""
Тобрук:Тобрук NP-TOP ; !""
Тобықты:Тобықты NP-AL ; ! ""
Тобыл:Тобыл NP-TOP ; ! ""
Того:Того NP-TOP ; !"Use/MT"
Тоғжанов:Тоғжанов NP-COG-OB ; ! "" ! Use/MT
Тоғжан:Тоғжан NP-ANT-F ; ! "Toğjan" (Kazakh)
Тоғжан:Тоғжан NP-ANT-M ; ! "Use/MT"
Тоғжан:Тоғжан NP-ANT-M ; !"Use/MT"
Тодд:Тодд NP-ANT-M ; !"Use/MT"
Тойбала:Тойбала NP-ANT-F ; ! "Toybala" (Kazakh)
Тойкен:Тойкен NP-ANT-M ; ! ""
Токантинс:Токантинс NP-TOP ; !"Use/MT"
Токарев:Токарев NP-COG-OB ; ! ""
Токио:Токио NP-TOP ; ! "Tokyo"
Тоқаев:Тоқаев NP-COG-OB ; ! ""
Тоқмағанбетов:Тоқмағанбетов NP-COG-OB ; ! ""
Тоқомбаев:Тоқомбаев NP-COG-OB ; ! ""
Тоқпақбаев:Тоқпақбаев NP-COG-OB ; ! "" ! Use/MT
Тоқпанов:Тоқпанов NP-COG-OB ; ! ""
Тоқсанбаев:Тоқсанбаев NP-COG-OB ; ! ""
Тоқтабаев:Тоқтабаев NP-COG-OB ; ! ""
Тоқтаров:Тоқтаров NP-COG-OB ; ! ""
Толғанай:Толғанай NP-ANT-F ; ! "Tolğanay" (Kazakh)
Толедо:Толедо NP-TOP ; !"Use/MT"
Толқын:Толқын NP-ANT-F ; ! "Tolqın" (Kazakh)
Толмачев:Толмачев NP-COG-OB ; ! ""
Толстов:Толстов NP-COG-OB ; ! ""
Толстой:Толстой NP-COG-M ; ! ""
Томас:Томас NP-ANT-M ; ! ""
Томирис:Томирис NP-ANT-F ; ! "Tomıyrıys" (Arabic)
Томск:Томск NP-TOP ; ! ""
Томсон:Томсон NP-ANT-M ; ! ""
Томсон:Томсон NP-COG-MF ; !"Use/MT"
Том:Том NP-ANT-M ; ! "Tom"
Тонго:Тонго NP-TOP ; !"Use/MT"
Тонио:Тонио NP-ANT-M ; !"Use/MT"
Тони:Тони NP-ANT-F ; !"Use/MT"
Тони:Тони NP-ANT-M ; ! ""
Топай:Топай NP-ANT-M ; ! ""
Торайғыров:Торайғыров NP-COG-OB ; ! ""
Торғын:Торғын NP-ANT-F ; ! "Torğın" (Kazakh)
Торонто:Торонто NP-TOP ; !"Use/MT"
Торопцев:Торопцев NP-COG-OB ; ! ""
Торрес:Торрес NP-COG-MF ; !"Use/MT"
Торымбала:Торымбала NP-ANT-F ; ! "Torımbala" (Old Turkic)
Тоскана:Тоскана NP-TOP ; !"Use/MT"
Тоска:Тоска NP-ANT-F ; !"Use/MT"
Тосов:Тосов NP-COG-OB ; ! ""
Тоты:Тоты NP-ANT-F ; ! "Totı" (Kazakh)
Төлебаев:Төлебаев NP-COG-OB ; ! ""
Төлегенов:Төлегенов NP-COG-OB ; ! ""
Төлегенов:Төлегенов NP-COG-OB ; ! ""
Төлеген:Төлеген NP-ANT-M ; ! ""
Төлепбаев:Төлепбаев NP-COG-OB ; ! ""
Төлепберді:Төлепберді NP-ANT-M ; ! "Tölepberdi"
Төменгі% Новгород:Төменгі% Новгород NP-TOP ; ! "" 
Төмен% Кама:Төмен% Кама NP-TOP ; ! ""
Төребаев:Төребаев NP-COG-OB ; ! ""
Төрежанов:Төрежанов NP-COG-OB ; ! ""
Төржанов:Төржанов NP-COG-OB ; ! ""
Төстік:Төстік NP-ANT-M ; ! ""
Травник:Травник NP-TOP ; !"Use/MT"
Транснистрия:Транснистрия NP-TOP ; !""
Транссиб:Транссиб NP-TOP ; !""
Траст:Траст NP-ORG ; ! ""
Трафальгар:Трафальгар NP-TOP ; !"Use/MT"
Требков:Требков NP-COG-OB ; ! "" ! Use/MT
Трейси:Трейси NP-ANT-F ; !"Use/MT"
Трентино:Трентино NP-TOP ; !"Use/MT"
Трентон:Трентон NP-ANT-M ; !"Use/MT"
Трент:Трент NP-ANT-M ; !"Use/MT"
Третьяков:Третьяков NP-COG-OB ; ! ""
Тринидад% және% Тобаго:Тринидад% және% Тобаго NP-TOP ; ! ""
Тринидад:Тринидад NP-ANT-F ; !"Use/MT"
Тринидад:Тринидад NP-TOP ; ! ""
Тринити:Тринити NP-ANT-F ; !"Use/MT"
Триполи:Триполи NP-TOP ; !"Use/MT"
Трифонов:Трифонов NP-COG-OB ; ! ""
Троица%-Сергиева:Троица%-Сергиева NP-TOP ; ! ""
Троицк:Троицк NP-TOP ; ! ""
Тронов:Тронов NP-COG-OB ; ! ""
Трофимов:Трофимов NP-COG-OB ; ! ""
Троф:Троф NP-ANT-M ; !"Use/MT"
Трумэн:Трумэн NP-COG-MF ; !"Use/MT"
Трэвис:Трэвис NP-ANT-M ; !"Use/MT"
Тубон:Тубон NP-TOP ; !""
Тува:Тува NP-TOP ; ! ""
Тудор:Тудор NP-COG-MF ; !"Use/MT"
Туқай:Туқай NP-TOP ; ! ""
Тула:Тула NP-TOP ; ! ""
Тулон:Тулон NP-TOP ; !""
Тулуза:Тулуза NP-TOP ; !"Use/MT"
Тум:Тум NP-ANT-M ; !"Use/MT"
Тунис:Тунис NP-TOP ; ! ""
Тун:Тун NP-ANT-M ; ! ""
Туполев:Туполев NP-COG-OB ; ! ""
Тургенев:Тургенев NP-COG-OB ; ! ""
Тургенов:Тургенов NP-COG-OB ; ! ""
Турин:Турин NP-TOP ; !""
Туркестан% АССР:Туркестан% АССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Туров:Туров NP-COG-OB ; ! ""
Тур:Тур NP-ORG ; ! ""
Турчинов:Турчинов NP-COG-OB ; ! ""
Тутмос:Тутмос NP-ANT-M ; ! ""
Ту:Ту NP-ANT-M ; !"Use/MT"
Туч:Туч NP-ANT-M ; !"Use/MT"
Тұлымхан:Тұлымхан NP-ANT-M ; ! "Tulımxan" (Kazakh)
Тұнғұшпаев:Тұнғұшпаев NP-COG-OB ; ! ""
Тұңғатар:Тұңғатар NP-ANT-M ; ! ""
Тұрағұлов:Тұрағұлов NP-COG-OB ; ! ""
Тұранәлембанк:Тұранәлембанк NP-TOP ; ! "" ! Use/MT
Тұран:Тұран NP-TOP ; !"Use/MT"
Тұрған:Тұрған NP-ANT-F ; ! "Turğan" (Kazakh)
Тұрдақын:Тұрдақын NP-ANT-M ; ! "Turdaqın" (Persian)
Тұржанов:Тұржанов NP-COG-OB ; ! ""
Тұрлыханов:Тұрлыханов NP-COG-OB ; ! ""
Тұрсынай:Тұрсынай NP-ANT-F ; ! "Tursınay" (Kazakh)
Тұяқбай:Тұяқбай NP-COG-MF ; ! ""
Тұяқов:Тұяқов NP-COG-OB ; ! ""
Түгелбаев:Түгелбаев NP-COG-OB ; ! ""
Түймебаев:Түймебаев NP-COG-OB ; ! "" ! Use/MT
Түйтеұлы:Түйтеұлы NP-COG-M ; ! ""
Түлкібас:Түлкібас NP-TOP ; !"" !Use/MT 
Түнтеков:Түнтеков NP-COG-OB ; ! ""
Түргеш:Түргеш NP-TOP ; ! ""
Түркия:Түркия NP-TOP ; ! "Turkey"
Түрксіб:Түрксіб NP-TOP ; ! ""
Түркістан:Туркiстан NP-TOP ; ! "Turkistan" Dir/LR
Түркістан:Түркістан NP-TOP ; ! "Turkestan" 
Түркістан:Түркістан NP-TOP ; ! "Turkistan"
Түрік% Әуежолдары:Түрік% Әуежолдар NP-ORG-COMPOUND ; ! "Turkish Airways"
Түрікменстан:Түркменстан NP-TOP ; ! "" Dir/LR
Түрікменстан:Түркіменстан NP-TOP ; ! "" Dir/LR
Түрікменстан:Түрікменстан NP-TOP ; ! "Turkmenistan" 
Түсіпбеков:Түсіпбеков NP-COG-OB ; ! "" ! Use/MT
Тхиенг:Тхиенг NP-ANT-M ; !"Use/MT"
Тыва:Тыва NP-TOP ; ! ""
Тынышбаев:Тынышбаев NP-COG-OB ; ! ""
Тынышпаев:Тынышпаев NP-COG-OB ; ! ""
Тырново:Тырново NP-TOP ; !"Use/MT"
Тысыбаев:Тысыбаев NP-COG-OB ; ! ""
Тілендиев:Тілендиев NP-COG-OB ; ! ""
Тіленшиев:Тіленшиев NP-COG-OB ; ! ""
Тілеубаев:Тілеубаев NP-COG-OB ; ! ""
Тілеуханов:Тілеуханов NP-COG-OB ; ! ""
Тілеуханұлы:Тілеуханұлы NP-COG-M ; ! ""
Тілешев:Тілешев NP-COG-OB ; ! "Tilešev"
Тінәлиев:Тінәлиев NP-COG-OB ; ! "" ! Use/MT
Тьерри:Тьерри NP-ANT-M ; !"Use/MT"
Тьшышбаев:Тьшышбаев NP-COG-OB ; ! ""
Тьюринг:Тьюринг NP-COG-MF ; !"Use/MT"
Тэд:Тэд NP-ANT-M ; !"Use/MT"
Тэтчер:Тэтчер NP-COG-MF ; ! ""
Тюдоров:Тюдоров NP-COG-OB ; ! ""
Тюленев:Тюленев NP-COG-OB ; ! ""
Тюрингия:Тюрингия NP-TOP ; !"Use/MT"
Тянь%-Шань:Тянь%-Шань NP-TOP ; ! "mountain"
Тячев:Тячев NP-COG-OB ; ! ""
Уайльд:Уайльд NP-COG-MF ; !"Use/MT"
Уайыс:Уайыс NP-ANT-M ; ! "Wayıs" (Arabic)
Уақа:Уақа NP-ANT-M ; ! "Uaka (Kazakh)"
Уалиев:Уалиев NP-COG-OB ; ! ""
Уалиханов:Уалиханов NP-COG-OB ; ! ""
Уалкер:Уалкер NP-ANT-M ; !"Use/MT"
Уард:Уард NP-COG-MF ; !"Use/MT"
Уаррен:Уаррен NP-ANT-M ; !"Use/MT"
Уахап:Уахап NP-ANT-M ; ! "Waxap" (Arabic)
Уахат:Уахат NP-ANT-M ; ! "Waxat" (Arabic)
Уәзипа:Уәзипа NP-ANT-F ; ! "Wäziypa" (Arabic)
Уәлиахмет:Уәлиахмет NP-ANT-M ; ! "Wäliyaxmet" (Arabic)
Уәлиев:Уәлиев NP-COG-OB ; ! ""
Уәлиев:Уәлиев NP-COG-OB ; ! "" ! Use/MT
Уәлиолла:Уәлиолла NP-ANT-M ; ! "Wäliyolla" (Arabic)
Уәли:Уәли NP-ANT-M ; ! "Wäliy" (Arabic)
Уәлиханов:Уәлиханов NP-COG-OB ; ! ""
Уәлихан:Уәлихан NP-ANT-M ; ! "Wäliyxan" (Arabic)
Уәсила:Уәсила NP-ANT-F ; ! "Wäsiyla" (Arabic)
Уәсима:Уәсима NP-ANT-F ; ! "Wäsiyma" (Arabic)
Уваров:Уваров NP-COG-OB ; ! ""
Уганда:Уганда NP-TOP ; ! ""
Угоренко:Угоренко NP-COG-MF ; ! ""
Удмуртия:Удмуртия NP-TOP ; ! ""
Узелков:Узелков NP-COG-OB ; ! ""
Уикипедия:Уикипедия NP-ORG ; !"Use/MT"
Уикипедия:Уикипедия NP-ORG ; ! "Wikipedia"
уикипедияшы:уикипедияшы N1 ; !"Use/MT"
Уилан:Уилан NP-COG-MF ; !"Use/MT"
Уилсон:Уилсон NP-ANT-M ; !"Use/MT"
Уилсон:Уилсон NP-COG-M ; ! ""
Уильямс:Уильямс NP-ANT-M ; !"Use/MT"
Уильям:Уильям NP-ANT-M ; ! ""
Уимблдон:Уимблдон NP-TOP ; !""
Уинстон:Уинстон NP-ANT-M ; !"Use/MT"
Уитнесс:Уитнесс NP-ANT-M ; ! ""
Уи:Уи NP-ANT-M ; !"Use/MT"
Украина% ССР:Украина% ССР%{э%}%{й%} NP-TOP-ASSR ; ! ""
Украина:Украина NP-TOP ; ! "Ukraine"
Уланов:Уланов NP-COG-OB ; ! ""
Улисс:Улисс NP-ANT-M ; !"Use/MT"
Улла:Улла NP-COG-MF ; !"Use/MT"
Ул:Ул NP-ANT-M ; !"Use/MT"
Ульяновск:Ульяновск NP-TOP ; ! ""
Ульянов:Ульянов NP-COG-OB ; ! ""
Уляшев:Уляшев NP-COG-OB ; ! ""
Умань:Умань NP-TOP ; !""
Умберто:Умберто NP-ANT-M ; !"Use/MT"
Умида:Умида NP-ANT-F ; ! "" 
Умов:Умов NP-COG-OB ; ! ""
Ум:Ум NP-ANT-M ; !"Use/MT"
Уоллес:Уоллес NP-ANT-M ; !"Use/MT"
Уолпол:Уолпол NP-ANT-M ; !"Use/MT"
Уолстонкрафт:Уолстонкрафт NP-COG-MF ; !"Use/MT"
Уолт:Уолт NP-ANT-M ; ! ""
Уолш:Уолш NP-COG-MF ; !"Use/MT"
Уорф:Уорф NP-COG-MF ; !"Use/MT"
Уорхол:Уорхол NP-COG-MF ; !"Use/MT"
Уотсон:Уотсон NP-COG-MF ; !"Use/MT"
Уралеев:Уралеев NP-COG-OB ; ! ""
Уралмаш:Уралмаш NP-TOP ; !""
Урал:Урал NP-TOP ; ! ""
Урбана:Урбана NP-ANT-F ; !"Use/MT"
Уреки:Уреки NP-COG-M ; !"Use/MT"
Урена-Расо:Урена-Расо NP-ANT-M ; !"Use/MT"
Урмаев:Урмаев NP-COG-OB ; ! ""
Урсула:Урсула NP-ANT-F ; !"Use/MT"
Уругвай:Уругвай NP-TOP ; ! ""
Устинов:Устинов NP-COG-OB ; ! ""
Усубакунов:Усубакунов NP-COG-OB ; ! ""
Ута:Ута NP-TOP ; !""
Уфа:Уфа NP-TOP ; ! ""
Ушаков:Ушаков NP-COG-OB ; ! ""
Уэйн:Уэйн NP-ANT-M ; !"Use/MT"
Уэллс:Уэллс NP-COG-MF ; !"Use/MT"
Уэльс:Уэльс NP-TOP ; !"Use/MT"
Уэсли:Уэсли NP-ANT-M ; !"Use/MT"
Уэстон:Уэстон NP-ANT-M ; !"Use/MT"
Ұдмұртстан:Ұдмұртстан NP-TOP ; ! "Udmurtia" 
Ұзақ:Ұзақ NP-ANT-M ; ! "Uzaq" (Kazakh)
Ұйықбаев:Ұйықбаев NP-COG-OB ; ! ""
Ұйықбай:Ұйықбай NP-ANT-M ; ! "Uyıqbay" (Kazakh)
Ұлан%-Батыр:Ұлан%-Батыр NP-TOP ; ! ""
Ұлан:Ұлан NP-ANT-M ; ! "Ulan" (Arabic)
Ұлас:Ұлас NP-ANT-M ; ! "Ulas" (Kazakh)
Ұлбала:Ұлбала NP-ANT-F ; ! "Ulbala" (Kazakh)
Ұлболсын:Ұлболсын NP-ANT-F ; ! "Ulbolsyn" (Kazakh)
Ұлжан:Ұлжан NP-ANT-F ; ! "Uljan" (Persian)
Ұлжан:Ұлжан NP-ANT-F ; ! "Uljan"  (sometimes M?)
Ұлпа:Ұлпа NP-ANT-F ; ! "Ulpa" (Kazakh)
Ұлфат:Ұлфат NP-ANT-F ; ! "Ulfat" (Arabic)
Ұлыбритания:Ұлыбритания NP-TOP ; ! ""
Ұлықбек:Ұлықбек NP-ANT-M ; ! "Ulıqbek" (Kazakh)
Ұлытау:Ұлытау NP-TOP ; ! ""
Ұрқия:Ұрқия NP-ANT-F ; ! "Urqıya" (Arabic)
Ұрқымбаев:Ұрқымбаев NP-COG-OB ; ! ""
Үдербай:Үдербай NP-ANT-M ; ! "Üderbay" (Kazakh)
Үкібаев:Үкібаев NP-COG-OB ; ! ""
Үкілай:Үкілай NP-ANT-F ; ! "" 
Үлгібай:Үлгібай NP-ANT-M ; ! "Ülgibay" (Kazakh)
Үлыбритания:Үлыбритания NP-TOP ; ! "Great Britain"
Үмбеталиев:Үмбеталиев NP-COG-OB ; ! ""
Үмбетәлиев:Үмбетәлиев NP-COG-OB ; ! "" ! Use/MT
Үмбетәлі:Үмбетәлі NP-ANT-M ; ! "Ümbetäli" (Arabic)
Үмбетбаев:Үмбетбаев NP-COG-OB ; ! ""
Үмбетбай:Үмбетбай NP-ANT-M ; ! "Ümbetbay" (Arabic)
Үмбетжан:Үмбетжан NP-ANT-M ; ! "Ümbetjan" (Arabic)
Үмбетов:Үмбетов NP-COG-OB ; ! "" ! Use/MT
Үмбет:Үмбет NP-ANT-M ; ! "Ümbet" (Arabic)
Үмігүлсім:Үмігүлсім NP-ANT-F ; ! "Ümigülsim" (Arabic)
Үміт:Үміт NP-ANT-F ; ! "Ümit" (Persian)
Үміт:Үміт NP-ANT-M ; ! "Ümit" (Persian)
Үндіқытай:Үндіқытай NP-TOP ; ! "Indochina"
Үндістан:Үндістан NP-TOP ; ! "India"
Үрия:Үрия NP-ANT-F ; ! "Ürıya" (Arabic)
Үркер:Үркер NP-ANT-M ; ! ""
Үркімбай:Үркімбай NP-ANT-M ; ! "Ürkimbay"
Үрқыз:Үрқыз NP-ANT-F ; ! "" 
Үрімбай:Үрімбай NP-ANT-M ; ! "Ürimbay" (Persian)
Үрімжан:Үрімжан NP-ANT-M ; ! "Ürimjan" (Persian)
Үрімжі:Үрімжі NP-TOP ; ! "Urumchi"
Үрімхан:Үрімхан NP-ANT-M ; ! "Ürimxan" (Persian)
Үсенов:Үсенов NP-COG-OB ; ! "" ! Use/MT
Үсен:Үсен NP-ANT-M ; ! "Üsen" (Arabic)
Үстем:Үстем NP-ANT-M ; ! "Üstem" (Kazakh)
Үшарал:Үшарал NP-TOP ; ! ""
Фабиано:Фабиано NP-ANT-M ; !"Use/MT"
Фабиан:Фабиан NP-ANT-M ; !"Use/MT"
Фабиола:Фабиола NP-ANT-F ; !"Use/MT"
Фабио:Фабио NP-ANT-M ; !"Use/MT"
Фабия:Фабия NP-ANT-F ; !"Use/MT"
Фабрицио:Фабрицио NP-ANT-M ; !"Use/MT"
Фадеев:Фадеев NP-COG-OB ; ! ""
Фазила:Фазила NP-ANT-F ; ! "Fazila" (Arabic)
Фазылбек:Фазылбек NP-ANT-M ; ! "Fazılbek" (Arabic)
Фазыл:Фазыл NP-ANT-M ; ! "Fazıl" 
Фазылхан:Фазылхан NP-ANT-M ; ! "Fazılxan" (Arabic)
Фаиз:Фаиз NP-ANT-M ; ! "Faiz" (Arabic)
Файзрахман:Файзрахман NP-ANT-M ; ! "Fayzraxman" (Arabic)
Фалес:Фалес NP-ANT-M ; !"Use/MT"
Фандеев:Фандеев NP-COG-OB ; ! ""
Фанни:Фанни NP-ANT-F ; !"Use/MT"
Фан:Фан NP-ANT-M ; !"Use/MT"
Фараби:Фараби NP-ANT-M ; !"Use/MT"
Фараби:Фараби NP-COG-MF ; ! "" ! Use/MT
Фарахани:Фарахани NP-COG-M ; !"Use/MT"
Фарер:Фарер NP-TOP ; ! ""
Фарида:Фарида NP-ANT-F ; ! "Farida" (Arabic)
Фарид:Фарид NP-ANT-M ; !"Use/MT"
Фариза:Фариза NP-ANT-F ; ! "Fariza" (Arabic)
Фаррелл:Фаррелл NP-COG-MF ; !"Use/MT"
Фар:Фар NP-ANT-M ; !"Use/MT"
Фархан:Фархан NP-ANT-M ; !"Use/MT"
Фатима:Фатима NP-ANT-F ; ! "Fatima" (Arabic)
Фатих:Фатих NP-ANT-M ; ! ""
Фаузия:Фаузия NP-ANT-F ; ! "Fauziya" (Arabic)
Фаусто:Фаусто NP-ANT-M ; !"Use/MT"
Фахир:Фахир NP-ANT-M ; ! "Faxir" (Arabic)
Фәрит:Фәрит NP-ANT-M ; ! ""
Федерер:Федерер NP-COG-MF ; !"Use/MT"
Федерика:Федерика NP-ANT-F ; !"Use/MT"
Федерико:Федерико NP-ANT-M ; !"Use/MT"
Федорович:Федоро NP-PAT-VICH ; ! "" ! Use/MT 
Федоров:Федоров NP-COG-OB ; ! ""
Федор:Федор NP-ANT-M ; ! ""
Федосеев:Федосеев NP-COG-OB ; ! ""
Федосеенко:Федосеенко NP-COG-MF ; ! ""
Федотов:Федотов NP-COG-OB ; ! ""
Федра:Федра NP-ANT-F ; !"Use/MT"
Феликс:Феликс NP-ANT-M ; ! ""
Фелимонов:Фелимонов NP-COG-OB ; ! ""
Фелипе:Фелипе NP-ANT-M ; !"Use/MT"
Фелисиано:Фелисиано NP-ANT-M ; !"Use/MT"
Фелисити:Фелисити NP-ANT-F ; !"Use/MT"
Фелпс:Фелпс NP-COG-MF ; !"Use/MT"
Фельтен:Фельтен NP-ANT-M ; !"Use/MT"
Фемистокл:Фемистокл NP-ANT-M ; !"Use/MT"
Фенносарматия:Фенносарматия NP-TOP ; !""
Феодосия:Феодосия NP-TOP ; !""
Ферғана:Ферғана NP-TOP ; ! ""
Фердинандо:Фердинандо NP-ANT-M ; !"Use/MT"
Фердинанд:Фердинанд NP-ANT-M ; !"Use/MT"
Фернандес:Фернандес NP-COG-MF ; !"Use/MT"
Фернандо:Фернандо NP-ANT-M ; !"Use/MT"
Фернан:Фернан NP-ANT-M ; !"Use/MT"
Ферран:Ферран NP-ANT-M ; !"Use/MT"
Ферреро:Ферреро NP-COG-MF ; !"Use/MT"
Ферхойген:Ферхойген NP-COG-MF ; !"Use/MT"
Фёдоров:Фёдоров NP-COG-OB ; ! ""
Фзули:Фзули NP-COG-MF ; ! ""
Фивы:Фивы NP-TOP ; !"Use/MT"
Фидель:Фидель NP-ANT-M ; ! ""
Фиджи:Фиджи NP-TOP ; ! ""
Физули:Физули NP-COG-MF ; ! ""
Физ:Физ NP-ANT-M ; !"Use/MT"
Филадельфия:Филадельфия NP-TOP ; !"Use/MT"
Филатов:Филатов NP-COG-OB ; ! ""
Филиппенко:Филиппенко NP-COG-MF ; ! ""
Филиппина:Филиппина NP-ANT-F ; !"Use/MT"
Филиппиндер:Филиппиндер NP-TOP ; ! ""
Филиппин:Филиппин NP-TOP ; ! ""
Филиппов:Филиппов NP-COG-OB ; ! ""
Филиппо:Филиппо NP-ANT-M ; !"Use/MT"
Филипп:Филипп NP-ANT-M ; !"Use/MT"
Филип:Филип NP-ANT-M ; ! ""
Филчев:Филчев NP-COG-OB ; ! "" ! Use/MT
Фина:Фина NP-ANT-F ; !"Use/MT"
Финикия:Финикия NP-TOP ; ! ""
Финикс:Финикс NP-ANT-M ; !"Use/MT"
Финли:Финли NP-ANT-M ; !"Use/MT"
Финляндия:Финляндия NP-TOP ; ! "Finland"
Фино:Фино NP-ANT-M ; !"Use/MT"
Фин:Фин NP-ANT-M ; !"Use/MT"
Фиорела:Фиорела NP-ANT-F ; !"Use/MT"
Фируза:Фируза NP-ANT-F ; ! "Firuza" (Arabic)
Фита:Фита NP-ANT-M ; !"Use/MT"
ФИФА:ФИФА NP-ORG-ABBR ; ! ""
Фицджеральд:Фицджеральд NP-COG-MF ; !"Use/MT"
Фишер:Фишер NP-COG-MF ; !"Use/MT"
Флавия:Флавия NP-ANT-F ; !"Use/MT"
Фламенко:Фламенко NP-COG-MF ; ! ""
Фленсбург:Фленсбург NP-TOP ; !""
Флетчер:Флетчер NP-COG-MF ; !"Use/MT"
Фле:Фле NP-ANT-M ; !"Use/MT"
Флинн:Флинн NP-COG-MF ; !"Use/MT"
Флора:Флора NP-ANT-F ; ! "Flora" (Latin)
Флорентина:Флорентина NP-ANT-F ; !"Use/MT"
Флорентин:Флорентин NP-ANT-M ; !"Use/MT"
Флориан:Флориан NP-ANT-M ; !"Use/MT"
Флорида:Флорида NP-TOP ; ! ""
Фокида:Фокида NP-TOP ; !""
Фокин:Фокин NP-COG-IN ; ! ""
Фоли:Фоли NP-COG-MF ; !"Use/MT"
Фольксваген:Фольксваген NP-ORG ; ! "USE/MT"
Фома:Фома NP-ANT-M ; ! ""
Фоменко:Фоменко NP-COG-MF ; ! ""
Фомин:Фомин NP-COG-IN ; ! ""
Фомичев:Фомичев NP-COG-OB ; ! "" ! Use/MT
Фомичёв:Фомичёв NP-COG-OB ; ! ""
фон% Урах:фон% Урах NP-ANT-M ; !"Use/MT"
Форбс:Форбс NP-ORG ; ! ""
Форд:Форд NP-COG-MF ; !"Use/MT"
Фортуна:Фортуна NP-ANT-F ; !"Use/MT"
Фотыга:Фотыга NP-COG-MF ; !"Use/MT"
Фрадков:Фрадков NP-COG-OB ; ! "" ! Use/MT
Фракия:Фракия NP-TOP ; !""
Фракия:Фракия NP-TOP ; !""
Франка:Франка NP-ANT-F ; !"Use/MT"
Франклин:Франклин NP-ANT-M ; ! ""
Франко:Франко NP-COG-MF ; ! ""
Франк:Франк NP-ANT-M ; !"Use/MT"
Франкфурт:Франкфурт NP-TOP ; ! ""
Франс%-Иосиф:Франс%-Иосиф NP-TOP ; ! ""
Франсуа:Франсуа NP-ANT-M ; !"Use/MT"
Франс:Франс NP-ANT-M ; !"Use/MT"
Франциско:Франциско NP-ANT-M ; !"Use/MT"
Франция:Франция NP-TOP ; ! "France"
Франц:Франц NP-ANT-M ; !"Use/MT"
Франческа:Франческа NP-ANT-F ; !"Use/MT"
Франческо:Франческо NP-ANT-M ; !"Use/MT"
Фраттини:Фраттини NP-COG-MF ; !"Use/MT"
Фредерикс:Фредерикс NP-ANT-M ; !"Use/MT"
Фредерик:Фредерик NP-ANT-M ; !"Use/MT"
Фред:Фред NP-ANT-M ; !"Use/MT"
Фрейзер:Фрейзер NP-ANT-M ; !"Use/MT"
Фрида:Фрида NP-ANT-F ; !"Use/MT"
Фридман:Фридман NP-COG-MF ; ! ""
Фридом% Хаус:Фридом% Хаус NP-ORG ; ! ""
Фридрихсбург:Фридрихсбург NP-TOP ; !""
Фридрихсвердер:Фридрихсвердер NP-TOP ; !""
Фридрих:Фридрих NP-ANT-M ; ! ""
Фриц:Фриц NP-COG-MF ; !"Use/MT"
Фролов:Фролов NP-COG-OB ; ! ""
Фрыг:Фрыг NP-ANT-M ; !"Use/MT"
Фрэнк:Фрэнк NP-ANT-M ; !"Use/MT"
Фрэнсис:Фрэнсис NP-ANT-M ; ! ""
Фукидид:Фукидид NP-ANT-M ; !"Use/MT"
Фукусима:Фукусима NP-TOP ; !"Use/MT"
Фульхенсио:Фульхенсио NP-ANT-M ; !"Use/MT"
Фунтик:Фунтик NP-AL ; ! ""
Фурманов:Фурманов NP-COG-OB ; ! ""
Фурсенко:Фурсенко NP-COG-MF ; ! ""
Фюльсбюттель:Фюльсбюттель NP-TOP ; !""
Хабалов:Хабалов NP-COG-OB ; ! ""
Хабаров:Хабаров NP-COG-OB ; ! ""
Хабиба:Хабиба NP-ANT-F ; ! "Xabıyba" (Arabic)
Хабиболлаұлы:Хабиболлаұлы NP-COG-M ; ! "USE/MT"
Хабиб:Хабиб NP-ANT-M ; ! "Xabıyb" (Arabic)
Хави:Хави NP-ANT-M ; !"Use/MT"
Хав:Хав NP-ANT-M ; !"Use/MT"
Хавьер:Хавьер NP-ANT-M ; !"Use/MT"
Хаг:Хаг NP-ANT-M ; !"Use/MT"
Хаддингтон:Хаддингтон NP-TOP ; !"" !Use/MT
Хадеев:Хадеев NP-COG-OB ; ! ""
Хаджиев:Хаджиев NP-COG-OB ; ! "" ! Use/MT
Хадим:Хадим NP-ANT-M ; ! "Xadıym" (Arabic)
Хадиша:Хадиша NP-ANT-F ; ! "Xadıysha" (Arabic)
Хадия:Хадия NP-ANT-F ; ! "Xadıya" (Arabic)
Хазария:Хазария NP-TOP ; ! ""
Хаз:Хаз NP-ANT-M ; !"Use/MT"
Хайдар:Хайдар NP-ANT-M ; ! "Xaydar" (Arabic)
Хакасия:Хакасия NP-TOP ; ! ""
Хакасия:Хакасия NP-TOP ; !"Use/MT"
Хакназар:Хакназар NP-ANT-M ; ! "Xaknazar" (Arabic)
Хаксли:Хаксли NP-COG-MF ; !"Use/MT"
Хак:Хак NP-ANT-M ; !"Use/MT"
Хакімбек:Хакімбек NP-ANT-M ; ! "Xakimbek" 
Хакімжанов:Хакімжанов NP-COG-OB ; ! ""
Хакімжан:Хакімжан NP-ANT-M ; ! "Xakimjan" 
Хакімзада:Хакімзада NP-ANT-M ; ! "Xakimzada" 
Хакімов:Хакімов NP-COG-OB ; ! ""
Хакім:Хакім NP-ANT-M ; ! "Xakim" 
Халед:Халед NP-ANT-M ; !"Use/MT"
Халел:Халел NP-ANT-M ; ! "Xalel" (Arabic)
Халида:Халида NP-ANT-F ; ! "Xalıyda" (Arabic)
Халид:Халид NP-ANT-M ; !"Use/MT"
Халиков:Халиков NP-COG-OB ; ! ""
Халила:Халила NP-ANT-F ; ! "Xalıyla" (Arabic)
Халил:Халил NP-ANT-M ; ! "Xalıyl" (Arabic)
Халима:Халима NP-ANT-F ; ! "Xalıyma" (Arabic)
Халиса:Халиса NP-ANT-F ; ! "Xalıysa" (Arabic)
Халиулла:Халиулла NP-ANT-M ; ! ""
Халкомсов:Халкомсов NP-COG-OB ; ! ""
Халық:Халық NP-ANT-M ; ! "Xalıq" (Arabic)
Хамас:Хамас NP-ORG ; ! ""
Хамдамов:Хамдамов NP-COG-OB ; ! ""
Хаменеи:Хаменеи NP-COG-MF ; ! ""
Хамза:Хамза NP-ANT-M ; ! "Xamza" (Arabic)
Хамидуллаев:Хамидуллаев NP-COG-OB ; ! "" ! Use/MT
Хамид:Хамид NP-ANT-M ; ! ""
Хамит:Хамит NP-ANT-M ; ! ""
Хамит:Хамит NP-ANT-M ; ! "Xamıyt" (Arabic)
Хаммат:Хаммат NP-ANT-M ; ! "Xammat" (Arabic)
Хангай:Хангай NP-TOP ; ! ""
Хангелді:Хангелді NP-ANT-M ; ! "Xangeldi" (Kazakh)
Хандверг:Хандверг NP-COG-MF ; ! ""
Хандошкин:Хандошкин NP-ANT-M ; !"Use/MT"
Ханифа:Ханифа NP-ANT-F ; ! "Xanifa" (Arabic)
Ханко:Ханко NP-COG-MF ; ! ""
Ханна:Ханна NP-ANT-F ; !"Use/MT"
Ханой:Ханой NP-TOP ; ! "Hanoy"
Ханс:Ханс NP-ANT-M ; ! "Hans (German name)"
Хантемір:Хантемір NP-ANT-M ; ! "Xantemir" 
Ханты%-Мансийск:Ханты%-Мансийск NP-TOP ; ! ""
Хан:Хан NP-ANT-M ; !"Use/MT"
Ханыков:Ханыков NP-COG-OB ; ! ""
Ханья:Ханья NP-ANT-F ; !"Use/MT"
Хап:Хап NP-ANT-M ; !"Use/MT"
Харбор:Харбор NP-AL ; ! ""
Харви:Харви NP-ANT-M ; !"Use/MT"
Харес:Харес NP-ANT-M ; ! "Xares" (Arabic)
Харитонов:Харитонов NP-COG-OB ; ! ""
Харитон:Харитон NP-ANT-M ; !"Use/MT"
Харламов:Харламов NP-COG-OB ; ! ""
Харламов:Харламов NP-COG-OB ; ! ""
Харлампьев:Харлампьев NP-COG-OB ; ! ""
Харпер:Харпер NP-COG-MF ; !"Use/MT"
Харрисон:Харрисон NP-ANT-M ; !"Use/MT"
Хартум:Хартум NP-TOP ; ! "" 
Хар:Хар NP-ANT-M ; !"Use/MT"
Харьков:Харьков NP-TOP ; ! ""
Хасанғалиев:Хасанғалиев NP-COG-OB ; ! ""
Хасан:Хасан NP-ANT-M ; ! "Xasan" (Arabic)
Хасенов:Хасенов NP-COG-OB ; ! ""
Хасен:Хасен NP-ANT-M ; ! ""
Хасково:Хасково NP-TOP ; !"Use/MT"
Хассанал:Хассанал NP-ANT-M ; !"Use/MT"
Хассан:Хассан NP-ANT-M ; !"Use/MT"
Хатами:Хатами NP-COG-MF ;
Хатима:Хатима NP-ANT-F ; ! "Xatıyma" (Arabic)
Хатип:Хатип NP-ANT-M ; ! "Xatıyp" (Arabic)
Хатифа:Хатифа NP-ANT-F ; ! "Xatifa" (Arabic)
Хафиза:Хафиза NP-ANT-F ; ! "Xafiza" (Arabic)
Хафиз:Хафиз NP-ANT-M ; ! "Xafiz" (Arabic)
Хаф:Хаф NP-ANT-M ; !"Use/MT"
Ха:Ха NP-ANT-M ; !"Use/MT"
Хач:Хач NP-ANT-M ; !"Use/MT"
Хашеми:Хашеми NP-ANT-M ; !"Use/MT"
Хашиев:Хашиев NP-COG-OB ; ! "" ! Use/MT
Хеа:Хеа NP-ANT-F ; !"Use/MT"
Хеб:Хеб NP-ANT-M ; !"Use/MT"
Хедквист:Хедквист NP-ANT-F ; ! "Xafiza" (Arabic)
Хез:Хез NP-ANT-M ; !"Use/MT"
Хел:Хел NP-ANT-M ; !"Use/MT"
Хельга:Хельга NP-ANT-F ; !"Use/MT"
Хельсинки:Хельсинки NP-TOP ; ! "Helsinki"
Хем:Хем NP-ANT-M ; !"Use/MT"
Хет:Хет NP-ANT-M ; !"Use/MT"
Хеф:Хеф NP-ANT-M ; !"Use/MT"
Хеш:Хеш NP-ANT-M ; !"Use/MT"
Хиггинс:Хиггинс NP-COG-MF ; !"Use/MT"
Хизер:Хизер NP-ANT-F ; !"Use/MT"
Хиз:Хиз NP-ANT-M ; !"Use/MT"
Хикмет:Хикмет NP-ANT-M ; ! "Xiykmet" (Arabic)
Хилал:Хилал NP-ANT-M ; ! ""
Хиллари:Хиллари NP-ANT-F ; ! "" 
Хиль:Хиль NP-ANT-M ; !"Use/MT"
Химка:Химка NP-TOP ; !""
Хин:Хин NP-ANT-M ; !"Use/MT"
Хиросима:Хиросима NP-TOP ; !"Use/MT"
Хисматуллин:Хисматуллин NP-COG-IN ; ! ""
Хитлер:Хитлер NP-COG-MF ; !"Use/MT"
Хичкок:Хичкок NP-COG-MF ; !"Use/MT"
Хлестаков:Хлестаков NP-COG-OB ; ! ""
Хлодвиг:Хлодвиг NP-ANT-M ; !"Use/MT"
Хло:Хло NP-ANT-M ; !"Use/MT"
Хлудов:Хлудов NP-COG-OB ; ! ""
Хмелев:Хмелев NP-COG-OB ; ! ""
Хоакин:Хоакин NP-ANT-M ; !"Use/MT"
Хог:Хог NP-ANT-M ; !"Use/MT"
Холбрук:Холбрук NP-COG-MF ; ! ""
Холли:Холли NP-ANT-F ; !"Use/MT"
Холлуорд:Холлуорд NP-ANT-M ;
Холмс:Холмс NP-COG-MF ; !"Use/MT"
Хомейни:Хомейни NP-COG-MF ; !"Use/MT"
Хоменко:Хоменко NP-COG-MF ; ! ""
Хомутов:Хомутов NP-COG-OB ; ! ""
Хонсю:Хонсю NP-TOP ; ! ""
Хопкинс:Хопкинс NP-COG-MF ; !"Use/MT"
Хорасан:Хорасан NP-TOP ; ! ""
Хорасев:Хорасев NP-COG-OB ; ! ""
Хорватия:Хорватия NP-TOP ; ! ""
Хоргос:Хоргос NP-TOP ; !"Use/MT"
Хорезм:Хорезм NP-TOP ; ! ""
Хорхе:Хорхе NP-ANT-M ; !"Use/MT"
Хор:Хор NP-ANT-M ; !"Use/MT"
Хосеп:Хосеп NP-ANT-M ; !"Use/MT"
Хосе:Хосе NP-ANT-M ; ! ""
Хосни:Хосни NP-ANT-M ; !"Use/MT"
Хоффман:Хоффман NP-COG-MF ; !"Use/MT"
Хоф:Хоф NP-ANT-M ; !"Use/MT"
Хохлов:Хохлов NP-COG-OB ; ! ""
Хо:Хо NP-ANT-M ; !"Use/MT"
Хо% Ши% Мин:Хо% Ши% Мин NP-TOP ; ! "Ho Chi Minh [city]"
Хош:Хош NP-ANT-M ; !"Use/MT"
Храброво:Храброво NP-TOP ; !""
Храпунов:Храпунов NP-COG-OB ; ! ""
Христенко:Христенко NP-COG-MF ; ! ""
Христос:Христос NP-ANT-M ; !"Use/MT"
Христофор:Христофор NP-ANT-M ; ! ""
Хрусталев:Хрусталев NP-COG-OB ; ! ""
Хрущев:Хрущев NP-COG-OB ; ! ""
Хуан:Хуан NP-ANT-M ; ! ""
Худуд%-ал% Алам:Худуд%-ал% Алам NP-AL ; ! "" Use/MT
Хулагу:Хулагу NP-ANT-M ; !"Use/MT"
Хул:Хул NP-ANT-M ; !"Use/MT"
Хумышев:Хумышев NP-COG-OB ; ! ""
Хусейн:Хусейн NP-COG-MF ; !"Use/MT"
Хуфен:Хуфен NP-TOP ; !""
Ху:Ху NP-ANT-M ; ! ""
Ху:Ху NP-ANT-M ; !"Use/MT"
Ху:Ху NP-COG-MF ; ! ""
Хуш:Хуш NP-ANT-M ; !"Use/MT"
Хұзыстан:Хұзыстан NP-TOP ; ! ""
Хұсайын:Хұсайын NP-ANT-M ; ! "Xusayın" (Arabic)
Хьюго:Хьюго NP-ANT-M ; !"Use/MT"
Хьюз:Хьюз NP-COG-MF ; !"Use/MT"
Хьюман% Райтс% Уотч:Хьюман% Райтс% Уотч NP-ORG ; ! ""
Хьюман:Хьюман NP-COG-MF ; ! ""
Хьюстон:Хьюстон NP-ANT-M ; !"Use/MT"
Хьюстон:Хьюстон NP-TOP ; !"Use/MT"
Хью:Хью NP-ANT-M ; !"Use/MT"
Хэдли:Хэдли NP-ANT-F ; !"Use/MT"
Хэл:Хэл NP-ANT-M ; !"Use/MT"
Хэмиш:Хэмиш NP-ANT-M ; !"Use/MT"
Хюэ:Хюэ NP-TOP ; ! ""
Һарон:Һарон NP-ANT-M ; ! ""
Царев:Царев NP-COG-OB ; ! ""
Цветаев:Цветаев NP-COG-OB ; ! ""
Цветков:Цветков NP-COG-OB ; ! ""
Цезарь:Цезарь NP-ANT-M ; !"Use/MT"
Цейлон:Цейлон NP-TOP ; ! ""
Целзо:Целзо NP-ANT-M ; !"Use/MT"
Целиноград:Целиноград NP-TOP ; ! ""
Центавр:Центавр NP-TOP ; !"Use/MT"
Цен:Цен NP-ANT-M ; !"Use/MT"
Цзиньпин:Цзиньпин NP-COG-M ; !"Use/MT"
Цзиньтао:Цзиньтао NP-COG-M ; !"Use/MT"
Цзы:Цзы NP-ANT-M ; !"Use/MT"
Цинказ:Цинказ NP-ORG ; ! ""
Цицерон:Цицерон NP-ANT-M ; !"Use/MT"
Цой:Цой NP-COG-M ; !"Use/MT"
Црес:Црес NP-ANT-M ; !"Use/MT"
Цюрих:Цюрих NP-ORG ; !"Use/MT"
Чавез:Чавез NP-COG-MF ; !"Use/MT"
Чагаев:Чагаев NP-COG-OB ; ! ""
Чад:Чад NP-TOP ; ! ""
Чайковский:Чайковский NP-COG-M ; ! ""
Чак:Чак NP-ANT-M ; ! ""
Чалабаев:Чалабаев NP-COG-OB ; ! ""
Чаллы:Чаллы NP-TOP ; ! ""
Чапаев:Чапаев NP-COG-OB ; ! ""
Чаплин:Чаплин NP-COG-IN ; ! ""
Чарлз:Чарлз NP-ANT-M ; ! ""
Чарли:Чарли NP-ANT-F ; !"Use/MT"
Чарли:Чарли NP-ANT-M ; ! ""
Чарльз:Чарльз NP-ANT-M ; ! ""
Чаушеску:Чаушеску NP-COG-MF ; !"Use/MT"
Чебышев:Чебышев NP-COG-OB ; ! ""
Чейз% Манхеттен:Чейз% Манхеттен NP-TOP ; !"Use/MT"
Чейни:Чейни NP-COG-MF ; ! ""
Челах:Челах NP-COG-MF ; ! ""
Челнокова:Челнокова NP-TOP ; !""
Челноков:Челноков NP-COG-OB ; ! ""
Челси:Челси NP-AL ; ! ""
Челси:Челси NP-ANT-F ; !"Use/MT"
Чел:Чел NP-ANT-M ; !"Use/MT"
Челюскин:Челюскин NP-TOP ; ! ""
Чемпиондар% лигасы:Чемпиондар% лига NP-ORG-COMPOUND ; ! "UEFA Champions League"
Червиченко:Червиченко NP-COG-MF ; ! ""
Червоненко:Червоненко NP-COG-MF ; ! ""
Чердабаев:Чердабаев NP-COG-OB ; ! ""
Черевиченко:Черевиченко NP-COG-MF ; ! ""
Черемухов:Черемухов NP-COG-OB ; ! ""
Черенков:Черенков NP-COG-OB ; ! ""
Черенко:Черенко NP-COG-MF ; ! ""
Черненко:Черненко NP-COG-MF ; ! ""
Чернов:Чернов NP-COG-OB ; ! ""
Черногория:Черногория NP-TOP ; ! ""
Чернышевский:Чернышевский NP-COG-M ; ! ""
Чернышев:Чернышев NP-COG-OB ; ! ""
Черняев:Черняев NP-COG-OB ; ! ""
Черняховск:Черняховск NP-TOP ; !""
Черск:Черск NP-TOP ; ! ""
Черчилль:Черчилль NP-COG-MF ; ! ""
Чесноков:Чесноков NP-COG-OB ; ! ""
Чес:Чес NP-ANT-M ; !"Use/MT"
Чехия:Чехия NP-TOP ; ! ""
Чехов:Чехов NP-COG-OB ; ! ""
Чехословакия:Чехословакия NP-TOP ; ! ""
Чех% республикасы:Чех% республикасы NP-TOP ; !"Use/MT"
Чижиков:Чижиков NP-COG-OB ; ! ""
Чикаго:Чикаго NP-TOP ; ! "Chicago"
Чико:Чико NP-ANT-M ; !"Use/MT"
Чили:Чили NP-TOP ; ! ""
Чиль:Чиль NP-ANT-M ; !"Use/MT"
Чирмешән:Чирмешән NP-TOP ; ! ""
Чистай:Чистай NP-TOP ; ! ""
Чис:Чис NP-ANT-M ; !"Use/MT"
Чита:Чита NP-TOP ; ! ""
Читнаев:Читнаев NP-COG-OB ; ! ""
Читта:Читта NP-AL ; ! ""
Чит:Чит NP-ANT-M ; !"Use/MT"
Чихачев:Чихачев NP-COG-OB ; ! ""
Чи:Чи NP-ANT-M ; !"Use/MT"
Чкалов:Чкалов NP-COG-OB ; ! ""
Чок:Чок NP-ANT-M ; ! ""
Чувашия:Чувашия NP-TOP ; !"Use/MT"
Чудинов:Чудинов NP-COG-OB ; ! "" ! Use/MT
Чукотка:Чукотка NP-TOP ; ! ""
Чукотка:Чукотка NP-TOP ; ! ""
Чулков:Чулков NP-COG-OB ; ! ""
Чулошников:Чулошников NP-COG-OB ; ! ""
Чупоршнев:Чупоршнев NP-COG-OB ; ! ""
Чуриков:Чуриков NP-COG-OB ; ! ""
Чуркин:Чуркин NP-COG-IN ; ! ""
Чу:Чу NP-ANT-M ; !"Use/MT"
Чүпрәле:Чүпрәле NP-TOP ; ! ""
Шабай:Шабай NP-ANT-M ; ! "Shabay" (Arabic)
Шабал:Шабал NP-ANT-F ; ! "Shabal" (Arabic)
Шабдан:Шабдан NP-ANT-M ; ! "Shabdan" (Persian)
Шабдарбаев:Шабдарбаев NP-COG-OB ; ! ""
Шаб:Шаб NP-ANT-M ; !"Use/MT"
Шавкат:Шавкат NP-ANT-M ; ! ""
Шав:Шав NP-ANT-M ; !"Use/MT"
Шагаев:Шагаев NP-COG-OB ; ! ""
Шағыр:Шағыр NP-ANT-M ; ! "Shağır" (Arabic)
Шадыман:Шадыман NP-ANT-M ; ! "Shadıman" (Persian)
Шажабек:Шажабек NP-ANT-M ; ! "Shajabek" (Kazakh)
Шайбан:Шайбан NP-ANT-M ; ! "Shayban" (Arabic)
Шайба:Шайба NP-ANT-M ; !"Use/MT"
Шайқысламов:Шайқысламов NP-COG-OB ; ! ""
Шаймерденов:Шаймерденов NP-COG-OB ; ! "" ! Use/MT
Шаймиев:Шаймиев NP-COG-OB ; ! ""
Шайхы:Шайхы NP-ANT-M ; ! "Shayxı" (Arabic)
Шакарім:Шакарім NP-ANT-M ; ! "Shakarim" (Arabic)
Шакира:Шакира NP-ANT-F ; !"Use/MT"
Шалаев:Шалаев NP-COG-OB ; ! ""
Шалғымбаев:Шалғымбаев NP-COG-OB ; ! "" ! Use/MT
Шалеев:Шалеев NP-COG-OB ; ! ""
Шалқұйрық:Шалқұйрық NP-ANT-M; ! "" USE/MT
Шал:Шал NP-ANT-M ; !"Use/MT"
Шамаев:Шамаев NP-COG-OB ; ! ""
Шамбыл:Шамбыл NP-ANT-M ; ! "Shambıl" (Kazakh)
Шамдария:Шамдария NP-ANT-F ; ! "Shamdarıya" (Arabic)
Шамиль:Шамиль NP-ANT-M ; ! "" ! Use/MT
Шамполов:Шамполов NP-COG-OB ; ! ""
Шам:Шам NP-ANT-M ; !"Use/MT"
Шанхай:Шанхай NP-TOP ; ! ""
Шаңгереев:Шаңгереев NP-COG-OB ; ! ""
Шаңғұтов:Шаңғұтов NP-COG-OB ; ! ""
Шаңғытбаев:Шаңғытбаев NP-COG-OB ; ! ""
Шапошников:Шапошников NP-COG-OB ; ! ""
Шапталов:Шапталов NP-COG-OB ; ! ""
Шарапат:Шарапат NP-ANT-F ; ! "Sharapat" (Arabic)
Шарапов:Шарапов NP-COG-OB ; ! ""
Шара:Шара NP-ANT-F ; ! "Shara" (Kazakh)
Шарбану:Шарбану NP-ANT-F ; ! "Sharbanıw" (Arabic)
Шарипович:Шарипо NP-PAT-VICH ; ! "" ! Use/MT 
Шариф:Шариф NP-ANT-M ; ! "USE/MT"
Шарлотта:Шарлотта NP-ANT-F ; !"Use/MT"
Шарлоттенбург:Шарлоттенбург NP-TOP ; !""
Шарль:Шарль NP-ANT-M ; ! ""
Шаров:Шаров NP-COG-OB ; ! ""
Шарон:Шарон NP-ANT-F ; !"Use/MT"
Шар:Шар NP-ANT-M ; !"Use/MT"
Шауқат:Шауқат NP-ANT-M ; ! ""
Шафиға:Шафиға NP-ANT-F ; ! "Shafiğa" (Arabic)
Шаханов:Шаханов NP-COG-OB ; ! ""
Шахзада:Шахзада NP-ANT-F ; ! "Shaxzada" (Arabic)
Шахия:Шахия NP-ANT-F ; ! "Shaxıya" (Persian)
Шахноза:Шахноза NP-ANT-F ; ! 
Шахноза:Шахноза NP-ANT-F ; !"Use/MT"
Шацков:Шацков NP-COG-OB ; ! "" ! Use/MT
Ша:Ша NP-ANT-M ; !"Use/MT"
Шашков:Шашков NP-COG-OB ; ! ""
Шаяхметов:Шаяхметов NP-COG-OB ; ! ""
Шаяхметов:Шаяхметов NP-COG-OB ; ! ""
Шаяхметов:Шаяхметов NP-COG-OB ; ! "" ! Use/MT
Шәймиев:Шәймиев NP-COG-OB ; ! ""
Шәкәрім:Шәкәрім NP-ANT-M ; ! "Shäkärim" (Arabic)
Шәкира:Шәкира NP-ANT-F ; ! "Shäkiyra" (Arabic)
Шәкіров:Шәкіров NP-COG-OB ; ! "" ! Use/MT
Шәмсия:Шәмсия NP-ANT-F ; ! "Shämsıya" (Arabic)
Шәмшә:Шәмшә NP-ANT-F ; ! "" ! Use/MT
Шәмші:Шәмші NP-ANT-M ; ! ""
Шәміл:Шәміл NP-ANT-M ; ! "" ! Use/MT
Шәрбат:Шәрбат NP-ANT-F ; ! "Shärbat" (Arabic)
Шәрипа:Шәрипа NP-ANT-F ; ! "Shäriypa" (Arabic)
Шәріпов:Шәріпов NP-COG-OB ; ! ""
Шварценберг:Шварценберг NP-COG-MF ; ! ""
Шварценеггер:Шварценеггер NP-COG-MF ; !"Use/MT"
Швейцария:Швейцария NP-TOP ; ! "Switzerland"
Швейцер:Швейцер NP-COG-MF ; !"Use/MT"
Шверин:Шверин NP-TOP ; !""
Швецария:Швецария NP-TOP ; !"Use/MT"
Швеция:Швеция NP-TOP ; ! "Sweden"
Шеб:Шеб NP-ANT-M ; !"Use/MT"
Шевалье:Шевалье NP-COG-MF ; ! "" ! Use/MT 
Шевернадзе:Шевернадзе NP-COG-MF ; ! "" ! Use/MT 
Шеврон:Шеврон NP-ORG ; ! ""
Шевченко:Шевченко NP-COG-MF ; ! ""
Шевченко:Шевченко NP-COG-MF ; ! ""
Шекерхан:Шекерхан NP-ANT-F ; ! "Shekerxan" (Arabic)
Шекер:Шекер NP-ANT-F ; ! "Sheker" (Arabic)
Шекспир:Шекспир NP-COG-MF ; ! ""
Шелли:Шелли NP-COG-M ; ! ""
Шелли:Шелли NP-TOP ; ! ""
Шемекеев:Шемекеев NP-COG-OB ; ! ""
Шенкурск:Шенкурск NP-TOP ; !""
Шеңне:Шеңне NP-ANT-F ; ! "Shengne" (Tuvan)
Шерғазиев:Шерғазиев NP-COG-OB ; ! ""
Шерементьев:Шерементьев NP-COG-OB ; ! ""
Шереметев:Шереметев NP-COG-OB ; ! ""
Шерлок:Шерлок NP-ANT-M ; ! ""
Шер:Шер NP-ANT-M ; !"Use/MT"
Шестаков:Шестаков NP-COG-OB ; ! ""
Шетпе:Шетпе NP-TOP ; ! ""
Шефтсбери:Шефтсбери NP-TOP ; ! ""
Шешен:Шешен NP-TOP ; !""
Шибаев:Шибаев NP-COG-OB ; ! ""
Шиб:Шиб NP-ANT-M ; !"Use/MT"
Шиелі:Шиелі NP-TOP ; ! "Shiyeli"
Шик:Шик NP-ANT-M ; !"Use/MT"
Шиллер:Шиллер NP-COG-M ; ! ""
Шилманов:Шилманов NP-COG-OB ; ! ""
Шилов:Шилов NP-COG-OB ; ! ""
Шил:Шил NP-ANT-M ; !"Use/MT"
Шимон:Шимон NP-ANT-M ; ! "Simon"
Шимон:Шимон NP-COG-MF ; !"Use/MT"
Шим:Шим NP-ANT-M ; !"Use/MT"
Шингарев:Шингарев NP-COG-OB ; ! ""
Шинкаренко:Шинкаренко NP-COG-MF ; ! ""
Шинко:Шинко NP-COG-MF ; ! ""
Шираз:Шираз NP-TOP-RUS ; ! ""
Ширак:Ширак NP-COG-M ; ! ""
Ширақбек:Ширақбек NP-ANT-M ; ! "Shıyraqbek" (Kazakh)
Ширли:Ширли NP-ANT-F ; !"Use/MT"
Широнин:Широнин NP-COG-IN ; ! ""
Ширяев:Ширяев NP-COG-OB ; ! ""
Шиф:Шиф NP-ANT-M ; !"Use/MT"
Ши:Ши NP-ANT-M ; !"Use/MT"
Школьник:Школьник NP-COG-MF ; ! "" ! Use/MT
Шлезвиг:Шлезвиг NP-TOP ; !""
Шлиссельбург:Шлиссельбург NP-TOP ; !""
Шляпников:Шляпников NP-COG-OB ; ! ""
Шмид:Шмид NP-COG-MF ; !"Use/MT"
Шнайдер:Шнайдер NP-COG-MF ; !"Use/MT"
Шнитников:Шнитников NP-COG-OB ; ! ""
Шоқаев:Шоқаев NP-COG-OB ; ! ""
Шоқан:Шоқан NP-ANT-M ; !"Use/MT"
Шолман:Шолман NP-TOP ; ! ""
Шолпан:Шолпан NP-AL ; ! ""
Шолпан:Шолпан NP-ANT-F ; ! "Sholpan" (Kazakh)
Шонанов:Шонанов NP-COG-OB ; ! ""
Шон:Шон NP-ANT-M ; !"Use/MT"
Шопенгауэр:Шопенгауэр NP-COG-MF ; !"Use/MT"
Шораұлы:Шораұлы NP-ANT-M ; !
Шорманов:Шорманов NP-COG-OB ; ! ""
Шостков:Шостков NP-COG-OB ; ! ""
Шотаев:Шотаев NP-COG-OB ; ! ""
Шотландия:Шотландия NP-TOP ; ! "Scotland"
Шо:Шо NP-ANT-M ; !"Use/MT"
Шөкеев:Шөкеев NP-COG-OB ; ! "" ! Use/MT
Шөлістан:Шөлістан NP-TOP ; ! "" 
Шпаков:Шпаков NP-COG-OB ; ! ""
Шпицберген:Шпицберген NP-TOP ; ! ""
Шри%-Ланка:Шри%-Ланка NP-TOP ; ! ""
Штайнмайер:Штайнмайер NP-COG-MF ; !"Use/MT"
Штайн:Штайн NP-TOP ; !""
Штейнберг:Штейнберг NP-COG-MF ; !"Use/MT"
Штейнер:Штейнер NP-COG-MF ; !"Use/MT"
Штирия:Штирия NP-TOP ; !"Use/MT"
Штутгарт:Штутгарт NP-TOP ; !"Use/MT"
Шубин:Шубин NP-COG-IN ; ! ""
Шувалов:Шувалов NP-COG-OB ; ! ""
Шулаев:Шулаев NP-COG-OB ; ! ""
Шуленбург:Шуленбург NP-TOP ; !""
Шульц:Шульц NP-COG-MF ; !"Use/MT"
Шуляченко:Шуляченко NP-COG-MF ; ! ""
Шуп:Шуп NP-ANT-M ; !"Use/MT"
Шустов:Шустов NP-COG-OB ; ! ""
Шу:Шу NP-ANT-M ; !"Use/MT"
Шу:Шу NP-TOP ; ! "" 
Шұбарши:Шұбарши NP-TOP ; ! ""
Шұғайып:Шұғайып NP-ANT-M ; ! ""
Шұға:Шұға NP-ANT-F ; ! "Shuğa" (Kazakh)
Шұғыла:Шұғыла NP-ANT-F ; ! "Shuğıla" (Kazakh)
Шүкеев:Шүкеев NP-COG-OB ; ! "" ! Use/MT
Шүменов:Шүменов NP-COG-OB ; ! ""
Шығай:Шығай NP-ANT-M ; ! "Shığay" (Old Turkic)
Шығанақ:Шығанақ NP-ANT-M ; ! "Shığanaq" (Kazakh)
Шылманов:Шылманов NP-COG-OB ; ! ""
Шымбұлақ:Шымбұлақ NP-TOP ; ! "Shymbulaq"
Шымкент:Шымкент NP-TOP ; ! "Shymkent"
Шымырбай:Шымырбай NP-ANT-M ; ! "Shımırbay" (Kazakh)
Шымыр:Шымыр NP-ANT-M ; ! "Shımır" (Kazakh)
Шынаргүл:Шынаргүл NP-ANT-F ; ! "Shınargül" (Arabic)
Шынар:Шынар NP-ANT-F ; ! "Shınar" (Kazakh)
Шынасілов:Шынасілов NP-COG-OB ; ! ""
Шынәлиев:Шынәлиев NP-COG-OB ; ! "" ! Use/MT
Шынтөренов:Шынтөренов NP-COG-OB ; ! "" ! Use/MT
Шыңғырлау:Шыңғырлау NP-TOP ; ! ""
Шыңғысхан:Шыңғысхан NP-ANT-M ; ! "" Mosty it's written separately, but occures as one word also
Шыңғыс:Шыңғыс NP-ANT-M ; ! "Shyngys"
Шыңжаң:Шыңжаң NP-TOP ; ! ""
Шырайгүл:Шырайгүл NP-ANT-F ; ! "Shıraygül" (Persian)
Шырдабаев:Шырдабаев NP-COG-OB ; ! ""
Шырынгүл:Шырынгүл NP-ANT-F ; ! "Shırıngül" (Arabic)
Шырын:Шырын NP-ANT-F ; ! "Shırın" (Arabic)
Шілдебай:Шілдебай NP-ANT-M ; ! "Shildebay" (Arabic)
Щепкин:Щепкин NP-COG-IN ; ! ""
Щербаков:Щербаков NP-COG-OB ; ! ""
Щукин:Щукин NP-COG-IN ; ! ""
Щучинск:Щучинск NP-TOP ; ! ""
Ыбрайымов:Ыбрайымов NP-COG-OB ; ! ""
Ыбыраев:Ыбыраев NP-COG-OB ; ! ""
Ыбырай:Ыбырай NP-ANT-M ; ! "Ibıray" (Arabic)
Ыдырысов:Ыдырысов NP-COG-OB ; ! ""
Ыдырыс:Ыдырыс NP-ANT-M ; ! "Idırıs" (Arabic)
Ызғұтты:Ызғұтты NP-ANT-M ; ! ""
Ықсан:Ықсан NP-ANT-M ; ! "Iqsan" (Arabic)
Ықылас:Ықылас NP-ANT-M ; ! "Iqılas" (Arabic)
Ырғызбай:Ырғызбай NP-ANT-M ; ! ""
Ырысалды:Ырысалды NP-ANT-F ; ! "Irısaldı" (Kazakh)
Ырысалды:Ырысалды NP-ANT-M ; ! "Irısaldı" (Kazakh)
Ырысбала:Ырысбала NP-ANT-F ; ! "Irısbala" (Arabic)
Ырысбек:Ырысбек NP-ANT-M ; ! "Irısbek" (Kazakh)
Ырысты:Ырысты NP-ANT-F ; ! "Irıstı" 
Ысқақов:Ысқақов NP-COG-OB ; ! ""
Ысқақ:Ысқақ NP-ANT-M ; ! "Isqaq" (Arabic)
Ысмағамбетов:Ысмағамбетов NP-COG-OB ; ! "" ! Use/MT
Ысмағұл:Ысмағұл NP-ANT-M ; ! ""
Ысмайылов:Ысмайылов NP-COG-OB ; ! ""
Ыстамбақы:Ыстамбақы NP-ANT-M ; ! "Istambaqı" (Arabic)
Ыстамбұл:Ыстамбұл NP-TOP ; !"Use/MT"
Ыстанбұл:Ыстанбұл NP-TOP ; ! "Istanbul"
Ыстықкөл:Ыстықкөл NP-TOP ; ! ""
Ібни:Ібни NP-ANT-M ; ! "İbniy" (Arabic)
Ізбасар:Ізбасар NP-ANT-M ; ! "İzbasar" (Kazakh)
Ізмұхамбетов:Ізмұхамбетов NP-COG-OB ; ! ""
Ізтілеуов:Ізтілеуов NP-COG-OB ; ! ""
Ілебай:Ілебай NP-ANT-M ; ! "İlebay" (Kazakh)
Ілеев:Ілеев NP-COG-OB ; ! ""
Ілияс:Ілияс NP-ANT-M ; ! "İlıyas" (Hebrew)
Інжу:Інжу NP-ANT-F ; ! "İnjıw" 
Інкәрбала:Інкәрбала NP-ANT-F ; ! "İnkärbala" 
Інкәр:Інкәр NP-ANT-F ; ! "İnkär" 
Іңкәрбек:Іңкәрбек NP-ANT-M ; ! "İŋkärbek" (Kazakh)
Ісмет:Ісмет NP-ANT-M ; ! "İsmet" (Arabic)
Эванс:Эванс NP-COG-MF ; ! ""
Эвбея:Эвбея NP-TOP ; !""
Эвелин:Эвелин NP-ANT-F ; !"Use/MT"
Эгон:Эгон NP-ANT-M ; !"Use/MT"
Эдвард:Эдвард NP-ANT-M ; ! ""
Эдвин:Эдвин NP-ANT-M ; ! ""
Эден:Эден NP-ANT-F ; !"Use/MT"
Эдинбург:Эдинбург NP-TOP ; ! ""
Эдита:Эдита NP-ANT-F ; !"Use/MT"
Эдит:Эдит NP-ANT-F ; !"Use/MT"
Эди:Эди NP-ANT-F ; !"Use/MT"
Эдмонд:Эдмонд NP-ANT-M ; !"Use/MT"
Эдмунд:Эдмунд NP-ANT-M ; !"Use/MT"
Эднан:Эднан NP-ANT-M ; ! "" ! Use/MT
Эдоков:Эдоков NP-COG-OB ; ! ""
Эдуардо:Эдуардо NP-ANT-M ; !"Use/MT"
Эдуард:Эдуард NP-ANT-M ; ! ""
Эдуард:Эдуард NP-ANT-M ; ! "Eduard" (Old High German)
Эзоп:Эзоп NP-ANT-M ; ! ""
Эйзенхауэр:Эйзенхауэр NP-COG-MF ; ! ""
Эйкборн:Эйкборн NP-COG-MF ; !"Use/MT"
Эймс:Эймс NP-TOP ; ! "Ames"
Эйнар:Эйнар NP-ANT-M ; !"Use/MT"
Эйнштейн:Эйнштейн NP-COG-MF ; ! ""
Эйфель:Эйфель NP-COG-MF ; !"Use/MT"
Эквадор:Эквадор NP-TOP ; ! ""
Экваторлық% Гвинея:Экваторлық% Гвинея NP-TOP ; ! ""
Экмеледдин:Экмеледдин NP-COG-IN ; ! "USE/MT"
Эктор:Эктор NP-ANT-M ; ! ""
Эк:Эк NP-ANT-M ; !"Use/MT"
Эладио:Эладио NP-ANT-M ; !"Use/MT"
Эла:Эла NP-ANT-F ; ! "Ela" (Latin)
Элвин:Элвин NP-ANT-M ; !"Use/MT"
Элвис:Элвис NP-ANT-M ; !"Use/MT"
Элейн:Элейн NP-ANT-F ; !"Use/MT"
Электра:Электра NP-ANT-F ; !"Use/MT"
Элеонора:Элеонора NP-ANT-F ; !"Use/MT"
Элефтериос:Элефтериос NP-ANT-M ; ! ""
Элиас:Элиас NP-ANT-M ; !"Use/MT"
Элиа:Элиа NP-ANT-M ; !"Use/MT"
Элизабетта:Элизабетта NP-ANT-F ; !"Use/MT"
Элизабет:Элизабет NP-ANT-F ; !"Use/MT"
Элиза:Элиза NP-ANT-F ; !"Use/MT"
Элисон:Элисон NP-ANT-F ; !"Use/MT"
Элиэзер:Элиэзер NP-ANT-M ; !"Use/MT"
Эли:Эли NP-ANT-M ; !"Use/MT"
Эллада:Эллада NP-TOP ; !""
Элла:Элла NP-ANT-F ; !"Use/MT"
Эллиотт:Эллиотт NP-ANT-M ; !"Use/MT"
Эллиот:Эллиот NP-ANT-M ; !"Use/MT"
Эллисон:Эллисон NP-ANT-F ; !"Use/MT"
Эллис:Эллис NP-ANT-M ; !"Use/MT"
Элма:Элма NP-ANT-F ; !"Use/MT"
Элтон:Элтон NP-ANT-M ; !"Use/MT"
Эль%-Барадеи:Эль%-Барадеи NP-COG-MF ; !"Use/MT"
Эльба:Эльба NP-TOP ; ! ""
Эльбрус:Эльбрус NP-TOP ; ! ""
Эльбурс:Эльбурс NP-TOP ; ! ""
Эльвира:Эльвира NP-ANT-F ; ! "ElʲVira" 
Эльзас:Эльзас NP-ANT-M ; !"Use/MT"
Эльза:Эльза NP-ANT-F ; !"Use/MT"
Эльмо:Эльмо NP-ANT-M ; !"Use/MT"
Эль% Сальвадор:эль% Сальвадор NP-TOP ; !"Use/MT"
Эльче:Эльче NP-TOP ; !"Use/MT"
Эль:Эль NP-ANT-M ; !"Use/MT"
Эмануэль:Эмануэль NP-ANT-M ; !"Use/MT"
Эмилжан:Эмилжан NP-ANT-M ; ! "Emiljan" (Arabic)
Эмилиано:Эмилиано NP-ANT-M ; !"Use/MT"
Эмилио:Эмилио NP-ANT-M ; !"Use/MT"
Эмили:Эмили NP-ANT-F ; !"Use/MT"
Эмилия:Эмилия NP-ANT-F ; !"Use/MT"
Эмиль:Эмиль NP-ANT-M ; ! ""
Эми:Эми NP-ANT-F ; !"Use/MT"
Эммануэль:Эммануэль NP-ANT-M ; !"Use/MT"
Эмма:Эмма NP-ANT-F ; ! "Emma" (Old High German)
Эмми:Эмми NP-ANT-F ; !"Use/MT"
Эмомали:Эмомали NP-ANT-M ; ! ""
Энгельс:Энгельс NP-COG-MF ; ! ""
Энди:Энди NP-ANT-M ; !"Use/MT"
Эндрю:Эндрю NP-ANT-M ; !"Use/MT"
Энни:Энни NP-ANT-F ; !"Use/MT"
Энн:Энн NP-ANT-F ; !"Use/MT"
Энрикета:Энрикета NP-ANT-F ; !"Use/MT"
Энрико:Энрико NP-ANT-M ; !"Use/MT"
Энрик:Энрик NP-ANT-M ; !"Use/MT"
Энтони:Энтони NP-ANT-M ; !"Use/MT"
Эпикур:Эпикур NP-ANT-M ; !"Use/MT"
Эразм:Эразм NP-ANT-M ; ! ""
Эрасмус:Эрасмус NP-ANT-M ; !"Use/MT"
Эраст:Эраст NP-ANT-M ; ! "Erast" (Greek)
Эра:Эра NP-ANT-F ; ! "Era" (New word)
Эрика:Эрика NP-ANT-F ; !"Use/MT"
Эрик:Эрик NP-ANT-M ; ! "Erik" (Old High German)
Эритрея:Эритрея NP-TOP ; ! ""
Эрих:Эрих NP-ANT-M ; !"Use/MT"
Эрколе:Эрколе NP-ANT-M ; !"Use/MT"
Эрл:Эрл NP-ANT-M ; !"Use/MT"
Эрнандо:Эрнандо NP-ANT-M ; !"Use/MT"
Эрнест:Эрнест NP-ANT-M ; !"Use/MT"
Эрно:Эрно NP-ANT-M ; !"Use/MT"
Эрнст:Эрнст NP-ANT-M ; ! "Ernst" (Old High German)
Эрос:Эрос NP-ANT-M ; !"Use/MT"
Эр%-Рияд:Эр%-Рияд NP-TOP ; ! "" 
Эсбьерг:Эсбьерг NP-TOP ; !""
Эспиноса:Эспиноса NP-COG-MF ; !"Use/MT"
Эстляндия:Эстляндия NP-TOP ; !""
Эстония:Эстония NP-TOP ; ! "Estonia"
Эсхил:Эсхил NP-ANT-M ; ! ""
Эс:Эс NP-ANT-M ; !"Use/MT"
Этнодизайн:Этнодизайн NP-ORG ; ! "Chevron" !Use/MT
Этторе:Этторе NP-ANT-M ; !"Use/MT"
Этьен:Этьен NP-ANT-M ; !"Use/MT"
Эфес:Эфес NP-TOP ; !"Use/MT"
Эфиопия:Эфиопия NP-TOP ; ! ""
Эфсе:Эфсе NP-TOP ; ! "" ! Use/MT
Эф:Эф NP-ANT-M ; !"Use/MT"
Эчкили%-Таш:Эчкили%-Таш NP-TOP ; ! ""
Эшли:Эшли NP-ANT-F ; !"Use/MT"
Эштремадура:Эштремадура NP-TOP; !"Use/MT"
Эш:Эш NP-ANT-M ; !"Use/MT"
Ювенальев:Ювенальев NP-COG-OB ; ! ""
Югославия:Югославия NP-TOP ; !"Use/MT"
Юденич:Юденич NP-COG-M ; ! ""
Юдин:Юдин NP-COG-IN ; ! ""
Юзефа:Юзефа NP-ANT-F ; !"Use/MT"
Юз:Юз NP-ANT-M ; !"Use/MT"
Юкатан:Юкатан NP-TOP ; ! ""
Юлий:Юлий NP-ANT-M ; !"Use/MT"
Юлия:Юлия NP-ANT-F ; ! ""
Юм:Юм NP-COG-MF ; ! ""
Юниор:Юниор NP-ANT-M ; !"Use/MT"
Юность:Юность NP-AL ; ! ""
Юн:Юн NP-ANT-M ; !"Use/MT"
Юпитер:Юпитер NP-AL ; ! ""
Юра:Юра NP-ANT-M ; ! ""
Юрген:Юрген NP-ANT-M ; ! "Jürgen (German name)"
Юрий:Юрий NP-ANT-M ; ! ""
Юроуп:Юроуп NP-ORG ; ! ""
Юрьевич:Юрье NP-PAT-VICH ; ! ""
Юрьев:Юрьев NP-COG-OB ; ! ""
Юстина:Юстина NP-ANT-F ; !"Use/MT"
Юстиниана:Юстиниана NP-ANT-F ; !"Use/MT"
Юсто:Юсто NP-ANT-M ; !"Use/MT"
Юстус:Юстус NP-ANT-M ; !"Use/MT"
Юсупов:Юсупов NP-COG-OB ; ! ""
Юсып:Юсып NP-ANT-M ; ! ""
Ютазы:Ютазы NP-TOP ; ! ""
Юта:Юта NP-TOP ; ! ""
Ютландия:Ютландия NP-TOP ; ! ""
Ющенко:Ющенко NP-COG-MF ; ! ""
Ю:Ю NP-ANT-M ; !"Use/MT"
Яакко:Яакко NP-ANT-M ; ! "Jaakko (Finnish name)"
Яблочков:Яблочков NP-COG-OB ; ! ""
Ягайло:Ягайло NP-ANT-M ; ! ""
Ягеллонов:Ягеллонов NP-COG-OB ; ! ""
Яғқұп:Яғқұп NP-ANT-M ; ! ""
Ядов:Ядов NP-COG-OB ; ! ""
Ядринцев:Ядринцев NP-COG-OB ; ! ""
Яир:Яир NP-ANT-M ; !"Use/MT"
Якеменко:Якеменко NP-COG-MF ; ! ""
Якоб:Якоб NP-ANT-M ; !"Use/MT"
Яковенко:Яковенко NP-COG-MF ; ! ""
Яковлев:Яковлев NP-COG-OB ; ! ""
Яков:Яков NP-ANT-M ; ! ""
Яколвев:Яколвев NP-COG-OB ; ! ""
Якубенко:Якубенко NP-COG-MF ; ! ""
Якутия:Якутия NP-TOP ; ! ""
Якушев:Якушев NP-COG-OB ; ! ""
Ялта:Ялта NP-TOP ; !""
Ямайка:Ямайка NP-TOP ; ! ""
Ямал:Ямал NP-ANT-M ; !"Use/MT"
Ямбол:Ямбол NP-TOP ; !"Use/MT"
Ямусукро:Ямусукро NP-TOP ; ! ""
Ямышев:Ямышев NP-COG-OB ; ! ""
Яна:Яна NP-ANT-F ; !"Use/MT"
Янг:Янг NP-COG-M ; ! ""
Янина:Янина NP-ANT-F ; !"Use/MT"
Янис:Янис NP-ANT-F ; !"Use/MT"
Янко:Янко NP-COG-MF ; ! ""
Яннис:Яннис NP-COG-MF ; ! ""
Яннис:Яннис NP-COG-M ; !"Use/MT"
Янота:Янота NP-COG-MF ; !"Use/MT"
Янукович:Януко NP-PAT-VICH ; ! ""
Янукович:Янукович NP-COG-MF ; ! ""
Ян:Ян NP-ANT-M ; !"Use/MT"
Яранцев:Яранцев NP-COG-OB ; ! ""
Ярополк:Ярополк NP-ANT-M ; ! ""
Ярославич:Ярославич NP-COG-M ; ! ""
Ярославцев:Ярославцев NP-COG-OB ; ! ""
Ярослав:Ярослав NP-ANT-M ; ! ""
Ярошук:Ярошук NP-COG-M ; !"Use/MT"
Ясауи:Ясауи NP-COG-MF ; ! ""
Ясеми:Ясеми NP-COG-MF ; !"Use/MT"
Ят:Ят NP-ANT-M ; !"Use/MT"
Яхья:Яхья NP-ANT-M ; ! ""
Яһуда:Яһуда NP-ANT-M ; ! "Yahuda"
Яценко:Яценко NP-COG-MF ; ! ""
Я:Я NP-ANT-M ; !"Use/MT"
