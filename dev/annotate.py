
#!/usr/bin/env python3

"""
annotate.py: a script for semi-automacally annotating texts for and with an
             Apertium machine translator.

INPUT is either:
- free-form text in language LANG, one sentence per line, or
- partially annotated text in the following format:

BEGIN EXAMPLE
<corpus>
  <checked>
    <doc title="Кішкентай ханзада" author="А. де Сент-Экзюпери"
         translator="Ж. Қонаева" pub="2013" lang="kk" origlang="fr"
         source="kitap.kz/12345/abcde.html" license="allRightsReserved">
      <p>
        <s>Бірде, алть жастағы кезімде [...]
          <t>|Бірде|бірде|adv|ADV||</t>
          <t>|алть|алты|num|NUM||алты|</t>
          [...]
        </s>
        [...]
     </p>
    </doc>    
  </checked>
  <new>
  </new
</corpus>
END EXAMPLE

That is, the contents of the <t>oken element are the following:
|surface form |lemma |tags |lexicon |lexicalAffixes |correctlySpelled|

The `lexical affixes' cell can be empty, but the cell itself has to be there.

An example of a token where it's not empty:
<t>|урманнар|урман|n pl nom|N1|урман>LAр|</t>

For tokens which were misspelled (or incorrectly OCR'd) in the original, there
can be an optional sixth cell, where the correct spelling of the surface form
is given.

The urge you to give the correct spelling in this additional cell and keep the
original spelling as it is, because the data about misspellings is a valuable
thing to have (for training an automatic spelling corrector, in particular).
"""
