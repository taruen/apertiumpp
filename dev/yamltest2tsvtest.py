## Convert https://github.com/apertium-kaz/tests/morphophonology/*.yaml
## to https://github.com/apertium-kaz/tests/morphophonology/*.tsv

import yaml, glob, os

for f in glob.glob("*.yaml"):
    with open(f) as inf, open(os.path.splitext(f)[0] + ".tsv", 'w') as outf:
        d = yaml.load(inf)
        for dprime in d["Tests"].values():
            for lexical, surface in dprime.items():
                print("_", lexical, surface, sep="\t", file=outf)
