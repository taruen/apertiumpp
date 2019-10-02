docs: FORCE
	scribble --markdown docs/index.scrbl; mv index.md README.md; cd docs; scribble index.scrbl; cd ../apertiumpp; raco pkg install; raco setup -p apertiumpp; cp doc/apertiumpp/*.html ../docs/apertiumpp/; cd ../apertiumpp-kaz; raco pkg install; raco setup -p apertiumpp-kaz; cp doc/apertiumpp-kaz/*.html ../docs/apertiumpp-kaz
FORCE:
