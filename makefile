docs: FORCE
	cd docs; scribble index.scrbl; cd ../apertiumpp; raco pkg install; raco setup -p apertiumpp; cp doc/apertiumpp/* ../docs/apertiumpp/

FORCE:
