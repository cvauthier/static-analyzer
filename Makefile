# Cours "Semantics and applications to verification"
#
# Marc Chevalier 2018
# Josselin Giet 2021
# Ecole normale supérieure, Paris, France / CNRS / INRIA

.PHONY: all clean doc

all:
	@rm -f analyzer.exe
	@dune build analyzer.exe
	@ln -sf _build/default/analyzer.exe analyzer
	./test.sh

clean: clean-ex
	@rm -rf _build/ analyzer *~ */*~

clean-ex:
	@rm -rf *.dot *.pdf examples/*.dot examples/*.pdf examples/*.txt

doc: all
	@dune build @doc-private
