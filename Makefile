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

clean:
	@rm -rf _build/ analyzer *~ */*~
	@rm -rf *.dot *.pdf */*.dot */*.pdf

doc: all
	@dune build @doc-private
