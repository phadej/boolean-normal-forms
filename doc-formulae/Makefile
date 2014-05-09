FORMULAE=nnf.svg dnf.svg cnf.svg

all: $(FORMULAE)

%.dvi: %.tex
	cat texhead.tex $< texfoot.tex > tmp_$<
	latex tmp_$<
	mv $(addsuffix .dvi,$(basename tmp_$<)) $@
	$(RM) $(addsuffix .*,$(basename tmp_$<))

%.svg: %.dvi
	dvisvgm $< -n

.PHONY: clean
clean:
	$(RM) *.dvi *.log *.aux tmp_*
