R := R --quiet
sweave := SWEAVE_STYLEPATH_DEFAULT="TRUE" $(R) CMD SWEAVE

PDFLATEX_FLAGS = -quiet

all : Rcaline.pdf SanFrancisco.pdf

%.pdf : %.tex %.bbl 
	pdflatex $(PDFLATEX_FLAGS) $< >> $*.log
	pdflatex $(PDFLATEX_FLAGS) $< >> $*.log

%.bbl : %.aux
	bibtex $<

%.aux : %.tex
	pdflatex $(PDFLATEX_FLAGS) $<

#.SECONDARY : 
%.tex : %.Rnw
	$(sweave) $<

# If we call this target 'clean', it will apparently wreak havoc; see
# http://cran.r-project.org/doc/manuals/R-exts.html#Writing-package-vignettes
.PHONY :
reallyclean : 
	rm -f *.log *.aux *.dvi *.eps *.pdf *.tex *.bbl *.blg *.png *.out
