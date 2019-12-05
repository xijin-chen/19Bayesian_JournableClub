# Makefile 

# The following contains a list of all Rnw files:
RNW = $(wildcard *.Rnw)
# All Rnw files will be processed to a tex file:              
TEX = $(patsubst %.Rnw,%.tex,$(RNW)) 


# Master file:
MASTER = script

# Meta rules:
.Rnw.tex:
	Rscript -e "library(knitr); knitr::knit('$<','$@')"

.tex.pdf:
	latexmk -bibtex -synctex=1 -pdf  -use-make -halt-on-error \
	 -pdflatex="pdflatex" -silent $(MASTER).tex

# Conversion rules:
all: 	## $(MASTER).pdf clean
	Rscript -e "knitr::knit2pdf('$(MASTER).Rnw')"

clean:
	rm -f *~   *.out Rplots.pdf comment.cut \
              *.idx *.ilg *.brf *.blg *.spl  $(MASTER).dvi \
              *.backup *.toc *.fls  *fdb_latexmk *.synctex.gz  

cleanall: clean
	rm -f *.aux *.log *.ind 
	rm -f figure/ch??_fig*.pdf
	rm -f $(MASTER).tex chapter??.tex $(MASTER).bbl
	rm -f cache/*.* cache/__packages

quick:
	Rscript -e "knitr::knit2pdf('$(MASTER).Rnw')"

.SUFFIXES: .Rnw .R .tex .pdf
.SILENT: *.pdf *.tex	
.PHONY: all short clean

# 2017 Reinhard Furrer, Florian Gerber

