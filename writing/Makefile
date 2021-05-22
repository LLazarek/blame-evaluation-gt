# Remake the pdf even if it's already present
.PHONY: paper.pdf

paper.pdf:	paper.tex cs.bib
	latexmk -interaction=nonstopmode -f --pdf $< || latexmk -interaction=nonstopmode -f --pdf $<

paper.tex: paper.tex
cs.bib: cs.bib

clean:
	latexmk -C

matthias:
	pdflatex paper && \
	open paper.pdf 

.PHONY: mf
mf: paper.pdf
	mutool poster -y 2 paper.pdf draft.pdf
	rmapi mkdir /Lukas
	rmapi put draft.pdf /Lukas/

.PHONY: mf-save
mf-save:
	rmapi get /Lukas/draft
	mv draft.zip recovered-draft.zip 
