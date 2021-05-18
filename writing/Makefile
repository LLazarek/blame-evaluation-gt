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
