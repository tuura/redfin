TARGET=paper
all: pdf

paper.pdf: $(wildcard *.tex) $(wildcard *.bib)
	- pdflatex -synctex=1 -shell-escape main
	- bibtex main
	- pdflatex -synctex=1 -shell-escape main
	pdflatex -synctex=1 -shell-escape main

pdf: paper.pdf

clean:
	rm -f *.eps
	rm -f *.aux *.log *.out *.bbl *.blg *~ *.bak $(TARGET).ps $(TARGET).pdf
	rm *.brf *.lof *.lot *.toc

# End
