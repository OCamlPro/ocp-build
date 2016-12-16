all: user-manual.pdf
html: user-manual.html

SOURCES=user-manual.tex ocp2-manual.tex chapter-*.tex

user-manual.pdf: $(SOURCES)
	pdflatex user-manual
	pdflatex user-manual

user-manual.html: user-manual.tex
	hevea htmlmacros.hva $<

ocp2-manual.pdf: $(SOURCES)
	pdflatex ocp2-manual
	pdflatex ocp2-manual

view: user-manual.pdf
	evince user-manual.pdf

clean:
	rm -f *~ user-manual.log content.log *.out *.aux *.toc

