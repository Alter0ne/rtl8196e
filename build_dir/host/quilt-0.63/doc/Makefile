all : quilt.pdf quilt-rus.pdf

quilt.ps : quilt.dvi
	dvips -t letter -o $@ $<

quilt.pdf: main.tex
	pdflatex --jobname=quilt -t letter $<

quilt.dvi : main.tex
	latex $< && \
	mv main.dvi $@

quilt-rus.ps : quilt-rus.dvi
	dvips -t letter -o $@ $<

quilt-rus.pdf: main-rus.tex
	pdflatex --jobname=quilt-rus -t letter $<

quilt-rus.dvi : main-rus.tex
	latex $< && \
	mv main-rus.dvi $@

clean:
	rm -f {main,quilt}.{dvi,log,aux} {main,quilt}-rus.{dvi,log,aux}

distclean: clean
	rm -f quilt.{ps,pdf,dvi} quilt-rus.{ps,pdf,dvi}
