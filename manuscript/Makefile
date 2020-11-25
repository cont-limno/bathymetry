all: manuscript.pdf diff.pdf combined.pdf

manuscript.pdf: agujournaltemplate.pdf wrr_submission.pdf
	cp agujournaltemplate.pdf manuscript.pdf
	@echo wordcount:
	@~/.TinyTeX/bin/x86_64-linux/texcount agujournaltemplate.tex -sum -sub=section

combined.pdf: figures.pdf appendix.pdf
	pdftk figures.pdf appendix.pdf output combined.pdf

figures.pdf: figures.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	-pdftk figures.pdf cat 2-end output figures2.pdf
	-mv figures2.pdf figures.pdf

appendix.pdf: si_template_2019.pdf si_template_2019.tex
	cp si_template_2019.pdf appendix.pdf

agujournaltemplate.pdf: agujournaltemplate.tex lagosdepth.bib
	pdflatex agujournaltemplate
	bibtex agujournaltemplate
	pdflatex agujournaltemplate

si_template_2019.pdf: figures.pdf si_template_2019.tex apacite.bst
	pdflatex si_template_2019
	-bibtex si_template_2019
	-pdflatex si_template_2019

wrr_submission.pdf: agujournaltemplate.pdf si_template_2019.pdf
	pdftk agujournaltemplate.pdf si_template_2019.pdf cat output wrr_submission.pdf

diff.pdf: wrr_submission.pdf
	latexdiff agujournaltemplate_0.1.tex agujournaltemplate.tex > diff.tex --flatten --append-safecmd=doi --allow-spaces --disable-citation-markup
	latexdiff agujournaltemplate_0.1.bbl agujournaltemplate.bbl > diff.bbl --flatten --append-safecmd=doi --allow-spaces --disable-citation-markup
	pdflatex diff.tex

clean:
	-@rm *.nav *.aux *.snm *.toc *.out *.log *.cpc 2>/dev/null || true
