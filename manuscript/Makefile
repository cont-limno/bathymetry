all: manuscript.pdf appendix.pdf diff.pdf combined.pdf submission.pdf

sync_figures:
	cp ../figures/slope_diagram_new.pdf slope_diagram_new.pdf
	cp ../figures/00_map_bathy-1.pdf 00_map_bathy-1.pdf
	cp ../figures/01_geometry_base-1.pdf 01_geometry_base-1.pdf
	cp ../figures/02_depth_model_grid_resid-1.pdf 02_depth_model_grid_resid-1.pdf

reviewer_comments.pdf: reviewer_comments.md
	pandoc $< -H quote_setup.tex -o $@

manuscript.pdf: agujournaltemplate.pdf
	cp agujournaltemplate.pdf manuscript.pdf
	@echo wordcount:
	@~/.TinyTeX/bin/x86_64-linux/texcount agujournaltemplate.tex -sum -sub=section

agujournaltemplate.pdf: agujournaltemplate.tex lagosdepth.bib
	pdflatex agujournaltemplate
	bibtex agujournaltemplate
	pdflatex agujournaltemplate

submission.pdf: agujournaltemplate.pdf appendix.pdf
	pdftk agujournaltemplate.pdf si_template_2019.pdf cat output submission.pdf

combined.pdf: figures.pdf appendix.pdf
	pdftk figures.pdf appendix.pdf output combined.pdf

figures.pdf: figures.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	-pdftk figures.pdf cat 2-end output figures2.pdf
	-mv figures2.pdf figures.pdf

appendix.pdf: si_template_2019.pdf si_template_2019.tex
	cp si_template_2019.pdf appendix.pdf

si_template_2019.pdf: figures.pdf si_template_2019.tex apacite.bst
	pdflatex si_template_2019
	-bibtex si_template_2019
	-pdflatex si_template_2019

diff.pdf: agujournaltemplate.tex agujournaltemplate.pdf
	latexdiff agujournaltemplate_0.1.tex agujournaltemplate.tex > diff.tex --flatten --append-safecmd=doi --allow-spaces --disable-citation-markup
	latexdiff agujournaltemplate_0.1.bbl agujournaltemplate.bbl > diff.bbl --flatten --append-safecmd=doi --allow-spaces --disable-citation-markup
	pdflatex diff.tex

clean:
	-@rm *.nav *.aux *.snm *.toc *.out *.log *.cpc 2>/dev/null || true
