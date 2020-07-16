manuscript.pdf: manuscript.Rmd \
pinp.cls \
lagosdepth.bib \
jsta.bst
	Rscript -e "rmarkdown::render('manuscript.Rmd')"
	pdftk manuscript.pdf appendix.pdf cat output manuscript2.pdf
	mv manuscript2.pdf manuscript.pdf

agujournaltemplate.pdf: agujournaltemplate.tex lagosdepth.bib
	pdflatex agujournaltemplate
	bibtex agujournaltemplate
	pdflatex agujournaltemplate

si_template_2019.pdf: si_template_2019.tex
	pdflatex si_template_2019
	bibtex si_template_2019
	pdflatex si_template_2019

wrr_submission.pdf: agujournaltemplate.pdf si_template_2019.pdf
	pdftk agujournaltemplate.pdf si_template_2019.pdf cat output wrr_submission.pdf

clean:
	-@rm *.nav *.aux *.snm *.toc *.out *.log *.cpc 2>/dev/null || true
