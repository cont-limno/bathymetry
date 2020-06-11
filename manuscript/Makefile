manuscript.pdf: manuscript.Rmd \
pinp.cls \
lagosdepth.bib \
jsta.bst
	Rscript -e "rmarkdown::render('manuscript.Rmd')"
	pdftk manuscript.pdf appendix.pdf cat output manuscript2.pdf
	mv manuscript2.pdf manuscript.pdf

wrr_submission.pdf: agujournaltemplate.pdf si_template_2019.pdf
	pdftk agujournaltemplate.pdf si_template_2019.pdf cat output wrr_submission.pdf
