all: manuscript.pdf appendix.pdf combined.pdf figures.pdf

sync_figures:
	cp ../figures/slope_diagram_new.pdf slope_diagram_new.pdf
	cp ../figures/00_map_bathy-1.pdf 00_map_bathy-1.pdf
	cp ../figures/01_geometry_base-1.pdf 01_geometry_base-1.pdf
	cp ../figures/02_depth_model_grid_resid-1.pdf 02_depth_model_grid_resid-1.pdf
	cp ../figures/02_depth_model_importance-1.pdf 02_depth_model_importance-1.pdf
	cp ../figures/slope_diagram_new.pdf inland_waters/Figure_1.pdf
	cp ../figures/00_map_bathy-1.pdf inland_waters/Figure_2.pdf
	cp ../figures/01_geometry_base-1.pdf inland_waters/Figure_3.pdf
	cp ../figures/02_depth_model_grid_resid-1.pdf inland_waters/Figure_4.pdf
	cp ../figures/02_depth_model_importance-1.pdf inland_waters/Figure_5.pdf

jpeg_figures: to_jpeg.R
	Rscript $<

reviewer_comments.pdf: reviewer_comments.md
	pandoc $< -H quote_setup.tex -o $@

manuscript.pdf: manuscript.Rmd appendix.pdf lagosdepth.bib
	Rscript -e "rmarkdown::render('manuscript.Rmd')"
	pdftk manuscript.pdf appendix.pdf cat output manuscript2.pdf
	mv manuscript2.pdf manuscript.pdf

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

clean:
	-@rm manuscript.pdf
	-@rm *.nav *.aux *.snm *.toc *.out *.log *.cpc 2>/dev/null || true
