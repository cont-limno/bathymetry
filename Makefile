all: datasets \
manuscript/figures.pdf \
manuscript/appendix.pdf \
manuscript/combined.pdf \
README.md

datasets: data/lagosus_depth.csv \
data/00_hypso/hypso.csv \
data/00_hypso/hypso_classes.csv \
data/lagosus_depth_predictors.csv \
data/lagosne_depth_predictors.csv \
data/00_hypso/hypso_predictors.csv \
data/00_bathy_depth/bathy_geometry.csv \
data/00_bathy_depth/bathy_pnts.rds

# data/gis.gpkg

data/00_hypso/hypso.csv: scripts/01_hypso_merge.R \
data/ct_hypso.csv \
data/mn_hypso.csv \
data/mi_hypso.csv \
data/nh_hypso.csv \
data/ks_hypso.csv \
data/ne_hypso.csv \
data/ma_hypso.csv \
data/ia_hypso.csv \
data/me_hypso.csv
	Rscript $<

data/00_hypso/hypso_classes.csv: scripts/03_hypso_classifier.R \
data/00_hypso/hypso.csv
	Rscript $<

data/ct_hypso.csv: scripts/00_get_hypso_ct.R
	Rscript $<

data/mn_hypso.csv: scripts/00_get_hypso_mn.R
	Rscript $<

data/mi_hypso.csv: scripts/00_get_hypso_mi.R
	Rscript $<

data/nh_hypso.csv: scripts/00_get_hypso_nh.R
	Rscript $<

data/ks_hypso.csv: scripts/00_get_hypso_ks.R
	Rscript $<

data/ne_hypso.csv: scripts/00_get_hypso_ne.R
	Rscript $<

data/ma_hypso.csv: scripts/00_get_hypso_ma.R
	Rscript $<

data/ia_hypso.csv: scripts/00_get_hypso_ia.R
	Rscript $<

data/me_hypso.csv: scripts/00_get_hypso_me.R
	Rscript $<

data/00_reservoir_classification/reservoir_classes_clean.csv: scripts/00_get_reservoir_class.R
	Rscript $<

data/lagosus_depth_predictors.csv: scripts/04_depth_predictors.R \
data/lagosus_depth.csv \
data/00_hypso/hypso_classes.csv \
data/00_reservoir_classification/reservoir_classes_clean.csv
	Rscript $<

data/lagosne_depth_predictors.csv: scripts/04_depth_predictors.R \
data/lagosus_depth.csv \
data/00_hypso/hypso_classes.csv \
data/00_reservoir_classification/reservoir_classes_clean.csv
	Rscript $<

data/00_hypso/hypso_predictors.csv: scripts/04_hypso_predictors.R \
data/lagosus_depth_predictors.csv
	Rscript $<

data/gis.gpkg: scripts/00_get_gis.R
	Rscript $<
# data/lagosus_depth.csv

archive:
	cd data && make archive

data/lagosus_depth.csv: scripts/00_get_lagosus.R \
../lagos_depth/data/lagosus_depth.csv
	Rscript $<

data/00_lagosne/00_lagosne.csv: scripts/00_get_lagosne.R \
data/00_lagosne/00_lagosne_xwalk.csv
	Rscript $<

data/00_lagosne/00_lagosne_xwalk.csv: scripts/00_get_lagosne_xwalk.R
	Rscript $<

data/00_bathy_depth/bathy_pnts.rds: scripts/00_get_geometry.R \
data/00_hypso/hypso.csv
	Rscript $<

data/00_bathy_depth/00_bathy_depth.csv: scripts/00_get_geometry.R \
data/00_hypso/hypso.csv
	Rscript $<

data/00_bathy_depth/bathy_geometry.csv: scripts/00_get_geometry.R \
data/00_hypso/hypso.csv
	Rscript $<

data/00_geometry/nearshore.csv: scripts/00_get_nearshore.R \
data/00_bathy_depth/bathy_pnts.rds \
data/lagosne_depth_predictors.csv
	Rscript $<
# data/lagosus_depth.csv \

manuscript/combined.pdf: manuscript/figures.pdf manuscript/appendix.pdf
	pdftk manuscript/figures.pdf manuscript/appendix.pdf output manuscript/combined.pdf

manuscript/appendix.pdf: manuscript/appendix.Rmd \
figures/01_hypsography-1.pdf
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	-pdftk manuscript/appendix.pdf cat 2-end output manuscript/appendix2.pdf
	-mv manuscript/appendix2.pdf manuscript/appendix.pdf

manuscript/figures.pdf: manuscript/figures.Rmd \
manuscript/tables.pdf \
figures/00_map_bathy-1.pdf \
figures/01_geometry_grid-1.pdf \
figures/lgnemanual-vs-bathy-depth-1.pdf \
tables/02_model_metrics.pdf \
figures/01_hypsography-1.pdf \
figures/02_depth_model_grid_resid-1.pdf \
figures/02_depth_model_importance-1.pdf \
figures/01_contrasts_depth-1.pdf \
figures/01_contrasts_tally-1.pdf \
figures/slope_diagram.pdf \
figures/lake_shape.pdf \
figures/gg_distance-1.pdf
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	-pdftk manuscript/figures.pdf cat 2-end output manuscript/figures2.pdf
	-mv manuscript/figures2.pdf manuscript/figures.pdf
#	cd figures && make pnglatest
# figures/01_heatmap-1.pdf \

# data/gis.gpkg
figures/00_map-1.pdf: figures/00_maps.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/00_map_bathy-1.pdf: figures/00_maps.Rmd data/gis.gpkg
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/00_cutoffs-1.pdf: figures/00_cutoffs.Rmd data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/01_heatmap-1.pdf: figures/01_heatmap.Rmd data/lagosne_depth_predictors.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/01_hypsography-1.pdf: figures/01_hypsography.Rmd data/00_hypso/hypso.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/01_contrasts_depth-1.pdf: figures/01_contrasts.Rmd \
data/00_hypso/hypso_predictors.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/01_contrasts_tally-1.pdf: figures/01_contrasts.Rmd \
data/00_hypso/hypso_predictors.csv \
data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/gg_distance-1.pdf: figures/01_geometry.Rmd \
data/00_bathy_depth/bathy_pnts.rds
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

tables/02_model_metrics.pdf: tables/02_model_metrics.Rmd \
data/01_depth_model/depth_grid_metrics.rds
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/02_depth_model_fitted-1.pdf: figures/02_depth_model.Rmd \
data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/02_depth_model_importance-1.pdf: figures/02_depth_model.Rmd \
data/01_depth_model/depth_fits.rds \
data/taxonomy.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/02_depth_model_grid_resid-1.pdf: figures/02_depth_model.Rmd \
data/01_depth_model/depth_grid_metrics.rds \
data/01_depth_model/depth_grid.rds \
data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/02_hypso_model_fitted-1.pdf: figures/02_hypso_model.Rmd \
data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/00_qa-1.pdf: figures/00_qa.Rmd \
data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/lgnemanual-vs-bathy-depth-1.pdf: figures/00_qa.Rmd \
data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

figures/01_geometry_grid-1.pdf: figures/01_geometry.Rmd \
data/00_geometry/nearshore.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

manuscript/tables.pdf: tables/00_data.pdf \
tables/01_predictors.pdf
	pdftk $^ cat output manuscript/tables.pdf

tables/01_predictors.pdf: tables/01_predictors.Rmd \
data/00_geometry/nearshore.csv \
data/lagosus_depth_predictors.csv \
data/taxonomy.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

tables/00_data.pdf: tables/00_data.Rmd data/lagosus_depth.csv
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
	pdfcrop $@ $@

data/taxonomy.csv: scripts/99_make_taxonomy.R
	Rscript $<

README.md: README.Rmd \
data/taxonomy.csv
	Rscript -e "rmarkdown::render('$<')"

lagos_depth.pdf: lagos_depth.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"

