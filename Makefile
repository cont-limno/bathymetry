all: datasets

figures:
	cd figures && make pnglatest

datasets: data/lagosus_depth.csv \
data/00_hypso/hypso.csv \
data/00_hypso/hypso_classes.csv \
data/lagosus_depth_predictors.csv \
data/lagosne_depth_predictors.csv \
data/00_hypso/hypso_predictors.csv

data/00_hypso/hypso.csv: scripts/01_hypso_merge.R \
data/ct_hypso.csv \
data/mn_hypso.csv \
data/mi_hypso.csv \
data/nh_hypso.csv \
data/ks_hypso.csv \
data/ne_hypso.csv \
data/ma_hypso.csv
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

data/lagosus_depth_predictors.csv: scripts/04_depth_predict.R \
data/lagosus_depth.csv
	Rscript $<

data/lagosne_depth_predictors.csv: scripts/04_depth_predict.R \
data/lagosus_depth.csv
	Rscript $<

data/00_hypso/hypso_predictors.csv: scripts/04_hypso_predict.R \
data/lagosus_depth_predictors.csv
	Rscript $<

data/lagosus_depth.csv: scripts/01_merge.R \
data/00_manual/00_manual.csv \
data/00_manual_extra/00_manual_extra.csv \
data/00_nla/00_nla.csv \
data/00_lagosne/00_lagosne.csv
	Rscript $<

data/00_manual/00_manual.csv: scripts/00_get_manual.R
	Rscript $<

data/00_manual_extra/00_manual_extra.csv: scripts/00_get_manual_extra.R
	Rscript $<

data/00_nla/00_nla.csv: scripts/00_get_nla.R
	Rscript $<

data/00_lagosne/00_lagosne.csv: scripts/00_get_lagosne.R
	Rscript $<

# TODO add line for deepest bathymetry point product

lagos_depth.pdf: lagos_depth.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'pdf_document')"
