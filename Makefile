
all: data

figures:
	cd figures && make pnglatest

data:	data/lagosus_depth.csv \
data/mn_hypso.csv \
data/ct_hypso.csv

data/mn_hypso.csv: scripts/00_get_hypso_mn.R \
data/lagosus_depth.csv
	Rscript $<

data/ct_hypso.csv: scripts/00_get_hypso_ct.R \
data/lagosus_depth.csv
	Rscript $<

data/lagosus_depth.csv: scripts/01_merge.R \
data/00_manual/00_manual.csv \
data/00_nla/00_nla.csv \
data/00_lagosne/00_lagosne.csv
	Rscript $<

data/00_manual/00_manual.csv: scripts/00_get_manual.R
	Rscript $<

data/00_nla/00_nla.csv: scripts/00_get_nla.R
	Rscript $<

data/00_lagosne/00_lagosne.csv: scripts/00_get_lagosne.R
	Rscript $<
