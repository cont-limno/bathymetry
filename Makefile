
all: data

data: data/00_manual/00_manual.csv \
data/00_nla/00_nla.csv

data/00_manual/00_manual.csv: scripts/00_get_manual.R
	Rscript $<

data/00_nla/00_nla.csv: scripts/00_get_nla.R
	Rscript $<
