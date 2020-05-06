archive: bathymetry.zip bathymetry_index.zip depth_raw.zip hypsography.csv

bathymetry.zip: ../scripts/99_generate_archive.zip
	Rscript $<

bathymetry_index.zip: ../scripts/99_generate_archive.zip
	Rscript $<

depth_raw.zip: ../scripts/99_generate_archive.zip
	Rscript $<

hypsography.csv: ../scripts/99_generate_archive.zip
	Rscript $<
