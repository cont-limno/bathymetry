.PHONY: png grayscale small

png:
	-mogrify -density 300 -format png -trim -resize 2500x2500\> *.pdf
	-rm *~
	echo done
	
pnglatest:
	mogrifylatest
	
tiff:
	-mogrify -density 300 -format tiff -resize 2500x2500\> *.pdf
	-rm *~
	echo done
	
grayscale:
	cp *.pdf grayscale
	cd grayscale && make all
	