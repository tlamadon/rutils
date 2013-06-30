all: doc install

install:
	cd ..
	R CMD INSTALL  ./ --clean

doc:
	Rscript -e "require(devtools);require(digest); document('./');"	

test:
	Rscript -e "library('testthat'); require(devtools); test('./');"
