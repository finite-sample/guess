.PHONY: help check test coverage doc build install clean

help:
	@echo "Available commands:"
	@echo "  make check    - Run R CMD check"
	@echo "  make test     - Run tests"
	@echo "  make coverage - Check code coverage"
	@echo "  make doc      - Generate documentation"
	@echo "  make build    - Build package"
	@echo "  make install  - Install package locally"
	@echo "  make clean    - Clean build artifacts"

check:
	R CMD check .

test:
	Rscript -e "devtools::test()"

coverage:
	@echo "Running code coverage analysis..."
	Rscript -e "cov <- covr::package_coverage(quiet = FALSE); print(cov); cat('Coverage: ', covr::percent_coverage(cov), '%\n')"

doc:
	Rscript -e "devtools::document()"

build:
	R CMD build .

install:
	Rscript -e "devtools::install()"

clean:
	rm -f *.tar.gz
	rm -rf guess.Rcheck
	rm -f .Rhistory