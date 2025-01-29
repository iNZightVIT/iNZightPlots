R := R
RCMD := $(R) --slave

data := data/plot_types.rda

document: $(data)
	@$(RCMD) -e "devtools::document()"

check:
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "if (!requireNamespace('revdepcheck')) install.packages('revdepcheck')"
	@$(RCMD) -e "revdepcheck::revdep_check(quiet = FALSE)"
	@$(RCMD) -f "revdep/check.R"

revcheck_reset:
	@$(RCMD) -e "revdepcheck::revdep_reset()"

crancheck:
	@$(RCMD) CMD build .
	@$(RCMD) CMD check *.tar.gz

install: $(data)
	$(R) CMD INSTALL ./

clean:
	@rm -rf *.tar.gz *.Rcheck revdep

test: $(data)
	@$(RCMD) -e "devtools::test()"

data/plot_types.rda: data-raw/plot_types.csv
	@$(RCMD) -f data-raw/plot_types.R

BRANCH := $(shell git branch --show-current | sed 's/[a-z]*\///')
releasePRs:
	@echo Creating PR to master
	@gh pr create -a "@me" -b "" -B master -l "release" -t "Release $(BRANCH)"
	@echo Creating PR to dev
	@gh pr create -a "@me" -b "" -B dev -l "release" -t "Release $(BRANCH) into dev"

README.md: README.Rmd
	@$(RCMD) -e 'rmarkdown::render("$<")'
	@rm README.html

site: README.md document install
	@$(RCMD) -e 'pkgdown::build_site()'
