R := R
RCMD := $(R) --slave

data := data/plot_types.rda

document: $(data)
	@$(RCMD) -e "devtools::document()"

check:
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "devtools::use_revdep()"
	@$(RCMD) -f "revdep/check.R"

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
