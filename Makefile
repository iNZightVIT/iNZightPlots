R := R
RCMD := $(R) --slave

document:
	@$(RCMD) -e "devtools::document()"

check:
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "devtools::use_revdep()"
	@$(RCMD) -f "revdep/check.R"

crancheck:
	@$(RCMD) CMD build .
	@$(RCMD) CMD check *.tar.gz

install:
	$(R) CMD INSTALL ./

clean:
	@rm -rf *.tar.gz *.Rcheck revdep
