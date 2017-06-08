RDEVEL := $(shell command -v R-devel 2> /dev/null)
R := R
RCMD := $(R) --vanilla --slave
ifndef RDEVEL
	Rdev := $(R)
else
	Rdev := R-devel
endif

default:
	@$(RCMD) -f templates/importTemplates.R

document:
	@$(RCMD) -e "devtools::document()"

check:
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "devtools::use_revdep()"
	@$(RCMD) -f "revdep/check.R"

crancheck:
	@$(Rdev) CMD build .
	@$(Rdev) CMD check *.tar.gz

install:
	$(R) CMD INSTALL ./

clean:
	@rm -rf *.tar.gz *.Rcheck revdep
