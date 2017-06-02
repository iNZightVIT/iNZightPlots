R := R --vanilla --slave

default:
	@$(R) -f templates/importTemplates.R

document:
	@$(R) -e "devtools::document()"

check:
	@$(R) -e "devtools::check()"

install:
	R CMD INSTALL ./
