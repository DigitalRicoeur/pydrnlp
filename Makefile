.PHONY: all
PKGNAME = pydrnlp
all:
	echo "No default rule."echo "Please specify a target," \
	"e.g. \"install\", \"update\", or \"setup\"."
	exit 1


.PHONY: install
REPO = https://bitbucket.org/digitalricoeur/pydrnlp.git
THIS_DIR = $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
INSTALL = raco pkg install -i --auto --name $(PKGNAME) \
	--clone $(THIS_DIR) $(REPO)
install:
	$(INSTALL)


.PHONY: cloc
cloc:
	cloc --exclude-dir=condaenv .

########################################

.PHONY: update
update:
	raco pkg update $(PKGNAME)


.PHONY: setup
setup:
	raco setup --doc-index --pkgs $(PKGNAME)


.PHONY: fast
fast:
	raco setup --no-docs --no-pkg-deps --pkgs $(PKGNAME)


