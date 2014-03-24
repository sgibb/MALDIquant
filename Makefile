## package name
NAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
VERSION := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PACKAGE := $(NAME)_$(VERSION).tar.gz
CHECKDIR := $(NAME).Rcheck
LOCALDIR := $(NAME).local
TESTDIR := $(NAME)/tests

## r binaries
R_BIN=R

## package dir
R_PACKAGE_DIR=$(HOME)/R
APE_R_PACKAGE_DIR=/usr/local/lib/R/site-library/

## package version

.PHONY: clean cran check build install remove ape_install ape_remove local_install local_remove test

## targets:
all: build

clean: local_remove clean_vignette
	cd .. ;\
	$(RM) Rplots.pdf ;\
	$(RM) $(NAME)/src/*.rds ;\
	$(RM) $(NAME)/src/*.o ;\
	$(RM) $(NAME)/src/*.so ;\
	$(RM) $(NAME)_*.tar.gz ;\
	$(RM) -r $(CHECKDIR)

cran: clean test testdemo check

check: build
	cd .. ;\
	$(R_BIN) CMD check $(PACKAGE) --as-cran

build:
	cd .. ;\
	$(R_BIN) CMD build $(NAME)

install: build
	cd .. ;\
	$(R_BIN) CMD INSTALL $(PACKAGE)

remove: clean
	$(R_BIN) CMD REMOVE -l $(R_PACKAGE_DIR) $(NAME)

ape_install: build
	cd .. ;\
	sudo $(R_BIN) CMD INSTALL -l $(APE_R_PACKAGE_DIR) $(PACKAGE)

ape_remove: clean
	sudo $(R_BIN) CMD REMOVE -l $(APE_R_PACKAGE_DIR) $(NAME)

local_install: local_remove
	cd .. ;\
	mkdir $(LOCALDIR) ;\
	$(R_BIN) CMD INSTALL --library=$(LOCALDIR) $(NAME)

local_remove:
	cd .. ;\
	$(RM) -r $(LOCALDIR)

test: local_install
	cd .. ;\
	$(R_BIN) -q -e "library(\"$(NAME)\", lib.loc=\"$(LOCALDIR)\")" \
		   		-e "library(\"testthat\")" \
				-e "test_dir(\"$(TESTDIR)\")"

testdemo: local_install
	cd .. ;\
	$(R_BIN) -q -e "library(\"$(NAME)\", lib.loc=\"$(LOCALDIR)\")" \
		   		-e "demo(\"MALDIquant\")"

win-builder: check
	cd .. ;\
	ncftpput -u anonymous -p '' win-builder.r-project.org R-devel $(PACKAGE)

vignette:
	cd vignettes ;\
	$(R_BIN) --vanilla -e "library(knitr); knit2pdf(\"MALDIquant.Rnw\");"

clean_vignette:
	cd vignettes ;\
	$(RM) *.aux *.bbl *.blg *.log *.out *.pdf *.tex *.toc ;\
	$(RM) -r figure

