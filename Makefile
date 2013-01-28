## package name
CUR_DIR := $(shell /bin/pwd)
NAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
CHECKDIR := $(NAME).Rcheck
LOCALDIR := .local
TESTDIR := $(NAME)/tests

## r binaries
R_BIN=R

## package dir
R_PACKAGE_DIR=$(HOME)/R
APE_R_PACKAGE_DIR=/opt/R-packages/cran

## package version
PACKAGE_VERSION := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

.PHONY: clean cran check check_archive build install remove ape_install ape_remove local_install local_remove autotest test

## targets:
all: build

clean: local_remove
	cd .. ;\
	$(RM) Rplots.pdf ;\
	$(RM) $(NAME)/src/*.rds ;\
	$(RM) $(NAME)/src/*.o ;\
	$(RM) $(NAME)/src/*.so ;\
	$(RM) $(NAME)_*.tar.gz ;\
	$(RM) -r $(CHECKDIR)

cran: clean test testdemo check_archive

check: clean
	cd .. ;\
	$(R_BIN) CMD check $(NAME)

check_archive: build
	cd .. ;\
	$(R_BIN) CMD check $(NAME)_$(PACKAGE_VERSION).tar.gz --as-cran

build:
	cd .. ;\
	$(R_BIN) CMD build $(NAME)

install: build
	cd .. ;\
	$(R_BIN) CMD INSTALL $(NAME)_$(PACKAGE_VERSION).tar.gz

remove: clean
	$(R_BIN) CMD REMOVE -l $(R_PACKAGE_DIR) $(NAME)

ape_install: build
	cd .. ;\
	sudo $(R_BIN) CMD INSTALL -l $(APE_R_PACKAGE_DIR) $(NAME)_$(PACKAGE_VERSION).tar.gz

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

