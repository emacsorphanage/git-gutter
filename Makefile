SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := $(shell ls test/git-gutter-*.el)

.PHONY: clean checkdoc lint install compile unix-test

ci: clean install compile

clean:
	@echo "Cleaning..."
	$(EASK) clean-all

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

lint:
	@echo "Linting..."
	$(EASK) lint

unix-test:
	@echo "Testing..."
	$(EASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'
