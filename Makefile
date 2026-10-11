EMACS ?= emacs
.PHONY: test compile clean
test:
	$(EMACS) --batch -Q -L . -l tests/unii-memory-test.el -f ert-run-tests-batch-and-exit
compile:
	$(EMACS) --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile unii-memory.el unii-memory-store.el unii-memory-context.el unii-memory-usage.el
clean:
	rm -f *.elc
