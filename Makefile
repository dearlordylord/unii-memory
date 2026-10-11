EMACS ?= emacs
.PHONY: test compile clean package
test:
	$(EMACS) --batch -Q -L . -l tests/unii-memory-test.el -f ert-run-tests-batch-and-exit
compile:
	$(EMACS) --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile unii-memory.el unii-memory-store.el unii-memory-context.el unii-memory-usage.el
clean:
	rm -f *.elc
package:
	mkdir -p dist
	tar --format=ustar --transform='s,^,unii-memory-0.1.0/,' -cf dist/unii-memory-0.1.0.tar unii-memory.el unii-memory-store.el unii-memory-context.el unii-memory-usage.el unii-memory-pkg.el API.org README.org LICENSE THIRD_PARTY_NOTICES.md
