.PHONY: check byte-compile clean

# Run sanity checks
check:
	emacs -Q --batch -l check.el

# Byte-compile all elisp files
byte-compile:
	emacs -Q --batch \
		-l init.el \
		-f batch-byte-compile init.el lisp/*.el

# Clean compiled files
clean:
	rm -f *.elc lisp/*.elc
