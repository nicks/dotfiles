.PHONY: test

test:
	emacs --batch -l ~/.emacs -l $(PWD)/emacs-test.el -f ert-run-tests-batch-and-exit
