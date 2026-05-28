EMACS ?= emacs
AGENT_SHELL_DIR ?= ../agent-shell
EMACS_BATCH = $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)" --eval "(package-initialize)"

.PHONY: test compile

test:
	$(EMACS_BATCH) -L . -L $(AGENT_SHELL_DIR) -l tests/agent-shell-desktop-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS_BATCH) -L . -L $(AGENT_SHELL_DIR) -f batch-byte-compile agent-shell-desktop.el
