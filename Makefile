SUBMODULE_MAKEFILES = $(wildcard site-lisp/*/Makefile)

all:
	# in shell: for x in $(SUBMODULE_MAKEFILES); do make -C $(dirname x); done

bootstrap:
	git remote add cjohansen git://github.com/cjohansen/.emacs.d.git || true
	git remote add magnars git://github.com/magnars/.emacs.d.git || true
	git fetch --all

.PHONY: all bootstrap
