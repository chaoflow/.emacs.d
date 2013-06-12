SUBMODULE_MAKEFILES = $(wildcard site-lisp/*/Makefile)

all: site-lisp/bbdb/Makefile site-lisp/emacs-jabber/Makefile
	for x in $(SUBMODULE_MAKEFILES); do make -C $$(dirname $$x); done

site-lisp/bbdb/Makefile:
	cd site-lisp/bbdb; autoconf; ./configure

site-lisp/emacs-jabber/Makefile:
	cd site-lisp/emacs-jabber; autoreconf; automake --add-missing; autoreconf; ./configure

add-remotes:
	git remote add cjohansen git://github.com/cjohansen/.emacs.d.git || true
	git remote add magnars git://github.com/magnars/.emacs.d.git || true

update-submodules:
	git submodule update --init --recursive

bootstrap: add-remotes update-submodules
	git fetch --all

.PHONY: all add-remotes update-submodules bootstrap
