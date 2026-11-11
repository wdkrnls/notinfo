# notinfo -- Makefile
#
# Development (needs guile and guile-xapian, as from `guix shell`):
#   make check             compile-check both scripts
#   make index             incremental index into ./notinfo.db
#   make rebuild           discard and rebuild ./notinfo.db
#   make search Q='...'    query ./notinfo.db
#   make render P=tar      print a manual as info produces it
#   make view P='(tar)Changes'
#
# Installation (defaults to PREFIX=/usr/local, so usually `sudo make install`;
# `make install PREFIX=$HOME/.local` installs into your home instead):
#   make install
#   make uninstall
#
# Scripts go in $(LIBDIR) ($(PREFIX)/lib/notinfo).  No data is installed: the
# user builds the database later, at
# ${XDG_DATA_HOME:-$HOME/.local/share}/notinfo/notinfo.db, so nothing under
# PREFIX is created or removed by the index, and uninstall cannot delete it.
#
# DESTDIR is honoured for staged installs.

GUILE  ?= guile
GUIX   ?= guix shell guile guile-xapian --
DB     ?= notinfo.db
Q      ?=
P      ?=
NOTINFO_LANG ?=
NOTINFO_MANUAL ?=

PREFIX  ?= /usr/local
BINDIR  ?= $(PREFIX)/bin
LIBDIR  ?= $(PREFIX)/lib/notinfo
DOCDIR  ?= $(PREFIX)/share/doc/notinfo

SRC      := info-index.scm info-search.scm
SIDECARS := $(DB) $(DB).manifest $(DB).stem

.PHONY: all check index rebuild search render view install uninstall clean

all: check

check:
	$(GUIX) guild compile $(SRC)

index:
	$(GUIX) $(GUILE) info-index.scm $(DB) \
	  $(if $(NOTINFO_LANG),--lang=$(NOTINFO_LANG),) \
	  $(if $(NOTINFO_MANUAL),--manual=$(NOTINFO_MANUAL),)

rebuild:
	rm -rf $(SIDECARS)
	$(GUIX) $(GUILE) info-index.scm $(DB) \
	  $(if $(NOTINFO_LANG),--lang=$(NOTINFO_LANG),) \
	  $(if $(NOTINFO_MANUAL),--manual=$(NOTINFO_MANUAL),)

# make search Q='manual:tar AND node:changes AND body:globbing --point'
search:
	$(GUIX) $(GUILE) info-search.scm $(DB) $(Q)

# make render P=tar
render:
	$(GUIX) $(GUILE) info-index.scm --render $(P)

# make view P='(tar)Changes'
view:
	$(GUIX) $(GUILE) info-search.scm $(DB) --view '$(P)'

# The dispatcher goes in bin/ and finds the .scm files through
# ../lib/notinfo, so no paths are baked in and this works from any prefix.
install:
	install -d "$(DESTDIR)$(BINDIR)" "$(DESTDIR)$(LIBDIR)" "$(DESTDIR)$(DOCDIR)"
	install -m 755 notinfo "$(DESTDIR)$(BINDIR)/notinfo"
	install -m 644 $(SRC) "$(DESTDIR)$(LIBDIR)/"
	install -m 644 README.md "$(DESTDIR)$(DOCDIR)/README.md"

uninstall:
	rm -f "$(DESTDIR)$(BINDIR)/notinfo"
	rm -rf "$(DESTDIR)$(LIBDIR)" "$(DESTDIR)$(DOCDIR)"

clean:
	rm -rf $(SIDECARS) *.go
