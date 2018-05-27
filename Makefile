PKGS := unix str xml-light

PREFIX ?= /usr/local
DESTDIR ?= /

TPM_VERSION := 1.0.7

.NOTPARALLEL:

.PHONY: all
all: tpm.native tpmdb.native

tpm.native: FORCE
	ocamlbuild $(PKGS:%=-pkg %) tpm.native

tpmdb.native: FORCE
	ocamlbuild $(PKGS:%=-pkg %) tpmdb.native

.PHONY: install uninstall
install: tpm.native tpmdb.native README
	install -dm755 $(DESTDIR)/$(PREFIX)/bin
	install -m755 tpm.native $(DESTDIR)/$(PREFIX)/bin/tpm
	install -m755 tpmdb.native $(DESTDIR)/$(PREFIX)/bin/tpmdb
	install -dm755 $(DESTDIR)/$(PREFIX)/share/doc/tpm
	install -m644 README $(DESTDIR)/$(PREFIX)/share/doc/tpm/

.PHONY: dist
dist:
	mkdir tpm-$(TPM_VERSION) && \
	cp -a *.ml *.mli Makefile .merlin doc README tpm-$(TPM_VERSION) && \
	tar -cJf tpm-$(TPM_VERSION).tar.xz tpm-$(TPM_VERSION) && \
	rm -rf tpm-$(TPM_VERSION)

.PHONY: clean
clean:
	rm -rf _build tpm.native tpmdb.native

FORCE:
