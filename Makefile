PKGS := unix str xml-light

PREFIX ?= /usr/local
DESTDIR ?= /

TPM_VERSION := 1.0.1

tpm.native: FORCE
	ocamlbuild $(PKGS:%=-pkg %) tpm.native

.PHONY: install uninstall
install: tpm.native
	install -dm755 $(DESTDIR)/$(PREFIX)/bin
	install -m755 $< $(DESTDIR)/$(PREFIX)/bin/tpm

uninstall:
	rm $(DESTDIR)/$(PREFIX)/bin/tpm

.PHONY: dist
dist:
	mkdir tpm-$(TPM_VERSION) && \
	cp -a *.ml *.mli Makefile .merlin doc tpm-$(TPM_VERSION) && \
	tar -cJf tpm-$(TPM_VERSION).tar.xz tpm-$(TPM_VERSION) && \
	rm -rf tpm-$(TPM_VERSION)

.PHONY: clean
clean:
	rm -rf _build tpm.native

FORCE: