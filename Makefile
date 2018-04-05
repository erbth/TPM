OF := ocamlfind
OO := ocamlopt
PKGS :=unix,str,xml-light

PREFIX ?= /usr/local
DESTDIR ?= /

TPM_VERSION := 1.0.1

OBJDIR := bin

OBJS := tpm_config.cmx \
		terminal.cmx \
		util.cmx \
		pkg.cmx \
		repository.cmx \
		configuration.cmx \
		unpacked_package.cmx \
		packed_package.cmx \
		status.cmx \
		installed_package.cmx \
		repository_search.cmx \
		installation.cmx \
		tpm.cmx

.PHONY: all
all: $(OBJDIR)/tpm

# Dependencies between modules
$(OBJDIR)/configuration.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx repository.cmx)
$(OBJDIR)/installation.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx pkg.cmx repository.cmx \
	repository_search.cmx status.cmx packed_package.cmx installed_package.cmx configuration.cmx)
$(OBJDIR)/installed_package.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx pkg.cmx status.cmx)
$(OBJDIR)/packed_package.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx pkg.cmx repository.cmx)
$(OBJDIR)/unpacked_package.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx pkg.cmx)
$(OBJDIR)/pkg.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx)
$(OBJDIR)/repository_search.cmx: $(patsubst %,$(OBJDIR)/%,pkg.cmx util.cmx configuration.cmx \
	repository.cmx packed_package.cmx)
$(OBJDIR)/repository.cmx: $(patsubst %,$(OBJDIR)/%,pkg.cmx util.cmx)
$(OBJDIR)/status.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx pkg.cmx)
$(OBJDIR)/tpm.cmx: $(patsubst %,$(OBJDIR)/%,util.cmx pkg.cmx unpacked_package.cmx \
	packed_package.cmx installed_package.cmx repository_search.cmx \
	configuration.cmx installation.cmx status.cmx)
$(OBJDIR)/util.cmx: $(patsubst %,$(OBJDIR)/%,tpm_config.cmx terminal.cmx)

$(OBJDIR)/tpm: $(OBJS:%=$(OBJDIR)/%) | $(OBJDIR)
	cd $(dir $@) && \
	$(OF) $(OO) -o $(notdir $@) -linkpkg -package $(PKGS) $(^:$(OBJDIR)/%=%)

$(OBJDIR)/%.cmx: %.ml $(wildcard %.mli) | $(OBJDIR)
	ln -sf ../$< $(dir $@) && \
	cd $(dir $@) && \
	$(OF) $(OO) -c -package $(PKGS) $<

.PHONY: install uninstall
install: $(OBJDIR)/tpm
	install -dm755 $(DESTDIR)/$(PREFIX)/bin
	install -m755 $< $(DESTDIR)/$(PREFIX)/bin

uninstall:
	rm $(DESTDIR)/$(PREFIX)/bin/tpm

.PHONY: dist
dist:
	mkdir tpm-$(TPM_VERSION) && \
	cp -a *.ml *.mli Makefile .merlin doc tpm-$(TPM_VERSION) && \
	tar -cJf tpm-$(TPM_VERSION).tar.xz tpm-$(TPM_VERSION) && \
	rm -rf tpm-$(TPM_VERSION)

$(OBJDIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(OBJDIR)