.PHONY: tpm.native
tpm.native:
	ocamlbuild -pkg xml-light -pkg unix -pkg str $@

.PHONY: install uninstall
install:
	install -m755 tpm.native $(ROOT)/usr/bin/tpm

uninstall:
	rm $(ROOT)/usr/bin/tpm

.PHONY: clean
clean:
	rm -rf _build tpm.native