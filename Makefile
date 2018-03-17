.PHONY: tpm.native
tpm.native:
	ocamlbuild -pkg xml-light -pkg unix $@