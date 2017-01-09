########################################
# hardcaml - hardware design in OCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all build clean sat_gen sat_check test
default: all

ATDSRC = src/yosys_atd_j.mli src/yosys_atd_j.ml src/yosys_atd_t.mli src/yosys_atd_t.ml
	
$(ATDSRC): src/yosys_atd.atd
	cd src && atdgen -t yosys_atd.atd
	cd src && atdgen -j -j-std -j-strict-fields yosys_atd.atd


####################################################

all: build

build:
	cp pkg/META.in pkg/META
	ocaml pkg/pkg.ml build

test:
	ocamlbuild test.otarget

clean:
	ocaml pkg/pkg.ml clean
	find . -name "*~" | xargs rm -f
	rm -f *.bc *.ll

####################################################
# sat checking simlib cells

sat_gen: all 
	utop test/sat_cells.ml > test/sat_cells.sh

sat_check: 
	sh ./test/sat_cells.sh


####################################################
# publishing

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

tag:
	git tag -a "v$(VERSION)" -m "v$(VERSION)."
	git push origin v$(VERSION)

prepare:
	opam publish prepare -r hardcaml $(NAME_VERSION) $(ARCHIVE)

publish:
	opam publish submit -r hardcaml $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

