########################################
# hardcaml - hardware design in OCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all install uninstall 
default: all

ATDSRC = src/yosys_atd_j.mli src/yosys_atd_j.ml src/yosys_atd_t.mli src/yosys_atd_t.ml
	
$(ATDSRC): src/yosys_atd.atd
	cd src && atdgen -t yosys_atd.atd
	cd src && atdgen -j -j-std -j-strict-fields yosys_atd.atd

all: $(ATDSRC) setup.data
	ocaml setup.ml -build

setup.ml:
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

####################################################

install: all
	ocaml setup.ml -install

uninstall: 
	ocamlfind remove hardcaml-yosys

clean:
	ocaml setup.ml -clean
	rm -f $(ATDSRC)
	- find . -name "*~" | xargs rm

distclean:
	ocaml setup.ml -distclean

####################################################
# sat checking simlib cells

sat_gen: all 
	utop test/sat_cells.ml > test/sat_cells.sh

sat_check: 
	sh ./test/sat_cells.sh


