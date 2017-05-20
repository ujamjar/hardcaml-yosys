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

####################################################

all: build

build:
	jbuilder build @install

test:
	jbuilder build @test/tests

clean:
	rm -fr _build
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

