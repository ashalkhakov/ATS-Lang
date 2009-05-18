#########################################################################
#                                                                       #
#                         Applied Type System                           #
#                                                                       #
#                              Hongwei Xi                               #
#                                                                       #
#########################################################################

#
#  ATS/Anairiats - Unleashing the Potential of Types!
#
#  Copyright (C) 2002-2008 Hongwei Xi.
#
#  ATS is  free software;  you can redistribute it and/or modify it under
#  the  terms of the  GNU General Public License as published by the Free
#  Software Foundation; either version 2.1, or (at your option) any later
#  version.
# 
#  ATS is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
#  for more details.
# 
#  You  should  have  received  a  copy of the GNU General Public License
#  along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
#  Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
#  02110-1301, USA.
#

#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
#

######

MAKE=make

######

GCC=gcc

PWD=$(shell pwd)
ifdef ATSHOME
  ATSHOMEDEF=1
else
  ATSHOMEDEF=0
  export ATSHOME=$(PWD)
endif

######

all: \
  atscheck \
  config.h \
  atsopt0 \
  bootstrapping \
  atsopt1 \
  bin/atscc \
  bin/atslib \
  libfiles \
  bin/atspack \
  bin/atslex \
  ccomp/runtime/GCATS/gc.o \
  atsopt1_gc
	echo "ATS/Anairiats has been built up successfully!"
	echo "The value of ATSHOME for this build is \"$(ATSHOME)\"."

###### system configuration ######

atscheck:
	echo "$(ATSHOME)" > .ATSHOME
ifeq ($(ATSHOMEDEF),1)
	/bin/bash -r ./ATSHOME_check.sh
endif
ifdef ATSHOMERELOC
	echo "$(ATSHOMERELOC)" > .ATSHOMERELOC
endif

config.h.in: configure.ac
	autoheader

configure: configure.ac config.h.in
	aclocal
	automake --add-missing --foreign || true
	autoconf

config.h: config.h.in configure
	./configure

###### bootstrap/Makefile ######

bootstrap0/Makefile:
	cp config.h bootstrap0/config.h
	$(GCC) -E -D_BOOTSTRAP0 -x c .bootstrap_header \
      | cat - .bootstrap_makefile > bootstrap0/Makefile

bootstrap1/Makefile:
	$(GCC) -E -D_BOOTSTRAP1 -x c .bootstrap_header \
      | cat - .bootstrap_makefile > bootstrap1/Makefile

###### w/o GC ######
atsopt0: bootstrap0/Makefile; cd bootstrap0; $(MAKE) atsopt

###### bootstrapping ######
bootstrapping: ; cd src; $(MAKE) -f Makefile_bootstrap all

###### w/o GC ######
atsopt1: bootstrap1/Makefile; cd bootstrap1; $(MAKE) atsopt; mv atsopt "$(ATSHOME)"/bin

###### with GC ######
atsopt1_gc: bootstrap1/Makefile; cd bootstrap1; $(MAKE) atsopt_gc; mv atsopt "$(ATSHOME)"/bin

###### some toplevel commands ######
bin/atscc bin/atslib:
	cd utils/scripts; $(MAKE) atscc; mv atscc "$(ATSHOME)"/bin
	cd utils/scripts; $(MAKE) atslib; mv atslib "$(ATSHOME)"/bin
	cd utils/scripts; $(MAKE) clean

bin/atspack:
	cd utils/scripts; $(MAKE) atspack; mv atspack "$(ATSHOME)"/bin

###### library ######

ATS_TERMINATION_CHECK=
# ATS_TERMINATION_CHECK=-D_ATS_TERMINATION_CHECK # it should be turned on from time to time

# [GCC -E] for preprocessing
.libfiles_local: ; $(GCC) -E -P -x c .libfiles -o .libfiles_local
libfiles: .libfiles_local
	"$(ATSHOME)"/bin/atslib $(ATS_TERMINATION_CHECK) -O2 --libats # for libats
	"$(ATSHOME)"/bin/atslib $(ATS_TERMINATION_CHECK) -O2 --libatslex # for libatslex

###### a lexer for ATS ######
bin/atslex:
	cd utils/atslex; $(MAKE) atslex; mv atslex "$(ATSHOME)"/bin
	cd utils/atslex; $(MAKE) clean

###### GC runtime ######

ccomp/runtime/GCATS/gc.o:
	cd ccomp/runtime/GCATS; $(MAKE) gc.o; $(MAKE) clean

######

package::
	bin/atspack --source

precompiled::
	/bin/bash -r ./ATSHOMERELOC_check.sh
	bin/atspack --precompiled
	rm -fr usr/share/atshome
	mv ats-lang-anairiats-* usr/share/atshome
	tar -zvcf ats-lang-anairiats-precompiled.tar.gz \
          --exclude=usr/.svn --exclude=usr/bin/.svn --exclude=usr/share/.svn usr/

######

clean::
	rm -f bootstrap0/*.o
	rm -f bootstrap1/*.o
	rm -f bootstrap1/Makefile
	cd utils/scripts; $(MAKE) clean
	cd utils/atslex; $(MAKE) clean
	cd ccomp/runtime/GCATS; $(MAKE) clean
	cd third_party/bdwgc; $(MAKE) clean || true

cleanall:: clean
	rm -f config.h
	rm -f .libfiles_local
	rm -f bin/atsopt bin/atscc bin/atslib bin/atslex bin/atspack
	rm -f ccomp/lib/libats.a
	rm -f ccomp/lib/libatslex.a
	rm -f ccomp/lib/output/*
	cd ccomp/runtime/GCATS; $(MAKE) cleanall
	cd third_party/bdwgc/libatomic_ops-1.2; $(MAKE) distclean || true
	cd third_party/bdwgc; $(MAKE) distclean || true
	find . -name .svn -prune -o -name \*~ -exec rm \{} \;

######

# The inclusion of [ats_grammar_yats.c] and [ats_grammar_yats.h]
# obviates the need for [yacc] or [byacc]
tar:: cleanall
	rm -rf ATS/*
	cp COPYING ATS
	cp INSTALL ATS
	cp Makefile ATS
	cp Makefile_chmod ATS
	cp .libfiles ATS
	cp configure.ac configure config.h.in ATS
	mkdir ATS/bin
	mkdir ATS/bootstrap
	cp bootstrap/Makefile ATS/bootstrap
	cp bootstrap/*.h ATS/bootstrap
	cp bootstrap/*.cats ATS/bootstrap
	cp bootstrap/*_?ats.c ATS/bootstrap
	mkdir ATS/ccomp
	mkdir ATS/ccomp/runtime
	cp -r ccomp/runtime/*.{c,h} ATS/ccomp/runtime
	mkdir ATS/ccomp/runtime/GCATS
	cp -r ccomp/runtime/GCATS/gc.h ATS/ccomp/runtime/GCATS
	cp -r ccomp/runtime/GCATS/*.?ats ATS/ccomp/runtime/GCATS
	cp -r ccomp/runtime/GCATS/Makefile ATS/ccomp/runtime/GCATS
	cp -r ccomp/runtime/GCATS/README ATS/ccomp/runtime/GCATS
	cp -r ccomp/runtime/GCATS/BUGS.txt ATS/ccomp/runtime/GCATS
	mkdir ATS/ccomp/lib
	mkdir ATS/ccomp/lib/output
	cp -r prelude libc libats ATS
	cp -r utils ATS
	cp -r third_party ATS
	mkdir ATS/src
	cp src/Makefile ATS/src
	cp src/Makefile_bootstrap ATS/src
	cp src/Makefile_typecheck ATS/src
	cp src/*.?ats ATS/src
	cp src/ats_grammar_yats.h ATS/src
	cp src/ats_grammar_yats.c ATS/src
	mkdir ATS/doc
	cp doc/ATS.css doc/ATS.html ATS/doc
	cp doc/FAQ.txt ATS/doc
	cp -r doc/BOOK ATS/doc
	cp -r doc/EXAMPLE ATS/doc
	cp -r doc/IMPLEMENTATION ATS/doc
	cp -r doc/LIBRARY ATS/doc
	cp -r doc/TUTORIAL ATS/doc
	tar -zvcf ATS.tgz ATS/
	mv ATS.tgz ATS
	cp ATS/ATS.tgz /home/fac2/hwxi/public_html/ATS/Anairiats.tgz

######
#
# end of [Makefile]
#
######
