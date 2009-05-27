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

DESTDIR =

# Default target.

all:
	@$(MAKE) -f Makefile_maintainer $@

# NOTE(liulk): integration with autoconf.

-include config.mk

config.h config.mk: config.h.in config.mk.in configure
	./configure
	@exec $(MAKE) $@	# rerunning make

configure: configure.ac
	aclocal
	automake --add-missing --foreign || true
	autoconf

config.h.in: configure.ac
	autoheader

# NOTE(liulk): installation to prefix

.PHONY: install
install:
	# recursively install all files in the list except .svn control files.
	for d in ccomp/runtime doc libats libc prelude; do \
	  cd $(abs_top_srcdir) && \
	  $(INSTALL) -d $(DESTDIR)$(ATSHOME)/$$d && \
	  find $$d -name .svn -prune -o \
            -exec $(INSTALL) -m 644 -D \{} $(DESTDIR)$(ATSHOME)/\{} \; \
	    -print; \
	done

	# install specific files in the list as regular files.
	for f in COPYING INSTALL *.txt ccomp/lib/*.a config.h; do \
	  cd $(abs_top_srcdir) && \
	  $(INSTALL) -m 644 -D $$f $(DESTDIR)$(ATSHOME)/$$f && \
	  echo $$f; \
	done

	# install specific files in the list as executables.
	for f in bin/*; do \
	  cd $(abs_top_srcdir) && \
	  $(INSTALL) -m 755 -D $$f $(DESTDIR)$(ATSHOME)/$$f && \
	  echo $$f; \
	done

# NOTE(liulk): once most major functions of Makefile_maintainer is
# superceded, remove the following code.
#
%: force
	@exec $(MAKE) -f Makefile_maintainer $@
force: ;
