#######################################################################
#                                                                     #
#                         Applied Type System                         #
#                                                                     #
#                              Hongwei Xi                             #
#                                                                     #
#######################################################################

#
# ATS/Postiats - Unleashing the Potential of Types!
# Copyright (C) 2011-20?? Hongwei Xi, ATS Trustworthy Software
# All rights reserved
#
# ATS is  free software;  you can redistribute it and/or modify it under
# the  terms of the  GNU General Public License as published by the Free
# Software Foundation; either version 2.1, or (at your option) any later
# version.
# 
# ATS is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
# for more details.
# 
# You  should  have  received  a  copy of the GNU General Public License
# along  with  ATS;  see the  file COPYING.  If not, please write to the
# Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
#

######
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Start Time: August, 2011
#
######

ATSHOMEQ="$(ATSHOME)"
ATSCC=$(ATSHOMEQ)/bin/atscc
ATSOPT=$(ATSHOMEQ)/bin/atsopt

######

ATSGCFLAG=
ATSGCFLAG=-D_ATS_GCATS

ATSCCFLAGS=-g
ATSCCFLAGS=-g -O
ATSCCFLAGS=-g -O2

######

ATSLIB=$(ATSHOMEQ)/ccomp/lib
ATSLIB=$(ATSHOMEQ)/ccomp/lib64

######

all:: libatsdoc clean

######

SOURCES := \
  libatsdoc_error.sats \
  libatsdoc_error.dats \
  libatsdoc_symbol.sats \
  libatsdoc_symbol.dats \
  libatsdoc_symmap.sats \
  libatsdoc_symmap.dats \
  libatsdoc_filename.sats \
  libatsdoc_filename.dats \
  libatsdoc_location.sats \
  libatsdoc_location.dats \
  libatsdoc_reader.sats \
  libatsdoc_reader.dats \
  libatsdoc_lexbuf.sats \
  libatsdoc_lexbuf.dats \
  libatsdoc_text.sats \
  libatsdoc_text.dats \

SOURCESsta := $(filter %.sats, $(SOURCES))
SOURCESdyn := $(filter %.dats, $(SOURCES))

OBJECTS := $(SOURCES)
OBJECTS := $(patsubst %.sats, %_sats.o, $(OBJECTS))
OBJECTS := $(patsubst %.dats, %_dats.o, $(OBJECTS))

######

libatsdoc_error_sats.o : SATS/libatsdoc_error.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_error_dats.o : DATS/libatsdoc_error.dats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_symbol_sats.o : SATS/libatsdoc_symbol.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_symbol_dats.o : DATS/libatsdoc_symbol.dats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_symmap_sats.o : SATS/libatsdoc_symmap.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_symmap_dats.o : DATS/libatsdoc_symmap.dats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_filename_sats.o : SATS/libatsdoc_filename.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_filename_dats.o : DATS/libatsdoc_filename.dats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_location_sats.o : SATS/libatsdoc_location.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_location_dats.o : DATS/libatsdoc_location.dats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_reader_sats.o : SATS/libatsdoc_reader.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_reader_dats.o : DATS/libatsdoc_reader.dats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_lexbuf_sats.o : SATS/libatsdoc_lexbuf.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_lexbuf_dats.o : DATS/libatsdoc_lexbuf.dats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_text_sats.o : SATS/libatsdoc_text.sats; $(ATSCC) $(ATSCCFLAGS) -c $<
libatsdoc_text_dats.o : DATS/libatsdoc_text.dats; $(ATSCC) $(ATSCCFLAGS) -c $<

######

libatsdoc: dynloadall.dats $(OBJECTS)
	$(ATSCC) $(ATSCCFLAGS) -c dynloadall.dats
	$(ARR) $(ATSLIB)/libatsdoc.a dynloadall_dats.o $(OBJECTS)

######
#
# %_sats.o: %.sats
#	$(ATSCC) $(ATSCCFLAGS) -c $<
#
# %_dats.o: %.dats
#	$(ATSCC) $(ATSCCFLAGS) -c $<
#
######

ARR=ar -r
CPF=cp -f
RMF=rm -f
WCL=wc -l

######

srclines:: ; $(WCL) $(SOURCES)

######

clean:: ; $(RMF) *~
clean:: ; $(RMF) *_?ats.o
clean:: ; $(RMF) *_?ats.c

cleanall:: clean
cleanall:: ; $(RMF) libatsdoc.a

###### end of [Makefile] ######
