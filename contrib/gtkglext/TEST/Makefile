#
#
# A simple Makefile for some programs involving OpenGL
#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
#
#

######

ATSUSRQ="$(ATSHOME)"

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSCTRB=$(ATSUSRQ)/contrib
GLOBJ=$(ATSUSRQ)/contrib/GL/atsctrb_GL.o
XROBJ=$(ATSUSRQ)/contrib/cairo/atsctrb_cairo.o
GTKOBJ=$(ATSUSRQ)/contrib/GTK/atsctrb_GTK.o
PSLIDEOBJ=$(ATSUSRQ)/contrib/atspslide/atsctrb_atspslide.o

######

.PHONY: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean

######

GTKGLEXT=`pkg-config --cflags --libs gtk+-2.0 gtkglext-1.0 gtkglext-x11-1.0`

######

checkall:: gtkglHello
cleanall:: ; $(RMF) gtkglHello
gtkglHello: gtkglHello.dats
	$(ATSCC) -I$(ATSCTRB) -IATS$(ATSCTRB) -o $@ $< $(GLOBJ) $(GTKOBJ) $(GTKGLEXT)

######

checkall:: gtkglFaces
cleanall:: ; $(RMF) gtkglFaces
gtkglFaces: gtkglFaces.dats
	$(ATSCC) -I$(ATSCTRB) -IATS$(ATSCTRB) -o $@ $< $(GLOBJ) $(GTKOBJ) $(GTKGLEXT)

######

checkall:: gtkglClock
cleanall:: ; $(RMF) gtkglClock
gtkglClock: gtkglClock.dats ; \
  $(ATSCC) -o gtkglClock gtkglClock.dats $(ATSCTRBGL) $(ATSCTRBXR) $(ATSCTRBGTK) $(ATSCTRBPSLIDE) $(GTKGLEXT)

######

checkall:: gtkglCubeRot
cleanall:: ; $(RMF) gtkglCubeRot
gtkglCubeRot: gtkglCubeRot.dats ; \
  $(ATSCC) -o gtkglCubeRot gtkglCubeRot.dats $(ATSCTRBGL) $(ATSCTRBXR) $(ATSCTRBGTK) $(ATSCTRBPSLIDE) $(GTKGLEXT)

######

checkall:: gtkglPascalTheorem
cleanall:: ; $(RMF) gtkglPascalTheorem
gtkglPascalTheorem: gtkglPascalTheorem.dats ; \
  $(ATSCC) -o gtkglPascalTheorem \
    gtkglPascalTheorem.dats $(ATSCTRBGL) $(ATSCTRBXR) $(ATSCTRBGTK) $(ATSCTRBPSLIDE) $(GTKGLEXT)

######

checkall:: gtkglSlidePresent
cleanall:: ; $(RMF) gtkglSlidePresent
gtkglSlidePresent: gtkglSlidePresent.dats ; \
  $(ATSCC) -O3 -o gtkglSlidePresent \
    gtkglSlidePresent.dats $(ATSCTRBGL) $(ATSCTRBXR) $(ATSCTRBGTK) $(ATSCTRBPSLIDE) $(GTKGLEXT)

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c
	$(RMF) *_?ats.o

###### end of [Makefile] ######
