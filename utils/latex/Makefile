AWK := awk
BASH := bash
PDFLATEX := pdflatex

.PHONY: all clean
all:: lstatslang.sty
clean::
	rm -f lstatslang.sty keywords.txt

lstatslang.sty: lstatslang.sty.sh keywords.txt
	$(BASH) $< > $@ || rm $@

keywords.txt: ../../src/ats_keyword.dats
	$(AWK) -F'"' \
	  '$$1 ~ /^keyval_insert/ && $$2 ~ /[A-Za-z]/ { print $$2 }' \
	  < $< > $@ || rm $@

clean::
	rm -f sample.aux sample.log sample.pdf

all:: sample.pdf
sample.pdf: sample.tex lstatslang.sty
	$(PDFLATEX) $<
