#########################################################################
##                                                                     ##
##                         Applied Type System                         ##
##                                                                     ##
##                              Hongwei Xi                             ##
##                                                                     ##
#########################################################################

##
## ATS - Unleashing the Potential of Types!
## Copyright (C) 2002-2012 Hongwei Xi, Boston University
## All rights reserved
##
## ATS is free software;  you can  redistribute it and/or modify it under
## the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
## Free Software Foundation; either version 2.1, or (at your option)  any
## later version.
## 
## ATS is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
## for more details.
## 
## You  should  have  received  a  copy of the GNU General Public License
## along  with  ATS;  see the  file COPYING.  If not, please write to the
## Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.
##

## ###### ###### ##
##
## Author of the file: Likai Liu (liulk at likai dot org)
##
## ###### ###### ##

# Generates lstatslang.sty which is printed to standard output.
#
# Requires keywords.txt generated by the Makefile from ats_keywords.dats

is_directive() {
  case "$1" in
    \#FILENAME|\#LOCATION|\#CHARCOUNT|\#LINECOUNT) return 1;;  # false
    \#*) return 0;;  # true
    dynload|staload|\$dynload) return 0;;  # true
  esac

  return 1  # false
}

# Usage: cat keywords.txt | typeset 'initialkeyword'
#
typeset() {
  local -r indent=5
  local n=$indent       # line width
  local c=0             # word count

  if [[ $1 ]]; then
    ((n += ${#1}))
  fi

  while read word; do
    ((!c++)) || echo -ne ','
    if ((${#word} + n >= 70)); then
      echo -ne '%\n     '
      n=$indent
    fi
    echo -ne "$word"
    ((n += ${#word} + 1))
  done
}

quote() {
  echo "${1/\#/\\\#}"
}

keywords() {
  while read word; do
    is_directive "$word" || quote "$word"
  done < keywords.txt
}

directives() {
  while read word; do
    ! is_directive "$word" || quote "$word"
  done < keywords.txt
}

literate() {
  echo "{$1}{\$$2$ }$3"
}

literate_tight() {
  echo "{$1}{\$$2$}$3"
}

version() {
  set -o pipefail
  if svn info | awk -F': ' '$1 == "Revision" { print "r" $2 }'; then
    return
  fi
  awk '/^[0-9]+(\.[0-9]+)+/ { print "v" $1; exit }' < ../../VERSION.txt
}

readonly VER="$(version)"
readonly DATE="$(date +'%Y/%m/%d')"

cat <<END
%%
%% This file is distributed under the terms of the LaTeX Project Public
%% License from CTAN archives in directory  macros/latex/base/lppl.txt.
%% Either version 1.3 or, at your option, any later version.
%%
%% This file is completely free and comes without any warranty.
%%
\ProvidesFile{lstatslang.sty}
    [$DATE $VER listings language file for ATS]
\newif\if@lstatslang@literate@ \@lstatslang@literate@false
\DeclareOption{literate}{\@lstatslang@literate@true}
\ProcessOptions\relax
\if@lstatslang@literate@
\RequirePackage{amssymb}
\fi
%%
%% ATS language definition (c) 2012  Matthew Danish and Likai Liu
%%
%% ATS is an ML-like dependent-typed functional language with theorem
%% proving using classical and linear propositions.
%%
\lstdefinelanguage[$VER]{ATS}%
  {morekeywords={$(keywords | typeset 'morekeywords')},%
   moredirectives={$(directives | typeset 'moredirectives')},%
   sensitive=true,%
   morecomment=[l]{//},%
   morecomment=[n]{(*}{*)},%
   morestring=[b]",%
  }[keywords,comments,directives,strings]%
\lstdefinelanguage[literate $VER]{ATS}[$VER]{ATS}%
  {literate=
   $(literate '=' '=' 1)
   $(literate '->' '\to' 1)
   $(literate '<=' '\leq' 1)
   $(literate '<' '<' 1)
   $(literate '>=' '\geq' 1)
   $(literate '>' '>' 1)
   $(literate '<>' '\neq' 1)
   $(literate ':<>' ':_{\textrm{pure}}' 4)
   $(literate ':<prf>' ':_{\textrm{prf}}' 4)
   $(literate '-<lin,prf>' '\mathop{\multimap}_{\textrm{prf}}' 4)
   $(literate '|' '\mid' 1)
   $(literate_tight '\{' '\{' 1)
   $(literate_tight '\}' '\}' 1)
  }%
\lstalias[]{ATS}[$VER]{ATS}
\lstalias[literate]{ATS}[literate $VER]{ATS}
END