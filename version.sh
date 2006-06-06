#!/bin/sh

    last_cvs_update=r`grep committed-rev .svn/entries | head -n 1 | cut -d '"' -f 2 2>/dev/null`

extra=""
if test "$1" ; then
  extra="-$1"
fi
echo "#define VERSION \"dev-SVN-${last_cvs_update}${extra}\"" >version.h
