#!/bin/sh

aclocal
autoconf

amprefix=`automake --help | sed '/^Usage:/!d;s/Usage: //;s,/bin/automake.*,,'`
amlibdir=`ls -d ${amprefix}/share/automake-* | tail -1`

for f in install-sh mkinstalldirs INSTALL COPYING ; do
  ln -sf ${amlibdir}/$f
done
