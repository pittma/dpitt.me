#!/bin/sh

tmpfile=$(mktemp)
echo "https://keybase.io/danielscottt -->" > $tmpfile #Optional text in commented area
cat $1 >> $tmpfile
echo "
<!--" >> $tmpfile
gpg --digest-algo SHA256 --clearsign $tmpfile
echo "<!--" > $1
cat "$tmpfile.asc" >> $1
echo "-->" >> $1
rm $tmpfile
rm "$tmpfile.asc"
