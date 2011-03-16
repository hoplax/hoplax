#!/bin/sh

exec >../lib/mdc.js

echo 'hoplax.autocompletes.mdc = ['
wget -q -O - 'https://developer.mozilla.org/en/JavaScript/Reference' \
 | sed 's/<a /\n<a /g' | perl -n -e 'print if s/.*(<a .*rel=.internal.*?<\/a>?).*/\1/' \
 | fgrep developer.mozilla.org/en/JavaScript | sed 's/ rel="internal"//' \
 | sed 's/<code>//' | sed 's,</code>,,' \
 | sed 's/^<a href="//' | sed 's/"[^>]*>/", topic: "/' | sed 's.</a>." },.' \
 | sed 's/^/{ url: "/' | sort | uniq
echo '];'
