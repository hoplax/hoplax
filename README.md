Hoplax binaries/releases
========================

This branch contains binaries for the Hoplax project.  Always pay
attention to not join this branch to the master in Git, we don't want
big files to get joined into the history of the source code.

Bookmark importer for Firefox and Chrome
----------------------------------------

### Usage

Usage: ./import-bookmarks <bookmarks.json >mybookmarks.js
Usage: ./import-bookmarks <bookmarks.html >mybookmarks.js

Chrome can only export its bookmarks in html format, while Firefox
supports the json format.  You should use json with Firefox, that is
the only way to keep your tags with the bookmarks.

The output is in Hoplax javascript format, ready to be included in
conf.js or used with ?l=mybookmarks.js.

If your bookmarks are not that secret, you can even upload them to
some webserver and then use Hoplax anywhere with your bookmarks:
http://hoplax.github.com/?l=http://www.yoursite.net/mybookmarks.js

### Code

The code for the importer is written in Haskell, you can find the
source code in the master branch's tools subdirectory.  I compile it
with GHC.  It requires the following Cabal packages: tagsoup,
monadlib, hjson, hjson-query.

### Bugs, contribute

If you run into any issues while using this importer, please send us
the html/json file which we can reproduce the issue with.  Patches are
also welcome of course, if there are any Haskellers out there! :)
