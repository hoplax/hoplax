#!/bin/sh

# install: http://code.google.com/closure/utilities/docs/linter_howto.html
find -name '*.js' -and -not -name 'jquery.min.js' | xargs gjslint
