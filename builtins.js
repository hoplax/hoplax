/*
 * Copyright 2011 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you
 * may not use this file except in compliance with the License. You
 * may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * Authors: klao@google.com, napszel@gmail.com, errge@google.com
 */

////////////////////////////////////////////////////////////////////////////////
// Google Maps
hoplax.autocompletes.maps = hoplax.autocompletes.maps || [];

var mapsSlinglet = new Slinglet();

mapsSlinglet.handlerFn = function(searchstring) {
  var url = "http://maps.google.com/";
  if (searchstring) url += "maps?f=q&t=m&source=s_q&q="+searchstring;
  window.location = url;
};

mapsSlinglet.fieldCreater = function() {
  return new Completer('mapssearch', 'Search Google Maps', '50%',
                       function() { return hoplax.autocompletes.maps; },
                       ['Countries'], ['n']);
};

mapsSlinglet.dependencies = 'lib/mapsdb.js';

////////////////////////////////////////////////////////////////////////////////
// imdb

var imdbSlinglet = new Slinglet();

imdbSlinglet.handlerFn = function(searchstring) {
  var url = "http://www.imdb.com/";
  if (searchstring) url += "find?q="+searchstring;
  window.location = url;
};

imdbSlinglet.fieldCreater = function() {
  return new InputBox("imdbsearch", "Search IMDb", "50%");
};

////////////////////////////////////////////////////////////////////////////////
// Fun fact about haskell, demonstrating the function() handling ability
// of the Bookmark class

var haskellFunction = function () { alert("Haskell is a scripting language inspired by Python."); };

////////////////////////////////////////////////////////////////////////////////
// builtin bookmarks

hoplax.bookmarks.push(
  { name: "IMDb", url: "http://www.imdb.com/find?q=%s", slinglet: imdbSlinglet },
  { name: "Google Maps", url: "http://maps.google.com/maps?q=%s", slinglet: mapsSlinglet },
  { name: "haskell", url: 'javascript:alert(info)', slinglet: haskellFunction },
  null // for your convenience, so can have the trailing commas in every line
);
