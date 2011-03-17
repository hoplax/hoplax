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
// Google Maps, autocompletion example
hoplax.autocompletes.maps = hoplax.autocompletes.maps || [];

var mapsSlinglet = new Slinglet();

mapsSlinglet.handlerFn = function(searchstring) {
  var url = "http://maps.google.com/";
  if (searchstring) url += "maps?f=q&t=m&source=s_q&q="+searchstring;
  window.location = url;
};

mapsSlinglet.fieldCreater = function() {
  return new Completer('mapssearch', 'Location', '50%',
                       function() { return hoplax.autocompletes.maps; },
                       ['Countries'], ['n']);
};

mapsSlinglet.dependencies = 'lib/mapsdb.js';

////////////////////////////////////////////////////////////////////////////////
// JavaScript reference
hoplax.autocompletes.mdc = hoplax.autocompletes.mdc || [];

var mdcSlinglet = new Slinglet();

mdcSlinglet.handlerFn = function(url) {
  window.location = url;
};

mdcSlinglet.fieldCreater = function() {
  return new Completer('mdc', 'Topic', '50%',
                       function() { return hoplax.autocompletes.mdc; },
                       ['URL', 'Topic'], ['url', 'topic'],
                       [1, 10]);
};

mdcSlinglet.dependencies = 'lib/mdc.js';

////////////////////////////////////////////////////////////////////////////////
// builtin bookmarks

hoplax.bookmarks.push(
  { name: "IMDb", url: "http://www.imdb.com/find?q=%s" },
  { name: "Google Maps", url: "http://maps.google.com/maps?q=%s", slinglet: mapsSlinglet },
  { name: "Google Maps Directions",
    url: "http://maps.google.com/maps?daddr=${To:2:40%}&saddr=${From:1:40%}" },
  { name: "Google Images", url: "http://images.google.com/images?q=%s" },
  { name: "Hoogle", url: "http://www.haskell.org/hoogle/?hoogle=%s" },
  { name: "Mozilla Javascript Reference",
    url: "https://developer.mozilla.org/en/JavaScript/Reference/%s",
    slinglet: mdcSlinglet },
  null // for your convenience, so can have the trailing commas in every line
);
