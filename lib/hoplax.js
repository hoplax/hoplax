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

/**
 * The main UI field, used for selecting a bookmark.
 */
hoplax.bookmark_completer = new Completer("bookmark", null, null,
                                          null,
                                          ['Name', 'URL', 'KW'],
                                          ['name', 'url', 'keyword'],
                                          [2, 1, 10]);

/**
 * If the user hasn't selected anything, but typed in a string, then
 * use Google search.
 */
hoplax.bookmark_completer.action = function(o) {
  if (typeof o == "string")
    window.location = 'http://www.google.com/search?q=' + encodeURIComponent(o);
  else
    if (typeof o.slinglet == "function")
      o.slinglet();
    else
      o.slinglet.activate();
};

/**
 * Display help if the user hasn't selected or typed in anything.
 */
hoplax.bookmark_completer.emptyText = "First time user?  Navigate here or click here for help!";
hoplax.bookmark_completer.emptyAction = function() {
  util.load("lib/help.js",
    function() {
      hoplax.bookmark_completer.compui.html(hoplax.help);
      $($("#completion a")[0]).focus();
    });
};

/**
 * Hide any interactive bookmark UI present until the user is finished
 * with selecting a bookmark.
 */
hoplax.bookmark_completer.inputhandler.focus(function() {
  $("#inputfields > span").hide();
  $(this).show();
  Completer.setCompletionHeight();
});

/**
 * Display unconfigured message if the loading doesn't finish in 1
 * second.
 */
$(document).ready(function() {
  util.timer(1000, function() {
    var confjs = window.location.href;
    confjs = confjs.slice(0, confjs.lastIndexOf('/'));
    confjs = confjs.slice(0, confjs.lastIndexOf('/'));
    confjs = confjs + '/conf.js';
    $("#first-start").html(
      'Your Hoplax is not configured yet!' +
      '<p>Please use the included <br><b>conf_example.js</b><br> as a template and create ' +
      '<br><b>' + confjs + '</b>');
  });
});

/**
 * Should be called when every initialization is done and the user has
 * imported all his bookmarks into the hoplax.bookmarks container.
 */
hoplax.init = function() {
  $("#first-start").hide();
  util.loadInGlobalOrder(hoplax.imports, hoplax.init2);

  Completer.setCompletionHeight();
  $(window).resize(Completer.setCompletionHeight);
};

hoplax.init2 = function() {
  hoplax.bookmark_completer.dataset =
    $.map(hoplax.bookmarks,
          function (e) {
            if (!e) return null;
            if (!e.slinglet)
              e.slinglet = Slinglet.NewBookmarkSlinglet(e.url);
            return e;
          });
  hoplax.bookmark_completer.appendTo($("#inputfields"));
  hoplax.bookmark_completer.inputhandler.focus();
};
