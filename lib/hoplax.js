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
hoplax.bookmark_completer = new Completer("bookmark", null, '100%',
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
  hoplax.bookmark_completer.compui.html('<tr><td>' + hoplax.help + '</tr></td>');
  $($("#completion a")[0]).focus();
};

/**
 * Hide any interactive bookmark UI present until the user is finished
 * with selecting a bookmark.
 */
hoplax.bookmark_completer.inputhandler.focus(function() {
  $("span.slinglet").hide();
  $(this).show();
  // When all the inputfields are hidden, the size of #inputfields is
  // very small, so redo the layout
  hoplax.doLayout();
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
    hoplax.doLayout();
  });
});

/**
 * Call this function, whenever absolutely positioned elements need
 * repositioning.  After spending one day by trying out the CSS hacks
 * and layout engines of the future (and fail with all), I am not sure
 * that this will be CSS anytime soon.
 */
hoplax.doLayout = function() {
  // people are not good in reading long lines
  var w = Math.min($(window).width(), 1400);
  var h = $(window).height();
  var inputfields = $("div#inputfields");
  var ih = inputfields.height();
  if (Math.abs(hoplax.doLayout.cachedW - w) < 5 &&
      Math.abs(hoplax.doLayout.cachedH - h) < 5 &&
      Math.abs(hoplax.doLayout.cachedIH - ih) < 5)
    return;
  var wleft;
  var wright;
  var leftcol = $("div#leftcol");

  if (w >= 750) { // big screen
    wleft = w * 0.2;

    // put the leftcol to the left 20% of the screen and to full height
    leftcol.offset({left: 20, top: 20});
    leftcol.width(wleft - 20);
    leftcol.height(h - 20);
    leftcol.show();
  } else { // small screen => no left column
    leftcol.hide();
    wleft = 0;
  }

  // everything else is on the right side of the scren
  wright = w - wleft;

  $("#first-start").offset({left: wleft + 20, top: 20});

  // top is for the input fields
  inputfields.offset({left: wleft + 20, top: 20});
  inputfields.width(wright - 40);

  // bottom is for the completion div
  var completion = $("div#completion");
  completion.offset({left: wleft + 20, top: 40 + ih});
  completion.width(wright - 40);
  completion.height(h - ih - 60);

  hoplax.doLayout.cachedW = w;
  hoplax.doLayout.cachedH = h;
  hoplax.doLayout.cachedIH = ih;
};
hoplax.doLayout.cachedW = -100;
hoplax.doLayout.cachedH = -100;
hoplax.doLayout.cachedIH = -100;

/**
 * Should be called when every initialization is done and the user has
 * imported all his bookmarks into the hoplax.bookmarks container.
 */
hoplax.init = function() {
  $("#first-start").hide();
  util.loadInGlobalOrder(hoplax.imports, hoplax.init2);

  $(window).resize(hoplax.doLayout);
};

hoplax.init2 = function() {
  var userload = $.parseQuery()['l'];
  if (userload) {
    util.loadInGlobalOrder(userload, hoplax.init3);
  } else {
    hoplax.init3();
  }
};

hoplax.init3 = function() {
  hoplax.bookmark_completer.dataset =
    $.map(hoplax.bookmarks,
          function (e) {
            if (!e) return null;
            if (e.tags || e.path) {
              e.tags = e.tags || [];
              if (e.path) e.tags.push(e.path);
              e.name = "[" + e.tags.join(",") + "] " + e.name;
            }
            if (!e.slinglet)
              e.slinglet = Slinglet.NewBookmarkSlinglet(e.url);
            return e;
          });
  hoplax.bookmark_completer.appendTo($("#inputfields"));
  hoplax.bookmark_completer.inputhandler.focus();
  $("#logo").css('visibility','visible');
};
