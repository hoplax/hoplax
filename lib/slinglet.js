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

// This file contains the main UI logic: inputbox, cursor,
// completer, slinglet.

keys = {
  ENTER: 13,
  BACKSPACE: 8,
  DELETE: 46,
  UP: 38,
  DOWN: 40,
  TAB: 9
};

////////////////////////////////////////////////////////////////////////////////
/**
 * Cursor
 *
 * Handles the UI features of the cursor in a completion table. The
 * CSS class 'selected' is put on the currently selected html
 * element (tr). The scrolling of the div is also hanled by Cursor.
 */
function Cursor() {}

/**
 * Reset the cursor
 *
 * @param {object} first the first selectable tr in the table.
 * @param {object} last the last selectable tr in the table.
 * @param {object} n the number of items between first and last (inclusve).
 */
Cursor.prototype.reset = function(first, last, n) {
  this.first_ = first;
  this.last_ = last;
  this.i_ = 0;
  this.selected_ = first;
  this.selected_.addClass('selected');
  this.n_ = n;
};

/**
 * Reposition the scrollTop of the containing div.
 */
Cursor.prototype.reposition = function() {
  var div = $('#completion');
  var divHei = div.height();
  var cursTop = this.selected_.position().top;
  var cursBot = cursTop + this.selected_.height();

  if (divHei * 0.85 - cursBot < 0) {
    div.scrollTop(div.scrollTop() - (divHei * 0.85 - cursBot));
  }

  if (divHei * 0.15 - cursTop > 0) {
    div.scrollTop(div.scrollTop() - (divHei * 0.15 - cursTop));
  }
};

/**
 * Select next tr.
 */
Cursor.prototype.next = function() {
  this.selected_.removeClass('selected');
  if (this.i_ == this.n_ - 1) {
    this.selected_ = this.first_;
    this.i_ = 0;
  } else {
    this.selected_ = this.selected_.next();
    this.i_++;
  }
  this.selected_.addClass('selected');
  this.reposition();
};

/**
 * Select previous tr.
 */
Cursor.prototype.prev = function() {
  this.selected_.removeClass('selected');
  if (this.i_ == 0) {
    this.selected_ = this.last_;
    this.i_ = this.n_ - 1;
  } else {
    this.selected_ = this.selected_.prev();
    this.i_--;
  }
  this.selected_.addClass('selected');
  this.reposition();
};

////////////////////////////////////////////////////////////////////////////////
/**
 * InputBox
 *
 * An HTML text input field in a span.
 *
 * @param {string} id the HTML identifier of the <input> tag.
 * @param {string} label the user visible label.
 * @param {string} width width of the text input field.
 * @constructor
 */
function InputBox(id, label, width) {
  this.inputhandler = $('<input spellcheck="false" type="text"' +
                        (width ? ' style="width: ' + width + '"' : '') +
                        ' id="' + id + '"></input>');
  this.inputhandler.keypress(util.bind(this.keypressBindme, this));
  this.inputhandler.focus(util.bind(this.focusBindme, this));
  this.inputhandler.focusout(util.bind(this.focusoutBindme, this));
  this.inputui = $('<span>');
  if (label) this.inputui.text(label + ': ');
  this.inputui.append(this.inputhandler);
}

/**
 * Call this.action() on Enter.
 *
 * @param {object} event keyboard event.
 */
InputBox.prototype.keypressBindme = function(event) {
  if (event.which == 13) { // enter
    event.preventDefault();
    this.action();
  }
};

/**
 * On focus: hide completion tables, restore selection.
 */
InputBox.prototype.focusBindme = function() {
  $('#completion > table').hide();
  if (this.selStart !== undefined) {
    var self = this;
    util.timer(0, function() {
      self.inputhandler[0].setSelectionRange(self.selStart, self.selEnd);
    });
  }
};

/**
 * On focusout: save selection.
 */
InputBox.prototype.focusoutBindme = function() {
  // save selection on focus loss
  this.selStart = this.inputhandler[0].selectionStart;
  this.selEnd = this.inputhandler[0].selectionEnd;
};

/**
 * abstract virtual
 *
 * {InputBox,Completer}.action() is a method used as an event.  It
 * should be called when everything related to completion is finished
 * and the inputfield's text is replaced by the end result (the
 * autocompleted text).  Slinglet listens to this event (by setting
 * this function in Slinglet.activate() to something useful) and
 * focuses the next input element or in the case of the last one
 * calling the Slinglet's handler function.
 */
InputBox.prototype.action = function() { util.c.assert(false); };

/**
 * Appends itself to the slinglet.
 *
 * @param {object} slinglet dom object of the slinglet's span.
 */
InputBox.prototype.appendTo = function(slinglet) {
  slinglet.append(this.inputui);
};

////////////////////////////////////////////////////////////////////////////////

/**
 * Completer
 *
 * An InputBox with an autocompletion table.
 *
 * @param {string} id as in InputBox.
 * @param {string} label as in InputBox.
 * @param {string} width as in InputBox.
 * @param {function} dataset list of objects to display in the table.
 * @param {Array.<string>} dataset_labels table heading on the UI.
 * @param {Array.<string>} dataset_ids data heading in the dataset.
 * @param {Array.<int>} dataset_weights scoring adjustment.
 *
 * @constructor
 */
function Completer(id, label, width,
                   dataset, dataset_labels, dataset_ids, dataset_weights) {
  this.__super.call(this, id, label, width);

  this.dataset = dataset;
  this.dataset_ids = dataset_ids;
  this.dataset_labels = dataset_labels;
  this.dataset_weights = dataset_weights;

  var inp = this.inputhandler;
  inp.attr('autocomplete', 'off');
  // keypressBindme is already event assigned by InputBox
  inp.keydown(util.bind(this.keydownBindme, this));
  inp.bind('paste', util.bind(this.pasteCutBindme, this));
  inp.bind('cut', util.bind(this.pasteCutBindme, this));
  // focusBindme is already event assigned by InputBox

  this.compui = $('<table></table>');
  this.compui.hide();

  this.pendingFillReqId = null;
  this.pendingFillReqHandler =
    util.bind(Completer.prototype.pendingFillReqHandlerBindme,
              this);

  this.maxResults = 50;
  this.cursor = new Cursor();
}
util.extend(Completer, InputBox);

/**
 * Called when the user selected an autocompletion result.
 * Virtual.
 */
Completer.prototype.completeAction = function() {
  this.inputhandler.val(this.completed_row[this.dataset_ids[0]]);
  this.action();
};

/**
 * Text to show for user's own input.
 * Virtual.
 *
 * @param {string} s user's string.
 * @return {string} to show.
 */
Completer.prototype.plainText = function(s) {
  return s;
};

/**
 * Action to execute if user selected her own input.
 * Virtual.
 *
 * @param {string} s user's string.
 */
Completer.prototype.plainAction = function(s) {
  this.inputhandler.val(s);
  this.action();
};

/**
 * Text to show for empty input in the completion table.  If false
 * (null, empty or undefined), then no line will be shown.
 * Virtual.
 */
Completer.prototype.emptyText = 'No value, thanks!';

/**
 * Action to do on empty input.
 * Virtual.
 */
Completer.prototype.emptyAction = function() {
  this.inputhandler.val('');
  this.action();
};

/**
 * Array of actions to put at the top of the completion table given
 * the user's input.  E.g. google search and http:// on the main page
 * or help in case of empty string.
 * Virtual, but consider to override plainText, plainAction, emptyText
 * and emptyAction instead, maybe that is enough for you.
 *
 * @param {string} s user's current input string.
 * @return {Array.<Object.<s:string,f:function>>} in the returned
 *  array every item is an object with a string (s) that will be
 *  displayed to the user and a function (f) that will be executed if
 *  the user selects this action.
 */
Completer.prototype.plainHandlers = function(s) {
  var self = this;
  if (s == '') {
    if (this.emptyText)
      return [ { s: this.emptyText, f: util.bind(this.emptyAction, this) } ];
    else
      return [];
  } else {
    return [ { s: this.plainText(s), f: function() { self.plainAction(s); } } ];
  }
};

/**
 * Use a row from the completion table to fill in the input text and
 * then call this.action.
 *
 * @param {number} i the row to use, if undef, then use the cursor's row.
 */
Completer.prototype.complete = function(i) {
  this.completed_row = undefined;
  if (i === undefined) i = this.cursor.i_;
  if (i >= this.cached_ph.length) {
    // user wants to use the result of autocompletion
    this.completed_row = this.cached_table[i - this.cached_ph.length][1];
    this.completeAction();
  } else {
    // user wants to use one of the options that is generated from her input
    this.cached_ph[i].f();
  }
};

// Updating the autocompletion table when the user changed the filter
// text can be slow, if we have a lot of data to idoMatch.  This is
// why we use the `Background tasks' section of util here.  The whole
// process is called fillTable.

/**
 * Starts the filtering.  Since we want to support delayed calls,
 * should only be called from pendingFillReqHandlerBindme.  Use
 * requestFillTable(true) for immediate filtering.
 *
 * @param {string} str the user supplied filter string.
 */
Completer.prototype.fillTableStart = function(str) {
  if (str === this.cached_str) {
    // The cached string is the same as str, so the table is already
    // OK, do not redisplay, because the cursor will reset then.
    return;
  }

  this.working_str = str;
  this.working_idx = 0;
  // Reuse the cached dataset if the user extended the filter.
  if (this.cached_str !== undefined && str.indexOf(this.cached_str) == 0) {
    this.working_dataset = this.cached_dataset;
  } else {
    this.working_dataset = util.asdata(this.dataset);
  }

  this.result_dataset = [];
  this.result_scores = [];

  util.startBackgroundTask('fillTable', this.fillTableStep, this);
};

/**
 * One workslice.  Called by the background task runner.  Filters 1000
 * dataset rows in one go.
 *
 * @param {bool} join do not yield, but do the whole work at once.
 * @return {bool} wether we want to run more to finish the filling.
 */
Completer.prototype.fillTableStep = function(join) {
  var str = this.working_str;
  var idx = this.working_idx;

  if (str == '') {
    this.fillTableStepEmptyShortcut();
    return false;
  }

  var n = this.working_dataset.length;
  var quota = join ? -1 : 1000;
  for (; idx < n && quota; ++idx, --quota) {
    var datarow = this.working_dataset[idx];
    // TODO(errge): Do not mangle datarows in every search, only once
    // on data load.  We can compute the shown value and search value
    // (all lower case) then.  Goal: save on toLowerCase _AND_ reduce
    // code complexity.
    var datarow_shown = this.dataset_ids.map(function(name) {
        return datarow[name] || ''; });
    var score = util.idoMatch(str, datarow_shown, this.dataset_weights);
    if (score < 0) {
      continue;
    }
    this.result_scores.push([score, datarow, datarow_shown]);
    this.result_dataset.push(datarow);
  }

  if (idx < n) {
    // Didn't finish yet, but our work slice has expired.
    // Save the working position and "yield".
    this.working_idx = idx;
    return true;
  }

  // Finished the scoring. Sort the result, update the cached values
  // and finish the fillTable.

  this.result_scores.sort(function(a, b) { return b[0] - a[0]; });
  this.cached_table = this.result_scores.slice(0, this.maxResults);
  // TODO(errge): Figure out if we need result_dataset and
  // cached_dataset both, klao suggested that we don't.
  this.cached_dataset = this.result_dataset;
  this.cached_str = str;

  this.fillTableEnd(str);
  return false;
};

/**
 * If filter string is empty, we want to take a shortcut and not do
 * any scoring and sorting.  This is for efficiency and also to
 * present the completion table in the original form.
 */
Completer.prototype.fillTableStepEmptyShortcut = function() {
  var self = this;
  this.cached_table = this.working_dataset.slice(0, this.maxResults).map(
      function(datarow) {
        var datarow_shown = self.dataset_ids.map(function(name) {
            return datarow[name] || ''; });
        return [0, datarow, datarow_shown];
      });
  this.cached_dataset = this.working_dataset;
  this.cached_str = '';

  this.fillTableEnd('');
};

/**
 * Finish the fill table background process in the foreground, called
 * from UI actions (up, down, enter, tab).
 */
Completer.prototype.joinFillTable = function() {
  if (this.pendingFillReqId !== null) {
    // There is a fillTable request pending, start it right now.
    this.requestFillTable(true);
    // TODO(klao): it's a bit silly to start the async process to
    // which we will join immediately. But the waste is minimal: a
    // single posted message will be discarded (worst case).
  }
  util.joinBackgroundTask('fillTable');
};

/**
 * Must be called if calling the dataset function may result in
 * returing different data than before.  E.g. autocompletion data
 * arrived.
 */
Completer.prototype.invalidateCache = function() {
  this.cached_str = undefined;
};

/**
 * Displays the first maxResults number of results in the completion
 * table.  Called when the filtering is done.  Works from this.cached_table.
 *
 * @param {string} str the user's filter string.
 */
Completer.prototype.fillTableEnd = function(str) {
  var self = this;
  var ui = self.compui;

  ui.html(''); // remove the current completion table
  var header_row = $('<tr/>');
  for (var i in self.dataset_labels) {
    header_row.append($('<th>' + this.dataset_labels[i] + '</th>'));
  }
  ui.append(header_row);

  // add rows using plainHandlers
  this.cached_ph = this.plainHandlers(str);
  this.cached_ph.forEach(function(r, i) {
    var row = $('<tr/>').addClass('clickable');
    var td = $('<td/>').text(util.zwws(r.s));
    td.attr('colspan', self.dataset_labels.length);
    row.append(td);
    row.click(function() { self.complete(i); });
    ui.append(row);
  });

  // add rows resulted from autocompletion ido matching
  this.cached_table.forEach(function(r, i) {
    var datarow_shown = r[2];
    var row = $('<tr/>').addClass('clickable');
    datarow_shown.forEach(function(x) {
      var td = $('<td/>').text(util.zwws(x));
      td.html(td.html().replace(/\u200B/g, '<wbr></wbr>'));
      row.append(td);
    });
    row.click(function() { self.complete(i + self.cached_ph.length); });
    ui.append(row);
  });

  ui.find('tr').removeClass('selected');
  var items = ui.find('tr');
  this.cursor.reset($(items[1]), $(items[items.length - 1]), items.length - 1);

  // position the cursor on the first autocompletion if there is any
  if (this.cached_table.length > 0)
    for (var j = 0; j < this.cached_ph.length; ++j) this.cursor.next();

  this.working_str = undefined;
};

/**
 * Same as InputBox.appendTo, but also adds the completion table to
 * the #completion div.
 *
 * @param {object} container passed to InputBox.appendTo.
 */
Completer.prototype.appendTo = function(container) {
  this.__super.prototype.appendTo.apply(this, arguments);
  $('#completion').append(this.compui);
};

// Starting the fillTable background process for every keypress would
// be a waste, since the user will continue typing anyway, this is why
// we have the pending logic here.

/**
 * Time is up, start a fillTable.
 */
Completer.prototype.pendingFillReqHandlerBindme = function() {
  this.fillTableStart($.trim(this.inputhandler.val()));
  this.pendingFillReqId = null;
};

/**
 * Schedule a fillTable with some delay, so the user has time to press
 * more keys.  Cancels previously scheduled fillTable request, if any.
 *
 * @param {bool} immediately skip the delay, start now.
 */
Completer.prototype.requestFillTable = function(immediately) {
  if (this.pendingFillReqId) {
    util.cancelTimer(this.pendingFillReqId);
  }
  if (!immediately) {
    this.pendingFillReqId = util.timer(100, this.pendingFillReqHandler);
  } else {
    this.pendingFillReqHandler();
  }
};

/**
 * On focus: show the completion table, input text mangling, request a fill.
 */
Completer.prototype.focusBindme = function() {
  this.__super.prototype.focusBindme.apply(this, arguments);
  this.compui.show();
  if (this.cached_str) {
    this.inputhandler.val(this.cached_str);
  } else {
    this.changedSinceFocus = false;
  }
  this.requestFillTable(true);
};

/**
 * Keypress: enter completes.
 *
 * @param {object} event keyboard event.
 */
Completer.prototype.keypressBindme = function(event) {
  if (event.which == keys.ENTER) { // enter
    event.preventDefault();
    this.joinFillTable();
    this.complete();
  } else if (event.which != 0) {
    // fox has 0 here if the keypress is not giving real input, not
    // changing the text (down, up, etc.)
    this.changedSinceFocus = true;
    this.requestFillTable(false);
  }
};

/**
 * Paste or Cut: request a fill.
 */
Completer.prototype.pasteCutBindme = function() {
  this.changedSinceFocus = true;
  this.requestFillTable(false);
};

/**
 * Special keys can only be handled on keydown, not on keypress,
 * because they doesn't change the input.  Chrome doesn't emit
 * keypress for delete and backspace even when they change the input,
 * so we handle the here.
 *
 * @param {object} event keyboard event.
 */
Completer.prototype.keydownBindme = function(event) {
  if (event.which == keys.BACKSPACE || event.which == keys.DELETE) {
    this.changedSinceFocus = true;
    this.requestFillTable(false);
  }
  if (event.which == keys.UP) { // up
    event.preventDefault();
    this.changedSinceFocus = true;
    this.joinFillTable();
    this.cursor.prev();
  }
  if (event.which == keys.DOWN) { // down
    event.preventDefault();
    this.changedSinceFocus = true;
    this.joinFillTable();
    this.cursor.next();
  }
  if (event.which == keys.TAB && event.shiftKey == false) { // tab without shift
    if (this.changedSinceFocus) {
      this.joinFillTable();
      this.complete();
      event.preventDefault();
    }
  }
};

////////////////////////////////////////////////////////////////////////////////
// Slinglet

/**
 * Interactive bookmark UI, a collection of input boxes
 * (maybe with completion) and the function to compute the target URL
 * when the user is finished.
 *
 * this.handlerFn: Called when the user is finished.
 * this.fieldCreater: Lazy array of InputBoxes and Completers.
 * this.dependencies: Javascript files to start
 *                    loading when the UI is used first.
 * @constructor
 */
function Slinglet() {
  // user has to set these up, see builtins.js for an example
  this.handlerFn = null;
  this.fieldCreater = null;
  this.dependencies = null;
}

/**
 * Show this slinglet.  Called when the user selects the interactive
 * bookmark for this slinglet.
 */
Slinglet.prototype.activate = function() {
  if (!this.fields) {
    this.fields = util.asarray(this.fieldCreater.call());

    var span = $('<span>');
    span.addClass('slinglet');
    this.inputs = span;
    var self = this;
    for (var i = 0; i < this.fields.length; ++i) {
      this.fields[i].appendTo(span);
      if (i + 1 < this.fields.length) {
        (function() {
           var nextField = self.fields[i + 1];
           self.fields[i].action = function() {
             nextField.inputhandler.focus();
           };
        })();
      } else {
        var lastField = this.fields[i];
        lastField.action = function() {
          self.handlerFn.apply(self,
                               $.map(span.find('input'),
                               function(o) { return $.trim(o.value); }));
      };
      }
    }
    $('#inputfields').append(span);
  } else {
    this.inputs.show();
  }
  this.fields[0].inputhandler.focus();

  if (this.dependencies && !this.dependenciesLoaded) {
    util.load(this.dependencies, util.bind(this.activateLoaded, this));
  } else {
    this.activateLoaded();
  }

  // When we show a slinglet the height of the header may change,
  // since new input boxes may have been inserted.  This is why we
  // have to redo the layout.
  hoplax.doLayout();
};

/**
 * Show this slinglet and assume that the dependencies has been loaded
 * just now.
 */
Slinglet.prototype.activateLoaded = function() {
  if (!this.dependenciesLoaded) {
    // Reset caches when completion data arrives.
    this.dependenciesLoaded = true;
    this.fields.forEach(function(completer) {
      if (completer.invalidateCache)
        completer.invalidateCache();
    });
  }
  $(document.activeElement).focus();
};

/**
 * Generate a new slinglet based on an URL.
 *
 * If the URL doesn't contain any special patterns, the slinglet will
 * not have any input fields and will redirect immediately.
 *
 * If %s is contained, then the slinglet will have one unnamed input
 * box, this is FF keyword shortcuts backward compatibility.
 *
 * Furthermore, ${name:x:w} can be included.  For example, to have
 * Google Maps directions with 100px wide From field and 30% wide To
 * field, you can write this (note that with the x parameter we can
 * change the ordering of the fields on the UI):
 * http://maps.google.com/maps?daddr=${To:2:50%}&saddr=${From:1:100px}
 *
 * If you do not want to use the last two special features, but have
 * to include % or $ in the URL, you can always use the standard URL
 * encoding, %25 for % and %24 for $.
 *
 * @param {string} url in the above specified format.
 * @return {object} the generated slignlet.
 */
Slinglet.NewBookmarkSlinglet = function(url) {
  var ret = new Slinglet();

  if (url.indexOf('%s') == -1 && url.indexOf('${') == -1) {
    ret.activate = function() { window.location = url; };
  } else if (url.indexOf('%s') != -1) {
    ret.fieldCreater = function() {
      return new InputBox('', 'Parameter', '80%');
    };
    ret.handlerFn = function(val) {
      window.location = url.replace(/%s/, val);
    };
  } else {
    ret.fieldCreater = function() {
      var subs = url.match(/\${[^:}]+:[0-9]+:[^:}]+}/g);
      var subs2 = [];
      subs.forEach(function(sub) {
        var splitted = sub.match(/\${(.*?):(.*?):(.*?)}/);
        subs2.push([splitted[2], splitted[1], splitted[3]]);
      });
      subs2.sort();
      return $.map(subs2,
        function(sub) {
          return new InputBox('', sub[1], sub[2]);
        });
    };
    ret.handlerFn = function() {
      var url_ = url;
      $.each(arguments, function(sind, sval) {
        url_ = url_.replace(new RegExp('\\${[^:}]+:' + (sind + 1) + ':[^:}]+}'),
                          sval);
      });
      window.location = url_;
    };
  }

  return ret;
};
