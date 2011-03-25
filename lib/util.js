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

var util = {};

util.global = this;

////////////////////////////////////////////////////////////////////////////////
// Console and logging.

// Please use util.c.debug, util.c.trace, util.c.etc through the
// project, since this is a safe way to log in Firebug and Chrome.
// If Firebug is not started, then the util.c.* won't cause any
// problems just simply do nothing.

(function() {
  function empty() {}

  if (util.global.console) {
    util.c = console;
  } else {
    util.c = {};
    var functions = "log debug info warning error assert trace profile profileEnd".split(" ");
    for (var i = 0; i < functions.length; ++i) {
      util.c[functions[i]] = empty;
    }
  }
})();

////////////////////////////////////////////////////////////////////////////////
// idoMatch

util.array = function(n, e) {
  switch (n) {
    case 1:  return [e];
    case 2:  return [e,e];
    case 3:  return [e,e,e];
    case 4:  return [e,e,e,e];
  }
  var ret = [];
  for (var i = 0; i < n; ++i) {
    ret[i] = e;
  }
  return ret;
};

util.sum = function(arr) {
  var sum = 0;
  for (i in arr) {
    sum += arr[i];
  }
  return sum;
};

util.max = function(arr) {
  return Math.max.apply(Math, arr);
};

util.isAlphaNum = function(chr) { return chr.match(/^[A-Za-z0-9]$/); };

util.isSep = function(chr) {
  return " ,./?;:'\"()[]{}=+-_!@#$%^&*\\\t\n".indexOf(chr) != -1;
};

util.toLowerCase = function(str) { return str.toLowerCase(); };

util.idoMatch = function(search_str, _strs, weights) {
  // Additional space at the end is to trigger word end handling after
  // the last word.
  search_str = search_str.toLowerCase() + " ";

  var n = _strs.length;
  var strs = _strs.map(util.toLowerCase);
  if (!weights) {
    weights = util.array(n, 1);
  } else {
    util.c.assert(weights.length == n);
  }

  // Helper array, only used to create a copy of.
  var zeroes = util.array(n, 0);

  var indexes = zeroes.slice(0);
  var scores = zeroes.slice(0);
  var mults = weights.slice(0);
  var totscore = 0;

  var matches = n;
  for (var i=0; i<search_str.length; ++i) {
    if (search_str.charAt(i) == " ") {
      // score bonus for word end.
      for (var j = 0; j < n; ++j) {
        var index = indexes[j];
        if (index > 0) {
          if (index == strs[j].length) {
            scores[j] += 1.8 * (mults[j] - weights[j]);
          } else if (util.isSep(strs[j].charAt(index))) {
            scores[j] += 1.6 * (mults[j] - weights[j]);
          }
        }
      }

      totscore += util.max(scores);
      // TODO(klao): profile if this is faster than recreating the arrays.
      for (var k = 0; k < n; ++k) {
        indexes[k] = 0;
        scores[k] = 0;
        mults[k] = weights[k];
      }
      matches = n;
      continue;
    }

    for (var j=0; j<n; ++j) {
      if (indexes[j] == -1) {
        continue;
      }
      var newindex = strs[j].indexOf(search_str.charAt(i), indexes[j]);
      if (newindex != -1) {
        // continuous match:
        if (newindex > 0 && newindex == indexes[j]) {
          mults[j] *= 2;
        } else {
          mults[j] = weights[j];
        }
        // beginning of a word
        if (newindex == 0) {
          mults[j] *= 2.2;
        } else if (util.isSep(strs[j].charAt(newindex-1))) {
          mults[j] *= 2.0;
        }
        scores[j] += mults[j];
        indexes[j] = newindex + 1;
      } else {
        scores[j] = 0;
        --matches;
        indexes[j] = -1;
      }
    }
    if (matches == 0) return -1;
  }
  return totscore;
};

util.zwws = function(str) {
  return str.replace(/([^ -]{6})([^ -])/g, "$1\u200B$2");
};

////////////////////////////////////////////////////////////////////////////////
// Misc

util.timer = function(timeout, fn) {
  return window.setTimeout(fn, timeout);
};

util.cancelTimer = function(timeout_id) {
  window.clearTimeout(timeout_id);
};

/**
 * Call a callback function after count number of invocations.
 */
util.barrierClosure = function(count, closure) {
  return function() {
    if (--count == 0 && closure) {
      closure.call();
    }
  };
};

////////////////////////////////////////////////////////////////////////////////
// Load-on-demand

util.loadFile = (function() {
    var scripts = {};

    function loadFile(file, callback) {
      if (!file) {
        if (callback) callback.call();
        return;
      }
      var s = scripts[file];
      if (s) {
        if (s.loaded && callback) {
          callback.call();
          return;
        }
        // Already added to DOM but still loading, so queue this
        // callback call on the load event.
        if (callback) {
          s.dom.load(callback);
        }
        return;
      }

      // We encountered this script the first time.
      s = {};
      s.loaded = false;

      s.dom = $("<script></script>");
      s.dom.attr("src", file);
      s.dom.attr("type", "text/javascript");
      var done = function() {
        s.loaded = true;
        if (callback) callback.call();
      };
      s.dom.load(done);
      // TODO(klao): we shouldn't just silently ignore load errors!
      s.dom.error(done);
      // This doesn't work. jQuery is too smart and transforms
      // <script> appends into ajax queries!
      //$("head").append(s.dom);
      $("head").get(0).appendChild(s.dom.get(0));
      scripts[file] = s;

      // In firefox error callback does not fire for files that fail
      // to load locally. This introduces hard to debug errors.
      util.timer(2000, function() {
          if (!s.loaded) {
            alert("Script " + file + " failed to load after 2 seconds! " +
                  "Please, check your config.");
          }
        });
    }

    return loadFile;
  })();

util.load = function(files, callback) {
  files = util.asarray(files);
  if (!files || files.length == 0) {
    if (callback) callback.call();
    return;
  }
  if (files.length == 1) {
    util.loadFile(files[0], callback);
    return;
  }
  var barrier = util.barrierClosure(files.length, callback);
  for (var i = 0; i < files.length; ++i) {
    util.loadFile(files[i], barrier);
  }
};

/**
 * Import other javascript files in order and then call back.
 *
 * If the imported javascript files use this method again, importing
 * will still happen in order.
 *
 * @param {Array.<string>} files The files to load.
 * @param {Function} callback Call this function at the end.
 */
util.loadInGlobalOrder = (function(/* files, callback */) {
    var loadStack = [];
    var processing = false;

    function processStack() {
      if (loadStack.length == 0) {
        processing = false;
        return;
      }
      var top = loadStack.pop();
      if (typeof top == "string") {
        // A file name -> load it.
        util.loadFile(top, processStack);
      } else {
        // A callback -> call it and proceed immediately.
        if (top) top.call();
        processStack();
      }
    }

    function loadInGlobalOrder(files, callback) {
      if (callback) loadStack.push(callback);
      Array.prototype.push.apply(loadStack, files.reverse());
      if (!processing) {
        processing = true;
        processStack();
      }
    }

    return loadInGlobalOrder;
  })();

////////////////////////////////////////////////////////////////////////////////
// Inheritance

util.extend = function(child, supertype) {
   child.prototype.__proto__ = supertype.prototype;
   child.prototype.__super = supertype;
};

////////////////////////////////////////////////////////////////////////////////
/**
 * Use as data.  If a function, then call it and use the result.
 */
util.asdata = function(obj) {
  return (typeof obj === "function"
          ? obj.call()
          : obj);
};

////////////////////////////////////////////////////////////////////////////////
/**
 * Use as array.  If not, then wrap it in a single element array.
 */
util.asarray = function(obj) {
  return (obj instanceof Array
          ? obj
          : [obj]);
};

////////////////////////////////////////////////////////////////////////////////
/**
 * Bind this.
 *
 * Creates a function in which the 'this' argument is bound.  Very
 * useful when this is not set according to your intention for some
 * event by the browser.
 *
 * @param {function} fn The function to wrap.
 * @param {object} self Bind "this" to self when running the wrapper.
 */
util.bind = (function(/*fn, self*/) {
    function bindNative(fn, self) {
      return fn.bind(self);
    }

    function bindJs(fn, self) {
      return function(var_args) {
        return fn.apply(self, arguments);
      };
    }

    if (Function.prototype.bind) {
      return bindNative;
    } else {
      return bindJs;
    }
  })();

////////////////////////////////////////////////////////////////////////////////
// Background tasks.
// Poor man's implementation of cooperative preempting.

util.backgroundCounter = 0;
util.backgroundTasks = {};

/**
 * Will automatically "cancel" any running background task with the
 * same id.
 *
 * @param {string} id Arbitrary string identifying the task. Must not
 * contain colon char.

 * @param {function} closure Code to run. On running should return
 * true if it wants to "yield" (i.e. it will be scheduled to run
 * again), or false to indicate that this background task has ended.
 * Will get a boolean argument, true: indicating that this task has
 * been "joined", so it can finish all the work in one slice; false:
 * regular operation, the task should "yield" after a short slice.

 * @param {object} self "this" will be bound to the self parameter
 * while running the closure.
 */
util.startBackgroundTask = function(id, closure, self) {
  util.c.assert(id.indexOf(":") == -1);
  ++util.backgroundCounter;
  self = self || util.global;
  var task = {c: util.backgroundCounter, run: closure, self: self};
  util.backgroundTasks[id] = task;
  // We do the first slice immediately.
  util.backgroundStep(id, task);
};

util.backgroundMessageHandler = function(event) {
  if (event.source != window || typeof event.data != "string"
      || event.data.indexOf("BT:") != 0) {
    return;
  }
  event.stopPropagation();
  var payload = event.data.substr(3).split(":");

  var task = util.backgroundTasks[payload[0]];
  if (!task) {
    // Task was joined or cancelled.
    return;
  }
  if (payload[1] != task.c) {
    // Task was overrun (user started another task with the same id).
    return;
  }
  util.backgroundStep(payload[0], task);
};

window.addEventListener("message", util.backgroundMessageHandler, true);

util.backgroundStep = function(id, task) {
  var yielded = task.run.call(task.self, false);
  if (yielded) {
    window.postMessage("BT:" + id + ":" + task.c, "*");
  } else {
    // The background task has terminated.
    util.backgroundTasks[id] = undefined;
  }
};

util.joinBackgroundTask = function(id) {
  var task = util.backgroundTasks[id];
  if (!task)
    return;
  util.backgroundTasks[id] = undefined;
  var running = true;
  while (running) {
    running = task.run.call(task.self, true);
  };
};

util.cancelBackgroundTask = function(id) {
  util.backgroundTasks[id] = undefined;
};

util.isBackgroundTaskRunning = function(id) {
  return Boolean(util.backgroundTasks[id]);
};
