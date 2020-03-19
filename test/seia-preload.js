(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
// init window._rt
require('./rt');

var loader = require('./loader');

var rt = _rt;

function u8a_to_utf8(a)
{
    if (typeof(TextDecoder) === "function") {
        var d = new TextDecoder();
        return d.decode(a);
    }
    else {
        // TODO: for IE11, need compatiable
        throw "TextDecoder not exist";
    }
}

var url = _rt.href;
if (!_rt.is_nodejs()) {
    url = document.currentScript.src;
}

rt.preloader_url = url;

loader.load(url, rt.SEQ,
            function (key) {
                if (rt.is_nodejs()) {
                    var path = rt.store.get_path(key);
                    try {
                        require(path);
                    }
                    catch(e) {
                        console.log(e);
                    }
                }
                else {
                    rt.store.get(key, function(err, res) {
                        if (res) {
                            var js = u8a_to_utf8(res);
                            var s = document.createElement("script");
                            s.text = js;
                            document.body.appendChild(s);
                        }
                        else console.log(err);
                    });
                }
            });

},{"./loader":2,"./rt":3}],2:[function(require,module,exports){
/*
1. first, for <seq> from _rt.SEQ to _rt.SEQ_MIN, if 'version/seia-<seq>' exist, load that one
2. if not any 'version/seia-<seq>' exist, download from remote,
  argument url will be 'src' of 'seia-preload.js',
  so will fetch 'seia-<_rt.SEQ>.js' from same dir
3. set _rt.SEQ_CURR to actually used seia-<seq>
*/

var store = require('./store');

function get_key(seq) {
    return "version/seia-" + seq;
}

function load_xhr(url, cb) {
    var seq = _rt.SEQ;
    var u = new _rt.URL(url);

    var p = u.pathname;
    p = p.replace(RegExp("/[^/]*$"),"");
    u.pathname = p + "/seia-" + seq + ".js";

    var url1 = u.href;

    console.log("fetch seia-%d from %s ...", seq, url1);

    function proc_data(data) {
        var key = get_key(seq);
        store.set(key, data, function(err) {
            if (!err) {
                _rt.SEQ_CURR = seq;
                cb(key);
            }
            else {
                throw err;
            }
        });
    }

    // TODO, accept gzipped data
    if ((typeof(_rt.cwd) == "string") && (u.protocol == 'file:')) {
        // NOTE: xhr2 not support 'file:', so fallback to fs.readFile
        p = u.pathname;
        fs.readFile(p, function(err, res) {
            if (err) throw err;
            else proc_data(new Uint8Array(res));
        })
    }
    else {
        // browser
        // NOTE:, for 'file:', firefox not accept CORS after firefox 67.
        // so, use 'python3 -m http.server' to run a trival http server for test.
        var req = new _rt.XMLHttpRequest();
        req.open("GET", url1);
        req.responseType = 'arraybuffer';
        req.onreadystatechange = function() {
            if (req.readyState === 4) {
                if (req.status === 200) {
                    var data = req.response;
                    proc_data(new Uint8Array(data));
                }
                else {
                    throw ("GET fail, status=" + req.status);
                }
            }
        }

        req.send();
    }
}

function load(url, seq, cb) {
    store.init(function() {
        do_load(url, seq, cb);
    });
}

function do_load(url, seq, cb) {
    var key = get_key(seq);
    console.log("try version %d", seq);

    store.exist(key, function(e) {
        if (e) {
            console.log("version %d exist, load %s, rt version=%d",
                        seq, key, _rt.SEQ);
            _rt.SEQ_CURR = seq;
            cb(key);
        }
        else {
            if (seq > _rt.SEQ_MIN) {
                setTimeout(function() { do_load(url, seq - 1, cb); }, 0);
            }
            else {
                load_xhr(url, cb);
            }
        }
    });
}

module.exports.load = load;

},{"./store":6}],3:[function(require,module,exports){
// update when update seia-SEQ.js
SEQ = 4;

// min seq we can accept
SEQ_MIN = 1;

function is_nodejs() {
    return (typeof(_rt.cwd) == 'string');
}

// ensure _rt exist
if (typeof(_rt) != 'object') {
    _rt = {};
}


_rt.SEQ = SEQ;  // this is RtConf._rt_conf_rt_seq
_rt.SEQ_MIN = SEQ_MIN;
_rt.SEQ_CURR = -1;

_rt.is_nodejs = is_nodejs;

if (!_rt.is_nodejs()) {
    _rt.URL = window.URL;
    _rt.JSON = window.JSON;
    _rt.RTCPeerConnection = window.RTCPeerConnection;
    _rt.XMLHttpRequest = window.XMLHttpRequest;
}

// NOTE: mqtt is set inside ghcjs, or at external exec.js
//_rt.mqtt = require('mqtt');
_rt.store = require('./store');

// runtime config
if (typeof(_rt.conf) != 'object') {
    _rt.conf = {};
}

// default turn server & bootstrap node
_rt.conf.turn_server = _rt.conf.turn_server || [
    "stun:stun.stunprotocol.org"
];

_rt.conf.bootstrap_node = _rt.conf.bootstrap_node || [
    "kgczbhhdrwopyu84a9foy66amepymc8kofaqry6s3t3y9dqmeseo:0",
    "9dp3gc3r7gngw3xhcs69bwyj3xaa75zodx5huw7rksyjs8in9qyy:0",
    "d531szhkbbjbd6q716mraz8qamji8akyyu17up8gbe731qtiw66y:0"
];

_rt.conf.mqtt_server = _rt.conf.mqtt_server ||
    "wss://mqtt.eclipse.org/mqtt";

_rt.conf.log_level = _rt.conf.log_level || [
    "E", "W", "I", "D"
];

_rt.conf.service = _rt.conf.service || [];

// default skip benchmakr
if (typeof(_rt.conf.skip_benchmark) == 'undefined') {
    _rt.conf.skip_benchmark = true;
}

},{"./store":6}],4:[function(require,module,exports){
var path = require('path');

function get_path(key) {
    return path.join(_rt.cwd, "store", key);
}

// return Uint8Array
function get_s(key) {
    var p = get_path(key);
    var buf = fs.readFileSync(p);

    return new Uint8Array(buf);
}

function set_s(key, ary, len) {
    var p = get_path(key);
    fs.writeFileSync(p, ary);
}

function set(key, ary, cb) {
    // cb: (err)
    var p = get_path(key);
    var dir = path.dirname(p);
    fs.mkdir(dir, { recursive: true }, function(err) {
        if (err) cb(err)
        else {
            fs.writeFile(p, ary, cb);
        }
    });
}

function remove(key, cb) {
    // cb(err)
    var p = get_path(key);
    fs.exists(p, function(e) {
        if (e) {
            fs.unlink(p, function(err) {
                if (err) {
                    cb(err);
                }
                else {
                    cb(null);
                }
            });
        }
        else {
            cb(null);
        }
    });
}

function get(key, cb) {
    var p = get_path(key);
    // cb: (err, res) =>
    fs.readFile(p, function(err, buf) {
        if (err != null) cb(err, new Uint8Array(new ArrayBuffer(0)));
        else cb(null, new Uint8Array(buf));
    });
}

function exist_s(key) {
    var p = get_path(key);
    var res = fs.existsSync(p);
    return res;
}

function exist(key, cb) {
    var p = get_path(key);
    fs.exists(p, cb);
}

function init(cb) {
    cb(null);
}

module.exports.get = get;
module.exports.set = set;

module.exports.init = init;

module.exports.get_path = get_path;
module.exports.exist = exist;
module.exports.remove = remove;

},{"path":7}],5:[function(require,module,exports){
module.exports = {}

var DB_VER = 1;
var DB_NAME = "seia";
var STORE_NAME = "conf";

var _db = null;

function do_init(cb) {
    var IDB = window.indexedDB;
    var req = IDB.open(DB_NAME, DB_VER);

    req.onupgradeneeded = function(ev) {
        console.log("curr db version is %d, need upgraded", DB_VER);

        // NOTE: curr ver is 1, so only create STORE_NAME
        var db = ev.target.result;
        db.createObjectStore(STORE_NAME, { keyPath: "k" } );
    };

    req.onerror = function(ev) {
        IDB.deleteDatabase(DB_NAME);
        cb("indexeddb open fail");
    };

    req.onsuccess = function(ev) {
        _db = ev.target.result;

        cb(null);
    };
}

function init(cb) {
    if ("indexedDB" in window) {
        console.log("use indexedDB for '_rt.store'");
        do_init(cb);
    } else {
        cb("indexedDB not support");
    }
}

function set(key, val, cb) {
    var tx = _db.transaction([STORE_NAME], "readwrite");
    var obj = {k: key, v: val};
    var req = tx.objectStore(STORE_NAME).put(obj);

    req.onsuccess = function() {
        cb(null);
    };

    req.onerror = function() {
        cb(req.error);
    };
}

function get(key, cb) {
    var tx = _db.transaction([STORE_NAME]);
    var req = tx.objectStore(STORE_NAME).get(key);

    req.onsuccess = function() {
        var res = req.result;
        if (res != null) {
            res = res.v;
            cb(null, res);
        }
        else {
            cb("key " + key + " not exist", null);
        }
    }

    req.onerror = function() {
        cb(req.error, null);
    }
}

function exist(key, cb) {
    var tx = _db.transaction([STORE_NAME]);
    var req = tx.objectStore(STORE_NAME).getKey(key);

    req.onsuccess = function() {
        cb(req.result != null);
    }

    req.onerror = function() {
        console.log(req.error);
        cb(false);
    }
}

function remove(key, cb) {
    var tx = _db.transaction([STORE_NAME], "readwrite");
    var req = tx.objectStore(STORE_NAME).delete(key);

    req.onsuccess = function() {
        cb(null);
    }

    req.onerror = function() {
        cb(req.error);
    }
}

module.exports.init = init;
module.exports.set = set;
module.exports.get = get;
module.exports.exist = exist;
module.exports.remove = remove;

},{}],6:[function(require,module,exports){
module.exports = (typeof _rt.cwd == "string") ? require("./store-fs") : require("./store-idb");

},{"./store-fs":4,"./store-idb":5}],7:[function(require,module,exports){
(function (process){
// .dirname, .basename, and .extname methods are extracted from Node.js v8.11.1,
// backported and transplited with Babel, with backwards-compat fixes

// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// resolves . and .. elements in a path array with directory names there
// must be no slashes, empty elements, or device names (c:\) in the array
// (so also no leading and trailing slashes - it does not distinguish
// relative and absolute paths)
function normalizeArray(parts, allowAboveRoot) {
  // if the path tries to go above the root, `up` ends up > 0
  var up = 0;
  for (var i = parts.length - 1; i >= 0; i--) {
    var last = parts[i];
    if (last === '.') {
      parts.splice(i, 1);
    } else if (last === '..') {
      parts.splice(i, 1);
      up++;
    } else if (up) {
      parts.splice(i, 1);
      up--;
    }
  }

  // if the path is allowed to go above the root, restore leading ..s
  if (allowAboveRoot) {
    for (; up--; up) {
      parts.unshift('..');
    }
  }

  return parts;
}

// path.resolve([from ...], to)
// posix version
exports.resolve = function() {
  var resolvedPath = '',
      resolvedAbsolute = false;

  for (var i = arguments.length - 1; i >= -1 && !resolvedAbsolute; i--) {
    var path = (i >= 0) ? arguments[i] : process.cwd();

    // Skip empty and invalid entries
    if (typeof path !== 'string') {
      throw new TypeError('Arguments to path.resolve must be strings');
    } else if (!path) {
      continue;
    }

    resolvedPath = path + '/' + resolvedPath;
    resolvedAbsolute = path.charAt(0) === '/';
  }

  // At this point the path should be resolved to a full absolute path, but
  // handle relative paths to be safe (might happen when process.cwd() fails)

  // Normalize the path
  resolvedPath = normalizeArray(filter(resolvedPath.split('/'), function(p) {
    return !!p;
  }), !resolvedAbsolute).join('/');

  return ((resolvedAbsolute ? '/' : '') + resolvedPath) || '.';
};

// path.normalize(path)
// posix version
exports.normalize = function(path) {
  var isAbsolute = exports.isAbsolute(path),
      trailingSlash = substr(path, -1) === '/';

  // Normalize the path
  path = normalizeArray(filter(path.split('/'), function(p) {
    return !!p;
  }), !isAbsolute).join('/');

  if (!path && !isAbsolute) {
    path = '.';
  }
  if (path && trailingSlash) {
    path += '/';
  }

  return (isAbsolute ? '/' : '') + path;
};

// posix version
exports.isAbsolute = function(path) {
  return path.charAt(0) === '/';
};

// posix version
exports.join = function() {
  var paths = Array.prototype.slice.call(arguments, 0);
  return exports.normalize(filter(paths, function(p, index) {
    if (typeof p !== 'string') {
      throw new TypeError('Arguments to path.join must be strings');
    }
    return p;
  }).join('/'));
};


// path.relative(from, to)
// posix version
exports.relative = function(from, to) {
  from = exports.resolve(from).substr(1);
  to = exports.resolve(to).substr(1);

  function trim(arr) {
    var start = 0;
    for (; start < arr.length; start++) {
      if (arr[start] !== '') break;
    }

    var end = arr.length - 1;
    for (; end >= 0; end--) {
      if (arr[end] !== '') break;
    }

    if (start > end) return [];
    return arr.slice(start, end - start + 1);
  }

  var fromParts = trim(from.split('/'));
  var toParts = trim(to.split('/'));

  var length = Math.min(fromParts.length, toParts.length);
  var samePartsLength = length;
  for (var i = 0; i < length; i++) {
    if (fromParts[i] !== toParts[i]) {
      samePartsLength = i;
      break;
    }
  }

  var outputParts = [];
  for (var i = samePartsLength; i < fromParts.length; i++) {
    outputParts.push('..');
  }

  outputParts = outputParts.concat(toParts.slice(samePartsLength));

  return outputParts.join('/');
};

exports.sep = '/';
exports.delimiter = ':';

exports.dirname = function (path) {
  if (typeof path !== 'string') path = path + '';
  if (path.length === 0) return '.';
  var code = path.charCodeAt(0);
  var hasRoot = code === 47 /*/*/;
  var end = -1;
  var matchedSlash = true;
  for (var i = path.length - 1; i >= 1; --i) {
    code = path.charCodeAt(i);
    if (code === 47 /*/*/) {
        if (!matchedSlash) {
          end = i;
          break;
        }
      } else {
      // We saw the first non-path separator
      matchedSlash = false;
    }
  }

  if (end === -1) return hasRoot ? '/' : '.';
  if (hasRoot && end === 1) {
    // return '//';
    // Backwards-compat fix:
    return '/';
  }
  return path.slice(0, end);
};

function basename(path) {
  if (typeof path !== 'string') path = path + '';

  var start = 0;
  var end = -1;
  var matchedSlash = true;
  var i;

  for (i = path.length - 1; i >= 0; --i) {
    if (path.charCodeAt(i) === 47 /*/*/) {
        // If we reached a path separator that was not part of a set of path
        // separators at the end of the string, stop now
        if (!matchedSlash) {
          start = i + 1;
          break;
        }
      } else if (end === -1) {
      // We saw the first non-path separator, mark this as the end of our
      // path component
      matchedSlash = false;
      end = i + 1;
    }
  }

  if (end === -1) return '';
  return path.slice(start, end);
}

// Uses a mixed approach for backwards-compatibility, as ext behavior changed
// in new Node.js versions, so only basename() above is backported here
exports.basename = function (path, ext) {
  var f = basename(path);
  if (ext && f.substr(-1 * ext.length) === ext) {
    f = f.substr(0, f.length - ext.length);
  }
  return f;
};

exports.extname = function (path) {
  if (typeof path !== 'string') path = path + '';
  var startDot = -1;
  var startPart = 0;
  var end = -1;
  var matchedSlash = true;
  // Track the state of characters (if any) we see before our first dot and
  // after any path separator we find
  var preDotState = 0;
  for (var i = path.length - 1; i >= 0; --i) {
    var code = path.charCodeAt(i);
    if (code === 47 /*/*/) {
        // If we reached a path separator that was not part of a set of path
        // separators at the end of the string, stop now
        if (!matchedSlash) {
          startPart = i + 1;
          break;
        }
        continue;
      }
    if (end === -1) {
      // We saw the first non-path separator, mark this as the end of our
      // extension
      matchedSlash = false;
      end = i + 1;
    }
    if (code === 46 /*.*/) {
        // If this is our first dot, mark it as the start of our extension
        if (startDot === -1)
          startDot = i;
        else if (preDotState !== 1)
          preDotState = 1;
    } else if (startDot !== -1) {
      // We saw a non-dot and non-path separator before our dot, so we should
      // have a good chance at having a non-empty extension
      preDotState = -1;
    }
  }

  if (startDot === -1 || end === -1 ||
      // We saw a non-dot character immediately before the dot
      preDotState === 0 ||
      // The (right-most) trimmed path component is exactly '..'
      preDotState === 1 && startDot === end - 1 && startDot === startPart + 1) {
    return '';
  }
  return path.slice(startDot, end);
};

function filter (xs, f) {
    if (xs.filter) return xs.filter(f);
    var res = [];
    for (var i = 0; i < xs.length; i++) {
        if (f(xs[i], i, xs)) res.push(xs[i]);
    }
    return res;
}

// String.prototype.substr - negative index don't work in IE8
var substr = 'ab'.substr(-1) === 'b'
    ? function (str, start, len) { return str.substr(start, len) }
    : function (str, start, len) {
        if (start < 0) start = str.length + start;
        return str.substr(start, len);
    }
;

}).call(this,require('_process'))
},{"_process":8}],8:[function(require,module,exports){
// shim for using process in browser
var process = module.exports = {};

// cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
    throw new Error('setTimeout has not been defined');
}
function defaultClearTimeout () {
    throw new Error('clearTimeout has not been defined');
}
(function () {
    try {
        if (typeof setTimeout === 'function') {
            cachedSetTimeout = setTimeout;
        } else {
            cachedSetTimeout = defaultSetTimout;
        }
    } catch (e) {
        cachedSetTimeout = defaultSetTimout;
    }
    try {
        if (typeof clearTimeout === 'function') {
            cachedClearTimeout = clearTimeout;
        } else {
            cachedClearTimeout = defaultClearTimeout;
        }
    } catch (e) {
        cachedClearTimeout = defaultClearTimeout;
    }
} ())
function runTimeout(fun) {
    if (cachedSetTimeout === setTimeout) {
        //normal enviroments in sane situations
        return setTimeout(fun, 0);
    }
    // if setTimeout wasn't available but was latter defined
    if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
        cachedSetTimeout = setTimeout;
        return setTimeout(fun, 0);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedSetTimeout(fun, 0);
    } catch(e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
            return cachedSetTimeout.call(null, fun, 0);
        } catch(e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
            return cachedSetTimeout.call(this, fun, 0);
        }
    }


}
function runClearTimeout(marker) {
    if (cachedClearTimeout === clearTimeout) {
        //normal enviroments in sane situations
        return clearTimeout(marker);
    }
    // if clearTimeout wasn't available but was latter defined
    if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
        cachedClearTimeout = clearTimeout;
        return clearTimeout(marker);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedClearTimeout(marker);
    } catch (e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
            return cachedClearTimeout.call(null, marker);
        } catch (e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
            // Some versions of I.E. have different rules for clearTimeout vs setTimeout
            return cachedClearTimeout.call(this, marker);
        }
    }



}
var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
    if (!draining || !currentQueue) {
        return;
    }
    draining = false;
    if (currentQueue.length) {
        queue = currentQueue.concat(queue);
    } else {
        queueIndex = -1;
    }
    if (queue.length) {
        drainQueue();
    }
}

function drainQueue() {
    if (draining) {
        return;
    }
    var timeout = runTimeout(cleanUpNextTick);
    draining = true;

    var len = queue.length;
    while(len) {
        currentQueue = queue;
        queue = [];
        while (++queueIndex < len) {
            if (currentQueue) {
                currentQueue[queueIndex].run();
            }
        }
        queueIndex = -1;
        len = queue.length;
    }
    currentQueue = null;
    draining = false;
    runClearTimeout(timeout);
}

process.nextTick = function (fun) {
    var args = new Array(arguments.length - 1);
    if (arguments.length > 1) {
        for (var i = 1; i < arguments.length; i++) {
            args[i - 1] = arguments[i];
        }
    }
    queue.push(new Item(fun, args));
    if (queue.length === 1 && !draining) {
        runTimeout(drainQueue);
    }
};

// v8 likes predictible objects
function Item(fun, array) {
    this.fun = fun;
    this.array = array;
}
Item.prototype.run = function () {
    this.fun.apply(null, this.array);
};
process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues
process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;
process.prependListener = noop;
process.prependOnceListener = noop;

process.listeners = function (name) { return [] }

process.binding = function (name) {
    throw new Error('process.binding is not supported');
};

process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};
process.umask = function() { return 0; };

},{}]},{},[1]);
