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
