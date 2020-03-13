/*
1. first, for <version> from _rt.VERSION to 1, if 'version/seia-<version>' exist, load that one
2. if not any 'version/seia-<version>' exist, download from remote,
  argument url will be 'src' of 'seia-preload.js',
  so will fetch 'seia-<version>.js' from same dir
*/

var store = require('./store');

function get_key(ver) {
    return "version/seia-" + ver;
}

function load_xhr(url, cb) {
    var ver = _rt.VERSION;
    var u = new _rt.URL(url);

    var p = u.pathname;
    p = p.replace(RegExp("/[^/]*$"),"");
    u.pathname = p + "/seia-" + ver + ".js";

    var url1 = u.href;

    console.log("fetch seia-%d from %s ...", ver, url1);

    function proc_data(data) {
        var key = get_key(ver);
        store.set(key, data, function(err) {
            if (!err) {
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

function load(url, ver, cb) {
    store.init(function() {
        do_load(url, ver, cb);
    });
}

function do_load(url, ver, cb) {
    var key = get_key(ver);
    console.log("try version %d", ver);

    store.exist(key, function(e) {
        if (e) {
            console.log("version %d exist, load %s, rt version=%d",
                        ver, key, _rt.VERSION);
            cb(key);
        }
        else {
            if (ver > 1) {
                setTimeout(function() { do_load(url, ver - 1, cb); }, 0);
            }
            else {
                load_xhr(url, cb);
            }
        }
    });
}

module.exports.load = load;
