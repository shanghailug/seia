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
    var ver = window._rt.VERSION;
    var u = new window.URL(url);

    var p = u.pathname;
    p = p.replace(RegExp("/[^/]*$"),"");
    u.pathname = p + "/seia-" + ver + ".js";

    var url1 = u.href;

    console.log("fetch seia-%d from %s ...", ver, url1);

    // TODO
}

function load(url, ver, cb) {
    var key = get_key(ver);
    console.log("try version %d", ver);

    store.exist(key, function(e) {
        if (e) {
            console.log("version %d exist, load %s, rt version=%d", ver, key,
                        window._rt.VERSION);
            cb(key);
        }
        else {
            if (ver > 1) {
                setTimeout(function() { load(url, ver - 1, cb); }, 0);
            }
            else {
                load_xhr(url, cb);
            }
        }
    });
}

module.exports.load = load;
