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
                    var u8a = new Uint8Array(data);
                    // avoid too long, we only need only last part
                    var str = String.fromCharCode.apply(null,
                                                        u8a.subarray(u8a.length - 256,
                                                                     u8a.length));

                    if (new RegExp("\\nh\\$main\\(").test(str)) {
                        proc_data(u8a);
                    }
                    else {
                        throw ("Invalid data");
                    }
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
