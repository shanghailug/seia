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
