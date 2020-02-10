// is href not http*://, then find seia.js at same level, append version
var store = require('./store');

function load(url, ver, cb) {
    key = "version/seia-" + ver;

    store.exist(key, function(e) {
        if (e) {
            cb(key);
        }
        else {
            throw "TODO, fetch, save, then call 'cb(key)'";
        }
    });
}

module.exports.load = load;
