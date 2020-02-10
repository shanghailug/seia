var SEIA_VERSION = 1;

require('fast-text-encoding');

var rt = require('./rt');

if (typeof(window._rt) == 'object') {
    Object.assign(window._rt, rt);
}
else {
    window._rt = rt;
}

var loader = require('./loader');

loader.load(document.location.href, SEIA_VERSION,
            function (key) {
                if (typeof require == 'function') {
                    var path = rt.store.get_path(key);
                    require(path);
                }
                else {
                    rt.store.get(key, function(err, res) {
                        if (res) eval(res);
                        else console.log(err);
                    });
                }
            });
