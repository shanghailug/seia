require('fast-text-encoding');

// ensure window._rt exist
if (typeof(window._rt) != 'object') {
    window._rt = {};
}

var rt = require('./rt');

Object.assign(window._rt, rt);

var loader = require('./loader');

var url = document.location.href;
if ((document.currentScript != null) &&
    (document.currentScript.src != null)) {
    url = document.currentScript.src;
}

loader.load(url, rt.VERSION,
            function (key) {
                if (rt.is_nodejs()) {
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
