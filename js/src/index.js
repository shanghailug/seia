function u8a_to_utf8(a)
{
    if (window.TextDecoder) {
        var d = new window.TextDecoder();
        return d.decode(a);
    }
    else {
        // TODO: for IE11, need compatiable
        throw "TextDecoder not exist";
    }
}

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
