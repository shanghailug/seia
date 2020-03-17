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
