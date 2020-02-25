// update when update seia-VER.js
VERSION = 2;

function is_nodejs() {
    return (typeof(window._rt.cwd) == 'string');
}

// ensure window._rt exist
if (typeof(window._rt) != 'object') {
    window._rt = {};
}


window._rt.VERSION = VERSION;  // this is RtConf._rt_conf_rt_version
window._rt.is_nodejs = is_nodejs;

// NOTE: mqtt is set inside ghcjs, or at external exec.js
//window._rt.mqtt = require('mqtt');
window._rt.store = require('./store');

// runtime config
if (typeof(window._rt.conf) != 'object') {
    window._rt.conf = {};
}

// default turn server & bootstrap node
if (!window._rt.conf.turn_server) {
    window._rt.conf.turn_server = [
        "stun:stun.stunprotocol.org"
    ];
}

if (!window._rt.conf.bootstrap_node) {
    window._rt.conf.bootstrap_node = [
        "9dp3gc3r7gngw3xhcs69bwyj3xaa75zodx5huw7rksyjs8in9qyy:0"
    ];
}
