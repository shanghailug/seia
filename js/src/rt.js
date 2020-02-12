// update when update seia-VER.js
VERSION = 1;

function is_nodejs() {
    return (typeof(window._rt.cwd) == 'string');
}

// ensure window._rt exist
if (typeof(window._rt) != 'object') {
    window._rt = {};
}

window._rt.VERSION = VERSION;
window._rt.is_nodejs = is_nodejs;
window._rt.mqtt = require('mqtt');
window._rt.store = require('./store');

// runtime config
if (typeof(window._rt.conf) != 'object') {
    window._rt.conf = {};
}
