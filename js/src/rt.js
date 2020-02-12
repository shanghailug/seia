// update when update seia-VER.js
module.exports.VERSION = 1;

function is_nodejs() {
    return (typeof(window._rt.cwd) == 'string');
}


module.exports.is_nodejs = is_nodejs;
module.exports.mqtt  = require('mqtt');
module.exports.store = require('./store');
