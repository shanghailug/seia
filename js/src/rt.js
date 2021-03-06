// update when update seia-SEQ.js
SEQ = 15;

// min seq we can accept
SEQ_MIN = 14;

function is_nodejs() {
    return (typeof(_rt.cwd) == 'string');
}

// ensure _rt exist
if (typeof(_rt) != 'object') {
    _rt = {};
}


_rt.SEQ = SEQ;  // this is RtConf._rt_conf_rt_seq
_rt.SEQ_MIN = SEQ_MIN;
_rt.SEQ_CURR = -1;

_rt.is_nodejs = is_nodejs;

if (!_rt.is_nodejs()) {
    _rt.URL = window.URL;
    _rt.JSON = window.JSON;
    _rt.RTCPeerConnection = window.RTCPeerConnection;
    _rt.XMLHttpRequest = window.XMLHttpRequest;
}

// NOTE: mqtt is set inside ghcjs, or at external exec.js
//_rt.mqtt = require('mqtt');
_rt.store = require('./store');

// runtime config
if (typeof(_rt.conf) != 'object') {
    _rt.conf = {};
}

// default turn server & bootstrap node
_rt.conf.turn_server = _rt.conf.turn_server || [
    "stun:stun.stunprotocol.org"
];

_rt.conf.bootstrap_node = _rt.conf.bootstrap_node || [
    "9dp3gc3r7gngw3xhcs69bwyj3xaa75zodx5huw7rksyjs8in9qyy:0",
    "ufewe4743xiyjygor3h8f4rbgs4b489efh9spdn38c6pns5gyaty:0",
    "d531szhkbbjbd6q716mraz8qamji8akyyu17up8gbe731qtiw66y:0"
];

_rt.conf.mqtt_server = _rt.conf.mqtt_server ||
    "wss://mqtt.eclipse.org/mqtt";

_rt.conf.log_level = _rt.conf.log_level || [
    "E", "W", "I", "D"
];

_rt.conf.service = _rt.conf.service || [];

// default skip benchmakr
if (typeof(_rt.conf.skip_benchmark) == 'undefined') {
    _rt.conf.skip_benchmark = true;
}

if (typeof(_rt.conf.auto_restart) == 'undefined') {
    _rt.conf.auto_restart = false;
}
