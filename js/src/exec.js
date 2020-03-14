#!/usr/bin/env node

const wrtc = require('wrtc');
const fs = require('fs');
const xhr2 = require('xhr2');
const path = require('path');
const url = require('url');

var p_path = process.argv[2];

var href;

if (/[a-z]+:/.test(p_path)) {
    href = p_path;
}
else {
    href = "file://" + path.resolve(p_path);
}

console.log("href: %s", href);

_rt = {};
_rt.href = href;

_rt.RTCPeerConnection = wrtc.RTCPeerConnection;
_rt.XMLHttpRequest = xhr2.XMLHttpRequest;

_rt.URL = url.URL;
_rt.JSON = JSON;

// cwd
_rt.cwd = process.cwd();
_rt.sid = 0; // TODO from args

//_rt.rust_crypto_ed25519 = require("./rust_crypto_ed25519");

_rt.mqtt = require("mqtt");

if (/^file:/.test(href)) {
    console.log("load local file...");
    var preload = fs.readFileSync(p_path, 'utf-8');
    eval(preload);
}
else if (/^https?:/.test(href)) {
    console.log("load remote url...");
    
    var req = new xhr2.XMLHttpRequest();
    req.open("GET", href);
    req.responseType = 'text';
    req.onreadystatechange = function() {
        console.log("HTTP(s): ready state = %d, status = %d",
                    req.readyState, req.status);
        if (req.readyState === 4) {
            if (req.status === 200) {
                var preload = req.responseText;
                eval(preload);
            }
            else {
                throw ("GET fail, status=" + req.status);
            }
        }
    }

    req.send();
}
else {
    throw ("unsupport url: " + href);
}

