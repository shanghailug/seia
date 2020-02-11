#!/usr/bin/env node

const wrtc = require('wrtc');
const fdom = require('./fdom');
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

var doc = new fdom.Document();
doc.location.href = href;
doc.location.reload = function() {
    console.log("reload");
};
doc.currentScript = { herf: href };

var win = doc.defaultView;
win.RTCPeerConnection = wrtc.RTCPeerConnection;
win.console = console;
win.XMLHttpRequest = xhr2.XMLHttpRequest;
win.URL = url.URL;

// cwd
win._rt = { cwd: process.cwd() };

global.document = doc;
global.window = win;


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

