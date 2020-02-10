#!/usr/bin/env node

const wrtc = require('wrtc');
const fdom = require('./fdom');
const fs = require('fs');

var p_path = process.argv[2];

console.log("href: %s", p_path);

console.log("load done");

global.document = new fdom.Document;
global.window = global.document.defaultView;
global.document.location.href = p_path;
global.document.location.reload = function() {
    console.log("reload");
};
global.RTCPeerConnection = wrtc.RTCPeerConnection;

// for node
global.window.cwd = process.cwd();

var preload = fs.readFileSync(p_path, 'utf-8');
eval(preload);

