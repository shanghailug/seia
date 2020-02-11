module.exports = {}


if ("indexedDB" in window) {
    console.log("use indexedDB for '_rt.store'");
} else {
    throw "indexedDB not support";
}


function exist(key, cb) {
    cb(false);
}

module.exports.exist = exist;
