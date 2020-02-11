module.exports = (typeof window._rt.cwd == "string") ? require("./store-fs") : require("./store-idb");
