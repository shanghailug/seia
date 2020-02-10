module.exports = (typeof window.cwd == "string") ? require("./store-fs") : require("./store-idb");
