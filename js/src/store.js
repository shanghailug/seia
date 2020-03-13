module.exports = (typeof _rt.cwd == "string") ? require("./store-fs") : require("./store-idb");
