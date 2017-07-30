module.exports = {
    stringify: function(root, builder) {
        builder(JSON.stringify(require("postcss-js").objectify(root)))
    }
}
