module.exports = {
    devtool: "source-map",
    entry: "./script.js",
    output: {
        path: __dirname + "/dist",
        filename: "script.js",
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                    "json-loader",
                    {
                        loader: "postcss-loader",
                        options: {
                            stringifier: "../../../stringify",
                            plugins: [require("postcss-cssnext")]
                        }
                    }
                ]
            }
        ]
    },
    plugins: [
        new (require("copy-webpack-plugin"))([
            {from: "*.css"},
            {from: "*.html"},
            {from: "*.png"}
        ])
    ],
    devServer: {
        contentBase: __dirname + "/dist",
    }
}
