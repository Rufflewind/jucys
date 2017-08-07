module.exports = {
    devtool: "source-map",
    entry: {
        "script": "./script.js",
        "tools/script": "./tools/src/Main.purs"
    },
    output: {
        path: __dirname + "/dist",
        filename: "[name].js",
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                use: ["source-map-loader"],
            },
            {
                test: /\.css$/,
                use: [
                    "json-loader",
                    {
                        loader: "postcss-loader",
                        options: {
                            stringifier: "../../../stringify",
                            plugins: [require("postcss-cssnext")]
                        },
                    },
                ],
            },
            {
                test: /\.purs$/,
                use: [
                    {
                        loader: "purs-loader",
                        options: {
                            src: [
                                "tools/bower_components/purescript-*/src/**/*.purs",
                                "tools/src/**/*.purs",
                            ],
                            output: "tools/output",
                            watch: true,
                        },
                    },
                ],
            },
        ]
    },
    plugins: [
        new (require("copy-webpack-plugin"))([
            {from: "*.css"},
            {from: "*.html"},
            {from: "*.png"},
            {from: "tools/*.css"},
            {from: "tools/*.html"},
        ]),
    ],
    devServer: {
        contentBase: __dirname + "/dist",
    },
}
