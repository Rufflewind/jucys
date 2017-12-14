const child_process = require("child_process")

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
            {
                from: "*.html",
                transform: (content, path) =>
                    Buffer.from(content.toString().replace("{{VERSION}}", () =>
                        `r${
                            child_process
                                .execSync("git rev-list --count HEAD")
                                .toString().trim()
                        }-g${
                            child_process
                                .execSync("git rev-parse --short HEAD")
                                .toString().trim()
                        }${
                            child_process
                                .execSync("git status --porcelain")
                                .toString().trim() ? "-dirty" : ""
                        }`)),
            },
            {from: "*.png"},
            {from: "tools/*.css"},
            {from: "tools/*.html"},
        ]),
    ],
    devServer: {
        contentBase: __dirname + "/dist",
    },
}
