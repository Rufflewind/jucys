const child_process = require("child_process")
const { CleanWebpackPlugin } = require("clean-webpack-plugin")
const HtmlWebpackPlugin = require("html-webpack-plugin")
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const postcssCssVariables = require("postcss-css-variables")
const webpack = require("webpack")

module.exports = {
    devtool: "source-map",
    entry: {
        "index": "./index.js",
        "test": "./test.js",
        "tools/script": "./tools/index.js",
    },
    output: {
        path: __dirname + "/dist",
        publicPath: "auto",
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                oneOf: [
                    {
                        resourceQuery: /postcss/,
                        use: [
                            "json-loader",
                            {
                                loader: "postcss-loader",
                                options: {
                                    postcssOptions: {
                                        stringifier: "../../../stringify",
                                        plugins: [postcssCssVariables],
                                    },
                                },
                            },
                        ],
                    },
                    {
                        use: [
                            MiniCssExtractPlugin.loader,
                            "css-loader",
                        ],
                    }
                ],
            },
            {
                test: /\.js$/,
                use: ["source-map-loader"],
            },
            {
                test: /\.md$/,
                use: [
                    "html-loader",
                    {
                        loader: "shell-loader",
                        options: {
                            script: "pandoc -s -f markdown_strict",
                        },
                    },
                ],
            },
            {
                test: /\.png$/,
                use: ["file-loader"],
            },
            {
                test: /\.purs$/,
                use: [
                    {
                        loader: "purs-loader",
                        options: {
                            src: [
                                "tools/src/**/*.purs",
                            ],
                            psc: "psa",
                            pscArgs: {"censor-stats": true},
                            spago: true,
                            watch: true,
                        },
                    },
                ],
            },
        ]
    },
    performance: {
        hints: false,
    },
    plugins: [
        new CleanWebpackPlugin(),
        new webpack.DefinePlugin({
            VERSION:
                JSON.stringify(`r${
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
                }`),
        }),
        new HtmlWebpackPlugin({
            filename: "help.html",
            template: "README.md",
            chunks: [],
        }),
        new HtmlWebpackPlugin({
            template: "index.html",
            chunks: ["index"],
            xhtml: true,
        }),
        new HtmlWebpackPlugin({
            filename: "test.html",
            template: "test.html",
            chunks: ["test"],
            xhtml: true,
        }),
        new HtmlWebpackPlugin({
            filename: "tools/index.html",
            template: "tools/index.html",
            chunks: ["tools/script"],
            xhtml: true,
        }),
        new MiniCssExtractPlugin(),
    ],
    devServer: {
        static: __dirname + "/dist",
    },
    watchOptions: {
        ignored: /\.#/,
    },
}
