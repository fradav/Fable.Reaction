var path = require("path");

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

var babelOptions = {
    presets: [
        ["@babel/preset-env", {
            "targets": {
                "browsers": ["last 2 versions"]
            },
            "modules": false
        }]
    ],
    plugins: ["@babel/plugin-transform-runtime"]
};

var isProduction = process.argv.indexOf("-p") >= 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

module.exports = {
    devtool: "source-map",
    mode: "development",
    entry: "./src/Client/Client.fsproj",
    output: {
        path: path.join(__dirname, "./src/Client/public/js"),
        publicPath: "/js",
        filename: "bundle.js",
    },
    devServer: {
        contentBase: "./src/Client/public",
        port: 8080,
    },
    resolve: {
        symlinks: false,
        modules: [resolve("node_modules/")]
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: {
                    loader: "fable-loader",
                    options: {
                        babel: babelOptions,
                        define: isProduction ? [] : ["DEBUG"]
                    }
                }
            },
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: babelOptions
                },
            }
        ]
    }
}