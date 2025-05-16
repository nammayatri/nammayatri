var path = require('path');
var webpack = require('webpack');
var packageJSON = require("./package.json");

module.exports = function (env, argv) {
  let plugins = [
    new webpack.DefinePlugin({
      __VERSION__: JSON.stringify(packageJSON.version),
      "window.configEnv": JSON.stringify(env)
    }),
  ]
  let config = {
    mode: "development",
    entry: {
      app: "./index.js"
    },
    output: {
      path: __dirname + "/dist",
      filename: "index_bundle.js",
      publicPath: '/dist/',
      sourceMapFilename: "index_bundle.map"
    },
    plugins: plugins,
    performance: {
      hints: false
    },
    // module: {
    //   rules: [{
    //     test: /\.js$/,
    //     use: {
    //       loader: './ps-lazy-loader.js',
    //     }
    //   }]
    // },
    devServer: {
      contentBase: path.join(__dirname, 'dist'),
      host: "0.0.0.0",
      inline: false,
      port: 8083
    },
    watchOptions: {
      aggregateTimeout: 200
    },
    optimization: {
      usedExports: true,
    }
  }
  return config;
}
