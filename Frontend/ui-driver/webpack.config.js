var path = require('path');
var webpack = require('webpack');
var packageJSON = require("./package.json");

module.exports = function(env, argv){
  let plugins = [
    new webpack.DefinePlugin({
      __VERSION__: JSON.stringify(packageJSON.version),
      "window.configEnv": JSON.stringify(env)
    })]
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
  module: {
    rules: [
      {
        test: /\.m?js$/,
        exclude: /(node_modules)/,
        use: {
        loader: 'babel-loader',
        options: {
          presets: ['@babel/preset-env']
        }
        }
      }
    ]
  },
  plugins: plugins,
  performance : {
    hints : false
  },
  devServer: {
    static: {
      directory: path.join(__dirname, 'dist'),
    },
    port: 8083
  },
  watchOptions: {
      aggregateTimeout: 200
  }
  }
  return config;
}
