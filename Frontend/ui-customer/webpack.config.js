var path = require('path');
var webpack = require('webpack');

module.exports = function(env){
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
        exclude: /(node_modules|bower_components)/,
        use: {
        loader: 'babel-loader',
        options: {
          presets: ['@babel/preset-env']
        }
        }
      }
    ]
  },
  performance : {
    hints : false
  },
  devServer: {
    contentBase: path.join(__dirname, 'dist'),
    host: "0.0.0.0",
    inline: false,
    port: 8080
  },
  optimization: {

   usedExports: true,

  },
  }
  return config;
}
