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
        exclude: /(node_modules|bower_components)\/?!(presto-react)/, 
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
