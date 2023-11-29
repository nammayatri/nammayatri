/*eslint-env node */
const {merge} = require('webpack-merge');
const common = require('./webpack.config.js');
const webpack = require('webpack');
const packageJSON = require("./package.json");
var path = require('path');

const getOutputFileDir = (mode) => mode ==  "development" ? "" :"android/";
const outputFileName = "index_bundle.js";
const getOutputFileLocation = (mode) => getOutputFileDir(mode) + outputFileName;


module.exports = (env, argv) => {
  let plugins = [
      new webpack.DefinePlugin({
        "window.__OS": JSON.stringify("ANDROID"),
        __VERSION__: JSON.stringify(packageJSON.version),
        "window.configEnv": JSON.stringify(env)
      })]
  return merge(common(env), {
    output: {
      filename: getOutputFileLocation(argv.mode),
      sourceMapFilename: getOutputFileLocation(argv.mode) + ".map"
    },
    plugins: plugins,
    devServer: {
      static: {
        directory: path.join(__dirname, 'dist'),
      },
      port: 8081
    }
  });
}