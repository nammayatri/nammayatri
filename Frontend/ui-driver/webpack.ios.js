/*eslint-env node */
const {merge} = require('webpack-merge');
const common = require('./webpack.config.js');
const webpack = require('webpack');
var path = require('path');
const packageJSON = require("./package.json");
// const JusPayJSAZipPlugin = require("jsa-tools").JusPayJSAZipPlugin;


const devtoolsPath = "../devtools-ios"

const getOutputFileDir = (mode) => mode ==  "development" ? "" :"ios/";
const getOutputFileName =  (mode) => "v1-index_bundle.js";
const getOutputFileLocation = (mode) => getOutputFileDir(mode) + getOutputFileName(mode);

module.exports = (env, argv) => {
  let plugins = [
      new webpack.DefinePlugin({
        'window.__OS': JSON.stringify("IOS"),
        __VERSION__: JSON.stringify(packageJSON.version),
        "window.configEnv": JSON.stringify(env)
      })
    ]
  return merge(common(env), {
    output: {
      filename: getOutputFileLocation(argv.mode),
      sourceMapFilename: getOutputFileLocation(argv.mode) + ".map"
    },
    plugins: plugins,
    devServer: {
      contentBase: path.join(__dirname, 'dist'),
      host: "0.0.0.0",
      inline: false,
      port: 8084
    }
  });
}