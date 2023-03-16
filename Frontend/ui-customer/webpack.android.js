/*eslint-env node */
const {merge} = require('webpack-merge');
const common = require('./webpack.config.js');
const webpack = require('webpack');
const packageJSON = require("./package.json");
const JusPayJSAZipPlugin = require("jsa-tools").JusPayJSAZipPlugin;
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
  if (argv.mode != "development"){
    plugins.push(new JusPayJSAZipPlugin({
          bundleInputFile: "./dist/" + getOutputFileLocation(argv.mode),
          jsaOutputFile: "./dist/" + getOutputFileDir(argv.mode) + "v1-index_bundle.jsa",
          zipOutputFile: "./dist/" + getOutputFileDir(argv.mode) + "v1-index_bundle.zip",
          keyFile: (typeof (argv.mode) == "string" && (argv.mode == "development")) ?
            "./dev.key" : "/home/jenkins/.juspay/remote_asset_private.key"
    }))
  }
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
      port: 8081
    }
  });
}