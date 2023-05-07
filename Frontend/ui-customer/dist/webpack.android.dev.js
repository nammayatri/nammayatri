"use strict";

/*eslint-env node */
var _require = require('webpack-merge'),
    merge = _require.merge;

var common = require('./webpack.config.js');

var webpack = require('webpack');

var packageJSON = require("./package.json");

var path = require('path');

var getOutputFileDir = function getOutputFileDir(mode) {
  return mode == "development" ? "" : "android/";
};

var outputFileName = "index_bundle.js";

var getOutputFileLocation = function getOutputFileLocation(mode) {
  return getOutputFileDir(mode) + outputFileName;
};

var BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;

module.exports = function (env, argv) {
  var plugins = [new webpack.DefinePlugin({
    "window.__OS": JSON.stringify("ANDROID"),
    __VERSION__: JSON.stringify(packageJSON.version),
    "window.configEnv": JSON.stringify(env)
  }), new BundleAnalyzerPlugin({
    analyzerMode: 'disabled',
    generateStatsFile: true,
    statsOptions: {
      source: false
    }
  })];
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
};