var fs = require('fs');
var zlib = require('zlib');
var crypto = require('crypto');

function readByteArray(inputPath, callback) {
	fs.readFile(inputPath, function read(err, data) {
		callback(err, data);
	});
}

function writeByteArray(outputPath, content, callback) {
	fs.writeFile(outputPath, content, 'base64', function written(err) {
		callback(err);
	});
}


function gzipByteArray(data, callback) {
	zlib.gzip(data, function (error, result) {
   	callback(error, result);
	})
}

function compressAndEncryptContent(content, callback) {
	var FRAME_SIZE = 8;
  var KEY_SIZE = 8;
  
  var randomKey = new Buffer([10,11,12,13,14,15,16,17]);

	gzipByteArray(content, function(err, compressed) {
		if (err) callback(err, null);
		var totalBytes = compressed.length;
	  var encodedBytes = new Buffer(totalBytes+KEY_SIZE);
	  encodedBytes.fill(0);
	  var hiddenCounter = 0;
	  var encodedCounter = 0;
	  
	  for(var compressedCounter = 0; compressedCounter < totalBytes && encodedCounter < totalBytes + KEY_SIZE; encodedCounter++) {
	      if(encodedCounter > 0 && encodedCounter % 10 == 9 && hiddenCounter < KEY_SIZE) {
	          encodedBytes[encodedCounter] = randomKey[hiddenCounter];
	          hiddenCounter++;
	      }else{
	          encodedBytes[encodedCounter] = ((parseInt(compressed[compressedCounter]) ^ parseInt(randomKey[compressedCounter%FRAME_SIZE])));
	          compressedCounter++;
	      }
	  }

		callback(null, encodedBytes);
	});
}

function start() {
	var inputPath = './dist/v1-index_bundle.js';
	var outputPath = './dist/v1-index_bundle.jsa';

	readByteArray(inputPath, function(err, content) {
		if (err) return console.log(err);
		compressAndEncryptContent(content, function(err, encrypted) {
			if (err) return console.log(err);
			writeByteArray(outputPath, encrypted, function(err) {
				if (err) return console.log(err);
				console.log("Encryption: Done and Dusted");
			});
		});
	})
}

start();
