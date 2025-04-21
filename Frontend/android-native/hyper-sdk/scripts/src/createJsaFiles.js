const fs = require('fs-extra');
const jsaTools = require('jsa-tools');
const pipe = require('multipipe');

const { SOURCE_DIR } = require('./constants.js');
const Util = require('./util');

const createJsaFiles = async function() {
  console.log("\n\n==============CREATE JSA FILES==============\n\n");

  // delete all existing .jsa files
  for(jsaFile of (await Util.findJsaFiles(SOURCE_DIR))) {
    await Util.deleteFile(`${SOURCE_DIR}/${jsaFile}`);
  }

  // find all jsa files in source directory and generate their corresponding .jsa file paths
  const jsFiles = await Util.findJsFiles(SOURCE_DIR);
  const getJsaPath = file => Util.replaceFilename(file, "v1-$1.jsa").replace("/js/", "/assets/");

  // create JSA for each file
  const jsaTasks = [];
  for(file of jsFiles) {
    const outputPath = `${SOURCE_DIR}/${getJsaPath(file)}`;
    await fs.ensureFile(outputPath);

    const readStream = fs.createReadStream(`${SOURCE_DIR}/${file}`);
    const writeStream = fs.createWriteStream(outputPath);
    const jsaStream = jsaTools.createJsa({ minify: !Util.isBundle(file), logs: true, id: file });
    pipe(readStream, jsaStream, writeStream);
    jsaTasks.push(Util.awaitStream(writeStream));
  }

  await Promise.all(jsaTasks);

  console.log("\n\n==============CREATE JSA DONE===============\n\n");
}





createJsaFiles()
  .catch(e => {
    console.error(e);
    process.exit(1);
  });
