const AWS = require('aws-sdk');
const jsaTools = require('jsa-tools');

const Util = require('./util');
const { DISTRIBUTION_IDS, MANIFEST_URI, MANIFEST_FILES } = require('./constants');

const manifestGen = async function(creds) {
  console.log("\n\n===========GENERATING manifest.json=========\n\n");

  const manifestInvalidationItem = {
    distributionId: DISTRIBUTION_IDS[Util.getBucketAndKey(MANIFEST_URI).bucket],
    path: "/" + Util.getBucketAndKey(MANIFEST_URI).key
  }

  const options = { logs: true }

  // Test Mode. Write manifest.json to /tmp instead of actally uploading to S3
  // Util.testMode(AWS); options.AWS = AWS;

  await jsaTools.updateManifest(MANIFEST_URI, manifestInvalidationItem, MANIFEST_FILES, creds, options);
}

if (require.main === module) {
  Util.getAWSCredentials()
    .then(creds => manifestGen(creds))
    .catch(() => {
      console.error(err);
      process.exit(1);
    });
} else {
  module.exports = manifestGen;
}
