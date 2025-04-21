const path = require('path');

module.exports = {
  SOURCE_DIR: path.resolve(__dirname , "../../src"),
  DISTRIBUTION_IDS: {
    "jp-remote-assets": "E23E60KXD46RYC"
  },
  // The files/folders in S3 whose hashes should be added to manifest.json
  MANIFEST_FILES: [ 's3://godel-remote-assets/godel/v1-config.zip', 's3://jp-remote-assets/juspay/' ],
  MANIFEST_URI: "s3://jp-remote-assets/juspay/payments/manifest.json"
}
