const fs = require('fs-extra');
const jsaTools = require('jsa-tools');
const AWS = require('aws-sdk');

const { SOURCE_DIR, DISTRIBUTION_IDS } = require('./constants')
const Util = require('./util')
const manifestGen = require('./manifestGen')
const remoteAssets = require('../../../remoteAssets.json');


const remoteAssetSync = async function() {
  let args = Array.from(process.argv.slice(2)).filter(a => !a.match(/^--/));


  // Get APP ID
  const appId = args[0];
  args = args.slice(1);

  if(!appId) {
    throw new Error("Missing APP ID")
  }
  if(!remoteAssets[appId]) {
    throw new Error(`Could not find APP ID ${appId} in remoteAssets.json`);
  }



  // Build a list of assets from remoteAssets.json
  const assets = [];
  while(true) {
    const file = args[0];
    if(remoteAssets[appId][file]) {
      assets.push(remoteAssets[appId][file]);
      args = args.slice(1);
    } else {
      break;
    }
  }

  if(assets.length == 0) {
    throw new Error(`You did not pass any valid files for APP ID ${appId} (make sure it exists in remoteAssets.json)`);
  }



  // Check if beta/release folder
  const folder = args[0];
  args = args.slice(1);

  if(folder != "beta" && folder != "release") {
    throw new Error("Please mention the folder (release/beta)");
  }

  const beta = folder !== "release";



  // Get remotesVersion (can be null)
  const remotesVersion = args[0];
  args = args.slice(1);



  //temporarily disallow pushing hyper config
  if(appId == "in.juspay.hyperos") {
    throw new Error("This script is still experimental. Don't push hyper config using this till you're sure about it");
  }

  //temporarily disallow pushing release assets
  if(beta == false) {
    throw new Error("This script is still experimental. Don't push assets to release until you're sure about it");
  }



  // Build `items` and `invalidationItems` arrays from the list of assets
  // These arrays will go as inputs into `jsaTools.remoteAssetSync()` function
  const items = [];
  const invalidationItems = [];

  for(const asset of assets) {
    if(asset.path.match("{{remotes_version}}") && !remotesVersion) {
      throw new Error("Please pass the remotes version when pushing non-config files");
    }

    const source = asset.source;
    let path = asset.path
      .replace("{{folder}}", folder)
      .replace("{{remotes_version}}", remotesVersion);

    if(appId == "in.juspay.hyperos" && file == "config.js" && folder == "release") {
      path = path.replace("/release", "");
    }

    const { bucket, key } = Util.getBucketAndKey(path);
    const invalidationPath = Util.isJsFile(source) ? Util.replaceFilename(key, "v1-$1.zip") : Util.replaceFilename(key, "$1.zip")

    const item = {
      uri: path,
      data: fs.createReadStream(`${SOURCE_DIR}/${source}`),
      createJsa: Util.isJsFile(source),
      minify: !Util.isBundle(source),
      id: source
    }

    const invalidationItem = {
      distributionId: DISTRIBUTION_IDS[Util.getBucketAndKey(path).bucket],
      path: "/" + invalidationPath
    }

    items.push(item);
    invalidationItems.push(invalidationItem);
  }




  console.log("\n\n==============REMOTE ASSET SYNC=============\n\n");

  console.log(`APP ID:\t${appId}`);
  console.log(`FILES:\t${assets.map(a => a.source).join(", ")}`);
  console.log(`FOLDER:\t${folder}`);
  if(remotesVersion) console.log(`Remotes Version: ${remotesVersion}\n`);

  if(process.argv.indexOf('-f') == -1) {
    const response = await Util.ask('\nAre you sure you want to continue?: ');
    if(!response.match(/y(es)?/i)) {
      console.log("Aborted...");
      process.exit(0);
    }
  }

  const signingKey = await Util.getRAKey();
  const creds = await Util.getAWSCredentials();
  const options = { invalidationItems, logs: true }

  // Testing mode. Writes files to /tmp instead of actually uploading to S3
  // Util.testMode(AWS); options.AWS = AWS;

  const data = await jsaTools.remoteAssetSync(items, signingKey, creds, options);




  // Generate and update manifest.json
  await manifestGen(creds);




  console.log("\n\n================SENDING EMAIL===============\n\n");

  const smtpConfig = await Util.getEmailConfig();
  const from = "support@juspay.in";
  const to = "godel@juspay.in";
  const replyTo =	["godel@juspay.in", "payments@juspay.in", "godel-qa@juspay.in"];
  const cc = replyTo;
  let subject = `Remote assets have been updated for the Payments SDK`;
  if(remotesVersion) {
    subject += ` with remote version: ${remotesVersion}`;
  }
  const text = await emailBody(appId, beta, remotesVersion, data.eTags);

  const mail = { from, to, replyTo, cc, subject, text };
  await Util.sendEmail(smtpConfig, mail);


  console.log("\n\n===========REMOTE ASSET SYNC DONE===========\n\n");
}


const emailBody = async function(appId, beta, remotesVersion, hashes) {
  const gitHash = await Util.lastGitHash()
  const commitMessage = await Util.lastGitCommitMessage()
  let hashForMail = "";

  for(file in hashes) {
    if(file.match(/\.zip$/)) {
      hashForMail += `    ${Util.getFilename(file)} - ${hashes[file]}\n`;
    }
  }

  const body =
`Hello,

Remote assets updated${remotesVersion ? ` for the version: ${remotesVersion}` : ''}.

Context:
Remote Version: ${remotesVersion ? remotesVersion : '-'}
commit(hyper-sdk-android): ${gitHash}
Changelog: ${commitMessage}
appId: ${appId}
Path: ${beta ? "beta" : "release"}
Hashes: ${hashForMail}

Thanks,
Team Payments
`
  return body;
}




if(process.argv.indexOf('--help') > -1) {
  const help = `Format:
    remoteAssetSync --help
    remoteAssetSync [--debug] [-f] app_id file1 [...fileN] (beta|release) [remotes_version]

Examples:
To push index_bundle.js and config.js for in.juspay.example to /beta/1.0rc10:
    remoteAssetSync in.juspay.example config.js index_bundle.js beta 1.0rc10

To push config.js for in.juspay.example to /release (remotes version isn't required for config.js alone)
    remoteAssetSync in.juspay.example config.js release

Use the '-f' flag to skip confirmation (not recommended)
  `
  console.log("\n\n");
  console.log(help);
  process.exit(0);
}


const debug = process.argv.indexOf('--debug') > -1;

remoteAssetSync()
  .catch(e => {
    console.log("\n\n");
    e = new Error("Error: " + e.message);
    debug ? console.error(e) : console.error(e.message);

    console.log("\n");
    if(!debug) console.log("Use the --debug flag to see the complete stack trace");
    console.log("Use the --help flag for help");
    console.log("\n\n");

    process.exit(1);
  });
