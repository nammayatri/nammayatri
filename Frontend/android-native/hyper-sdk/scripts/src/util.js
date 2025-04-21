const glob = require('glob');
const fs = require('fs-extra');
const execa = require('execa');
const readline = require('readline');
const nodemailer = require('nodemailer');
const AWS = require('aws-sdk');
const InputObjectStream = require('java.io').InputObjectStream;


const REMOTE_ASSETS_PRIVATE_KEY_FILE = process.env.HOME + "/.juspay/remote_asset_private.key";
const S3_CREDENTIALS_FILE = process.env.HOME + "/.juspay/aws.properties"
const EMAIL_CONFIG_FILE = process.env.HOME + "/.juspay/config.groovy";

const ask = question => new Promise((resolve, reject) => {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  rl.question(question, answer => {
    rl.close();
    resolve(answer);
  });
})

const isDirectory = async function(dir) {
  try {
    return (await fs.stat(dir)).isDirectory();
  } catch(e) {
    return false;
  }
}

const globPromise = function(dir, globString) {
  return new Promise(function(resolve, reject) {
    glob(globString, { cwd: dir }, function(err, files) {
      if(err) {
        reject(err)
      } else {
        resolve(files);
      }
    });
  });
}

const findJsFiles = function(dir) {
  return globPromise(dir, "**/*.js")
}

const findJsaFiles = function(dir) {
  return globPromise(dir, "**/*.jsa")
}

const deleteFile = function(file) {
  return fs.remove(file);
}

const getFilename = function(file) {
  return file.substring(file.lastIndexOf('/') + 1)
}

const replaceFilename = function(filename, replacement) {
  return filename.replace(/([^/]+)\.[^.]+$/, replacement);
}

// Split S3 URI into bucket and key
const getBucketAndKey = function(uri) {
  const [matched, bucket, key] = uri.match(/^s3:\/\/([^/]+)\/(.*)/) || [];
  return { bucket, key }
}

// Convenience function to make a Promise out of a stream
const awaitStream = function(stream) {
  return new Promise(function(resolve, reject) {
    stream.on('error', reject).on('finish', resolve);
  });
}

const isJsFile = file => file.match(/\.js$/) != null

const isBundle = file => file.match(/bundle/) != null

const getRAKey = async function() {
  const fileContents = await fs.readFile(REMOTE_ASSETS_PRIVATE_KEY_FILE);
  const keyObj = new InputObjectStream(fileContents, true).readObject();
  const keyBytes = keyObj.$.encoded.$;
  const keyString = "-----BEGIN PRIVATE KEY-----\n"
    + keyBytes.toString('base64')
    + "\n-----END PRIVATE KEY-----";

  return keyString;
}

const getAWSCredentials = async function() {
  const fileContents = await fs.readFile(S3_CREDENTIALS_FILE, { encoding: 'utf8' });
  const accessKey = (fileContents.match(/accessKey[\s]*=[\s]*([^\s]+)/) || [])[1];
  const secretKey = (fileContents.match(/secretKey[\s]*=[\s]*([^\s]+)/) || [])[1];

  if(!accessKey || !secretKey) {
    throw new Error("Failed to extract AWS access key and secret key from " + S3_CREDENTIALS_FILE);
  }

  return new AWS.Credentials(accessKey, secretKey);
}


const getEmailConfig = async function() {
  const emailConfig = (await fs.readFile(EMAIL_CONFIG_FILE)).toString();

  const getValue = name => emailConfig.match(new RegExp(`${name}[\\s]*=[\\s]*"(.*)"`))[1];

  return {
    host: getValue('host'),
    port: getValue('port'),
    username: getValue('username'),
    password: getValue('password')
  }
}


const sendEmail = async function(smtp, mail) {
  const transporter = nodemailer.createTransport({
    host: smtp.host,
    port: smtp.port,
    requireTLS: true,
    auth: {
      user: smtp.username,
      pass: smtp.password
    }
  });

  await transporter.sendMail(mail);
}

const lastGitHash = async function() {
  return (await execa('git', ['rev-parse', '--short', 'HEAD'])).stdout;
}

const lastGitCommitMessage = async function() {
  return (await execa('git', ['log', '-1', '--pretty=%B'])).stdout;
}


// Overrides some methods in AWS.S3 so that files are written to /tmp instead of actually uploading to S3
const testMode = (AWSObj) => {
  const upload = AWSObj.S3.prototype.upload;
  AWSObj.S3.prototype.upload = function(options, callback) {
    options.Key = "ras_test/" + options.Key;
    const path = `/tmp/${options.Bucket}/${options.Key}`;

    fs.ensureFile(path).then(() => {
      const writeStream = fs.createWriteStream(path);
      options.Body
        .on('error', callback)
        .pipe(writeStream)
        .on('error', callback)
        .on('finish', () => callback(null, { ETag: "\"Look at me, I'm a hash\"", Location: "http://beesbeesbees.com" }))
    });
  }

  const createInvalidation = AWSObj.CloudFront.prototype.createInvalidation;
  AWSObj.CloudFront.prototype.createInvalidation = function(options, callback) {
    options.InvalidationBatch.Paths.Items = options.InvalidationBatch.Paths.Items.map(i => `ras_test/${i}`);
    const data = { Invalidation: { Id: "fakeId", InvalidationBatch: options.InvalidationBatch } }
    callback(null, data);
  }
}

module.exports = {
  ask,
  isDirectory,
  findJsFiles,
  findJsaFiles,
  deleteFile,
  getFilename,
  replaceFilename,
  getBucketAndKey,
  awaitStream,
  getBucketAndKey,
  isJsFile,
  isBundle,
  getRAKey,
  getAWSCredentials,
  getEmailConfig,
  sendEmail,
  lastGitHash,
  lastGitCommitMessage,
  testMode
}
