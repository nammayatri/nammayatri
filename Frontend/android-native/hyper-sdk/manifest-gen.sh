#!/usr/bin/env bash

function traverse() {
  dirList=($(ls $1))
  for file in ${dirList[@]}
  do
    if [[ -f ${1}/${file} ]] ; then
      md5Value=$(cat ${1}/$file | md5 | awk '{ print $1 }');
      outputJson+="\"${file}\":{\"hash\":\"$md5Value\"}"
    elif [[ -d ${1}/${file} ]]
    then
      printf "."
      outputJson+="\"${file}\":{";
      traverse "${1}/${file}" ;
      outputJson+="}"
    fi
  done
}

printf "Downloading latest assets...\\n"
aws s3 cp s3://jp-remote-assets/juspay/ /tmp/archive/juspay --recursive --exclude "*" --include "*.zip"
aws s3 cp s3://jp-remote-assets/ec-upi-sdk/beta/v1-config.zip /tmp/archive/ec-upi-sdk/beta/v1-config.zip
aws s3 cp s3://jp-remote-assets/ec-upi-sdk/release/v1-config.zip /tmp/archive/ec-upi-sdk/release/v1-config.zip
aws s3 cp s3://godel-remote-assets/godel/v1-config.zip /tmp/archive/godel/v1-config.zip
aws s3 cp s3://godel-remote-assets/0.6rc9/certificates_v1.zip /tmp/archive/0.6rc9/certificates_v1.zip

# printf "\\nfetching all non-jsa and non-js files...\\n"
# find /tmp/archive -name "*.jsa" | xargs rm
# find /tmp/archive -name "*.js"  | xargs rm
# files=$(find /tmp/archive -type f ! -name "*.jsa" ! -name "*.js" ! -name "*.png")
cat /dev/null > manifest.json

printf "Generating Manifest..."

outputJson="{";
traverse "/tmp/archive"
outputJson+="}";
outputJson=$(echo $outputJson | sed 's/}"/},"/g')

printf "Done\\n"

echo "Writing to manifest.json"
echo $outputJson > manifest.json

echo "Uploading to s3://jp-remote-assets. Path: /juspay/payments/manifest.json"
aws s3 cp manifest.json s3://jp-remote-assets/juspay/payments/manifest.json --acl public-read --cache-control "private, no-cache, no-store, must-revalidate, max-age=0, proxy-revalidate, s-maxage=0"
echo "Invalidating in cloudfront. Distribution ID: E23E60KXD46RYC"
aws cloudfront create-invalidation --distribution-id E23E60KXD46RYC --paths "/juspay/payments/manifest.json"

if [[ "$1" = "release" ]];
    then
        echo "Uploading to s3://jp-remote-assets. Path: /juspay/payments/release/manifest.json"
        aws s3 cp manifest.json s3://jp-remote-assets/juspay/payments/release/manifest.json --acl public-read --cache-control "private, no-cache, no-store, must-revalidate, max-age=0, proxy-revalidate, s-maxage=0"
        echo "Invalidating in cloudfront. Distribution ID: E23E60KXD46RYC"
        aws cloudfront create-invalidation --distribution-id E23E60KXD46RYC --paths "/juspay/payments/release/manifest.json"
    else
        echo "Uploading to s3://jp-remote-assets. Path: /juspay/payments/beta/manifest.json"
        aws s3 cp manifest.json s3://jp-remote-assets/juspay/payments/beta/manifest.json --acl public-read --cache-control "private, no-cache, no-store, must-revalidate, max-age=0, proxy-revalidate, s-maxage=0"
        echo "Invalidating in cloudfront. Distribution ID: E23E60KXD46RYC"
        aws cloudfront create-invalidation --distribution-id E23E60KXD46RYC --paths "/juspay/payments/beta/manifest.json"
fi

echo "Invalidated"
echo "Done and Dusted"