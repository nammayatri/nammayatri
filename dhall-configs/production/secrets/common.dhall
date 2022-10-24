let globalCommon = ../../generic/common.dhall

let exotelCfg =
  { apiKey = "xxxxxxx"
  , apiToken = "xxxxxxx"
  , sid = "xxxxxxx"
  , callerId = "xxxxxxx"
  } : globalCommon.ExotelCfg

let s3Config1 =
  { secretAccessKey = "xxxxxxx"
  , accessKeyId = "xxxxxxx"
  , bucketName = "xxxxxxx"
  , region = "xxxxxxx"
  , pathPrefix = "xxxxxxx"
  }
let s3Config = globalCommon.S3Config.S3AwsConf s3Config1

let idfyCfg =
  { account_id = "xxxxxxx",
    api_key = "xxxxxxx",
    secret = "xxxxxxx",
    url = "http://localhost:6235"
  }

in

{ smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
, exotelCfg = exotelCfg
, s3Config = s3Config
, idfyCfg = idfyCfg
, googleMapsKey = ""
, slackToken = "xxxxxxx"
}
