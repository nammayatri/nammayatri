let globalCommon = ../../generic/common.dhall

let exotelCfg =
  { apiKey = "xxxxxxx"
  , apiToken = "xxxxxxx"
  , sid = "xxxxxxx"
  , callerId = "xxxxxxx"
  }

let s3Config =
  { secretAccessKey = "xxxxxxx"
  , accessKeyId = "xxxxxxx"
  , bucketName = "xxxxxxx"
  , region = "xxxxxxx"
  }

in

{ smsUserName = "xxxxxxx"
, smsPassword = "yyyyyyy"
, exotelCfg = exotelCfg
, s3Config = s3Config
, googleMapsKey = ""
}
