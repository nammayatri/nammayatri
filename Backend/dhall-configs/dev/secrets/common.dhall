let topSecret = ./top-secret.dhall

let globalCommon = ../../generic/common.dhall

let mockS3Config1 =
      { baseLocalDirectory = "./s3/local"
      , bucketName = "test-bucket"
      , pathPrefix = ""
      }

let mockS3Config = globalCommon.S3Config.S3MockConf mockS3Config1

let idfyCfg =
      { account_id = "xxxxxxx"
      , api_key = "xxxxxxx"
      , secret = "xxxxxxx"
      , url = "http://localhost:6235"
      }

let InfoBIPConfig = { username = "xxxxx", password = "xxxxx", token = "xxxxx" }

in  { smsUserName = "xxxxxxx"
    , smsPassword = "yyyyyyy"
    , s3Config = mockS3Config
    , idfyCfg
    , googleKey = topSecret.googleKey
    , googleTranslateKey = topSecret.googleTranslateKey
    , slackToken = "xxxxxxx"
    , InfoBIPConfig
    }
