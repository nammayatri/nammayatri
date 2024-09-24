let topSecret = ./top-secret.dhall

let globalCommon = ../../generic/common.dhall

let mockS3Config1 =
      { baseLocalDirectory = "./s3/local"
      , bucketName = "test-bucket"
      , pathPrefix = ""
      }

let mockS3Config = globalCommon.S3Config.S3MockConf mockS3Config1

let InfoBIPConfig = { username = "xxxxx", password = "xxxxx", token = "xxxxx" }

in  { smsUserName = "xxxxxxx"
    , smsPassword = "yyyyyyy"
    , s3Config = mockS3Config
    , s3PublicConfig = mockS3Config
    , googleKey = topSecret.googleKey
    , googleTranslateKey = topSecret.googleTranslateKey
    , slackToken = "xxxxxxx"
    , InfoBIPConfig
    , urlShortnerApiKey = "some-internal-api-key"
    , nammayatriRegistryApiKey = "some-secret-api-key"
    }
