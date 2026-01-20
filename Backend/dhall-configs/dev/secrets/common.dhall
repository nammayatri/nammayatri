let topSecret = ./top-secret.dhall

let globalCommon = ../../generic/common.dhall

let mockS3Config1 =
      { baseLocalDirectory = "./s3/local"
      , bucketName = "test-bucket"
      , pathPrefix = ""
      }

let mockS3Config = globalCommon.S3Config.S3MockConf mockS3Config1

let mockGCSConfig1 =
      { baseLocalDirectory = "./gcs/local"
      , pathPrefix = ""
      , bucketName = "test-gcs-bucket"
      }

let mockGCSConfig = globalCommon.GCSConfig.GCSMockConf mockGCSConfig1

let mockStorageConfig =
      { primaryStorage = globalCommon.StorageProvider.StorageS3 mockS3Config
      , secondaryStorage =
          Some (globalCommon.StorageProvider.StorageGCS mockGCSConfig)
      , enableMultiCloudWrite = False
      }

let InfoBIPConfig = { username = "xxxxx", password = "xxxxx", token = "xxxxx" }

in  { smsUserName = "xxxxxxx"
    , smsPassword = "yyyyyyy"
    , s3Config = mockS3Config
    , s3PublicConfig = mockS3Config
    , gcsConfig = mockGCSConfig
    , storageConfig = mockStorageConfig
    , googleKey = topSecret.googleKey
    , googleTranslateKey = topSecret.googleTranslateKey
    , slackToken = "xxxxxxx"
    , InfoBIPConfig
    , urlShortnerApiKey = "some-internal-api-key"
    , nammayatriRegistryApiKey = "some-secret-api-key"
    }
