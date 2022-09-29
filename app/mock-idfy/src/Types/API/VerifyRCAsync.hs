{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types.API.VerifyRCAsync where

import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.OpenApi
  ( ToSchema (..),
    fromAesonOptions,
    genericDeclareNamedSchema,
  )
import EulerHS.Prelude
import Servant (Header, JSON, Post, ReqBody, (:>))
import Types.Common
import Types.IdfyRes

type VerifyRCAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_rc_basic"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] VerifyRCReq
    :> Post '[JSON] IdfyRes

verifyRCAPI :: Proxy VerifyRCAPI
verifyRCAPI = Proxy

data VerifyRCReq = VerifyRCReq
  { task_id :: Text,
    group_id :: Text,
    _data :: VerifyRCData
  }
  deriving (Show, Generic)

instance ToSchema VerifyRCReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyRCReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyRCReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype VerifyRCData = VerifyRCData {rc_number :: Text}
  deriving (Show, Generic)

deriving newtype instance ToJSON VerifyRCData

deriving newtype instance FromJSON VerifyRCData

deriving newtype instance ToSchema VerifyRCData
