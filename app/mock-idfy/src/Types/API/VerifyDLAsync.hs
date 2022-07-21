module Types.API.VerifyDLAsync where

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

type VerifyDLAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_driving_license"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] VerifyDLReq
    :> Post '[JSON] IdfyRes

verifyDLAPI :: Proxy VerifyDLAPI
verifyDLAPI = Proxy

data VerifyDLReq = VerifyDLReq
  { task_id :: Text,
    group_id :: Text,
    _data :: VerifyDLData
  }
  deriving (Show, Generic)

instance ToSchema VerifyDLReq where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON VerifyDLReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON VerifyDLReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data VerifyDLData = VerifyDLData
  { id_number :: Text,
    date_of_birth :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
