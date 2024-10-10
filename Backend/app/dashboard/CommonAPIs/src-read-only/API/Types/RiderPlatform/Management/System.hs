{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.System where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data QueryData = QueryData {queryType :: API.Types.RiderPlatform.Management.System.QueryType, tableName :: Kernel.Prelude.Text, setClause :: Data.Aeson.Value, whereClause :: Data.Aeson.Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets QueryData where
  hideSecrets = Kernel.Prelude.identity

data QueryType
  = INSERT
  | UPDATE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("system" :> PostSystemRunQuery)

type PostSystemRunQuery = ("runQuery" :> ReqBody ('[JSON]) API.Types.RiderPlatform.Management.System.QueryData :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

newtype SystemAPIs = SystemAPIs {postSystemRunQuery :: (API.Types.RiderPlatform.Management.System.QueryData -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}

mkSystemAPIs :: (Client EulerHS.Types.EulerClient API -> SystemAPIs)
mkSystemAPIs systemClient = (SystemAPIs {..})
  where
    postSystemRunQuery = systemClient

data SystemEndpointDSL
  = PostSystemRunQueryEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON SystemEndpointDSL where
  toJSON (PostSystemRunQueryEndpoint) = Data.Aeson.String "PostSystemRunQueryEndpoint"

instance FromJSON SystemEndpointDSL where
  parseJSON (Data.Aeson.String "PostSystemRunQueryEndpoint") = pure PostSystemRunQueryEndpoint
  parseJSON _ = fail "PostSystemRunQueryEndpoint expected"
