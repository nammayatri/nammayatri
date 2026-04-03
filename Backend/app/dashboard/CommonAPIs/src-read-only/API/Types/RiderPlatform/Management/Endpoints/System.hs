{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.RiderPlatform.Management.Endpoints.System where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Data.Aeson
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Kernel.Types.HideSecrets



data QueryData
    = QueryData {queryType :: QueryType, tableName :: Kernel.Prelude.Text, setClause :: Data.Aeson.Value, whereClause :: Data.Aeson.Value}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets QueryData
    where hideSecrets = Kernel.Prelude.identity
data QueryType
    = INSERT | UPDATE
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = ("system" :> PostSystemRunQuery)
type PostSystemRunQuery = ("runQuery" :> ReqBody ('[JSON]) QueryData :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)
newtype SystemAPIs = SystemAPIs {postSystemRunQuery :: (QueryData -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}
mkSystemAPIs :: (Client EulerHS.Types.EulerClient API -> SystemAPIs)
mkSystemAPIs systemClient = (SystemAPIs {..})
                 where postSystemRunQuery = systemClient
data SystemUserActionType
    = POST_SYSTEM_RUN_QUERY
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON SystemUserActionType
    where toJSON (POST_SYSTEM_RUN_QUERY) = Data.Aeson.String "POST_SYSTEM_RUN_QUERY"
instance FromJSON SystemUserActionType
    where parseJSON (Data.Aeson.String "POST_SYSTEM_RUN_QUERY") = pure POST_SYSTEM_RUN_QUERY
          parseJSON _ = fail "POST_SYSTEM_RUN_QUERY expected"

$(Data.Singletons.TH.genSingletons [(''SystemUserActionType)])

