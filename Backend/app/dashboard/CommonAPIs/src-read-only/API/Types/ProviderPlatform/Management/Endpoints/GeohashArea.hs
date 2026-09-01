{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.GeohashArea where

import qualified Dashboard.Common
import qualified Dashboard.Common.GeohashArea
import Data.Aeson
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Utils.TH
import Servant
import Servant.Client

type GetGeohashAreaList = ("list" :> Get ('[JSON]) [Dashboard.Common.GeohashArea.GeohashAreaItem])

type PostGeohashAreaUpsert = ("upsert" :> ReqBody ('[JSON]) Dashboard.Common.GeohashArea.GeohashAreaBulkUpsertReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostGeohashAreaUpsertCsv =
  ( "upsert" :> "csv" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.GeohashArea.GeohashAreaCsvReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type API = ("geohashArea" :> (GetGeohashAreaList :<|> PostGeohashAreaUpsert :<|> PostGeohashAreaUpsertCsv))

data GeohashAreaAPIs = GeohashAreaAPIs
  { getGeohashAreaList :: EulerHS.Types.EulerClient [Dashboard.Common.GeohashArea.GeohashAreaItem],
    postGeohashAreaUpsert :: (Dashboard.Common.GeohashArea.GeohashAreaBulkUpsertReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postGeohashAreaUpsertCsv ::
      ( ( Data.ByteString.Lazy.ByteString,
          Dashboard.Common.GeohashArea.GeohashAreaCsvReq
        ) ->
        EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
      )
  }

mkGeohashAreaAPIs :: (Client EulerHS.Types.EulerClient API -> GeohashAreaAPIs)
mkGeohashAreaAPIs geohashAreaClient = (GeohashAreaAPIs {..})
  where
    getGeohashAreaList :<|> postGeohashAreaUpsert :<|> postGeohashAreaUpsertCsv = geohashAreaClient

data GeohashAreaUserActionType
  = GET_GEOHASH_AREA_LIST
  | GEOHASH_AREA_BULK_UPSERT
  | GEOHASH_AREA_CSV_UPSERT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''GeohashAreaUserActionType)])
