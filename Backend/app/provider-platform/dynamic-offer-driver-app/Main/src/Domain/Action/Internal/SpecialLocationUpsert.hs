{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.SpecialLocationUpsert
  ( SpecialLocationUpsertReq (..),
    SpecialLocationUpsertResp (..),
    upsertSpecialLocation,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import qualified Data.Text as T
import Domain.Types.Merchant (Merchant)
import Environment
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SpecialLocationUpsert as SLU
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

---------------------------------------------------------------------
-- Request Type for Internal API
---------------------------------------------------------------------
data SpecialLocationUpsertReq = SpecialLocationUpsertReq
  { locationGeoms :: [(Text, FilePath)],
    gateGeoms :: [(Text, FilePath)],
    file :: FilePath
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SpecialLocationUpsertReq where
  fromMultipart form = do
    let locationGeoms = map (\file -> (fdFileName file, fdPayload file)) (filter (\file -> fdInputName file == T.pack "locationGeoms") $ files form)
        gateGeoms = map (\file -> (fdFileName file, fdPayload file)) (filter (\file -> fdInputName file == T.pack "gateGeoms") $ files form)
    csvFile <- fmap fdPayload (lookupFile "file" form)
    return $ SpecialLocationUpsertReq locationGeoms gateGeoms csvFile

instance ToMultipart Tmp SpecialLocationUpsertReq where
  toMultipart form =
    MultipartData
      []
      ( [FileData "file" (T.pack form.file) "" (form.file)]
          <> (map (\(fileName, file) -> FileData "locationGeoms" fileName (T.pack file) file) form.locationGeoms)
          <> (map (\(fileName, file) -> FileData "gateGeoms" fileName (T.pack file) file) form.gateGeoms)
      )

---------------------------------------------------------------------
-- Response Type
---------------------------------------------------------------------
newtype SpecialLocationUpsertResp = SpecialLocationUpsertResp
  { unprocessedEntities :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------
-- Handler
---------------------------------------------------------------------
upsertSpecialLocation ::
  Id Merchant ->
  Context.City ->
  SpecialLocationUpsertReq ->
  Flow SpecialLocationUpsertResp
upsertSpecialLocation merchantId opCity req = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchantId.getId <> " ,city: " <> show opCity)
  Common.APISuccessWithUnprocessedEntities unprocessedEntities <-
    SLU.upsertSpecialLocationsFromCsv opCity merchantOpCity req.file req.locationGeoms req.gateGeoms
  return $ SpecialLocationUpsertResp unprocessedEntities
