{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.TollUpsert
  ( TollUpsertReq (..),
    TollUpsertResp (..),
    upsertTollsFromCsv,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Merchant as Common
import qualified Data.Text as T
import Domain.Types.Merchant (Merchant)
import Environment
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.TollUpsert as TU
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

data TollUpsertReq = TollUpsertReq
  { file :: FilePath
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp TollUpsertReq where
  fromMultipart form = do
    csvFile <- fmap fdPayload (lookupFile "file" form)
    return $ TollUpsertReq csvFile

instance ToMultipart Tmp TollUpsertReq where
  toMultipart form =
    MultipartData
      []
      [FileData "file" (T.pack form.file) "" form.file]

newtype TollUpsertResp = TollUpsertResp
  { unprocessedEntities :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

upsertTollsFromCsv ::
  Id Merchant ->
  Context.City ->
  TollUpsertReq ->
  Flow TollUpsertResp
upsertTollsFromCsv merchantId opCity req = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchantId.getId <> " ,city: " <> show opCity)
  Common.APISuccessWithUnprocessedEntities unprocessedEntities <-
    TU.upsertTollsFromCsv opCity merchantOpCity req.file
  return $ TollUpsertResp unprocessedEntities
