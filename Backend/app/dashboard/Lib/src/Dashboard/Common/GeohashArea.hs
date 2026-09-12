{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Dashboard.Common.GeohashArea
  ( module Dashboard.Common.GeohashArea,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart

-- | One geohash cell's display label. Same shape regardless of how the caller
-- arrived at it -- map cell-painting and CSV rows both flatten down to this.
data GeohashAreaItem = GeohashAreaItem
  { geohash :: Kernel.Prelude.Text,
    areaName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets GeohashAreaItem where
  hideSecrets = identity

-- | Map-selection path: city comes from the dashboard route (merchant + city),
-- same as the CSV upload below -- the body only carries the cells themselves.
newtype GeohashAreaBulkUpsertReq = GeohashAreaBulkUpsertReq
  { areas :: [GeohashAreaItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets GeohashAreaBulkUpsertReq where
  hideSecrets = identity

-- | CSV-upload path. Same underlying upsert as the JSON path above -- this only
-- carries the file; the dashboard route still supplies merchant + city.
newtype GeohashAreaCsvReq = GeohashAreaCsvReq
  { file :: Kernel.Prelude.FilePath
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets GeohashAreaCsvReq where
  hideSecrets = identity

instance FromMultipart Tmp GeohashAreaCsvReq where
  fromMultipart form =
    GeohashAreaCsvReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp GeohashAreaCsvReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]
