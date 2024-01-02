{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Driver.Coin where

import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForJSON)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Servant hiding (Summary, throwError)

data BulkUploadCoinsReq = BulkUploadCoinsReq
  { driverIdListWithCoins :: [DriverIdListWithCoins],
    bulkUploadTitle :: Translations,
    expirationTime :: Maybe Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverIdListWithCoins = DriverIdListWithCoins
  { driverId :: Text,
    coins :: Int
  }
  deriving (Generic, Read, Eq, Show, FromJSON, ToJSON, Ord, ToSchema)

data Translations = Translations
  { en :: Text,
    bn :: Text,
    hi :: Text,
    ml :: Text,
    ta :: Text,
    te :: Text,
    kn :: Text,
    fr :: Text
  }
  deriving (Generic, Read, Eq, Show, FromJSON, ToJSON, Ord, ToSchema)

$(mkBeamInstancesForJSON ''Translations)

type BulkUploadCoinsAPI =
  "bulkUploadCoins"
    :> ReqBody '[JSON] BulkUploadCoinsReq
    :> Post '[JSON] APISuccess
