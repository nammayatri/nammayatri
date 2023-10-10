{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchRequest where

import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Tools.Maps as Maps

data SearchRequestDetails =
  SearchRequestDetailsOnDemand {
      fromLocation :: DLoc.Location,
      toLocation :: DLoc.Location,
      estimatedDistance :: Meters,
      estimatedDuration :: Seconds,
      specialLocationTag :: Maybe Text,
      autoAssignEnabled :: Maybe Bool
      }
    | SearchRequestDetailsRental {
        rentalFromLocation :: DLoc.Location
       }
    deriving (Generic,Show)

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    transactionId :: Text,
    providerId :: Id DM.Merchant,
    searchRequestDetails :: SearchRequestDetails,
    area :: Maybe FareProductD.Area,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Maybe Context.City,
    bapCountry :: Maybe Context.Country,
    device :: Maybe Text,
    customerLanguage :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    createdAt :: UTCTime,
    tag :: Text
  }
  deriving (Generic,Show)