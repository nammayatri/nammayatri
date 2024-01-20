{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnSearch where

import Domain.Types.FRFSTrip as DTrip
import qualified Domain.Types.Station as DStation
import Kernel.Prelude
import Kernel.Utils.Common

data DOnSearch = DOnSearch
  { bppSubscriberId :: Text,
    providerDescription :: Maybe Text,
    providerId :: Text,
    providerName :: Text,
    quotes :: [DQuote],
    validTill :: Maybe UTCTime
  }

data DQuote = DQuote
  { bppItemId :: Text,
    price :: HighPrecMoney,
    vehicleType :: DStation.FRFSVehicleType,
    stations :: [DStation]
  }

data DStation = DStation
  { stationCode :: Text,
    stationName :: Text,
    stationType :: DTrip.StationType,
    stopSequence :: Int
  }
