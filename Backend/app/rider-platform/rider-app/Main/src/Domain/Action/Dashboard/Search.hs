{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Search where

import qualified API.UI.Search as SH
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Types.LocationAddress as DLA
import Kernel.Prelude
import Storage.Beam.SystemConfigs ()
import Text.Regex.Posix ((=~))

-- NOTE : rentalSearchReq and interCitySearchReq have not been implemented for dashboard yet and will require their own address parsing logic before enabling
parseReq :: SH.SearchReq -> SH.SearchReq
parseReq (SH.OneWaySearch oneWaySearchReq) = SH.OneWaySearch (splitAddressInOneWaySearchReq oneWaySearchReq)
parseReq (SH.RentalSearch rentalSearchReq) = SH.RentalSearch rentalSearchReq
parseReq (SH.InterCitySearch interCitySearchReq) = SH.InterCitySearch interCitySearchReq

splitAddressInOneWaySearchReq :: SH.OneWaySearchReq -> SH.OneWaySearchReq
splitAddressInOneWaySearchReq SH.OneWaySearchReq {..} = do
  SH.OneWaySearchReq
    { origin =
        SH.SearchReqLocation
          { address = splitAddress origin.address.area origin.address.ward,
            gps = origin.gps
          },
      destination =
        SH.SearchReqLocation
          { address = splitAddress destination.address.area destination.address.ward,
            gps = destination.gps
          },
      ..
    }

splitAddress :: Maybe Text -> Maybe Text -> DLA.LocationAddress
splitAddress fullAddress mbSpecialLocationTag = do
  let totalAddressComponents = L.length $ T.splitOn "," (fromMaybe "" fullAddress)
  let splitedAddress = T.splitOn "," (fromMaybe "" fullAddress)
      door
        | totalAddressComponents > 7 = Just $ head splitedAddress <> ", " <> splitedAddress !! 1
        | totalAddressComponents == 7 = Just $ head splitedAddress
        | otherwise = Nothing
      area = splitedAddress !? (totalAddressComponents - 4)
      building = splitedAddress !? (totalAddressComponents - 6)
  DLA.LocationAddress
    { street = splitedAddress !? (totalAddressComponents - 5),
      door = door,
      city = splitedAddress !? (totalAddressComponents - 3),
      state = splitedAddress !? (totalAddressComponents - 2),
      country = splitedAddress !? (totalAddressComponents - 1),
      building = building,
      areaCode = extractAreaCode (fromMaybe "" fullAddress),
      area = area,
      ward = Just $ fromMaybe (T.intercalate ", " $ catMaybes [door, building, area]) mbSpecialLocationTag,
      placeId = Nothing
    }

extractAreaCode :: Text -> Maybe Text
extractAreaCode fullAddress = do
  let addressString = T.unpack fullAddress
  let areaCodeString = addressString =~ ("[0-9]{6}" :: String) :: String
  Just $ T.pack areaCodeString

(!?) :: [a] -> Int -> Maybe a
(!?) xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just $ xs !! i
