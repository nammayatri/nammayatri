{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Search where

import qualified API.UI.Search as SH
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()
import Text.Regex.Posix ((=~))

data RideSearchEndPoint = SearchEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "RideSearchEndPoint"

type API =
  "search"
    :> CustomerRideSearchAPI

type CustomerRideSearchAPI =
  Capture "customerId" (Id DP.Person)
    :> "rideSearch"
    :> ReqBody '[JSON] SH.SearchReq
    :> Post '[JSON] SH.SearchResp

handler :: ShortId DM.Merchant -> FlowServer API
handler = callSearch

callSearch :: ShortId DM.Merchant -> Id DP.Person -> SH.SearchReq -> FlowHandler SH.SearchResp
callSearch merchantId personId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  let req' = parseReq req
  SH.search (personId, m.id) req' Nothing Nothing Nothing

parseReq :: SH.SearchReq -> SH.SearchReq
parseReq (SH.OneWaySearch oneWaySearchReq) = SH.OneWaySearch (splitAddressInSearchReq oneWaySearchReq)
parseReq (SH.RentalSearch rentalSearchReq) = SH.RentalSearch rentalSearchReq

splitAddressInSearchReq :: SH.OneWaySearchReq -> SH.OneWaySearchReq
splitAddressInSearchReq SH.OneWaySearchReq {..} = do
  let originAddress = origin.address.area
      originGps = origin.gps
      destinationAddress = destination.address.area
      destinationGps = destination.gps
      splitOriginAddress = splitAddress originAddress
      splitDestinationAddress = splitAddress destinationAddress
  -- logDebug $ "shrey00: Search OriginAddress: " <> show splitOriginAddress
  SH.OneWaySearchReq
    { origin =
        SH.SearchReqLocation
          { address = splitOriginAddress,
            gps = originGps
          },
      destination =
        SH.SearchReqLocation
          { address = splitDestinationAddress,
            gps = destinationGps
          },
      ..
    }

splitAddress :: Maybe Text -> DLA.LocationAddress
splitAddress fullAddress = do
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
      ward = Just $ T.intercalate ", " $ catMaybes [door, building, area],
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
