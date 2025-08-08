{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FleetVehiclesAssociation where

import qualified Dashboard.Common as Common
import qualified Data.Map as Map
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIE
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as RCQuery
import Tools.Error

-- Custom response types tailored for boat fleet listing
newtype BoatFleetVehicleListRes = BoatFleetVehicleListRes {vehicles :: [BoatFleetVehicle]}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data BoatFleetVehicle = BoatFleetVehicle
  { fleetOwnerId :: Text,
    fleetOwnerName :: Text,
    rcId :: Text,
    vehicleNo :: Maybe Text,
    vehicleType :: Maybe Common.VehicleVariant,
    driverId :: Maybe Text,
    driverName :: Maybe Text,
    isActive :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

getFleetVehicleAssociation :: Maybe Text -> Text -> Flow BoatFleetVehicleListRes
getFleetVehicleAssociation apiKey placeId = do
  -- Get fleet owners by fleet type and ticket place ID (BOAT_FLEET)
  fleetOwners <- QFOIE.getFleetOwnerByTicketPlaceId (Just placeId)
  let fleetOwnerIds = map (.fleetOwnerPersonId.getId) fleetOwners

  if null fleetOwnerIds
    then pure $ BoatFleetVehicleListRes []
    else do
      -- Use the first fleet owner's merchant for the RC query
      let firstOwnerId = head fleetOwnerIds
      firstOwnerPerson <- QP.findById (Id firstOwnerId) >>= fromMaybeM (PersonNotFound firstOwnerId)
      let merchantId = firstOwnerPerson.merchantId

      -- Authorize using the merchant's internalApiKey
      merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      unless (Just merchant.internalApiKey == apiKey) $
        throwError $ AuthBlocked "Invalid internal API key"

      -- Build name map from Person table for all owners
      persons <- mapM (\foId -> QP.findById (Id foId)) fleetOwnerIds
      let fleetOwnerNameMap =
            Map.fromList $
              zipWith
                (\foId mbP -> (foId, maybe "" (\p -> p.firstName <> fromMaybe "" ((" " <>) <$> p.lastName)) mbP))
                fleetOwnerIds
                persons

      -- Fetch active RCs for these fleet owners
      rcs <- RCQuery.findAllActiveRCForFleetMF fleetOwnerIds merchantId
      items <- mapM (buildItem fleetOwnerNameMap) rcs
      pure $ BoatFleetVehicleListRes items

buildItem :: Map.Map Text Text -> DVRC.VehicleRegistrationCertificate -> Flow BoatFleetVehicle
buildItem nameMap rc = do
  let mFleetOwnerIdTxt = rc.fleetOwnerId
      fleetOwnerIdTxt = fromMaybe "" mFleetOwnerIdTxt
      fleetOwnerName = fromMaybe "" (Map.lookup fleetOwnerIdTxt nameMap)
  vehicleNumber <- decrypt rc.certificateNumber
  pure
    BoatFleetVehicle
      { fleetOwnerId = fleetOwnerIdTxt,
        fleetOwnerName = fleetOwnerName,
        rcId = rc.id.getId,
        vehicleNo = Just vehicleNumber,
        vehicleType = rc.vehicleVariant,
        driverId = Nothing,
        driverName = Nothing,
        isActive = False
      }
