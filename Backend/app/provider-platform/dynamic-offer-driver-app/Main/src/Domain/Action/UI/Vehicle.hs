{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Vehicle
  ( UpdateVehicleReq (..),
    GetVehicleRes (..),
    ListVehicleRes (..),
    UpdateVehicleRes,
    VehicleRes (..),
    Driver (..),
    listVehicles,
    updateVehicle,
    getVehicle,
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Domain.Types.Vehicle as SV
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Tools.Error

data UpdateVehicleReq = UpdateVehicleReq
  { variant :: Maybe Variant.Variant,
    model :: Maybe Text,
    color :: Maybe Text,
    capacity :: Maybe Int,
    category :: Maybe Category,
    make :: Maybe Text,
    size :: Maybe Text,
    energyType :: Maybe Text,
    registrationNo :: Maybe Text,
    registrationCategory :: Maybe RegistrationCategory,
    fleetOwnerId :: Maybe Text
  }
  deriving (Generic, FromJSON, ToSchema)

newtype GetVehicleRes = GetVehicleRes
  {vehicle :: SV.VehicleAPIEntity}
  deriving (Generic, ToJSON, ToSchema)

newtype ListVehicleRes = ListVehicleRes
  {vehicles :: [VehicleRes]}
  deriving (Generic, ToJSON, ToSchema)

validateUpdateVehicleReq :: Validate UpdateVehicleReq
validateUpdateVehicleReq UpdateVehicleReq {..} =
  sequenceA_
    [ validateField "model" model . InMaybe $
        NotEmpty `And` star P.latinOrSpace,
      validateField "color" color . InMaybe $ NotEmpty `And` P.name,
      validateField "registrationNo" registrationNo . InMaybe $
        LengthInRange 1 11 `And` star (P.latinUC \/ P.digit)
    ]

type UpdateVehicleRes = VehicleAPIEntity

data VehicleRes = VehicleRes
  { vehicle :: SV.VehicleAPIEntity,
    driver :: Maybe Driver
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data Driver = Driver
  { id :: Text,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    rating :: Maybe Centesimal,
    merchantId :: Id DM.Merchant
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

listVehicles :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => SP.Person -> Maybe Variant.Variant -> Maybe Text -> Maybe Int -> Maybe Int -> m ListVehicleRes
listVehicles admin variantM mbRegNum limitM offsetM = do
  let merchantId = admin.merchantId
  personList <- B.runInReplica $ QP.findAllByMerchantId [SP.DRIVER] merchantId
  vehicleList <- B.runInReplica $ QV.findAllByVariantRegNumMerchantId variantM mbRegNum limit offset merchantId
  respList <- buildVehicleRes personList `traverse` vehicleList
  return $ ListVehicleRes respList
  where
    limit = toInteger $ fromMaybe 50 limitM
    offset = toInteger $ fromMaybe 0 offsetM

updateVehicle :: (CacheFlow m r, EsqDBFlow m r) => SP.Person -> Id SP.Person -> UpdateVehicleReq -> m UpdateVehicleRes
updateVehicle admin driverId req = do
  let merchantId = admin.merchantId
  runRequestValidation validateUpdateVehicleReq req
  driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.merchantId == merchantId || driver.role == SP.DRIVER) $ throwError Unauthorized
  vehicle <- QV.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
  whenJust req.registrationNo $ \regNum -> do
    vehicleWithRegistrationNoM <- QV.findByRegistrationNo regNum
    when (isJust vehicleWithRegistrationNoM) $
      throwError $ InvalidRequest "Registration number already exists."
  let updatedVehicle =
        vehicle{variant = fromMaybe vehicle.variant req.variant,
                model = fromMaybe vehicle.model req.model,
                color = fromMaybe vehicle.color req.color,
                capacity = req.capacity <|> vehicle.capacity,
                category = req.category <|> vehicle.category,
                make = req.make <|> vehicle.make,
                size = req.size <|> vehicle.size,
                energyType = req.energyType <|> vehicle.energyType,
                registrationNo = fromMaybe vehicle.registrationNo req.registrationNo,
                registrationCategory = req.registrationCategory <|> vehicle.registrationCategory,
                fleetOwnerId = req.fleetOwnerId <|> vehicle.fleetOwnerId
               }

  _ <- QV.updateVehicleRec updatedVehicle
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateVehicle : ") (show updatedVehicle)
  return $ SV.makeVehicleAPIEntity updatedVehicle

getVehicle :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maybe (Id SP.Person) -> m GetVehicleRes
getVehicle (personId, _, _) registrationNoM vehicleIdM = do
  user <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  vehicle <- case (registrationNoM, vehicleIdM) of
    (Nothing, Nothing) -> throwError $ InvalidRequest "You should pass registration number and vehicle id."
    _ -> B.runInReplica $ QV.findByAnyOf registrationNoM vehicleIdM >>= fromMaybeM (VehicleDoesNotExist personId.getId)
  hasAccess user vehicle
  return . GetVehicleRes $ SV.makeVehicleAPIEntity vehicle
  where
    hasAccess user vehicle =
      when (user.merchantId /= vehicle.merchantId) $
        throwError Unauthorized

buildVehicleRes :: MonadFlow m => [SP.Person] -> SV.Vehicle -> m VehicleRes
buildVehicleRes personList vehicle = do
  let mdriver =
        find
          ( \person ->
              person.id == vehicle.driverId
          )
          personList
  return
    VehicleRes
      { vehicle = SV.makeVehicleAPIEntity vehicle,
        driver = mkDriverObj <$> mdriver
      }

mkDriverObj :: SP.Person -> Driver
mkDriverObj person =
  Driver
    { id = getId $ person.id,
      firstName = person.firstName,
      middleName = person.middleName,
      lastName = person.lastName,
      rating = SP.roundToOneDecimal <$> person.rating,
      merchantId = person.merchantId
    }
