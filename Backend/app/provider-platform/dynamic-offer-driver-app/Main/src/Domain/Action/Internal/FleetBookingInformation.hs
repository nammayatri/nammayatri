{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.FleetBookingInformation where

import Control.Applicative ((<|>))
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.FleetBookingInformation as DFBI
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.Queries.FleetBookingInformation as QFBI

data CreateFleetBookingInformationReq = CreateFleetBookingInformationReq
  { bookingId :: Text,
    serviceId :: Text,
    placeName :: Maybe Text,
    serviceName :: Maybe Text,
    vehicleNo :: Maybe Text,
    personId :: Maybe (Kernel.Types.Id.Id DP.Person),
    amount :: Maybe HighPrecMoney,
    visitDate :: Maybe Day,
    bookedSeats :: Maybe Int,
    status :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CreateFleetBookingInformationResp = CreateFleetBookingInformationResp
  { assignmentId :: Kernel.Types.Id.Id DFBI.FleetBookingInformation
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data UpdateFleetBookingInformationReq = UpdateFleetBookingInformationReq
  { id :: Maybe (Kernel.Types.Id.Id DFBI.FleetBookingInformation),
    bookingId :: Text,
    serviceId :: Text,
    fleetOwnerId :: Maybe (Kernel.Types.Id.Id DP.Person),
    vehicleNo :: Text,
    personId :: Maybe (Kernel.Types.Id.Id DP.Person),
    status :: Maybe Text,
    visitDate :: Maybe Day,
    bookedSeats :: Maybe Int,
    amount :: Maybe HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data UpdateFleetBookingInformationResp = UpdateFleetBookingInformationResp
  { assignmentId :: Kernel.Types.Id.Id DFBI.FleetBookingInformation
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

createBookingInformation :: CreateFleetBookingInformationReq -> Flow CreateFleetBookingInformationResp
createBookingInformation req = do
  infoId <- generateGUID
  now <- getCurrentTime
  let record =
        DFBI.FleetBookingInformation
          { id = infoId,
            merchantId = Nothing,
            merchantOperatingCityId = Nothing,
            bookingId = req.bookingId,
            serviceId = Just req.serviceId,
            placeName = req.placeName,
            serviceName = req.serviceName,
            fleetOwnerId = Nothing,
            vehicleNo = req.vehicleNo,
            personId = req.personId,
            amount = req.amount,
            visitDate = req.visitDate,
            bookedSeats = req.bookedSeats,
            status = req.status,
            createdAt = now,
            updatedAt = now
          }
  QFBI.create record
  pure $ CreateFleetBookingInformationResp infoId

updateBookingInformation :: UpdateFleetBookingInformationReq -> Flow UpdateFleetBookingInformationResp
updateBookingInformation req = do
  mExisting <- case req.id of
    Just id -> QFBI.findById id
    Nothing -> QFBI.findByServiceId (Just req.serviceId)
  -- _ -> pure Nothing
  now <- getCurrentTime
  case mExisting of
    Just existing -> do
      let updated =
            existing
              { DFBI.fleetOwnerId = req.fleetOwnerId <|> existing.fleetOwnerId,
                DFBI.vehicleNo = (Just req.vehicleNo) <|> existing.vehicleNo,
                DFBI.personId = req.personId <|> existing.personId,
                DFBI.status = req.status <|> existing.status,
                DFBI.amount = req.amount <|> existing.amount,
                DFBI.updatedAt = now
              }
      QFBI.updateByPrimaryKey updated
      pure $ UpdateFleetBookingInformationResp existing.id
    Nothing -> do
      let createReq =
            CreateFleetBookingInformationReq
              { bookingId = req.bookingId,
                serviceId = req.serviceId,
                placeName = Nothing,
                serviceName = Nothing,
                personId = req.personId,
                amount = req.amount,
                vehicleNo = Just req.vehicleNo,
                visitDate = req.visitDate,
                bookedSeats = req.bookedSeats,
                status = req.status
              }
      CreateFleetBookingInformationResp newId <- createBookingInformation createReq
      pure $ UpdateFleetBookingInformationResp newId
