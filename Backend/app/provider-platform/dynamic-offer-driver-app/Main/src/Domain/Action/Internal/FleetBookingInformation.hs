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
import qualified Domain.Types.FleetBookingAssignments as DFBA
import qualified Domain.Types.FleetBookingInformation as DFBI
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Encryption (encrypt)
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.Queries.FleetBookingAssignments as QFBA
import qualified Storage.Queries.FleetBookingInformation as QFBI

data CreateFleetBookingInformationReq = CreateFleetBookingInformationReq
  { bookingId :: Text,
    serviceId :: Text,
    ticketBookingShortId :: Maybe Text,
    ticketBookingServiceShortId :: Maybe Text,
    placeName :: Maybe Text,
    serviceName :: Maybe Text,
    vehicleNo :: Maybe Text,
    personId :: Maybe (Kernel.Types.Id.Id DP.Person),
    amount :: Maybe HighPrecMoney,
    visitDate :: Maybe Day,
    bookedSeats :: Maybe Int,
    status :: Maybe Text,
    ticketPlaceId :: Maybe Text,
    paymentMethod :: Maybe Text,
    customerMobileNumber :: Maybe Text,
    customerName :: Maybe Text
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
    ticketBookingShortId :: Maybe Text,
    ticketBookingServiceShortId :: Maybe Text,
    fleetOwnerId :: Maybe (Kernel.Types.Id.Id DP.Person),
    vehicleNo :: Text,
    personId :: Maybe (Kernel.Types.Id.Id DP.Person),
    status :: Maybe Text,
    visitDate :: Maybe Day,
    bookedSeats :: Maybe Int,
    amount :: Maybe HighPrecMoney,
    ticketPlaceId :: Maybe Text,
    assignments :: Maybe [BookingAssignment],
    paymentMethod :: Maybe Text,
    customerMobileNumber :: Maybe Text,
    customerName :: Maybe Text,
    serviceName :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data BookingAssignment = BookingAssignment
  { fleetOwnerId :: Text,
    vehicleNo :: Text,
    skuDurationMins :: Maybe Int,
    assignmentStartTime :: Maybe UTCTime,
    assignmentEndTime :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data UpdateFleetBookingInformationResp = UpdateFleetBookingInformationResp
  { assignmentId :: Kernel.Types.Id.Id DFBI.FleetBookingInformation
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

createBookingInformation :: CreateFleetBookingInformationReq -> Flow (DFBI.FleetBookingInformation, CreateFleetBookingInformationResp)
createBookingInformation req = do
  infoId <- generateGUID
  now <- getCurrentTime
  encryptedMobileNumber <- mapM encrypt req.customerMobileNumber
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
            ticketPlaceId = req.ticketPlaceId,
            ticketBookingShortId = req.ticketBookingShortId,
            ticketBookingServiceShortId = req.ticketBookingServiceShortId,
            paymentMethod = req.paymentMethod,
            customerMobileNumber = encryptedMobileNumber,
            customerName = req.customerName,
            createdAt = now,
            updatedAt = now
          }
  QFBI.create record
  pure $ (record, CreateFleetBookingInformationResp infoId)

updateBookingInformation :: UpdateFleetBookingInformationReq -> Flow UpdateFleetBookingInformationResp
updateBookingInformation req = do
  mExisting <- case req.id of
    Just id -> QFBI.findById id
    Nothing -> QFBI.findByServiceId (Just req.serviceId)
  now <- getCurrentTime
  encryptedMobileNumber <- mapM encrypt req.customerMobileNumber
  case mExisting of
    Just existing -> do
      let updated =
            existing
              { DFBI.fleetOwnerId = req.fleetOwnerId <|> existing.fleetOwnerId,
                DFBI.vehicleNo = (Just req.vehicleNo) <|> existing.vehicleNo,
                DFBI.personId = req.personId <|> existing.personId,
                DFBI.status = req.status <|> existing.status,
                DFBI.amount = req.amount <|> existing.amount,
                DFBI.ticketPlaceId = req.ticketPlaceId <|> existing.ticketPlaceId,
                DFBI.ticketBookingShortId = req.ticketBookingShortId <|> existing.ticketBookingShortId,
                DFBI.ticketBookingServiceShortId = req.ticketBookingServiceShortId <|> existing.ticketBookingServiceShortId,
                DFBI.paymentMethod = req.paymentMethod <|> existing.paymentMethod,
                DFBI.customerMobileNumber = encryptedMobileNumber <|> existing.customerMobileNumber,
                DFBI.customerName = req.customerName <|> existing.customerName,
                DFBI.serviceName = req.serviceName <|> existing.serviceName,
                DFBI.updatedAt = now
              }
      QFBI.updateByPrimaryKey updated
      createAssignments updated req.assignments
      pure $ UpdateFleetBookingInformationResp existing.id
    Nothing -> do
      let createReq =
            CreateFleetBookingInformationReq
              { bookingId = req.bookingId,
                serviceId = req.serviceId,
                placeName = Nothing,
                serviceName = req.serviceName,
                personId = req.personId,
                amount = req.amount,
                vehicleNo = Just req.vehicleNo,
                visitDate = req.visitDate,
                bookedSeats = req.bookedSeats,
                status = req.status,
                ticketPlaceId = req.ticketPlaceId,
                ticketBookingShortId = req.ticketBookingShortId,
                ticketBookingServiceShortId = req.ticketBookingServiceShortId,
                paymentMethod = req.paymentMethod,
                customerMobileNumber = req.customerMobileNumber,
                customerName = req.customerName
              }
      (newAssignment, CreateFleetBookingInformationResp newId) <- createBookingInformation createReq
      createAssignments newAssignment req.assignments
      pure $ UpdateFleetBookingInformationResp newId
  where
    createAssignments :: DFBI.FleetBookingInformation -> Maybe [BookingAssignment] -> Flow ()
    createAssignments mainAssignment mbAssignments = do
      let assignments = fold mbAssignments
      now <- getCurrentTime
      let individualAmount = maybe Nothing (\amt -> Just (amt / (fromIntegral (max 1 (fromMaybe 1 mainAssignment.bookedSeats))))) mainAssignment.amount
      forM_ assignments $ \assignment -> do
        assignmentId <- generateGUID
        let record =
              DFBA.FleetBookingAssignments
                { id = assignmentId,
                  mainAssignmentId = mainAssignment.id,
                  bookingId = mainAssignment.bookingId,
                  serviceId = mainAssignment.serviceId,
                  placeName = mainAssignment.placeName,
                  serviceName = mainAssignment.serviceName,
                  DFBA.visitDate = mainAssignment.visitDate,
                  fleetOwnerId = assignment.fleetOwnerId,
                  vehicleNo = assignment.vehicleNo,
                  amount = individualAmount,
                  merchantId = mainAssignment.merchantId,
                  paymentMethod = mainAssignment.paymentMethod,
                  merchantOperatingCityId = mainAssignment.merchantOperatingCityId,
                  assignmentStartTime = assignment.assignmentStartTime,
                  assignmentEndTime = assignment.assignmentEndTime,
                  skuDurationMins = assignment.skuDurationMins,
                  createdAt = now,
                  updatedAt = now
                }
        QFBA.create record
