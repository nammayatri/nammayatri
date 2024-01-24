{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.Common where

import qualified Database.Beam as B
import Kernel.Prelude (Generic)
import Storage.Beam.Booking
import Storage.Beam.BookingCancellationReason
import Storage.Beam.CallStatus
import Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest
import Storage.Beam.DriverFee
import Storage.Beam.DriverInformation
import Storage.Beam.DriverOnboarding.DriverLicense
import Storage.Beam.DriverOnboarding.DriverRCAssociation
import Storage.Beam.DriverOnboarding.IdfyVerification
import Storage.Beam.DriverOnboarding.Image
import Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Beam.DriverReferral
import Storage.Beam.Exophone
import Storage.Beam.FleetDriverAssociation
import Storage.Beam.Geometry
import Storage.Beam.Invoice
import Storage.Beam.Message.Message
import Storage.Beam.Message.MessageReport
import Storage.Beam.Message.MessageTranslation
import Storage.Beam.Notification
import Storage.Beam.Person
import Storage.Beam.Quote
import Storage.Beam.Rating (RatingT, ratingTable)
import Storage.Beam.Ride.Table
import Storage.Beam.RideDetails
import Storage.Beam.RiderDetails
import Storage.Beam.Vehicle

atlasDB :: B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { exophone = exophoneTable,
        geometry = geometryTable,
        vehicle = vehicleTable,
        image = imageTable,
        person = personTable,
        driverLicense = driverLicenseTable,
        idfyVerification = idfyVerificationTable,
        driverRCAssociation = driverRcAssociationTable,
        vehicleRegistrationCertificate = vehicleRegistrationCertificateTable,
        driverInformation = driverInformationTable,
        booking = bookingTable,
        ride = rideTable,
        rideDetails = rideDetailsTable,
        rDetails = riderDetailsTable,
        callStatus = callStatusTable,
        quote = quoteTable,
        messageReport = messageReportTable,
        bookingCancellationReason = bookingCancellationReasonTable,
        driverFee = driverFeeTable,
        notification = notificationTable,
        invoice = invoiceTable,
        rating = ratingTable,
        message = messageTable,
        messageTranslation = messageTranslationTable,
        driverGoHomeRequest = driverGoHomeRequestTable,
        driverReferral = driverReferralTable,
        fleetDriverAssociation = fleetDriverAssociationTable
      }

data AtlasDB f = AtlasDB
  { exophone :: f (B.TableEntity ExophoneT),
    geometry :: f (B.TableEntity GeometryT),
    vehicle :: f (B.TableEntity VehicleT),
    image :: f (B.TableEntity ImageT),
    person :: f (B.TableEntity PersonT),
    driverLicense :: f (B.TableEntity DriverLicenseT),
    idfyVerification :: f (B.TableEntity IdfyVerificationT),
    driverRCAssociation :: f (B.TableEntity DriverRCAssociationT),
    vehicleRegistrationCertificate :: f (B.TableEntity VehicleRegistrationCertificateT),
    driverInformation :: f (B.TableEntity DriverInformationT),
    booking :: f (B.TableEntity BookingT),
    ride :: f (B.TableEntity RideT),
    rideDetails :: f (B.TableEntity RideDetailsT),
    rDetails :: f (B.TableEntity RiderDetailsT),
    callStatus :: f (B.TableEntity CallStatusT),
    quote :: f (B.TableEntity QuoteT),
    messageReport :: f (B.TableEntity MessageReportT),
    bookingCancellationReason :: f (B.TableEntity BookingCancellationReasonT),
    rating :: f (B.TableEntity RatingT),
    message :: f (B.TableEntity MessageT),
    messageTranslation :: f (B.TableEntity MessageTranslationT),
    driverGoHomeRequest :: f (B.TableEntity DriverGoHomeRequestT),
    driverReferral :: f (B.TableEntity DriverReferralT),
    driverFee :: f (B.TableEntity DriverFeeT),
    notification :: f (B.TableEntity NotificationT),
    invoice :: f (B.TableEntity InvoiceT),
    fleetDriverAssociation :: f (B.TableEntity FleetDriverAssociationT)
  }
  deriving (Generic, B.Database be)
