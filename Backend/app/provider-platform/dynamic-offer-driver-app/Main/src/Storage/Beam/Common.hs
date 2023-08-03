{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Storage.Beam.Common where

import qualified Database.Beam as B
import GHC.Generics (Generic)
-- import Storage.Beam.DriverOnboarding.Image

-- import Storage.Beam.DriverOnboarding.IdfyVerification

import Storage.Beam.Booking
import Storage.Beam.CallStatus
import Storage.Beam.DriverInformation
import Storage.Beam.DriverLocation
import Storage.Beam.DriverOnboarding.DriverLicense
import Storage.Beam.DriverOnboarding.DriverRCAssociation
import Storage.Beam.DriverOnboarding.IdfyVerification
import Storage.Beam.DriverOnboarding.Image
import Storage.Beam.DriverOnboarding.OperatingCity
import Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Beam.Exophone
import Storage.Beam.Geometry
import Storage.Beam.Message.MessageReport
import Storage.Beam.Person
import Storage.Beam.QuoteSpecialZone
import Storage.Beam.Rating (RatingT, ratingTable)
import Storage.Beam.Ride.Table
import Storage.Beam.RideDetails
import Storage.Beam.RiderDetails
import Storage.Beam.Vehicle

atlasDB :: B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { driverLocation = dLocationTable,
        exophone = dExophone,
        geometry = geometryTable,
        vehicle = vehicleTable,
        operatingCity = operatingCityTable,
        image = imageTable,
        person = personTable,
        driverLicense = driverLicenseTable,
        idfyVerification = idfyVerificationTable,
        driverRCAssociation = driverRCAssociationTable,
        vehicleRegistrationCertificate = vehicleRegistrationCertificateTable,
        driverInformation = dInformationTable,
        booking = bookingTable,
        ride = rideTable,
        rideDetails = rideDetailsTable,
        rDetails = rDetailsTable,
        callStatus = callStatusTable,
        quoteSpecialZone = quoteSpecialZoneTable,
        messageReport = messageReportTable,
        rating = ratingTable
      }

data AtlasDB f = AtlasDB
  { driverLocation :: f (B.TableEntity DriverLocationT),
    exophone :: f (B.TableEntity ExophoneT),
    geometry :: f (B.TableEntity GeometryT),
    vehicle :: f (B.TableEntity VehicleT),
    operatingCity :: f (B.TableEntity OperatingCityT),
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
    quoteSpecialZone :: f (B.TableEntity QuoteSpecialZoneT),
    messageReport :: f (B.TableEntity MessageReportT),
    rating :: f (B.TableEntity RatingT)
  }
  deriving (Generic, B.Database be)
