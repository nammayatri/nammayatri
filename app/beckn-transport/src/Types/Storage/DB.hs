{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.AllocationEvent as AllocationEvent
import qualified Types.Storage.CallStatus as CallStatus
import qualified Types.Storage.CancellationReason as CancellationReason
import qualified Types.Storage.DiscountTransaction as DiscountTransaction
import qualified Types.Storage.DriverInformation as DriverInformation
import qualified Types.Storage.DriverLocation as DriverLocation
import qualified Types.Storage.DriverStats as DriverStats
import qualified Types.Storage.FarePolicy as FarePolicy
import qualified Types.Storage.FarePolicy.Discount as FPDiscount
import qualified Types.Storage.FarePolicy.PerExtraKmRate as FPExtraKmRate
import qualified Types.Storage.NotificationStatus as NotificationStatus
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Products as Product
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.Rating as Rating
import qualified Types.Storage.RegistrationToken as RegistrationToken
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideBooking as RideB
import qualified Types.Storage.RideBookingCancellationReason as RideBookingCancellationReason
import qualified Types.Storage.RideRequest as RideRequest
import qualified Types.Storage.RiderDetails as RiderDetails
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.TransporterConfig as TransporterConfig
import qualified Types.Storage.Vehicle as Vehicle

data TransporterDb f = TransporterDb
  { organization :: f (B.TableEntity Organization.OrganizationT),
    searchReqLocation :: f (B.TableEntity Location.SearchReqLocationT),
    driverLocation :: f (B.TableEntity DriverLocation.DriverLocationT),
    vehicle :: f (B.TableEntity Vehicle.VehicleT),
    person :: f (B.TableEntity Person.PersonT),
    riderDetails :: f (B.TableEntity RiderDetails.RiderDetailsT),
    searchRequest :: f (B.TableEntity SearchRequest.SearchRequestT),
    products :: f (B.TableEntity Product.ProductsT),
    quote :: f (B.TableEntity Quote.QuoteT),
    rideBooking :: f (B.TableEntity RideB.RideBookingT),
    ride :: f (B.TableEntity Ride.RideT),
    registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT),
    rating :: f (B.TableEntity Rating.RatingT),
    driverStats :: f (B.TableEntity DriverStats.DriverStatsT),
    transporterConfig :: f (B.TableEntity TransporterConfig.TransporterConfigT),
    driverInformation :: f (B.TableEntity DriverInformation.DriverInformationT),
    farePolicy :: f (B.TableEntity FarePolicy.FarePolicyT),
    farePolicyExtraKmRate :: f (B.TableEntity FPExtraKmRate.FarePolicyPerExtraKmRateT),
    farePolicyDiscount :: f (B.TableEntity FPDiscount.FarePolicyDiscountT),
    discountTransaction :: f (B.TableEntity DiscountTransaction.DiscountTransactionT),
    rideRequest :: f (B.TableEntity RideRequest.RideRequestT),
    notificationStatus :: f (B.TableEntity NotificationStatus.NotificationStatusT),
    allocationEvent :: f (B.TableEntity AllocationEvent.AllocationEventT),
    cancellationReason :: f (B.TableEntity CancellationReason.CancellationReasonT),
    bookingCancellationReason :: f (B.TableEntity RideBookingCancellationReason.RideBookingCancellationReasonT),
    callStatus :: f (B.TableEntity CallStatus.CallStatusT)
  }
  deriving (Generic, B.Database be)

transporterDb :: Text -> B.DatabaseSettings be TransporterDb
transporterDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { organization = setSchema dbSchemaName <> Organization.fieldEMod,
        searchReqLocation = setSchema dbSchemaName <> Location.fieldEMod,
        driverLocation = setSchema dbSchemaName <> DriverLocation.fieldEMod,
        vehicle = setSchema dbSchemaName <> Vehicle.fieldEMod,
        person = setSchema dbSchemaName <> Person.fieldEMod,
        riderDetails = setSchema dbSchemaName <> RiderDetails.fieldEMod,
        searchRequest = setSchema dbSchemaName <> SearchRequest.fieldEMod,
        products = setSchema dbSchemaName <> Product.fieldEMod,
        quote = setSchema dbSchemaName <> Quote.fieldEMod,
        rideBooking = setSchema dbSchemaName <> RideB.fieldEMod,
        ride = setSchema dbSchemaName <> Ride.fieldEMod,
        registrationToken = setSchema dbSchemaName <> RegistrationToken.fieldEMod,
        rating = setSchema dbSchemaName <> Rating.fieldEMod,
        driverStats = setSchema dbSchemaName <> DriverStats.fieldEMod,
        transporterConfig = setSchema dbSchemaName <> TransporterConfig.fieldEMod,
        driverInformation = setSchema dbSchemaName <> DriverInformation.fieldEMod,
        farePolicy = setSchema dbSchemaName <> FarePolicy.fieldEMod,
        farePolicyExtraKmRate = setSchema dbSchemaName <> FPExtraKmRate.fieldEMod,
        farePolicyDiscount = setSchema dbSchemaName <> FPDiscount.fieldEMod,
        discountTransaction = setSchema dbSchemaName <> DiscountTransaction.fieldEMod,
        rideRequest = setSchema dbSchemaName <> RideRequest.fieldEMod,
        notificationStatus = setSchema dbSchemaName <> NotificationStatus.fieldEMod,
        allocationEvent = setSchema dbSchemaName <> AllocationEvent.fieldEMod,
        cancellationReason = setSchema dbSchemaName <> CancellationReason.fieldEMod,
        bookingCancellationReason = setSchema dbSchemaName <> RideBookingCancellationReason.fieldEMod,
        callStatus = setSchema dbSchemaName <> CallStatus.fieldEMod
      }
  where
    setSchema schema = setEntitySchema (Just schema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
