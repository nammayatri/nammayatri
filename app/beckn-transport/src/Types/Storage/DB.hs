{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.AllocationEvent as AllocationEvent
import qualified Types.Storage.Case as Case
import qualified Types.Storage.DriverInformation as DriverInformation
import qualified Types.Storage.DriverStats as DriverStats
import qualified Types.Storage.FarePolicy as FarePolicy
import qualified Types.Storage.Location as Location
import qualified Types.Storage.NotificationStatus as NotificationStatus
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as ProductInstance
import qualified Types.Storage.Products as Product
import qualified Types.Storage.Rating as Rating
import qualified Types.Storage.RegistrationToken as RegistrationToken
import qualified Types.Storage.RideRequest as RideRequest
import qualified Types.Storage.TransporterConfig as TransporterConfig
import qualified Types.Storage.Vehicle as Vehicle

data TransporterDb f = TransporterDb
  { organization :: f (B.TableEntity Organization.OrganizationT),
    location :: f (B.TableEntity Location.LocationT),
    vehicle :: f (B.TableEntity Vehicle.VehicleT),
    person :: f (B.TableEntity Person.PersonT),
    _case :: f (B.TableEntity Case.CaseT),
    products :: f (B.TableEntity Product.ProductsT),
    productInstance :: f (B.TableEntity ProductInstance.ProductInstanceT),
    registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT),
    rating :: f (B.TableEntity Rating.RatingT),
    driverStats :: f (B.TableEntity DriverStats.DriverStatsT),
    transporterConfig :: f (B.TableEntity TransporterConfig.TransporterConfigT),
    driverInformation :: f (B.TableEntity DriverInformation.DriverInformationT),
    farePolicy :: f (B.TableEntity FarePolicy.FarePolicyT),
    rideRequest :: f (B.TableEntity RideRequest.RideRequestT),
    notificationStatus :: f (B.TableEntity NotificationStatus.NotificationStatusT),
    allocationEvent :: f (B.TableEntity AllocationEvent.AllocationEventT)
  }
  deriving (Generic, B.Database be)

transporterDb :: Text -> B.DatabaseSettings be TransporterDb
transporterDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { organization = setSchema dbSchemaName <> Organization.fieldEMod,
        location = setSchema dbSchemaName <> Location.fieldEMod,
        vehicle = setSchema dbSchemaName <> Vehicle.fieldEMod,
        person = setSchema dbSchemaName <> Person.fieldEMod,
        _case = setSchema dbSchemaName <> Case.fieldEMod,
        products = setSchema dbSchemaName <> Product.fieldEMod,
        productInstance = setSchema dbSchemaName <> ProductInstance.fieldEMod,
        registrationToken = setSchema dbSchemaName <> RegistrationToken.fieldEMod,
        rating = setSchema dbSchemaName <> Rating.fieldEMod,
        driverStats = setSchema dbSchemaName <> DriverStats.fieldEMod,
        transporterConfig = setSchema dbSchemaName <> TransporterConfig.fieldEMod,
        driverInformation = setSchema dbSchemaName <> DriverInformation.fieldEMod,
        farePolicy = setSchema dbSchemaName <> FarePolicy.fieldEMod,
        rideRequest = setSchema dbSchemaName <> RideRequest.fieldEMod,
        notificationStatus = setSchema dbSchemaName <> NotificationStatus.fieldEMod,
        allocationEvent = setSchema dbSchemaName <> AllocationEvent.fieldEMod
      }
  where
    setSchema schema = setEntitySchema (Just schema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
