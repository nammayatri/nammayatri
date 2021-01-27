{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.Rating as Rating
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Customer as Customer
import qualified Types.Storage.DriverInformation as DriverInformation
import qualified Types.Storage.DriverStats as DriverStats
import qualified Types.Storage.FarePolicy as FarePolicy
import qualified Types.Storage.Leads as Leads
import qualified Types.Storage.Quotation as Quotation
import qualified Types.Storage.Tracker as Tracker
import qualified Types.Storage.TransporterConfig as TransporterConfig
import qualified Types.Storage.TripReference as TripReference

data TransporterDb f = TransporterDb
  { _organization :: f (B.TableEntity Organization.OrganizationT),
    _leads :: f (B.TableEntity Leads.LeadsT),
    _customer :: f (B.TableEntity Customer.CustomerT),
    _location :: f (B.TableEntity Location.LocationT),
    _quotation :: f (B.TableEntity Quotation.QuotationT),
    _tracker :: f (B.TableEntity Tracker.TrackerT),
    _tripReference :: f (B.TableEntity TripReference.TripReferenceT),
    _vehicle :: f (B.TableEntity Vehicle.VehicleT),
    _person :: f (B.TableEntity Person.PersonT),
    _case :: f (B.TableEntity Case.CaseT),
    _products :: f (B.TableEntity Product.ProductsT),
    _productInstance :: f (B.TableEntity ProductInstance.ProductInstanceT),
    _registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT),
    _rating :: f (B.TableEntity Rating.RatingT),
    _driverStats :: f (B.TableEntity DriverStats.DriverStatsT),
    _transporterConfig :: f (B.TableEntity TransporterConfig.TransporterConfigT),
    _driverInformation :: f (B.TableEntity DriverInformation.DriverInformationT),
    _farePolicy :: f (B.TableEntity FarePolicy.FarePolicyT)
  }
  deriving (Generic, B.Database be)

transporterDb :: Text -> B.DatabaseSettings be TransporterDb
transporterDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _organization = setSchema dbSchemaName <> Organization.fieldEMod,
        _leads = setSchema dbSchemaName <> Leads.fieldEMod,
        _customer = setSchema dbSchemaName <> Customer.fieldEMod,
        _location = setSchema dbSchemaName <> Location.fieldEMod,
        _quotation = setSchema dbSchemaName <> Quotation.fieldEMod,
        _tracker = setSchema dbSchemaName <> Tracker.fieldEMod,
        _tripReference = setSchema dbSchemaName <> TripReference.fieldEMod,
        _vehicle = setSchema dbSchemaName <> Vehicle.fieldEMod,
        _person = setSchema dbSchemaName <> Person.fieldEMod,
        _case = setSchema dbSchemaName <> Case.fieldEMod,
        _products = setSchema dbSchemaName <> Product.fieldEMod,
        _productInstance = setSchema dbSchemaName <> ProductInstance.fieldEMod,
        _registrationToken = setSchema dbSchemaName <> RegistrationToken.fieldEMod,
        _rating = setSchema dbSchemaName <> Rating.fieldEMod,
        _driverStats = setSchema dbSchemaName <> DriverStats.fieldEMod,
        _transporterConfig = setSchema dbSchemaName <> TransporterConfig.fieldEMod,
        _driverInformation = setSchema dbSchemaName <> DriverInformation.fieldEMod,
        _farePolicy = setSchema dbSchemaName <> FarePolicy.fieldEMod
      }
  where
    setSchema schema = setEntitySchema (Just schema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
