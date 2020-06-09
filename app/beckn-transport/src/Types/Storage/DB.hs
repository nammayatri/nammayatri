{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)
import Storage.DB.Config (dbSchema)
import qualified Types.Storage.Customer as Customer
import qualified Types.Storage.Leads as Leads
import qualified Types.Storage.Quotation as Quotation
import qualified Types.Storage.Tracker as Tracker
import qualified Types.Storage.TripReference as TripReference

data TransporterDb f
  = TransporterDb
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
        _caseProduct :: f (B.TableEntity CaseProduct.CaseProductT),
        _registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT)
      }
  deriving (Generic, B.Database be)

transporterDb :: B.DatabaseSettings be TransporterDb
transporterDb =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _organization = setSchema <> Organization.fieldEMod,
        _leads = setSchema <> Leads.fieldEMod,
        _customer = setSchema <> Customer.fieldEMod,
        _location = setSchema <> Location.fieldEMod,
        _quotation = setSchema <> Quotation.fieldEMod,
        _tracker = setSchema <> Tracker.fieldEMod,
        _tripReference = setSchema <> TripReference.fieldEMod,
        _vehicle = setSchema <> Vehicle.fieldEMod,
        _person = setSchema <> Person.fieldEMod,
        _case = setSchema <> Case.fieldEMod,
        _products = setSchema <> Product.fieldEMod,
        _caseProduct = setSchema <> CaseProduct.fieldEMod,
        _registrationToken = setSchema <> RegistrationToken.fieldEMod
      }
  where
    setSchema = setEntitySchema (Just dbSchema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
