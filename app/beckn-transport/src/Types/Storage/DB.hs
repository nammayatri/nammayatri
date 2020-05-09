{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Customer as Customer
import qualified Types.Storage.Driver as Driver
import qualified Types.Storage.Leads as Leads
import qualified Types.Storage.Location as Location
import qualified Types.Storage.Quotation as Quotation
import qualified Types.Storage.Tracker as Tracker
import qualified Types.Storage.TripReference as TripReference
import qualified Types.Storage.Vehicle as Vehicle

data TransporterDb f = TransporterDb
  { _organization :: f (B.TableEntity Organization.OrganizationT),
    _leads :: f (B.TableEntity Leads.LeadsT),
    _customer :: f (B.TableEntity Customer.CustomerT),
    _driver :: f (B.TableEntity Driver.DriverT),
    _location :: f (B.TableEntity Location.LocationT),
    _quotation :: f (B.TableEntity Quotation.QuotationT),
    _tracker :: f (B.TableEntity Tracker.TrackerT),
    _tripReference :: f (B.TableEntity TripReference.TripReferenceT),
    _vehicle :: f (B.TableEntity Vehicle.VehicleT),
    _person :: f (B.TableEntity Person.PersonT),
    _case :: f (B.TableEntity Case.CaseT)
  }
  deriving (Generic, B.Database be)

transporterDb :: B.DatabaseSettings be TransporterDb
transporterDb =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _organization = Organization.fieldEMod,
        _leads = Leads.fieldEMod,
        _customer = Customer.fieldEMod,
        _driver = Driver.fieldEMod,
        _location = Location.fieldEMod,
        _quotation = Quotation.fieldEMod,
        _tracker = Tracker.fieldEMod,
        _tripReference = TripReference.fieldEMod,
        _vehicle = Vehicle.fieldEMod,
        _person = Person.fieldEMod,
        _case = Case.fieldEMod
      }
