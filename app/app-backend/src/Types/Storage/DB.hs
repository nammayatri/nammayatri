{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.Products as Products
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Tracker as Tracker
import qualified Types.Storage.TripReference as TripReference

data AppDb f = AppDb
  { _organization :: f (B.TableEntity Organization.OrganizationT),
    _location :: f (B.TableEntity Location.LocationT),
    _tracker :: f (B.TableEntity Tracker.TrackerT),
    _tripReference :: f (B.TableEntity TripReference.TripReferenceT),
    _person :: f (B.TableEntity Person.PersonT),
    _case :: f (B.TableEntity Case.CaseT),
    _caseProduct :: f (B.TableEntity CaseProduct.CaseProductT),
    _products :: f (B.TableEntity Products.ProductsT),
    _registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT)
  }
  deriving (Generic, B.Database be)

appDb :: B.DatabaseSettings be AppDb
appDb =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _organization = Organization.fieldEMod,
        _location = Location.fieldEMod,
        _tracker = Tracker.fieldEMod,
        _tripReference = TripReference.fieldEMod,
        _person = Person.fieldEMod,
        _case = Case.fieldEMod,
        _caseProduct = CaseProduct.fieldEMod,
        _products = Products.fieldEMod,
        _registrationToken = RegistrationToken.fieldEMod
      }
