{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)

data AppDb f = AppDb
  { _organization :: f (B.TableEntity Organization.OrganizationT),
    _location :: f (B.TableEntity Location.LocationT),
    _person :: f (B.TableEntity Person.PersonT),
    _case :: f (B.TableEntity Case.CaseT),
    _productInstance :: f (B.TableEntity ProductInstance.ProductInstanceT),
    _products :: f (B.TableEntity Products.ProductsT),
    _registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT)
  }
  deriving (Generic, B.Database be)

appDb :: Text -> B.DatabaseSettings be AppDb
appDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _organization = setSchema dbSchemaName <> Organization.fieldEMod,
        _location = setSchema dbSchemaName <> Location.fieldEMod,
        _person = setSchema dbSchemaName <> Person.fieldEMod,
        _case = setSchema dbSchemaName <> Case.fieldEMod,
        _productInstance = setSchema dbSchemaName <> ProductInstance.fieldEMod,
        _products = setSchema dbSchemaName <> Products.fieldEMod,
        _registrationToken = setSchema dbSchemaName <> RegistrationToken.fieldEMod
      }
  where
    setSchema schema = setEntitySchema (Just schema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
