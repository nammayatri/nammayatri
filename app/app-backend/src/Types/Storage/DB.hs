{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.CancellationReason as CancellationReason
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Geometry as Geometry
import qualified Types.Storage.Issue as Issue
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as ProductInstance
import qualified Types.Storage.Products as Products
import qualified Types.Storage.RegistrationToken as RegistrationToken
import qualified Types.Storage.RideCancellationReason as RideCancellationReason
import qualified Types.Storage.SearchReqLocation as Location

data AppDb f = AppDb
  { organization :: f (B.TableEntity Organization.OrganizationT),
    issues :: f (B.TableEntity Issue.IssueT),
    searchReqLocation :: f (B.TableEntity Location.SearchReqLocationT),
    person :: f (B.TableEntity Person.PersonT),
    _case :: f (B.TableEntity Case.CaseT),
    productInstance :: f (B.TableEntity ProductInstance.ProductInstanceT),
    products :: f (B.TableEntity Products.ProductsT),
    registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT),
    geometry :: f (B.TableEntity Geometry.GeometryT),
    cancellationReason :: f (B.TableEntity CancellationReason.CancellationReasonT),
    rideCancellationReason :: f (B.TableEntity RideCancellationReason.RideCancellationReasonT)
  }
  deriving (Generic, B.Database be)

appDb :: Text -> B.DatabaseSettings be AppDb
appDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { organization = setSchema dbSchemaName <> Organization.fieldEMod,
        issues = setSchema dbSchemaName <> Issue.fieldEMod,
        searchReqLocation = setSchema dbSchemaName <> Location.fieldEMod,
        person = setSchema dbSchemaName <> Person.fieldEMod,
        _case = setSchema dbSchemaName <> Case.fieldEMod,
        productInstance = setSchema dbSchemaName <> ProductInstance.fieldEMod,
        products = setSchema dbSchemaName <> Products.fieldEMod,
        registrationToken = setSchema dbSchemaName <> RegistrationToken.fieldEMod,
        geometry = setSchema dbSchemaName <> Geometry.fieldEMod,
        cancellationReason = setSchema dbSchemaName <> CancellationReason.fieldEMod,
        rideCancellationReason = setSchema dbSchemaName <> RideCancellationReason.fieldEMod
      }
  where
    setSchema schema = setEntitySchema (Just schema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
