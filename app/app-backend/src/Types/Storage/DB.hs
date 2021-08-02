{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.CancellationReason as CancellationReason
import qualified Types.Storage.Geometry as Geometry
import qualified Types.Storage.Issue as Issue
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.RegistrationToken as RegistrationToken
import qualified Types.Storage.RideCancellationReason as RideCancellationReason
import qualified Types.Storage.SearchReqLocation as Location
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Ride as Ride

data AppDb f = AppDb
  { organization :: f (B.TableEntity Organization.OrganizationT),
    issues :: f (B.TableEntity Issue.IssueT),
    searchReqLocation :: f (B.TableEntity Location.SearchReqLocationT),
    person :: f (B.TableEntity Person.PersonT),
    searchRequest :: f (B.TableEntity SearchRequest.SearchRequestT),
    quote :: f (B.TableEntity Quote.QuoteT),
    ride :: f (B.TableEntity Ride.RideT),
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
        searchRequest = setSchema dbSchemaName <> SearchRequest.fieldEMod,
        quote = setSchema dbSchemaName <> Quote.fieldEMod,
        ride = setSchema dbSchemaName <> Ride.fieldEMod,
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
