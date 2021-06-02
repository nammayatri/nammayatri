{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.Organization as Organization
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)

newtype AppDb f = AppDb
  { organization :: f (B.TableEntity Organization.OrganizationT)
  }
  deriving (Generic, B.Database be)

appDb :: Text -> B.DatabaseSettings be AppDb
appDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { organization = setSchema dbSchemaName <> Organization.fieldEMod
      }
  where
    setSchema schema = setEntitySchema (Just schema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
