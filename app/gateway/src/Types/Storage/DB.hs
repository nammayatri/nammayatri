{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import qualified Beckn.Types.Storage.ExternalTrail as ExternalTrail
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Trail as Trail
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import EulerHS.Prelude hiding (id)
import Storage.DB.Config (dbSchema)

data AppDb f = AppDb
  { _organization :: f (B.TableEntity Organization.OrganizationT),
    _trail :: f (B.TableEntity Trail.TrailT),
    _externalTrail :: f (B.TableEntity ExternalTrail.ExternalTrailT)
  }
  deriving (Generic, B.Database be)

appDb :: B.DatabaseSettings be AppDb
appDb =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _organization = setSchema <> Organization.fieldEMod,
        _trail = setSchema <> Trail.fieldEMod,
        _externalTrail = setSchema <> ExternalTrail.fieldEMod
      }
  where
    setSchema = setEntitySchema (Just dbSchema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
