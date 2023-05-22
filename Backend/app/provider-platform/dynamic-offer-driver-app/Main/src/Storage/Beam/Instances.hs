{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Instances where

-- import qualified Data.Aeson as A
import Data.ByteString.Internal (unpackChars)
-- import qualified Data.HashMap.Internal as HM
-- import qualified Data.Map.Strict as M
import Data.Serialize
-- import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
-- import qualified Domain.Types.DriverStats as Domain
-- import Domain.Types.Person (Driver)
-- import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
-- import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)

-- import Lib.Utils
-- import Lib.UtilsTH
-- import Sequelize
-- import Storage.Tabular.Person (PersonTId)

-- instance FromField Meters where
--   fromField f mbValue = case mbValue of
--     Nothing -> DPSF.returnError UnexpectedNull f mempty
--     Just value' ->
--       case readMaybe (unpackChars value') of
--         Just val -> pure $ Meters val
--         _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

-- instance FromBackendRow Postgres Meters
