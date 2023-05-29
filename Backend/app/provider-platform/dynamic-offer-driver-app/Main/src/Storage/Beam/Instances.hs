{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Instances where

-- import qualified Data.Aeson as A
-- import qualified Data.HashMap.Internal as HM
-- import qualified Data.Map.Strict as M
-- import qualified Data.Time as Time
import Database.Beam.MySQL ()

-- import qualified Domain.Types.DriverStats as Domain
-- import Domain.Types.Person (Driver)
-- import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
-- import GHC.Generics (Generic)

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
