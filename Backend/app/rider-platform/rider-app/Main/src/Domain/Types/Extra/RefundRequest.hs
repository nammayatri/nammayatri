{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types.Extra.RefundRequest where

import Data.Aeson
import Data.OpenApi
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField)
import Kernel.Prelude
import Servant

-- Extra code goes here --

newtype RefundRequestCode = RefundRequestCode {getRefundRequestCode :: Text}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData, ToHttpApiData)

deriving newtype instance FromField RefundRequestCode

deriving newtype instance HasSqlValueSyntax be Text => HasSqlValueSyntax be RefundRequestCode

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RefundRequestCode

instance FromBackendRow Postgres RefundRequestCode
