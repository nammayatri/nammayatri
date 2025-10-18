{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.Pass where

import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (sqlValueSyntax))
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Pass
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

$(mkBeamInstancesForJSON ''Domain.Types.Pass.Benefit)

$(mkBeamInstancesForEnumAndList ''Domain.Types.Pass.PassDocumentType)

-- Beam instances for [Data.Aeson.Value]
instance FromField [A.Value] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [A.Value] where
  sqlValueSyntax valueList =
    let x = (show <$> valueList :: [Text])
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [A.Value]

instance FromBackendRow Postgres [A.Value]
