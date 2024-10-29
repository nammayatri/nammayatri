{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.CancellationReason where

import Data.Aeson
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField)
import Kernel.Prelude
import Kernel.Utils.TH (mkFromHttpInstanceForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

-- Extra code goes here --

deriving newtype instance FromField CancellationReasonCode

deriving newtype instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CancellationReasonCode

instance BeamSqlBackend be => B.HasSqlEqualityCheck be CancellationReasonCode

instance FromBackendRow Postgres CancellationReasonCode

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

getCancellationReasonCode :: CancellationReasonCode -> Text
getCancellationReasonCode (CancellationReasonCode text) = text
