{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.Notification (Payment.NotificationStatus (..)) where

import Data.Aeson
import qualified Database.Beam as B
import Database.Beam.Backend (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import GHC.Generics (Generic)
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

-- Extra code goes here --

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.NotificationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromField Payment.NotificationStatus where
  fromField = fromFieldEnum

instance FromBackendRow Postgres Payment.NotificationStatus

deriving instance Ord Payment.NotificationStatus

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.NotificationStatus
