{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.Notification (Payment.NotificationStatus (..)) where

import qualified Database.Beam as B
import Database.Beam.Backend (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)

-- Extra code goes here --

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Payment.NotificationStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromField Payment.NotificationStatus where
  fromField = fromFieldEnum

instance FromBackendRow Postgres Payment.NotificationStatus

deriving instance Ord Payment.NotificationStatus

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Payment.NotificationStatus
