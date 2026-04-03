{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Reminder where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data Reminder
    = Reminder {createdAt :: Kernel.Prelude.UTCTime,
                currentIntervalIndex :: Kernel.Prelude.Int,
                documentType :: Domain.Types.DocumentVerificationConfig.DocumentType,
                driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                dueDate :: Kernel.Prelude.UTCTime,
                entityId :: Kernel.Prelude.Text,
                id :: Kernel.Types.Id.Id Domain.Types.Reminder.Reminder,
                merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
                reminderDate :: Kernel.Prelude.UTCTime,
                status :: Domain.Types.Reminder.ReminderStatus,
                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data ReminderStatus = PENDING | SENT | COMPLETED | CANCELLED deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''ReminderStatus))

