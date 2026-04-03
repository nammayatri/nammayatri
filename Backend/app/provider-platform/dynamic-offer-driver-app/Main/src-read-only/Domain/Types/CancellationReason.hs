{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.CancellationReason where
import Kernel.Prelude
import Data.Aeson
import qualified Data.Text
import qualified Tools.Beam.UtilsTH



data CancellationReason
    = CancellationReason {description :: Data.Text.Text,
                          enabled :: Kernel.Prelude.Bool,
                          priority :: Kernel.Prelude.Int,
                          reasonCode :: Domain.Types.CancellationReason.CancellationReasonCode,
                          createdAt :: Kernel.Prelude.UTCTime,
                          updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
newtype CancellationReasonCode = CancellationReasonCode Data.Text.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CancellationReasonCode))

