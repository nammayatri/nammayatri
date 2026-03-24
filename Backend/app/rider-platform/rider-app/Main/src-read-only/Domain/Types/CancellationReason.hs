{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.CancellationReason (module Domain.Types.CancellationReason, module ReExport) where
import Kernel.Prelude
import Data.Aeson
import Domain.Types.Extra.CancellationReason as ReExport
import qualified Domain.Types.Extra.CancellationReason
import qualified Kernel.Utils.TH
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data CancellationReason
    = CancellationReason {createdAt :: Kernel.Prelude.UTCTime,
                          description :: Kernel.Prelude.Text,
                          enabled :: Kernel.Prelude.Bool,
                          onAssign :: Kernel.Prelude.Bool,
                          onConfirm :: Kernel.Prelude.Bool,
                          onInit :: Kernel.Prelude.Bool,
                          onSearch :: Kernel.Prelude.Bool,
                          priority :: Kernel.Prelude.Int,
                          reasonCode :: Domain.Types.Extra.CancellationReason.CancellationReasonCode,
                          updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data CancellationStage = OnSearch | OnInit | OnConfirm | OnAssign deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ToParamSchema)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''CancellationStage))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''CancellationStage))

