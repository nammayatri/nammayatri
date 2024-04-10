{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CancellationReason (module Domain.Types.CancellationReason, module ReExport) where

import Data.Aeson (eitherDecode)
import Domain.Types.Extra.CancellationReason as ReExport
import qualified Domain.Types.Extra.CancellationReason
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data CancellationReason = CancellationReason
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    onAssign :: Kernel.Prelude.Bool,
    onConfirm :: Kernel.Prelude.Bool,
    onInit :: Kernel.Prelude.Bool,
    onSearch :: Kernel.Prelude.Bool,
    priority :: Kernel.Prelude.Int,
    reasonCode :: Domain.Types.Extra.CancellationReason.CancellationReasonCode,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancellationStage = OnSearch | OnInit | OnConfirm | OnAssign deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), ToParamSchema)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''CancellationStage))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''CancellationStage))
