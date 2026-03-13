{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateWallet (module Domain.Types.CorporateWallet, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.CorporateWallet as ReExport
import qualified Domain.Types.CorporateEntity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateWallet = CorporateWallet
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateWallet.CorporateWallet,
    corporateEntityId :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    balance :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    status :: Domain.Types.CorporateWallet.CorporateWalletStatus,
    graceStartedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastTopUpAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateWalletStatus = CW_ACTIVE | GRACE_PERIOD | FROZEN | CLOSED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateWalletStatus)
