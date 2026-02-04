{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.CurrentState where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.StateTransition

data CurrentState = CurrentState
  { currentState :: Lib.Finance.Domain.Types.StateTransition.PaymentState,
    entityId :: Kernel.Prelude.Text,
    entityType :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
