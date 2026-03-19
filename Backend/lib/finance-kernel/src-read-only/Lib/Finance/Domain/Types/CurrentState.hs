{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.CurrentState where

import Data.Aeson
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.StateTransition
import qualified Tools.Beam.UtilsTH

data CurrentState = CurrentState
  { currentState :: Lib.Finance.Domain.Types.StateTransition.PaymentState,
    entityId :: Kernel.Prelude.Text,
    entityType :: Lib.Finance.Domain.Types.StateTransition.PaymentEntityType,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
