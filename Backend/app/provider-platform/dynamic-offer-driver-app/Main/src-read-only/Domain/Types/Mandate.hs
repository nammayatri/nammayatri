{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Mandate where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Mandate = Mandate
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    endDate :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.Mandate.Mandate,
    mandatePaymentFlow :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maxAmount :: Kernel.Types.Common.HighPrecMoney,
    payerApp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerAppName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startDate :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.Mandate.MandateStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MandateStatus = ACTIVE | INACTIVE deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''MandateStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''MandateStatus)

{-
	DSL Source Link: file://./../../../spec/Storage/Mandate.yaml
-}
