{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.EntityValueLog where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.ActorInfo
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data EntityValueLog = EntityValueLog
  { actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Kernel.Types.ActorInfo.ActorType,
    delta :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    entityId :: Kernel.Prelude.Text,
    entityType :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.EntityValueLog.EntityValueLog,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    operationId :: Kernel.Prelude.Text,
    operationType :: Domain.Types.EntityValueLog.OperationType,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.EntityValueLog.OperationStatus,
    valueAfter :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    valueBefore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OperationStatus = PENDING | APPLIED | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data OperationType = TripCountAdjust | PassTerminate deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''OperationStatus))

$(mkHttpInstancesForEnum (''OperationStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''OperationType))

$(mkHttpInstancesForEnum (''OperationType))
