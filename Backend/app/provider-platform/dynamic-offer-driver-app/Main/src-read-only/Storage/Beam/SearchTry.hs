{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchTry where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.SearchTry
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified SharedLogic.Type
import Tools.Beam.UtilsTH

data SearchTryT f = SearchTryT
  { baseFare :: B.C f Kernel.Types.Common.Money,
    baseFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    billingCategory :: B.C f (Kernel.Prelude.Maybe SharedLogic.Type.BillingCategory),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    customerExtraFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    customerExtraFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimateId :: B.C f Kernel.Prelude.Text,
    estimateIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    id :: B.C f Kernel.Prelude.Text,
    isAdvancedBookingEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    messageId :: B.C f Kernel.Prelude.Text,
    petCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    petChargesAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    preferSafetyPlus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    requestId :: B.C f Kernel.Prelude.Text,
    searchRepeatCounter :: B.C f Kernel.Prelude.Int,
    searchRepeatType :: B.C f Domain.Types.SearchTry.SearchRepeatType,
    serviceTierArray :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.SearchTry.SearchTryStatus,
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    validTill :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    vehicleVariant :: B.C f Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchTryT where
  data PrimaryKey SearchTryT f = SearchTryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchTryId . id

type SearchTry = SearchTryT Identity

$(enableKVPG ''SearchTryT ['id] [['estimateId], ['requestId]])

$(mkTableInstances ''SearchTryT "search_try")
