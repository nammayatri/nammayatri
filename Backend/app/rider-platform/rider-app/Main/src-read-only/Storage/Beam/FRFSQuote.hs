{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSQuote where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuote
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSQuoteT f = FRFSQuoteT
  { _type :: B.C f Domain.Types.FRFSQuote.FRFSQuoteType,
    bppDelayedInterest :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bppItemId :: B.C f Kernel.Prelude.Text,
    bppSubscriberId :: B.C f Kernel.Prelude.Text,
    bppSubscriberUrl :: B.C f Kernel.Prelude.Text,
    discountedTickets :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    discountsJson :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    eventDiscountAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fromStationId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    oldCacheDump :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgTransactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    providerDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerId :: B.C f Kernel.Prelude.Text,
    providerName :: B.C f Kernel.Prelude.Text,
    quantity :: B.C f Kernel.Prelude.Int,
    riderId :: B.C f Kernel.Prelude.Text,
    routeStationsJson :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    searchId :: B.C f Kernel.Prelude.Text,
    stationsJson :: B.C f Kernel.Prelude.Text,
    toStationId :: B.C f Kernel.Prelude.Text,
    validTill :: B.C f Kernel.Prelude.UTCTime,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSQuoteT where
  data PrimaryKey FRFSQuoteT f = FRFSQuoteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSQuoteId . id

type FRFSQuote = FRFSQuoteT Identity

$(enableKVPG ''FRFSQuoteT ['id] [['searchId]])

$(mkTableInstances ''FRFSQuoteT "frfs_quote")
