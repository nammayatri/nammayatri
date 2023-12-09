{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketBookingPeopleCategory where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketBookingPeopleCategory
import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketBookingPeopleCategoryT f = TicketBookingPeopleCategoryT
  { id :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    numberOfUnits :: B.C f Kernel.Prelude.Int,
    pricePerUnit :: B.C f Kernel.Types.Common.HighPrecMoney,
    ticketBookingServiceCategoryId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingPeopleCategoryT where
  data PrimaryKey TicketBookingPeopleCategoryT f = TicketBookingPeopleCategoryId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = TicketBookingPeopleCategoryId . id

type TicketBookingPeopleCategory = TicketBookingPeopleCategoryT Identity

$(enableKVPG ''TicketBookingPeopleCategoryT ['id] [])

$(mkTableInstances ''TicketBookingPeopleCategoryT "ticket_booking_people_category")
