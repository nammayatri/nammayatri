{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DraftTicketChangeHistory where

import Data.Aeson
import qualified Domain.Types.DraftTicketChange
import qualified Domain.Types.EventManagement
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DraftTicketChangeHistory = DraftTicketChangeHistory
  { createdAt :: Kernel.Prelude.UTCTime,
    draftPayload :: Kernel.Prelude.Maybe Domain.Types.EventManagement.TicketPlaceDef,
    id :: Kernel.Types.Id.Id Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory,
    isApprovalRequired :: Kernel.Prelude.Bool,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reviewedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.DraftTicketChange.DraftStatus,
    ticketMerchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketPlaceId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
