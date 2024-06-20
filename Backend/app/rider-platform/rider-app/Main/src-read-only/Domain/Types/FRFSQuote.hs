{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSQuote where

import Data.Aeson
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSQuote = FRFSQuote
  { id :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    bppItemId :: Kernel.Prelude.Text,
    searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    fromStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    toStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    quantity :: Kernel.Prelude.Int,
    stationsJson :: Kernel.Prelude.Text,
    vehicleType :: Domain.Types.Station.FRFSVehicleType,
    bppSubscriberId :: Kernel.Prelude.Text,
    bppSubscriberUrl :: Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    providerDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    price :: Kernel.Types.Common.Price,
    validTill :: Kernel.Prelude.UTCTime,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data FRFSQuoteType = SingleJourney | ReturnJourney | Pass | SpecialFareSingleJourney deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FRFSQuoteType)
