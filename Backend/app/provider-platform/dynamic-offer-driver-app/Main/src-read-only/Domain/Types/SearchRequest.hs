{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequest where

import Data.Aeson
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderDetails
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH
import qualified Tools.Maps

data SearchRequest = SearchRequest
  { area :: Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area,
    autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    bapCity :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City,
    bapCountry :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.Country,
    bapId :: Kernel.Prelude.Text,
    bapUri :: Kernel.Types.Common.BaseUrl,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    customerCancellationDues :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    customerLanguage :: Kernel.Prelude.Maybe Tools.Maps.Language,
    device :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disabilityTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverDefaultExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocation :: Domain.Types.Location.Location,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    isBlockedRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isCustomerPrefferedSearchRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isReallocationEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isScheduled :: Kernel.Prelude.Bool,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    messageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupZoneGateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    riderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails),
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.UTCTime,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    transactionId :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)
