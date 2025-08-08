{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PickupInstructions where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PickupInstructions = PickupInstructions
  { createdAt :: Kernel.Prelude.UTCTime,
    geohash :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PickupInstructions.PickupInstructions,
    instruction :: Kernel.Prelude.Text,
    mediaFileId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile),
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)
