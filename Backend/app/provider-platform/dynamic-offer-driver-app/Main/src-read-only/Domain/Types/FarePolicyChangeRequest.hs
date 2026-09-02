{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicyChangeRequest where

import Data.Aeson
import qualified Domain.Types.FareProduct
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FarePolicyChangeRequest = FarePolicyChangeRequest
  { action :: Domain.Types.FarePolicyChangeRequest.FarePolicyChangeAction,
    checkedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fareProductId :: Kernel.Types.Id.Id Domain.Types.FareProduct.FareProduct,
    fareProductSnapshot :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FarePolicyChangeRequest.FarePolicyChangeRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestedBy :: Kernel.Prelude.Text,
    status :: Domain.Types.FarePolicyChangeRequest.FarePolicyChangeStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FarePolicyChangeAction = REMOVE_FARE_PRODUCT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FarePolicyChangeStatus = PENDING | APPROVED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FarePolicyChangeAction))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FarePolicyChangeStatus))
