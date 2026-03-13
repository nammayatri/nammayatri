{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MorthRCVerification where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id

data MorthRCVerification = MorthRCVerification
  { id :: Kernel.Types.Id.Id Domain.Types.MorthRCVerification.MorthRCVerification,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    -- | RC number used for Morth verification
    rcNumber :: Kernel.Prelude.Text,
    -- | Whether the Morth API call succeeded
    success :: Kernel.Prelude.Bool,
    -- | Validity status returned by Morth (e.g. "ACTIVE", "INVALID")
    rcValidity :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    -- | RC validity expiry date returned by Morth
    rcValidUpto :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    -- | Human-readable message from Morth response
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    -- | HTTP status code from Morth response
    statusCode :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)
