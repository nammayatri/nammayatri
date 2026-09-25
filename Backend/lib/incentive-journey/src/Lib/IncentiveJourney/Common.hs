{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Common
  ( module Lib.IncentiveJourney.Domain.Types.Common,
    MerchantInfo (..),
    PersonInfo (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Id
import Lib.IncentiveJourney.Domain.Types.Common

-- | Minimal merchant projection for dashboard handlers (avoids app Domain.Types).
data MerchantInfo = MerchantInfo
  { id :: Id Merchant,
    shortId :: ShortId Merchant
  }
  deriving (Generic, Show)

-- | Minimal person projection for assign/unassign merchant-city checks.
data PersonInfo = PersonInfo
  { id :: Id Person,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity
  }
  deriving (Generic, Show)
