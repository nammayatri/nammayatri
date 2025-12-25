{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerMinRateSection where

import Data.Aeson as DA
import qualified Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection as DFFPPDM
import Kernel.Prelude
import Kernel.Types.Common

data FPProgressiveDetailsPerMinRateSection = FPProgressiveDetailsPerMinRateSection
  { rideDurationInMin :: Int,
    perMinRate :: Price
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON FPProgressiveDetailsPerMinRateSection

instance ToJSON FPProgressiveDetailsPerMinRateSection

makeFPProgressiveDetailsPerMinRateSection :: DFFPPDM.FullFarePolicyProgressiveDetailsPerMinRateSection -> FPProgressiveDetailsPerMinRateSection
makeFPProgressiveDetailsPerMinRateSection DFFPPDM.FullFarePolicyProgressiveDetailsPerMinRateSection {..} =
  let perMinRatePrice = mkPrice (Just currency) perMinRate
   in FPProgressiveDetailsPerMinRateSection
        { rideDurationInMin = rideDurationInMin,
          perMinRate = perMinRatePrice
        }

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPProgressiveDetailsPerMinRateSectionAPIEntity = FPProgressiveDetailsPerMinRateSectionAPIEntity
  { rideDurationInMin :: Int,
    perMinRate :: PriceAPIEntity
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeFPProgressiveDetailsPerMinRateSectionAPIEntity :: FPProgressiveDetailsPerMinRateSection -> FPProgressiveDetailsPerMinRateSectionAPIEntity
makeFPProgressiveDetailsPerMinRateSectionAPIEntity FPProgressiveDetailsPerMinRateSection {..} =
  let perMinRatePriceAPIEntity = mkPriceAPIEntity perMinRate
   in FPProgressiveDetailsPerMinRateSectionAPIEntity
        { perMinRate = perMinRatePriceAPIEntity,
          ..
        }
