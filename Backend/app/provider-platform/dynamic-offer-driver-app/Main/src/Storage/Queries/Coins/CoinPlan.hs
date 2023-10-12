{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Coins.CoinPlan
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Coins.CoinPlan
import qualified Domain.Types.Merchant as DM
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Sequelize as Se
import qualified Storage.Beam.Coins.CoinPlan as BeamDC

getCoinPlans :: MonadFlow m => Id DM.Merchant -> m [CoinPlan]
getCoinPlans (Id merchantId) = findAllWithKV [Se.Is BeamDC.merchantId $ Se.Eq merchantId]

getCoinPlanDetails :: MonadFlow m => Text -> m (Maybe CoinPlan)
getCoinPlanDetails coinPlanId = findOneWithKV [Se.Is BeamDC.id $ Se.Eq coinPlanId]

instance FromTType' BeamDC.CoinPlan CoinPlan where
  fromTType' BeamDC.CoinPlanT {..} = do
    pure $
      Just
        CoinPlan
          { id = id,
            subPlanId = subPlanId,
            subPlanMode = subPlanMode,
            requiredCoins = requiredCoins,
            numOfDays = numOfDays,
            coinPlanName = coinPlanName,
            merchantId = merchantId
          }

instance ToTType' BeamDC.CoinPlan CoinPlan where
  toTType' CoinPlan {..} = do
    BeamDC.CoinPlanT
      { BeamDC.id = id,
        BeamDC.subPlanId = subPlanId,
        BeamDC.subPlanMode = subPlanMode,
        BeamDC.requiredCoins = requiredCoins,
        BeamDC.numOfDays = numOfDays,
        BeamDC.coinPlanName = coinPlanName,
        BeamDC.merchantId = merchantId
      }
