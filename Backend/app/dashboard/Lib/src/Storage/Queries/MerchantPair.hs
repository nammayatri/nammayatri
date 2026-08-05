{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MerchantPair where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPair as DMP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.MerchantPair as BeamMP

-- | The pair row containing this merchant on either side, if any.
findByMerchantId :: BeamFlow m r => Id DM.Merchant -> m (Maybe DMP.MerchantPair)
findByMerchantId merchantId =
  findOneWithKV
    [ Se.Or
        [ Se.Is BeamMP.bapMerchantId $ Se.Eq $ Just $ getId merchantId,
          Se.Is BeamMP.bppMerchantId $ Se.Eq $ Just $ getId merchantId
        ]
    ]

instance FromTType' BeamMP.MerchantPair DMP.MerchantPair where
  fromTType' BeamMP.MerchantPairT {..} = do
    return $
      Just
        DMP.MerchantPair
          { bapMerchantId = Id <$> bapMerchantId,
            bppMerchantId = Id <$> bppMerchantId,
            ..
          }

instance ToTType' BeamMP.MerchantPair DMP.MerchantPair where
  toTType' DMP.MerchantPair {..} =
    BeamMP.MerchantPairT
      { bapMerchantId = getId <$> bapMerchantId,
        bppMerchantId = getId <$> bppMerchantId,
        ..
      }
