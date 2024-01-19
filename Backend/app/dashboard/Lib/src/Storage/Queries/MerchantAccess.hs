{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.MerchantAccess where

import Data.List (nubBy)
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Merchant as BeamM
import qualified Storage.Beam.MerchantAccess as BeamMA
import Storage.Queries.Merchant ()

create :: BeamFlow m r => DAccess.MerchantAccess -> m ()
create = createWithKV

findByPersonIdAndMerchantId ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  m [DAccess.MerchantAccess]
findByPersonIdAndMerchantId personId merchantId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamMA.personId $ Se.Eq $ getId personId,
          Se.Is BeamMA.merchantId $ Se.Eq $ getId merchantId
        ]
    ]

findByPersonIdAndMerchantIdAndCity ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  City.City ->
  m (Maybe DAccess.MerchantAccess)
findByPersonIdAndMerchantIdAndCity personId merchantId city =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMA.personId $ Se.Eq $ getId personId,
          Se.Is BeamMA.merchantId $ Se.Eq $ getId merchantId,
          Se.Is BeamMA.operatingCity $ Se.Eq city
        ]
    ]

findAllMerchantAccessByPersonId ::
  BeamFlow m r =>
  Id DP.Person ->
  m [DAccess.MerchantAccess]
findAllMerchantAccessByPersonId personId =
  findAllWithKV
    [ Se.Is BeamMA.personId $ Se.Eq $ getId personId
    ]

-- findAllByPersonId ::
--   (Transactionable m) =>
--   Id DP.Person ->
--   m [DMerchant.Merchant]
-- findAllByPersonId personId = findAll $ do
--   (merchantAccess :& merchant) <-
--     from $
--       table @MerchantAccessT
--         `innerJoin` table @MerchantT
--           `Esq.on` ( \(merchantAccess :& merchant) ->
--                        merchantAccess ^. MerchantAccessMerchantId ==. merchant ^. MerchantTId
--                    )
--   where_ $
--     merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
--   return merchant

findAllByPersonId :: BeamFlow m r => Id DP.Person -> m [DMerchant.Merchant]
findAllByPersonId personId = do
  merchantAccessMerchantId <-
    findAllWithKV
      [Se.Is BeamMA.personId $ Se.Eq $ getId personId]
      <&> (getId . DAccess.merchantId <$>)
  findAllWithKV
    [Se.Is BeamM.id $ Se.In merchantAccessMerchantId]

deleteById :: BeamFlow m r => Id DAccess.MerchantAccess -> m ()
deleteById merchantAccessId = deleteWithKV [Se.Is BeamMA.id $ Se.Eq $ getId merchantAccessId]

updatePerson2faForMerchant ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  Text ->
  m ()
updatePerson2faForMerchant personId merchantId secretKey =
  updateWithKV
    [ Se.Set BeamMA.secretKey $ Just secretKey,
      Se.Set BeamMA.is2faEnabled True
    ]
    [ Se.And
        [ Se.Is BeamMA.personId $ Se.Eq $ getId personId,
          Se.Is BeamMA.merchantId $ Se.Eq $ getId merchantId
        ]
    ]

findAllUserAccountForMerchant ::
  BeamFlow m r =>
  Id DMerchant.Merchant ->
  m [DAccess.MerchantAccess]
findAllUserAccountForMerchant merchantId = do
  res <-
    findAllWithKV
      [ Se.Is BeamMA.merchantId $ Se.Eq $ getId merchantId
      ]
  pure $ getUniquePersonId res
  where
    getUniquePersonId = nubBy (\x y -> x.personId == y.personId)

instance FromTType' BeamMA.MerchantAccess DAccess.MerchantAccess where
  fromTType' BeamMA.MerchantAccessT {..} = do
    return $
      Just
        DAccess.MerchantAccess
          { id = Id id,
            merchantId = Id merchantId,
            personId = Id personId,
            merchantShortId = ShortId merchantShortId,
            ..
          }

instance ToTType' BeamMA.MerchantAccess DAccess.MerchantAccess where
  toTType' DAccess.MerchantAccess {..} =
    BeamMA.MerchantAccessT
      { id = getId id,
        merchantId = getId merchantId,
        personId = getId personId,
        merchantShortId = getShortId merchantShortId,
        ..
      }
