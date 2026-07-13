{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Entity where

import Domain.Types.Entity
import qualified Domain.Types.Merchant as DMerchant
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Entity as BeamE

create :: BeamFlow m r => Entity -> m ()
create = createWithKV

findById :: BeamFlow m r => Id Entity -> m (Maybe Entity)
findById entityId = findOneWithKV [Se.Is BeamE.id $ Se.Eq $ getId entityId]

findByMerchantAndShortId :: BeamFlow m r => Id DMerchant.Merchant -> ShortId Entity -> m (Maybe Entity)
findByMerchantAndShortId merchantId shortId =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamE.merchantId $ Se.Eq $ getId merchantId,
          Se.Is BeamE.entityShortId $ Se.Eq $ getShortId shortId
        ]
    ]

-- Full scan; entity table is bounded (~35 rows per merchant).
findAllByFilters :: BeamFlow m r => Id DMerchant.Merchant -> Maybe Bool -> m [Entity]
findAllByFilters merchantId mbIncludeDeleted = do
  let baseConds = [Se.Is BeamE.merchantId $ Se.Eq $ getId merchantId]
      deletedConds = case mbIncludeDeleted of
        Just True -> []
        _ -> [Se.Is BeamE.deleted $ Se.Eq False]
  findAllWithKV [Se.And (baseConds ++ deletedConds)]

updateByPrimaryKey :: BeamFlow m r => Entity -> m ()
updateByPrimaryKey e =
  updateWithKV
    [ Se.Set BeamE.entityName e.entityName,
      Se.Set BeamE.entityShortId (getShortId e.entityShortId),
      Se.Set BeamE.deleted e.deleted,
      Se.Set BeamE.updatedAt e.updatedAt
    ]
    [Se.Is BeamE.id $ Se.Eq (getId e.id)]

instance FromTType' BeamE.Entity Entity where
  fromTType' BeamE.EntityT {..} =
    pure $
      Just
        Entity
          { id = Id id,
            merchantId = Id merchantId,
            entityShortId = ShortId entityShortId,
            ..
          }

instance ToTType' BeamE.Entity Entity where
  toTType' Entity {..} =
    BeamE.EntityT
      { id = getId id,
        merchantId = getId merchantId,
        entityShortId = getShortId entityShortId,
        ..
      }
