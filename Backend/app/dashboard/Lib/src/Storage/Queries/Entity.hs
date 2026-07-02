{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Entity where

import Domain.Types.Entity
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

findByEntityShortId :: BeamFlow m r => ShortId Entity -> m (Maybe Entity)
findByEntityShortId entityShortId =
  findOneWithKV [Se.Is BeamE.entityShortId $ Se.Eq $ getShortId entityShortId]

-- Default excludes soft-deleted rows unless includeDeleted=True.
findAllByFilters :: BeamFlow m r => Maybe Bool -> m [Entity]
findAllByFilters mbIncludeDeleted = do
  let deletedConds = case mbIncludeDeleted of
        Just True -> []
        _ -> [Se.Is BeamE.deleted $ Se.Eq False]
  findAllWithKV [Se.And deletedConds]

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
            entityShortId = ShortId entityShortId,
            ..
          }

instance ToTType' BeamE.Entity Entity where
  toTType' Entity {..} =
    BeamE.EntityT
      { id = getId id,
        entityShortId = getShortId entityShortId,
        ..
      }
