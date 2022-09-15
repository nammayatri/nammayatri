{-# LANGUAGE TypeApplications #-}

module Storage.Queries.AccessMatrix where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
import Storage.Tabular.AccessMatrix

create :: DMatrix.AccessMatrixItem -> SqlDB ()
create = Esq.create

findByRoleAndEntity ::
  (Transactionable m) =>
  Id DRole.Role ->
  DMatrix.ApiEntity ->
  m (Maybe DMatrix.AccessMatrixItem)
findByRoleAndEntity roleId apiEntity = findOne $ do
  accessMatrix <- from $ table @AccessMatrixT
  where_ $
    accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
      &&. accessMatrix ^. AccessMatrixApiEntity ==. val apiEntity
  return accessMatrix

findAllByRoles ::
  Transactionable m =>
  [DRole.Role] ->
  m [DMatrix.AccessMatrixItem]
findAllByRoles roles = do
  let roleKeys = map (toKey . (.id)) roles
  Esq.findAll $ do
    accessMatrix <- from $ table @AccessMatrixT
    where_ $
      accessMatrix ^. AccessMatrixRoleId `in_` valList roleKeys
    return accessMatrix

findAllByRoleId ::
  Transactionable m =>
  Id DRole.Role ->
  m [DMatrix.AccessMatrixItem]
findAllByRoleId roleId = do
  Esq.findAll $ do
    accessMatrix <- from $ table @AccessMatrixT
    where_ $
      accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
    return accessMatrix
