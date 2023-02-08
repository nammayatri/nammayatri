{-# LANGUAGE TypeApplications #-}

module Storage.Queries.AccessMatrix where

import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.AccessMatrix

create :: DMatrix.AccessMatrixItem -> SqlDB ()
create = Esq.create

findByRoleIdAndEntity ::
  (Transactionable m) =>
  Id DRole.Role ->
  DMatrix.ApiEntity ->
  m (Maybe DMatrix.AccessMatrixItem)
findByRoleIdAndEntity roleId apiEntity = findOne $ do
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

updateUserAccessType :: Id DMatrix.AccessMatrixItem -> DMatrix.UserAccessType -> SqlDB ()
updateUserAccessType accessMatrixItemId userAccessType = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ AccessMatrixUserAccessType =. val userAccessType,
        AccessMatrixUpdatedAt =. val now
      ]
    where_ $ tbl ^. AccessMatrixTId ==. val (toKey accessMatrixItemId)
