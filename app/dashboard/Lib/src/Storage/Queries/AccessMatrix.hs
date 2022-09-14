{-# LANGUAGE TypeApplications #-}

module Storage.Queries.AccessMatrix where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
import Storage.Tabular.AccessMatrix

create :: DMatrix.AccessMatrix -> SqlDB ()
create = Esq.create

findByRoleAndEntity ::
  (Transactionable m) =>
  Id DRole.Role ->
  DMatrix.ApiEntity ->
  m (Maybe DMatrix.AccessMatrix)
findByRoleAndEntity roleId apiEntity = findOne $ do
  accessMatrix <- from $ table @AccessMatrixT
  where_ $
    accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
      &&. accessMatrix ^. AccessMatrixApiEntity ==. val apiEntity
  return accessMatrix
