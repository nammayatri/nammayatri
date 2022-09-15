{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Role where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Role as Role
import Storage.Tabular.Role

create :: Role -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Role ->
  m (Maybe Role)
findById = Esq.findById

findAllByLimitOffset ::
  Transactionable m =>
  Maybe Integer ->
  Maybe Integer ->
  m [Role]
findAllByLimitOffset mbLimit mbOffset = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  Esq.findAll $ do
    role <- from $ table @RoleT
    orderBy [asc $ role ^. RoleName]
    limit limitVal
    offset offsetVal
    pure role
