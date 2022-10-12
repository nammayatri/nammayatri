{-# LANGUAGE TypeApplications #-}

module Storage.Queries.ServerAccess where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServerAccess as DServer
import qualified Domain.Types.ServerName as DSN
import Storage.Tabular.ServerAccess

create :: DServer.ServerAccess -> SqlDB ()
create = Esq.create

findByPersonIdAndServerName ::
  (Transactionable m) =>
  Id DP.Person ->
  DSN.ServerName ->
  m (Maybe DServer.ServerAccess)
findByPersonIdAndServerName personId serverName = findOne $ do
  serverAccess <- from $ table @ServerAccessT
  where_ $
    serverAccess ^. ServerAccessPersonId ==. val (toKey personId)
      &&. serverAccess ^. ServerAccessServerName ==. val serverName
  return serverAccess

findAllByPersonId ::
  (Transactionable m) =>
  Id DP.Person ->
  m [DServer.ServerAccess]
findAllByPersonId personId = findAll $ do
  serverAccess <- from $ table @ServerAccessT
  where_ $
    serverAccess ^. ServerAccessPersonId ==. val (toKey personId)
  return serverAccess

deleteById :: Id DServer.ServerAccess -> SqlDB ()
deleteById = Esq.deleteByKey @ServerAccessT
