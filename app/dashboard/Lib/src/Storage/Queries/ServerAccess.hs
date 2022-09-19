{-# LANGUAGE TypeApplications #-}

module Storage.Queries.ServerAccess where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DReg
import qualified Domain.Types.ServerAccess as DServer
import Storage.Tabular.ServerAccess

create :: DServer.ServerAccess -> SqlDB ()
create = Esq.create

findByPersonIdAndServerName ::
  (Transactionable m) =>
  Id DP.Person ->
  DReg.ServerName ->
  m (Maybe DServer.ServerAccess)
findByPersonIdAndServerName personId serverName = findOne $ do
  serverAccess <- from $ table @ServerAccessT
  where_ $
    serverAccess ^. ServerAccessPersonId ==. val (toKey personId)
      &&. serverAccess ^. ServerAccessServerName ==. val serverName
  return serverAccess
