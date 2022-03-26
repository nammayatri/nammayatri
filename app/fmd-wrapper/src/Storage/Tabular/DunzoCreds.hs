{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DunzoCreds where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.DunzoCreds as Domain
import Types.Common

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DunzoCredsT sql=dunzo_creds
      id Text
      clientId Text
      clientSecret Text
      Primary id
      deriving Generic
    |]

instance TEntityKey DunzoCredsT where
  type DomainKey DunzoCredsT = Id Domain.DunzoCreds
  fromKey (DunzoCredsTKey _id) = Id _id
  toKey id = DunzoCredsTKey id.getId

instance TEntity DunzoCredsT Domain.DunzoCreds where
  fromTEntity entity = do
    let DunzoCredsT {..} = entityVal entity
    return $
      Domain.DunzoCreds
        { id = Id id,
          clientId = ClientId clientId,
          clientSecret = ClientSecret clientSecret
        }
  toTType Domain.DunzoCreds {..} =
    DunzoCredsT
      { id = id.getId,
        clientId = getClientId clientId,
        clientSecret = getClientSecret clientSecret
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
