{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.PublicTranport where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Database.Persist.TH
import qualified Domain.PublicTranport as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PublicTranportT sql=public_transport
      id Text
      name Text
      stationCode Text
      lat Double
      lon Double
      deriving Generic
      Primary id
    |]

instance TEntityKey PublicTranportT where
  type DomainKey PublicTranportT = Id Domain.PublicTranport
  fromKey (PublicTranportTKey _id) = Id _id
  toKey id = PublicTranportTKey id.getId

instance TEntity PublicTranportT Domain.PublicTranport where
  fromTEntity entity = do
    let PublicTranportT {..} = entityVal entity
    return $
      Domain.PublicTranport
        { id = Id id,
          ..
        }
  toTType Domain.PublicTranport {..} =
    PublicTranportT
      { id = id.getId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
