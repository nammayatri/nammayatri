{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Webengage where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Webengage as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    WebengageT sql=webengage
        id Text
        version Text
        contentTemplateId Text
        principalEntityId Text
        infoMessageId Text
        webMessageId Text
        toNumber Text
        status Text Maybe
        Primary id
        deriving Generic
    |]

instance TEntityKey WebengageT where
  type DomainKey WebengageT = Id Domain.Webengage
  fromKey (WebengageTKey _id) = Id _id
  toKey (Id id) = WebengageTKey id

instance TType WebengageT Domain.Webengage where
  fromTType WebengageT {..} = do
    return $
      Domain.Webengage
        { id = Id id,
          ..
        }
  toTType Domain.Webengage {..} =
    WebengageT
      { id = getId id,
        ..
      }