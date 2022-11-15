{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BlackListOrg where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.BlackListOrg as Domain

derivePersistField "Domain.BlackListOrgType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BlackListOrgT sql=black_list_org
      id Text
      shortId Text
      orgType Domain.BlackListOrgType sql=type
      Primary id
      Unique OrganizationShortId
      deriving Generic
    |]

instance TEntityKey BlackListOrgT where
  type DomainKey BlackListOrgT = Id Domain.BlackListOrg
  fromKey (BlackListOrgTKey _id) = Id _id
  toKey (Id id) = BlackListOrgTKey id

instance TType BlackListOrgT Domain.BlackListOrg where
  fromTType BlackListOrgT {..} = do
    return $
      Domain.BlackListOrg
        { id = Id id,
          shortId = ShortId shortId,
          _type = orgType,
          ..
        }
  toTType Domain.BlackListOrg {..} =
    BlackListOrgT
      { id = getId id,
        shortId = getShortId shortId,
        orgType = _type,
        ..
      }
