{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Tabular.SearchLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Data.Time (UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings]
  [defaultQQ|
    SearchLocationT sql=search_location
      Id Text default=uuid_generate_v4() sqltype=varchar(36)
      lat Double
      lon Double
      createdAt UTCTime
      deriving Generic
    |]
