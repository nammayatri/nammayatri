{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Tabular.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Data.Time (UTCTime)
import Database.Persist.TH
import Storage.Tabular.Search

share
  [mkPersist sqlSettings]
  [defaultQQ|
    QuoteT sql=quote
      Id Text default=uuid_generate_v4() sqltype=varchar(36)
      searchId SearchTId
      bppId Text
      bppUrl Text
      parkingSpaceName Text
      parkingSpaceLocationId Text
      fare Amount
      availableSpaces Int
      createdAt UTCTime
      deriving Generic
    |]
