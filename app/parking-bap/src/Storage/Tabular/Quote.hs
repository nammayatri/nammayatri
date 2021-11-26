{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    QuoteT sql=parking_quote
      Id Text default=uuid_generate_v4() sqltype=varchar(36)
      searchId SearchTId
      additionalInfo Text
      bppId Text
      bppUrl Text
      parkingSpaceName Text
      parkingSpaceLocationId Text
      parkingSupportNumber Text
      fare Amount
      availableSpaces Int
      maxSpaces Int
      fromDate UTCTime
      toDate UTCTime
      createdAt UTCTime
      deriving Generic
    |]
