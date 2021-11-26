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

module Storage.Tabular.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Data.Time (UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings]
  [defaultQQ|
    SearchT sql=parking_search
      Id Text default=uuid_generate_v4() sqltype=varchar(36)
      searchLocationId Text
      requestorId Text
      fromDate UTCTime
      toDate UTCTime
      createdAt UTCTime
      deriving Generic
    |]
