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

module Storage.Tabular.Booking
  ( module Storage.Tabular.Booking,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Data.Time (UTCTime)
import Database.Persist.TH
import Storage.Tabular.Booking.Internal as Reexport
import Storage.Tabular.Quote
import Storage.Tabular.Search

share
  [mkPersist sqlSettings]
  [defaultQQ|
    BookingT sql=parking_booking
      Id Text default=uuid_generate_v4() sqltype=varchar(36)
      searchId SearchTId
      quoteId QuoteTId
      requestorId Text
      requestorNumber Text
      vehicleNumber Text
      additionalInfo Text
      bppId Text
      bppUrl Text
      parkingSpaceName Text
      parkingSpaceLocationId Text
      parkingSupportNumber Text
      fare Amount
      fromDate UTCTime
      toDate UTCTime
      status BookingStatus
      ticketId Text Maybe
      ticketCreatedAt UTCTime Maybe
      updatedAt UTCTime
      createdAt UTCTime
      deriving Generic
    |]
