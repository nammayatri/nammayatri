{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.Booking.BookingLocation where

import Domain.Types.Booking.BookingLocation
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Booking.BookingLocation

updateAddress :: Id BookingLocation -> LocationAddress -> SqlDB ()
updateAddress blId LocationAddress {..} = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingLocationStreet =. val street,
        BookingLocationCity =. val city,
        BookingLocationState =. val state,
        BookingLocationCountry =. val country,
        BookingLocationBuilding =. val building,
        BookingLocationAreaCode =. val areaCode,
        BookingLocationArea =. val area,
        BookingLocationUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingLocationTId ==. val (toKey blId)
