{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.Booking.BookingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Booking.BookingLocation
import Storage.Tabular.Booking.BookingLocation

updateAddress :: Id BookingLocation -> LocationAddress -> SqlDB ()
updateAddress blId LocationAddress {..} = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingLocationStreet =. val street,
        BookingLocationDoor =. val door,
        BookingLocationCity =. val city,
        BookingLocationState =. val state,
        BookingLocationCountry =. val country,
        BookingLocationBuilding =. val building,
        BookingLocationAreaCode =. val areaCode,
        BookingLocationArea =. val area,
        BookingLocationUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingLocationTId ==. val (toKey blId)
