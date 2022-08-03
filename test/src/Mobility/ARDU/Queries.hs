module Mobility.ARDU.Queries where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import "driver-offer-bpp" Domain.Types.Person
import "driver-offer-bpp" Domain.Types.Ride as Ride
import "driver-offer-bpp" Storage.Tabular.Ride as Ride
import "driver-offer-bpp" Utils.Common

cancelAllByDriverId ::
  Id Person ->
  SqlDB ()
cancelAllByDriverId driverId = do
  now <- getCurrentTime
  Esq.update $ \ride -> do
    set
      ride
      [ RideStatus =. val Ride.CANCELLED,
        RideUpdatedAt =. val now
      ]
    where_ $
      ride ^. RideDriverId ==. val (toKey driverId)
        &&. ride ^. RideStatus ==. val Ride.INPROGRESS
