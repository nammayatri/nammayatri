{-# OPTIONS_GHC -Wno-orphans #-}

module API.UI.ParkingBooking
  ( API,
    handler,
  )
where

import qualified API.Action.UI.ParkingBooking
import Environment
import Storage.Beam.SystemConfigs ()

type API = API.Action.UI.ParkingBooking.API

handler :: FlowServer API
handler = API.Action.UI.ParkingBooking.handler
