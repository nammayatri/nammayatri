module Beckn.Types.Core.Taxi.OnInit
  ( module Beckn.Types.Core.Taxi.OnInit,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.OnInit.BreakupItem as Reexport
import Beckn.Types.Core.Taxi.OnInit.Order as Reexport
import Beckn.Types.Core.Taxi.OnInit.OrderState as Reexport
import Beckn.Types.Core.Taxi.OnInit.Payment as Reexport
import Beckn.Types.Core.Taxi.OnInit.Quote as Reexport

newtype OnInitMessage = OnInitMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
