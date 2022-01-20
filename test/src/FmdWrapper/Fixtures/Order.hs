module FmdWrapper.Fixtures.Order where

import "fmd-wrapper" Types.Beckn.Order (OrderItem (..))

orderItem :: OrderItem
orderItem = OrderItem {id = "2"}
