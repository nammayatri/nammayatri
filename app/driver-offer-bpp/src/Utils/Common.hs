{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Core.Taxi.Common.DecimalValue
import Beckn.Utils.Common as CoreCommon

amountToDecimalValue :: Amount -> DecimalValue
amountToDecimalValue (Amount v) = DecimalValue v
