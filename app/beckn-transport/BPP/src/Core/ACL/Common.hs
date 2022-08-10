module Core.ACL.Common where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Core.Taxi.Common.DecimalValue

amountToRoundedDecimal :: Amount -> DecimalValue
amountToRoundedDecimal = realToFrac . roundToUnits
