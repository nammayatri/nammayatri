module SharedLogic.FareBreakup
  ( FareBreakupLine (..),
    computeFareBreakup,
    exactFareBreakup,
    breakupComponentAmounts,
  )
where

import BecknV2.OnDemand.FareBreakup.SubItems (FareBreakupLine (..), computeFareBreakupLines, exactFareBreakupLines)
import qualified Data.Map.Strict as Map
import Domain.Types.Extra.TransporterConfig (FareBreakupNode)
import Domain.Types.FareParameters
import Domain.Types.FarePolicy (FareChargeComponent (..))
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.FareCalculator (buildComponentMap)

computeFareBreakup :: Currency -> [FareBreakupNode] -> FareParameters -> [FareBreakupLine]
computeFareBreakup currency configuredSubItems fareParams =
  computeFareBreakupLines currency configuredSubItems (breakupComponentAmounts fareParams)

-- | Like 'computeFareBreakup', but with exact amounts: not rounded, and lines that are 0 are kept.
exactFareBreakup :: [FareBreakupNode] -> FareParameters -> [FareBreakupLine]
exactFareBreakup configuredSubItems fareParams =
  exactFareBreakupLines configuredSubItems (breakupComponentAmounts fareParams)

breakupComponentAmounts :: FareParameters -> Map.Map FareChargeComponent HighPrecMoney
breakupComponentAmounts fareParams =
  let amounts = buildComponentMap fareParams
   in if SL.edcCollectsParking fareParams.fareSettlementType
        then foldr Map.delete amounts [ParkingChargeComponent, ParkingTaxComponent, AirportEntryFeeComponent, AirportEntryFeeTaxComponent]
        else amounts
