module SharedLogic.FareBreakup
  ( FareBreakupLine (..),
    computeFareBreakup,
  )
where

import BecknV2.OnDemand.FareBreakup.SubItems (FareBreakupLine (..), computeFareBreakupLines)
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

breakupComponentAmounts :: FareParameters -> Map.Map FareChargeComponent HighPrecMoney
breakupComponentAmounts fareParams =
  let amounts = buildComponentMap fareParams
   in if SL.edcCollectsParking fareParams.fareSettlementType
        then foldr Map.delete amounts [ParkingChargeComponent, ParkingTaxComponent, AirportEntryFeeComponent, AirportEntryFeeTaxComponent]
        else amounts
