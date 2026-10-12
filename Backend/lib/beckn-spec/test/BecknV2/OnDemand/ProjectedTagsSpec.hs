module BecknV2.OnDemand.ProjectedTagsSpec (spec) where

import qualified BecknV2.OnDemand.Enums as Enums
import Domain.SharedLogic.RideDiscount (projectedFareParamTags)
import Kernel.Prelude
import Test.Hspec

spec :: Spec
spec =
  describe "Projected fare param tags" $
    it "match the Beckn enum constructor names" $
      projectedFareParamTags
        `shouldBe` [ show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE,
                     show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX,
                     show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_UNTAXED,
                     show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE,
                     show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX,
                     show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_UNTAXED,
                     show Enums.TOLL_FARE_TAX_EXCLUSIVE,
                     show Enums.TOLL_FARE_TAX,
                     show Enums.CANCELLATION_FEE_TAX_EXCLUSIVE,
                     show Enums.CANCELLATION_TAX,
                     show Enums.PARKING_CHARGE_TAX_EXCLUSIVE,
                     show Enums.PARKING_CHARGE_TAX,
                     show Enums.PAYMENT_CHARGE_TAX_EXCLUSIVE,
                     show Enums.PAYMENT_CHARGE_TAX,
                     show Enums.PAYMENT_CHARGE_RATE,
                     show Enums.PAYMENT_CHARGE_VAT_PCT
                   ]
