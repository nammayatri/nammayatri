module DriverInfoByPhoneSpec (spec) where

import Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone (orderCitiesForSearch)
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.City as City
import Test.Hspec

spec :: Spec
spec = describe "orderCitiesForSearch" $ do
  let blr = City.City "Bangalore"
      maa = City.City "Chennai"
      koc = City.City "Kochi"

  it "puts the token city first" $
    orderCitiesForSearch maa [blr, maa, koc] `shouldBe` [maa, blr, koc]

  it "deduplicates cities" $
    orderCitiesForSearch blr [blr, blr, maa] `shouldBe` [blr, maa]

  it "returns empty for no cities" $
    orderCitiesForSearch blr [] `shouldBe` []

  it "keeps non-token order stable when token city absent" $
    orderCitiesForSearch (City.City "Delhi") [blr, maa, koc] `shouldBe` [blr, maa, koc]
