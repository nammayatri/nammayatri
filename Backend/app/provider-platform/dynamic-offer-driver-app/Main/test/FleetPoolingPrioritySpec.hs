-- | Pure tests for the fleet pooling priority map: JSON keys and 'resolvePoolingPriority'.
module FleetPoolingPrioritySpec (spec) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Domain.Types.FleetPoolingPriority
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import qualified Lib.Types.SpecialLocation as SL
import Test.Hspec

decodePriority :: LBS.ByteString -> Either String FleetPoolingPriority
decodePriority = A.eitherDecode

priorityOf :: [(SL.Area, PoolingPriorityLevel)] -> FleetPoolingPriority
priorityOf = FleetPoolingPriority . Map.fromList

pickup, drop' :: Text -> SL.Area
pickup slId = SL.Pickup (Id slId) Nothing
drop' slId = SL.Drop (Id slId)

spec :: Spec
spec = do
  describe "FleetPoolingPriority JSON" $ do
    it "decodes Default, Pickup_<id> and Drop_<id> keys" $
      decodePriority "{\"Default\":\"LOW\",\"Pickup_sl1\":\"MEDIUM\",\"Drop_sl2\":\"HIGH\"}"
        `shouldBe` Right (priorityOf [(SL.Default, LOW), (pickup "sl1", MEDIUM), (drop' "sl2", HIGH)])

    it "round-trips through encode / decode with the Area rendering as keys" $ do
      let priority = priorityOf [(SL.Default, NO_PRIORITY), (pickup "sl1", HIGH), (drop' "sl2", LOW)]
      A.eitherDecode (A.encode priority) `shouldBe` Right priority
      A.toJSON priority `shouldBe` A.object ["Default" A..= NO_PRIORITY, "Pickup_sl1" A..= HIGH, "Drop_sl2" A..= LOW]

    it "decodes an empty object as an empty map" $
      decodePriority "{}" `shouldBe` Right (priorityOf [])

    it "rejects a gated pickup key" $
      decodePriority "{\"Pickup_sl1_Gate_g1\":\"HIGH\"}" `shouldSatisfy` isLeft

    it "rejects a PickupDrop pair key" $
      decodePriority "{\"PickupDrop_sl1_sl2\":\"HIGH\"}" `shouldSatisfy` isLeft

    it "rejects a garbage key" $
      decodePriority "{\"Airport\":\"HIGH\"}" `shouldSatisfy` isLeft

    it "rejects an unknown level" $
      decodePriority "{\"Default\":\"URGENT\"}" `shouldSatisfy` isLeft

  describe "resolvePoolingPriority" $ do
    let priority = priorityOf [(SL.Default, LOW), (pickup "sl1", MEDIUM), (drop' "sl2", HIGH), (drop' "sl3", MEDIUM)]
    it "picks the highest level among the pickup and drop entries" $
      resolvePoolingPriority (Just (SL.PickupDrop (Id "sl1") (Id "sl2") Nothing)) priority `shouldBe` Just (HIGH, "Drop_sl2")

    it "gives a tie to the pickup entry" $
      resolvePoolingPriority (Just (SL.PickupDrop (Id "sl1") (Id "sl3") Nothing)) priority `shouldBe` Just (MEDIUM, "Pickup_sl1")

    it "matches a gated pickup area by its special location" $
      resolvePoolingPriority (Just (SL.Pickup (Id "sl1") (Just "g1"))) priority `shouldBe` Just (MEDIUM, "Pickup_sl1")

    it "matches a drop area" $
      resolvePoolingPriority (Just (drop' "sl2")) priority `shouldBe` Just (HIGH, "Drop_sl2")

    it "falls back to Default when no pickup / drop entry applies" $ do
      resolvePoolingPriority (Just (pickup "other")) priority `shouldBe` Just (LOW, "Default")
      resolvePoolingPriority Nothing priority `shouldBe` Just (LOW, "Default")

    it "is Nothing when nothing applies and there is no Default" $
      resolvePoolingPriority (Just (pickup "other")) (priorityOf [(drop' "sl2", HIGH)]) `shouldBe` Nothing
