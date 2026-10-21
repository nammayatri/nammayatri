module KaalChakraTemplate (kaalChakraTemplateTests) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Yudhishthira.Event.KaalChakra.Template (RawQuery (..), Template (..), replaceTemplateUnits)
import qualified Lib.Yudhishthira.Types as LYT
import Test.Tasty
import Test.Tasty.HUnit

kaalChakraTemplateTests :: TestTree
kaalChakraTemplateTests =
  testGroup
    "Kaal chakra query template tests"
    [ testCase "fills the merchant operating city placeholder" $
        render cityQuery (Just cityId) @?= Right "SELECT driver_id AS userId FROM ride WHERE merchant_operating_city_id = 'f067bccf-5b34-fb51-a5a3-9d6fa6baac26' AND True LIMIT 10 OFFSET 20",
      testCase "leaves queries without the city placeholder unchanged" $ do
        render plainQuery (Just cityId) @?= Right plainRendered
        render plainQuery Nothing @?= Right plainRendered,
      testCase "rejects the city placeholder when no city is given" $
        case render cityQuery Nothing of
          Left err -> assertBool "error names the placeholder" ("{merchantOperatingCityId}" `T.isInfixOf` err)
          Right res -> assertFailure $ "expected an error, got: " <> T.unpack res,
      testCase "still requires the limit placeholder" $
        assertBool "missing {limit} is rejected" (isLeft $ render "SELECT 1 WHERE {usersSetCondition} OFFSET {offset}" (Just cityId))
    ]
  where
    cityId = Id "f067bccf-5b34-fb51-a5a3-9d6fa6baac26"
    cityQuery = "SELECT driver_id AS userId FROM ride WHERE merchant_operating_city_id = '{merchantOperatingCityId}' AND {usersSetCondition} LIMIT {limit} OFFSET {offset}"
    plainQuery = "SELECT driver_id AS userId FROM ride WHERE {usersSetCondition} LIMIT {limit} OFFSET {offset}"
    plainRendered = "SELECT driver_id AS userId FROM ride WHERE True LIMIT 10 OFFSET 20"

render :: Text -> Maybe (Id LYT.MerchantOperatingCity) -> Either Text Text
render query merchantOperatingCityId =
  getRawQuery <$> replaceTemplateUnits (RawQuery query) Template {limit = LYT.QLimit 10, offset = LYT.QOffset 20, usersSet = LYT.ALL_USERS, merchantOperatingCityId}
