{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import qualified Dashboard (dashboardUnitTests)
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main = do
  putStrLn "🧪 Running HUnit Dashboard Tests..."

  defaultMain $
    testGroup
      "Unit Tests"
      [ testGroup
          "Dashboard Unit Tests"
          [ Dashboard.dashboardUnitTests -- This is the main test suite for all dashboard unit tests
          ]
          -- testGroup
          --   "Integration Tests (Requires Services)"
          --   [ Dashboard.registrationTests,
          --     Dashboard.profileTests,
          --     Dashboard.fleetOwnerInfoTests,
          --     Dashboard.fleetDriverOnboardingTests,
          --     Dashboard.documentTests
          --   ]
      ]
