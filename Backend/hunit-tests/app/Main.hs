{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import qualified Dashboard (documentTests, fleetOwnerInfoTests, onboardingTests, profileTests, registrationTests)
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ testGroup
          "Dashboard Tests"
          [ Dashboard.registrationTests,
            Dashboard.profileTests,
            Dashboard.fleetOwnerInfoTests,
            Dashboard.documentTests,
            Dashboard.onboardingTests
          ]
          -- Future test groups will go here
      ]
