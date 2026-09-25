{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabDriverActionTests (tests) where

import Data.Text (Text)
import "beckn-spec" Domain.Types.FRFSTicketStatus (FRFSTicketStatus (..))
import "rider-app" SharedLogic.SharedCab.DriverAction (DriverAction (..), SharedCabDriverActionError (..), decideDriverAction)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

plate :: Text
plate = "ML05A9999"

decide :: DriverAction -> Maybe Text -> [FRFSTicketStatus] -> Either SharedCabDriverActionError ()
decide action = decideDriverAction action plate

tests :: TestTree
tests =
  testGroup
    "shared-cab driver actions"
    [ testGroup
        "cancel (D5)"
        [ testCase "an unboarded allocation on this cab" $ decide DriverCancel (Just plate) [ACTIVE] @?= Right (),
          testCase "refused once a seat has boarded" $ decide DriverCancel (Just plate) [ACTIVE, INPROGRESS] @?= Left BookingAlreadyBoarded,
          testCase "refused with no seat waiting" $ decide DriverCancel (Just plate) [USED] @?= Left BookingNotLive,
          testCase "another cab's booking" $ decide DriverCancel (Just "ML05B8888") [ACTIVE] @?= Left BookingNotOnThisCab,
          testCase "a FINDING booking (no cab)" $ decide DriverCancel Nothing [ACTIVE] @?= Left BookingNotOnThisCab
        ],
      testGroup
        "boarded without code (D5)"
        [ testCase "a waiting seat boards" $ decide DriverBoarded (Just plate) [ACTIVE] @?= Right (),
          testCase "a repeat tap is a no-op, not an error" $ decide DriverBoarded (Just plate) [INPROGRESS] @?= Right (),
          testCase "nothing held" $ decide DriverBoarded (Just plate) [CANCELLED] @?= Left BookingNotLive,
          testCase "plate compared in canonical form" $ decide DriverBoarded (Just "ml 05-a 9999") [ACTIVE] @?= Right ()
        ],
      testGroup
        "dropped (D7)"
        [ testCase "a boarded seat drops" $ decide DriverDropped (Just plate) [INPROGRESS] @?= Right (),
          testCase "not boarded yet" $ decide DriverDropped (Just plate) [ACTIVE] @?= Left BookingNotBoarded,
          testCase "another cab's booking" $ decide DriverDropped (Just "ML05B8888") [INPROGRESS] @?= Left BookingNotOnThisCab
        ]
    ]
