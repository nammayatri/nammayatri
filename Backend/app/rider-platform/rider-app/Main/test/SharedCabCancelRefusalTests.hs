{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | R80 + batch9 MED: ExternalBPP.CallAPI.Cancel's ONDC branch refuses a shared-cab booking with
-- CancellationNotSupported. Testing the entry point takes a stubbed IntegratedBPPConfig lookup, an FRFS
-- config and the reschedule lock, so the refusal is tested ONE LEVEL IN, as the branch's pure decide
-- (decideCancelDispatchRefusal): its two real inputs are the integrated provider's config and the
-- booking's service tier out of routeStationsJson -- the same test
-- SharedLogic.SharedCab.RefundDecision.isSharedCabBooking performs. The branch itself stays one line:
-- @when (decide... == RefuseSharedCabOndc) $ throwError CancellationNotSupported@.
module SharedCabCancelRefusalTests (tests) where

import "beckn-spec" BecknV2.FRFS.Enums as Spec
import "rider-app" Domain.Types.Extra.IntegratedBPPConfig (DIRECTConfig (..), ONDCBecknConfig (..))
import "rider-app" Domain.Types.IntegratedBPPConfig (ProviderConfig (..))
import "rider-app" ExternalBPP.CallAPI.Cancel (CancelDispatchRefusal (..), decideCancelDispatchRefusal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

ondcProvider :: ProviderConfig
ondcProvider =
  ONDC
    ONDCBecknConfig
      { networkHostUrl = Nothing,
        networkId = Nothing,
        multiInitAllowed = Nothing,
        fareCachingAllowed = Nothing,
        singleTicketForMultiplePassengers = Nothing,
        mergeQuoteCriteria = Nothing,
        routeBasedQuoteSelection = Nothing,
        providerInfo = Nothing,
        routeBasedVehicleTracking = Nothing,
        overrideCity = Nothing,
        redisPrefix = Nothing,
        busBlockExpiryTime = Nothing,
        busBlockMaxLimit = Nothing,
        qrEncoding = Nothing
      }

-- | A direct-integrated provider: the shared-cab seller's real integration shape (agency key
-- `<feed>:SHARED_CAB`). IsString on Base64 is exactly for constant values in test code.
directProvider :: ProviderConfig
directProvider =
  DIRECT
    DIRECTConfig
      { cipherKey = "dGVzdGtleQ==",
        qrRefreshTtl = Nothing,
        redisPrefix = Nothing,
        busBlockExpiryTime = Nothing,
        busBlockMaxLimit = Nothing
      }

decide :: Maybe Spec.ServiceTierType -> ProviderConfig -> CancelDispatchRefusal
decide tier provider = decideCancelDispatchRefusal provider tier

tests :: TestTree
tests =
  testGroup
    "shared-cab ONDC cancel refusal (R80, batch9: the branch's decide)"
    [ testCase "shared-cab + ONDC => RefuseSharedCabOndc (the branch maps it to throwError CancellationNotSupported)" $
        decide (Just Spec.SHARED_CAB) ondcProvider @?= RefuseSharedCabOndc,
      testCase "shared-cab + a DIRECT provider dispatches (the real shared-cab seller must keep cancelling through the refund policy)" $
        decide (Just Spec.SHARED_CAB) directProvider @?= DispatchProceeds,
      testCase "ONDC + a non-shared tier dispatches (the refusal must not eat ordinary ONDC cancels)" $
        decide (Just Spec.ORDINARY) ondcProvider @?= DispatchProceeds,
      testCase "ONDC + no tier (no routeStationsJson) dispatches" $
        decide Nothing ondcProvider @?= DispatchProceeds
    ]
