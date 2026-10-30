{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | ExternalBPP.CallAPI.Cancel's ONDC branch refuses a shared-cab booking with CancellationNotSupported. Tested one level in,
-- as the branch's pure decide (decideCancelDispatchRefusal), since the entry point needs stubbed configs and the reschedule lock.
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

-- | A direct-integrated provider: the shared-cab seller's real integration shape (agency key `<feed>:SHARED_CAB`).
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
    "shared-cab ONDC cancel refusal (the branch's decide)"
    [ testCase "shared-cab + ONDC => RefuseSharedCabOndc (the branch maps it to throwError CancellationNotSupported)" $
        decide (Just Spec.SHARED_CAB) ondcProvider @?= RefuseSharedCabOndc,
      testCase "shared-cab + a DIRECT provider dispatches (the real shared-cab seller must keep cancelling through the refund policy)" $
        decide (Just Spec.SHARED_CAB) directProvider @?= DispatchProceeds,
      testCase "ONDC + a non-shared tier dispatches (the refusal must not eat ordinary ONDC cancels)" $
        decide (Just Spec.ORDINARY) ondcProvider @?= DispatchProceeds,
      testCase "ONDC + no tier (no routeStationsJson) dispatches" $
        decide Nothing ondcProvider @?= DispatchProceeds
    ]
