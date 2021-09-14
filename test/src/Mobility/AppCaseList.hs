module Mobility.AppCaseList where

import EulerHS.Prelude
import Mobility.Fixtures
import Test.Hspec
import qualified "app-backend" Types.API.Registration as Reg
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App Backend APIs" $
    it "Testing List case API" $
      hspec $
        it "List case API should return success" $ withBecknClients clients do
          Reg.InitiateLoginRes {..} <- callBAP initiateLoginReq
          Reg.LoginRes {..} <- callBAP (verifyLoginReq tokenId)
          void $ callBAP (buildCaseListRes registrationToken)
