{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.IdfyWebhook
  ( onVerify,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Core.Ack
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DL
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as RC
import Environment
import Idfy.Types
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery

onVerify :: VerificationResponse -> Flow AckResponse
onVerify [] = pure Ack
onVerify [resp] = do
  verificationReq <- IVQuery.findByRequestId resp.request_id >>= fromMaybeM (InternalError "Verification request not found")
  runTransaction $ IVQuery.updateResponse resp.request_id resp.status (show <$> resp.result)

  callFromMaybeArray (verifyDocument verificationReq) resp.result
  where
    verifyDocument verificationReq rslt
      | isJust rslt.extraction_output =
        callFromMaybeArray (RC.onVerifyRC verificationReq) rslt.extraction_output
      | isJust rslt.source_output =
        callFromMaybeArray (DL.onVerifyDL verificationReq) rslt.source_output
      | otherwise = pure Ack

    callFromMaybeArray func mOutputArr = do
      maybe
        (pure Ack)
        ( \arr_ -> do
            let mOutput = listToMaybe arr_
            maybe (pure Ack) func mOutput
        )
        mOutputArr
onVerify _ = pure Ack
