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
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as RC
import Environment
import Idfy.Types
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery

onVerify :: VerificationResponse -> Text -> Flow AckResponse
onVerify resp respDump = do
  verificationReq <- IVQuery.findByRequestId resp.request_id >>= fromMaybeM (InternalError "Verification request not found")
  runTransaction $ IVQuery.updateResponse resp.request_id resp.status respDump

  ack_ <- maybe (pure Ack) (verifyDocument verificationReq) resp.result
  -- running statusHandler to enable Driver
  _ <- Status.statusHandler verificationReq.driverId

  return ack_
  where
    verifyDocument verificationReq rslt
      | isJust rslt.extraction_output =
        maybe (pure Ack) (RC.onVerifyRC verificationReq) rslt.extraction_output
      | isJust rslt.source_output =
        maybe (pure Ack) (DL.onVerifyDL verificationReq) rslt.source_output
      | otherwise = pure Ack
