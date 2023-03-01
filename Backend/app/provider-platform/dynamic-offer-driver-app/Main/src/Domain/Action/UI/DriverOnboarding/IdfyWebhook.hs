{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.IdfyWebhook
  ( onVerify,
  )
where

import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DL
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as RC
import Environment
import Idfy.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as IVQuery

onVerify :: VerificationResponse -> Text -> Flow AckResponse
onVerify resp respDump = do
  verificationReq <- IVQuery.findByRequestId resp.request_id (Proxy @Flow) >>= fromMaybeM (InternalError "Verification request not found")
  runTransaction $ IVQuery.updateResponse @Flow resp.request_id resp.status respDump

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
