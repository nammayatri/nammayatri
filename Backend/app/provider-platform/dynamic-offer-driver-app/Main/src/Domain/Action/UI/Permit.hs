-- | Public citizen APIs of the permit library. They are NoAuth at the route level:
-- every call after sign-in authenticates the citizen's own permit token here.
module Domain.Action.UI.Permit
  ( postPermitAuthOtp,
    postPermitAuthVerify,
    postPermitAuthLogout,
    getPermitLimits,
    getPermitApplicationList,
    postPermitApplication,
    putPermitApplication,
    getPermitApplication,
    postPermitApplicationDocument,
    postPermitApplicationUndertakingOtp,
    postPermitApplicationSubmit,
    postPermitApplicationWithdraw,
    getPermitApplicationPass,
  )
where

import qualified Data.Text as T
import Data.Time (Day)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Permit.Api.Types
import qualified Lib.Permit.Application as Permit
import qualified Lib.Permit.Auth as PermitAuth
import qualified Lib.Permit.Domain.Types.PermitApplication as DPermit
import Lib.Permit.Otp (PermitHost)
import qualified Lib.Permit.Picnic.Rules as Picnic
import Lib.Permit.Types (PermitType)
import qualified SharedLogic.Permit as SLPermit
import Storage.Beam.Permit ()
import Storage.Beam.SystemConfigs ()

-- | Anyone can reach the sign-in calls, so only the pilot city is served.
resolvePilotCity :: Text -> Context.City -> Flow (Id DM.Merchant, Id DMOC.MerchantOperatingCity)
resolvePilotCity merchantShortId city = do
  unless (city == Picnic.pilotCity) $ throwError $ InvalidRequest "Permits are not available in this city."
  SLPermit.resolveCity (ShortId merchantShortId) city

hostFor :: PermitAuth.PermitSession -> Flow (PermitHost Flow)
hostFor session = SLPermit.mkPermitHost (cast session.merchantId) (cast session.merchantOperatingCityId)

postPermitAuthOtp :: Maybe Text -> PermitAuthOtpReq -> Flow APISuccess
postPermitAuthOtp mbForwardedFor req = do
  (merchantId, cityId) <- resolvePilotCity req.merchantShortId req.city
  host <- SLPermit.mkPermitHost merchantId cityId
  let clientAddress = T.strip . T.takeWhile (/= ',') <$> mbForwardedFor
  PermitAuth.sendLoginOtp host (cast merchantId) clientAddress req.mobileCountryCode req.mobileNumber
  pure Success

postPermitAuthVerify :: PermitAuthVerifyReq -> Flow PermitAuthVerifyRes
postPermitAuthVerify req = do
  (merchantId, cityId) <- resolvePilotCity req.merchantShortId req.city
  token <- PermitAuth.verifyLoginOtp (cast merchantId) (cast cityId) req.mobileCountryCode req.mobileNumber req.otp
  pure PermitAuthVerifyRes {token}

postPermitAuthLogout :: Maybe Text -> Flow APISuccess
postPermitAuthLogout mbToken = do
  PermitAuth.logout mbToken
  pure Success

getPermitLimits :: Maybe Day -> PermitType -> Maybe Text -> Flow PermitLimitsRes
getPermitLimits mbTravelDate permitType mbToken = do
  _ <- PermitAuth.authenticate mbToken
  Permit.getLimits permitType mbTravelDate

getPermitApplicationList :: PermitType -> Maybe Text -> Flow PermitApplicationListRes
getPermitApplicationList permitType mbToken = do
  session <- PermitAuth.authenticate mbToken
  Permit.listMine session permitType

postPermitApplication :: Maybe Text -> PermitCreateApplicationReq -> Flow PermitCreateApplicationRes
postPermitApplication mbToken req = do
  session <- PermitAuth.authenticate mbToken
  Permit.createDraft session req

putPermitApplication :: Id DPermit.PermitApplication -> Maybe Text -> PermitUpdateApplicationReq -> Flow APISuccess
putPermitApplication applicationId mbToken req = do
  session <- PermitAuth.authenticate mbToken
  Permit.updateDraft session applicationId req

getPermitApplication :: Id DPermit.PermitApplication -> Maybe Text -> Flow PermitMyApplicationRes
getPermitApplication applicationId mbToken = do
  session <- PermitAuth.authenticate mbToken
  Permit.getMine session applicationId

postPermitApplicationDocument :: Id DPermit.PermitApplication -> Maybe Text -> PermitDocumentUploadReq -> Flow APISuccess
postPermitApplicationDocument applicationId mbToken req = do
  session <- PermitAuth.authenticate mbToken
  Permit.uploadDocument session applicationId req

postPermitApplicationUndertakingOtp :: Id DPermit.PermitApplication -> Maybe Text -> Flow PermitUndertakingOtpRes
postPermitApplicationUndertakingOtp applicationId mbToken = do
  session <- PermitAuth.authenticate mbToken
  host <- hostFor session
  Permit.requestUndertakingOtps host session applicationId

postPermitApplicationSubmit :: Id DPermit.PermitApplication -> Maybe Text -> PermitSubmitReq -> Flow APISuccess
postPermitApplicationSubmit applicationId mbToken req = do
  session <- PermitAuth.authenticate mbToken
  Permit.submit session applicationId req

postPermitApplicationWithdraw :: Id DPermit.PermitApplication -> Maybe Text -> Flow APISuccess
postPermitApplicationWithdraw applicationId mbToken = do
  session <- PermitAuth.authenticate mbToken
  Permit.withdraw session applicationId

getPermitApplicationPass :: Id DPermit.PermitApplication -> Maybe Text -> Flow PermitPassRes
getPermitApplicationPass applicationId mbToken = do
  session <- PermitAuth.authenticate mbToken
  Permit.getPass session applicationId
