-- | Dashboard APIs of the permit library. The route has already checked the
-- operator's session, endpoint capability and access to the merchant and city in
-- the path. The library then keeps each call to that city's applications.
module Domain.Action.Dashboard.Permit.Permit
  ( getPermitApplicationList,
    getPermitApplication,
    getPermitApplicationDocument,
    postPermitApplicationDecision,
    postPermitApplicationDcDecision,
    postPermitPassVerify,
  )
where

import Data.Time (Day)
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Lib.Permit.Api.Types
import qualified Lib.Permit.Domain.Types.PermitApplication as DPermit
import Lib.Permit.Domain.Types.PermitApplicationDocument (PermitDocumentType)
import qualified Lib.Permit.Review as Review
import Lib.Permit.Types (PermitType)
import qualified SharedLogic.Permit as SLPermit
import Storage.Beam.Permit ()
import Storage.Beam.SystemConfigs ()

scopeOf :: ShortId DM.Merchant -> Context.City -> Flow Review.Scope
scopeOf merchantShortId city = do
  (merchantId, cityId) <- SLPermit.resolveCity merchantShortId city
  pure Review.Scope {merchantId = cast merchantId, merchantOperatingCityId = cast cityId}

official :: Text -> Text -> Review.Official
official requestorId requestorName = Review.Official {officialId = requestorId, officialName = requestorName}

getPermitApplicationList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe DPermit.PermitStatus ->
  Maybe Day ->
  Maybe Int ->
  Maybe Int ->
  PermitType ->
  Flow PermitApplicationListRes
getPermitApplicationList merchantShortId city mbStatus mbDate mbLimit mbOffset permitType = do
  scope <- scopeOf merchantShortId city
  Review.listForReview scope permitType mbStatus mbDate mbLimit mbOffset

getPermitApplication :: ShortId DM.Merchant -> Context.City -> Id DPermit.PermitApplication -> Flow PermitApplicationDetailsRes
getPermitApplication merchantShortId city applicationId = do
  scope <- scopeOf merchantShortId city
  Review.getForReview scope applicationId

getPermitApplicationDocument ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DPermit.PermitApplication ->
  PermitDocumentType ->
  Text ->
  Text ->
  Flow PermitDocumentRes
getPermitApplicationDocument merchantShortId city applicationId documentType requestorId requestorName = do
  scope <- scopeOf merchantShortId city
  Review.getDocument scope (official requestorId requestorName) applicationId documentType

postPermitApplicationDecision ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DPermit.PermitApplication ->
  Text ->
  Text ->
  PermitDecisionReq ->
  Flow APISuccess
postPermitApplicationDecision merchantShortId city applicationId requestorId requestorName req = do
  scope <- scopeOf merchantShortId city
  Review.decide DPermit.STANDARD scope (official requestorId requestorName) applicationId req

postPermitApplicationDcDecision ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DPermit.PermitApplication ->
  Text ->
  Text ->
  PermitDecisionReq ->
  Flow APISuccess
postPermitApplicationDcDecision merchantShortId city applicationId requestorId requestorName req = do
  scope <- scopeOf merchantShortId city
  Review.decide DPermit.DC scope (official requestorId requestorName) applicationId req

postPermitPassVerify :: ShortId DM.Merchant -> Context.City -> Text -> Text -> PermitVerifyPassReq -> Flow PermitVerifyPassRes
postPermitPassVerify merchantShortId city requestorId requestorName req = do
  scope <- scopeOf merchantShortId city
  Review.verifyPass scope (official requestorId requestorName) req
