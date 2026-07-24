module Domain.Action.Internal.VerifyEmailUpdate
  ( verifyEmailUpdate,
    verifyRequestorRequesteeAssociation,
    VerifyEmailUpdateReq (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverFleetOperatorAssociation (isAssociationBetweenTwoPerson)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Person as QPerson

data VerifyEmailUpdateReq = VerifyEmailUpdateReq
  { email :: Text,
    requesteeId :: Text,
    requestorId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

verifyEmailUpdate ::
  Maybe Text ->
  Text ->
  VerifyEmailUpdateReq ->
  Flow APISuccess
verifyEmailUpdate apiKey merchantShortIdText req = do
  merchant <- findMerchantByShortId (ShortId merchantShortIdText)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError (AuthBlocked "Invalid BPP internal api key")
  let requesteeId = Id req.requesteeId
      requestorId = Id req.requestorId
      email = T.toLower req.email
  verifyRequestorRequesteeAssociation requestorId requesteeId
  mbExisting <- QPerson.findByEmail (Just email)
  whenJust mbExisting $ \existing ->
    when (existing.id /= requesteeId) $
      throwError (InvalidRequest "Email already registered by another user")
  QPerson.updateEmailByPersonId requesteeId email
  pure Success

verifyRequestorRequesteeAssociation ::
  Id DP.Person ->
  Id DP.Person ->
  Flow ()
verifyRequestorRequesteeAssociation requestorId requesteeId =
  when (requestorId /= requesteeId) $ do
    persons <- QPerson.getDriversByIdIn [requesteeId, requestorId]
    bppRequestee <- find (\p -> p.id == requesteeId) persons & fromMaybeM (PersonNotFound requesteeId.getId)
    bppRequestor <- find (\p -> p.id == requestorId) persons & fromMaybeM (PersonNotFound requestorId.getId)
    isValid <- isAssociationBetweenTwoPerson bppRequestor bppRequestee
    unless isValid $ throwError AccessDenied
