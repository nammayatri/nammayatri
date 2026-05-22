module Domain.Action.Internal.VerifyEmailUpdate
  ( verifyEmailUpdate,
    VerifyEmailUpdateReq (..),
  )
where

import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.PersonExtra as QPerson

data VerifyEmailUpdateReq = VerifyEmailUpdateReq
  { email :: Text,
    personId :: Text
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
  let personId = Id req.personId
  mbExisting <- QPerson.findByEmail (Just req.email)
  whenJust mbExisting $ \existing ->
    when (existing.id /= personId) $
      throwError (InvalidRequest "Email already registered by another user")
  QPerson.updateEmailByPersonId personId req.email
  pure Success
