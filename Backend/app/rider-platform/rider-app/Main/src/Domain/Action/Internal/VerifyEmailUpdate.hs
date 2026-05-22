module Domain.Action.Internal.VerifyEmailUpdate
  ( verifyEmailUpdate,
    VerifyEmailUpdateReq (..),
  )
where

import Environment
import Kernel.External.Encryption (encrypt)
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
  dashboardToken <- asks (.dashboardToken)
  unless (Just dashboardToken == apiKey) $
    throwError (AuthBlocked "Invalid Rider App dashboard token")
  let personId = Id req.personId
  mbExisting <- QPerson.findByEmailAndMerchantId merchant.id req.email
  whenJust mbExisting $ \existing ->
    when (existing.id /= personId) $
      throwError (InvalidRequest "Email already registered by another user")
  encEmail <- encrypt req.email
  QPerson.updateEmailByPersonId personId encEmail
  pure Success
