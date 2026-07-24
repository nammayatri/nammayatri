module Domain.Action.Internal.VerifyEmailUpdate
  ( verifyEmailUpdate,
    VerifyEmailUpdateReq (..),
  )
where

import qualified Data.Text as T
import Environment
import Kernel.External.Encryption (encrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Person as QPerson

data VerifyEmailUpdateReq = VerifyEmailUpdateReq
  { email :: Text,
    requesteeId :: Text,
    requestorId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- No requestor-requestee association check here for now — fleet/operator associations
-- exist only on the BPP side. TODO: add the association logic based on future usecases
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
  let requesteeId = Id req.requesteeId
      email = T.toLower req.email
  void $ QPerson.findById requesteeId >>= fromMaybeM (PersonNotFound req.requesteeId)
  mbExisting <- QPerson.findByEmailAndMerchantId merchant.id email
  whenJust mbExisting $ \existing ->
    when (existing.id /= requesteeId) $
      throwError (InvalidRequest "Email already registered by another user")
  encEmail <- encrypt email
  QPerson.updateEmailByPersonId requesteeId encEmail
  pure Success
