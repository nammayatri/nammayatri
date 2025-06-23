{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.TicketKapture (postKaptureCustomerLogin) where

import qualified API.Types.UI.TicketKapture as TicketKapture
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Ticket.Interface.Types as TIT
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.Queries.Person as QPerson
import Tools.Auth
import Tools.Ticket

postKaptureCustomerLogin ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    TIT.TicketType ->
    Environment.Flow TicketKapture.TicketKaptureResp
  )
postKaptureCustomerLogin (mbPersonId, _) ticketType = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  email <- fromMaybe "" <$> mapM decrypt person.email
  addAndUpdateKaptureCustomerResponse <-
    try @_ @SomeException
      (addAndUpdateKaptureCustomer person.merchantId person.merchantOperatingCityId (TIT.KaptureCustomerReq personId.getId (fromMaybe "" person.lastName <> fromMaybe "" person.firstName) mobileNumber email personId.getId))

  kaptureEncryptionResponse <-
    case addAndUpdateKaptureCustomerResponse of
      Right resp ->
        try @_ @SomeException
          (kaptureEncryption person.merchantId person.merchantOperatingCityId (TIT.KaptureEncryptionReq resp.kaptureCustomerId ticketType))
      Left err -> throwError $ InternalError ("Add And Update Kapture Customer Ticket API failed - " <> show err)

  case kaptureEncryptionResponse of
    Left err -> throwError $ InternalError ("Kapture Encryption API failed - " <> show err)
    Right resp ->
      return $
        TicketKapture.TicketKaptureResp
          { encryptedCc = resp.encrytedCc,
            encryptedIv = resp.encryptedIv
          }
