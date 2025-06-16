{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.DeletedPerson (postDeletedPerson) where

import qualified API.Types.UI.DeletedPerson
import Data.OpenApi (ToSchema)
import Domain.Types.DeletedPerson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import qualified Storage.Queries.DeletedPerson as QD
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SavedReqLocation as QSRL
import Tools.Auth
import Tools.Error
import Tools.Notifications (notifyAboutDeletedPerson)

postDeletedPerson :: ((Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.DeletedPerson.DeletedPersonReq -> Environment.Flow APISuccess.APISuccess)
postDeletedPerson (mbPersonId, merchantId) (API.Types.UI.DeletedPerson.DeletedPersonReq {reasonToDelete}) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  void $ notifyAboutDeletedPerson personId
  QD.create (createDeletedPerson person now)
  _ <- QP.deleteById personId
  _ <- QSRL.deleteAllByRiderId personId
  pure APISuccess.Success
  where
    createDeletedPerson :: Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Domain.Types.DeletedPerson.DeletedPerson
    createDeletedPerson person now =
      Domain.Types.DeletedPerson.DeletedPerson
        { clientOsType = person.clientDevice <&> (.deviceType),
          createdAt = now,
          deviceId = person.deviceId,
          merchantId = merchantId,
          merchantOperatingCityId = person.merchantOperatingCityId,
          personId = person.id,
          updatedAt = now,
          reasonToDelete
        }
