module Domain.Action.UI.Profile
  ( ProfileRes,
    UpdateProfileReq (..),
    getPersonDetails,
    updatePerson,
  )
where

import qualified Data.Text as DT
import qualified Domain.Types.Person as Person
import Kernel.External.Encryption
import qualified Kernel.External.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica, runTransaction)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QPerson
import Tools.Error

type ProfileRes = Person.PersonAPIEntity

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    referralCode :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getPersonDetails :: (EsqDBReplicaFlow m r, EncFlow m r) => Id Person.Person -> m ProfileRes
getPersonDetails personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson

updatePerson :: (EsqDBFlow m r, EncFlow m r) => Id Person.Person -> UpdateProfileReq -> m APISuccess.APISuccess
updatePerson personId req = do
  mPerson <- join <$> QPerson.findByEmail `mapM` req.email
  whenJust mPerson (\_ -> throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email
  unless ((DT.length <$> req.referralCode) == Just 6) $
    throwError $ InvalidRequest "referralCode must be of 6 digits"
  runTransaction $
    QPerson.updatePersonalInfo
      personId
      (req.firstName)
      (req.middleName)
      (req.lastName)
      (req.referralCode)
      mbEncEmail
      (req.deviceToken)
  pure APISuccess.Success
