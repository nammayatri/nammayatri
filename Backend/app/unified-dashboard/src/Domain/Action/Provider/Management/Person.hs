{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Provider.Management.Person
  ( postPersonCreate,
    postUserLoginSendOtp,
  )
where

import qualified API.Client.Provider.Management
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management.Person
import qualified "dynamic-offer-driver-app" API.Types.UnifiedDashboard.Management.Person as BPPPerson
import qualified Data.Text as T
import qualified Domain.Action.Management.Transaction
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DMerchantAccess
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Role as DRole
import qualified Environment
import EulerHS.Prelude
import Kernel.External.Encryption (encrypt, getDbHash)
import qualified Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as City
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Storage.Beam.CommonInstances ()
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Merchant
import qualified Tools.Utils

authHitsCountKey :: DPerson.Person -> Text
authHitsCountKey person = "UnifiedDashboard:Registration:auth:" <> Kernel.Types.Id.getId person.id <> ":hitsCount"

postPersonCreate :: (Kernel.Types.Id.ShortId DMerchant.Merchant -> City.City -> ApiTokenInfo -> API.Types.UnifiedDashboard.Management.Person.CreatePersonReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPersonCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- Domain.Action.Management.Transaction.buildTransaction apiTokenInfo.userActionType (Kernel.Prelude.Just DMatrix.DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  Domain.Action.Management.Transaction.withTransactionStoring transaction $ do
    whenJust req.email $ \email -> do
      emailHash <- getDbHash (T.toLower email)
      mbExistingPerson <- QPerson.findByEmailHash (Just emailHash)
      whenJust mbExistingPerson $ \_ ->
        throwError $ InvalidRequest "Email already registered"

    mobileHash <- getDbHash req.mobileNumber
    mbExistingPersonByMobile <- QPerson.findByMobileNumberHash (Just mobileHash) (Just req.mobileCountryCode)
    whenJust mbExistingPersonByMobile $ \_ ->
      throwError $ InvalidRequest "Phone already registered"

    role <- QRole.findByName req.roleName >>= fromMaybeM (InvalidRequest $ "Role with name " <> req.roleName <> " does not exist")

    personId <-
      if role.needsBppAccountCreation
        then do
          bppReq <-
            API.Client.Provider.Management.callManagementAPI
              checkedMerchantId
              opCity
              ( \apis ->
                  apis.personDSL.postPersonCreate
                    ( BPPPerson.CreatePersonReq
                        { email = req.email,
                          firstName = req.firstName,
                          lastName = req.lastName,
                          mobileCountryCode = req.mobileCountryCode,
                          mobileNumber = req.mobileNumber,
                          password = Nothing, -- BPP doesn't need password - users can login via OTP
                          roleName = req.roleName
                        }
                    )
              )
          pure $ Kernel.Types.Id.cast bppReq.personId
        else do
          generateGUID

    now <- getCurrentTime
    encryptedEmail <- forM req.email $ encrypt . T.toLower
    encryptedMobileNumber <- encrypt req.mobileNumber
    passwordHash <- forM req.password getDbHash

    let person =
          DPerson.Person
            { id = personId,
              firstName = req.firstName,
              lastName = req.lastName,
              roleId = role.id,
              email = encryptedEmail,
              mobileNumber = Just encryptedMobileNumber,
              mobileCountryCode = Just req.mobileCountryCode,
              passwordHash = passwordHash,
              createdAt = now,
              updatedAt = now,
              receiveNotification = Nothing,
              verified = Nothing,
              rejectionReason = Nothing,
              rejectedAt = Nothing,
              passwordUpdatedAt = now,
              approvedBy = Nothing,
              rejectedBy = Nothing
            }
    QPerson.create person
    pure Kernel.Types.APISuccess.Success

postUserLoginSendOtp ::
  ( Kernel.Types.Id.ShortId DMerchant.Merchant ->
    City.City ->
    API.Types.UnifiedDashboard.Management.Person.SendOtpReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postUserLoginSendOtp merchantShortId opCity req = do
  mobileNumberHash <- getDbHash req.mobileNumber
  person <- QPerson.findByMobileNumberHash (Just mobileNumberHash) (Just req.mobileCountryCode) >>= fromMaybeM (PersonDoesNotExist req.mobileNumber)
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  _merchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity person.id merchant.id opCity >>= fromMaybeM AccessDenied

  checkSlidingWindowLimit (authHitsCountKey person)

  -- Call BPP to generate OTP and send SMS (via helper API)
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  bppResp <-
    API.Client.Provider.Management.callManagementAPI
      checkedMerchantId
      opCity
      ( \apis ->
          apis.personDSL.postUserLoginSendOtp
            ( BPPPerson.SendOtpReq
                { mobileNumber = req.mobileNumber,
                  mobileCountryCode = req.mobileCountryCode
                }
            )
      )

  -- Store OTP in Redis cache (BPP generated and sent it, now we store it for verification)
  let otpKey = Tools.Utils.getMobileNumberOtpKey req.mobileCountryCode req.mobileNumber
  Redis.setExp otpKey bppResp.otp 300

  pure Kernel.Types.APISuccess.Success
