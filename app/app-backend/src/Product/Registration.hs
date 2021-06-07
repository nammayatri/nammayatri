module Product.Registration (initiateLogin, login, reInitiateLogin, logout) where

import App.Types
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Common as BC
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Crypto.Number.Generate as Cryptonite
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.Registration
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandlerAPI $
    case (req.medium, req.__type) of
      (SR.SMS, SR.OTP) -> ask >>= initiateFlow req . smsCfg
      _ -> throwError $ InvalidRequest "medium and type must be sms and otp respectively"

initiateFlow :: InitiateLoginReq -> SmsConfig -> Flow InitiateLoginRes
initiateFlow req smsCfg = do
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  person <-
    Person.findByRoleAndMobileNumber SP.USER countryCode mobileNumber
      >>= maybe (createPerson req) return
  let entityId = getId . SP.id $ person
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg req entityId (show <$> useFakeOtpM)
      DB.runSqlDB (RegistrationToken.create token)
      return token
    Nothing -> do
      token <- makeSession scfg req entityId Nothing
      DB.runSqlDB (RegistrationToken.create token)
      otpSmsTemplate <- otpSmsTemplate <$> ask
      SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
      return token
  let attempts = SR.attempts regToken
      tokenId = SR.id regToken
  Notify.notifyOnRegistration regToken person
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: InitiateLoginReq -> Flow SP.Person
makePerson req = do
  role <- (req.role) & fromMaybeM (InvalidRequest "You should pass person's role.")
  pid <- BC.generateGUID
  now <- getCurrentTime
  return $
    SP.Person
      { id = pid,
        firstName = Nothing,
        middleName = Nothing,
        lastName = Nothing,
        fullName = Nothing,
        role = role,
        gender = SP.UNKNOWN,
        identifierType = SP.MOBILENUMBER,
        email = Nothing,
        passwordHash = Nothing,
        mobileNumber = Just $ req.mobileNumber,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        verified = False,
        status = SP.INACTIVE,
        deviceToken = req.deviceToken,
        udf1 = Nothing,
        udf2 = Nothing,
        organizationId = Nothing,
        locationId = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now
      }

makeSession ::
  SmsSessionConfig -> InitiateLoginReq -> Text -> Maybe Text -> Flow SR.RegistrationToken
makeSession SmsSessionConfig {..} req entityId fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = rtid,
        token = token,
        attempts = attempts,
        authMedium = req.medium,
        authType = req.__type,
        authValueHash = otp,
        verified = False,
        authExpiry = authExpiry,
        tokenExpiry = tokenExpiry,
        entityId = entityId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

generateOTPCode :: Flow Text
generateOTPCode =
  L.runIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandlerAPI $ do
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    when verified $ throwError $ AuthBlocked "Already verified."
    checkForExpiry authExpiry updatedAt
    let isValid =
          authMedium == req.medium
            && authType == req.__type
            && authValueHash == req.hash
    if isValid
      then do
        person <- checkPersonExists entityId
        clearOldRegToken person $ Id tokenId
        let personId = person.id
            updatedPerson =
              person
                { SP.status = SP.ACTIVE,
                  SP.deviceToken =
                    (req.deviceToken) <|> (person.deviceToken)
                }
        DB.runSqlDB (Person.updateMultiple personId updatedPerson)
        LoginRes token . SP.maskPerson
          <$> ( Person.findById personId
                  >>= fromMaybeM PersonNotFound
              )
      else throwError InvalidAuthData
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

getRegistrationTokenE :: Text -> Flow SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM (TokenNotFound tokenId)

createPerson :: InitiateLoginReq -> Flow SP.Person
createPerson req = do
  person <- makePerson req
  Person.create person
  pure person

checkPersonExists :: Text -> Flow SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) >>= fromMaybeM PersonDoesNotExist

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandlerAPI $ do
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    void $ checkPersonExists entityId
    if attempts > 0
      then do
        smsCfg <- smsCfg <$> ask
        otpSmsTemplate <- otpSmsTemplate <$> ask
        let mobileNumber = req.mobileNumber
            countryCode = req.mobileCountryCode
        SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
        _ <- RegistrationToken.updateAttempts (attempts - 1) id
        return $ InitiateLoginRes tokenId (attempts - 1)
      else throwError $ AuthBlocked "Attempts limit exceed."

clearOldRegToken :: SP.Person -> Id SR.RegistrationToken -> Flow ()
clearOldRegToken person newRT = do
  RegistrationToken.deleteByPersonIdExceptNew (getId $ person.id) newRT

logout :: SP.Person -> FlowHandler APISuccess
logout person = withFlowHandlerAPI $ do
  DB.runSqlDBTransaction $ do
    Person.updateMultiple (person.id) person {SP.deviceToken = Nothing}
    RegistrationToken.deleteByPersonId $ getId $ person.id
  pure Success
