module Domain.Action.Dashboard.Operator.Registration (postOperatorRegister) where

import qualified API.Types.ProviderPlatform.Operator.Registration as Common
import qualified Domain.Action.UI.DriverReferral as DR
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import Tools.Error

postOperatorRegister ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.OperatorRegisterReq ->
  Flow Common.OperatorRegisterResp
postOperatorRegister merchantShortId opCity req = do
  mobileNumberHash <- getDbHash req.mobileNumber
  deploymentVersion <- asks (.version)
  merchant <-
    QMerchant.findByShortId merchantShortId
      >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)

  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personAuth = buildOperatorAuthReq merchant.id opCity req
  personOpt <- QP.findByMobileNumberAndMerchantAndRoles req.mobileCountryCode mobileNumberHash merchant.id [DP.OPERATOR, DP.FLEET_OWNER]
  case personOpt of
    Just pData -> throwError $ UserAlreadyExists pData.id.getId
    Nothing -> do
      person <- createOperatorDetails personAuth merchant.id merchantOpCityId True deploymentVersion.getDeploymentVersion
      return $ Common.OperatorRegisterResp {personId = cast @DP.Person @Common.Operator person.id}

createOperatorDetails :: Registration.AuthReq -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Text -> Flow DP.Person
createOperatorDetails authReq merchantId merchantOpCityId isDashboard deploymentVersion = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  person <- Registration.makePerson authReq transporterConfig Nothing Nothing Nothing Nothing (Just deploymentVersion) merchantId merchantOpCityId isDashboard (Just DP.OPERATOR)
  void $ QP.create person
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  QDriverStats.createInitialDriverStats merchantOperatingCity.currency merchantOperatingCity.distanceUnit person.id
  when (transporterConfig.generateReferralCodeForOperator == Just True) $ do
    void $ DR.generateReferralCode (Just DP.OPERATOR) (person.id, merchantId, merchantOpCityId)
  pure person

buildOperatorAuthReq ::
  Id DM.Merchant ->
  Context.City ->
  Common.OperatorRegisterReq ->
  Registration.AuthReq
buildOperatorAuthReq merchantId opCity Common.OperatorRegisterReq {..} =
  Registration.AuthReq
    { name = Just (firstName <> " " <> lastName),
      mobileNumber = Just mobileNumber,
      mobileCountryCode = Just mobileCountryCode,
      merchantId = merchantId.getId,
      merchantOperatingCity = Just opCity,
      identifierType = Just DP.MOBILENUMBER,
      email = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing
    }
