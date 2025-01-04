{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.PassService where

import qualified API.Types.UI.PassService
import Data.Aeson as A
import Data.Coerce (coerce)
import Data.OpenApi (ToSchema)
import Data.Time (addUTCTime)
import Domain.Types.AadhaarVerification
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Pass as DPass
import Domain.Types.PassCategory
import Domain.Types.PassType
import qualified Domain.Types.Person as DP
import Domain.Types.PurchasedPass
import qualified Environment
import EulerHS.Prelude hiding (id, map, pass)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.External.Payment.Types as Payment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Lib.Yudhishthira.Tools.Utils as LYTU
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AadhaarVerification as QAV
import qualified Storage.Queries.Pass as QPass
import qualified Storage.Queries.PassCategory as QPassCategory
import qualified Storage.Queries.PassType as QPassType
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import Tools.Auth
import Tools.Error
import qualified Tools.Payment as Payment

getPasses :: ((Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person), Kernel.Types.Id.Id DM.Merchant) -> Environment.Flow [API.Types.UI.PassService.PassCategoryAPIEntity])
getPasses (_, merchantId) = do
  passCategories <- QPassCategory.findAll merchantId
  let res =
        map
          ( \category ->
              API.Types.UI.PassService.PassCategoryAPIEntity
                { id = category.id,
                  name = category.name,
                  description = category.description
                }
          )
          passCategories
  return res

getDetails ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
    Kernel.Types.Id.Id DM.Merchant
  ) ->
  Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory ->
  Environment.Flow [API.Types.UI.PassService.PassAPIEntity]
getDetails (_, merchantId) categoryId = do
  passCategory <- QPassCategory.findById categoryId >>= fromMaybeM (PassCategoryNotFound categoryId.getId)

  when (passCategory.merchantId /= merchantId) $
    throwError $ MerchantMismatch merchantId.getId

  passTypes <- QPassType.findByCategoryId categoryId
  passEntities <- mapM (fetchPassDetails passCategory) passTypes
  return passEntities
  where
    fetchPassDetails passCategory passType = do
      passes <- QPass.findAllByTypeId passType.id
      let passInfoEntities = map (fetchPassInfo passType) passes

      return
        API.Types.UI.PassService.PassAPIEntity
          { entityType =
              API.Types.UI.PassService.PassTypeAPIEntity
                { id = passType.id,
                  name = passType.name,
                  catchline = passType.catchline,
                  title = passType.title,
                  description = passType.description
                },
            category =
              API.Types.UI.PassService.PassCategoryAPIEntity
                { id = passCategory.id,
                  name = passCategory.name,
                  description = passCategory.description
                },
            info = passInfoEntities
          }

    fetchPassInfo _ pass =
      API.Types.UI.PassService.PassInfoAPIEntity
        { id = pass.id,
          amount = pass.amount,
          savings = pass.savings,
          benefit = pass.benefit,
          vehicleServiceTierType = pass.vehicleServiceTierType,
          trips = pass.trips,
          days = pass.days
        }

data PassEligibilityData = PassEligibilityData
  { aadhaarData :: Maybe Domain.Types.AadhaarVerification.AadhaarVerification,
    passData :: DPass.Pass,
    passTypeData :: Domain.Types.PassType.PassType
  }
  deriving (Show, ToJSON, FromJSON, Generic)

postSelect ::
  ( ( Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id DM.Merchant
    ) ->
    Kernel.Types.Id.Id DPass.Pass ->
    Environment.Flow API.Types.UI.PassService.PassSelectionAPIEntity
  )
postSelect (mbPersonId, merchantId) passId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  pass <- QPass.findById passId >>= fromMaybeM (PassNotFound $ getId passId)
  passType <- QPassType.findById pass.passTypeId >>= fromMaybeM (PassTypeNotFound $ getId pass.passTypeId)
  currentTime <- getCurrentTime

  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  aadhaarVerification <- QAV.findByPersonId personId

  let passEligibilityData =
        PassEligibilityData
          { aadhaarData = aadhaarVerification,
            passData = pass,
            passTypeData = passType
          }

  eligibilityResult <- try @_ @SomeException $ LYTU.runLogics pass.purchaseEligibilityJsonLogic passEligibilityData
  case eligibilityResult of
    Left err -> do
      logError $ "Error in running Pass Eligibility Logic: " <> show err
      throwError $ InvalidRequest "Eligibility check failed"
    Right logicResp ->
      case (A.fromJSON logicResp.result :: Result Bool) of
        A.Success True -> pure ()
        A.Success False -> throwError $ InvalidRequest "User not eligible for this pass"
        A.Error err -> do
          logError $ "Error in parsing Pass Eligibility Logic result: " <> show err
          throwError $ InternalError "Error in eligibility check"

  newId <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  let expiryDate = pass.days >>= \d -> Just $ addUTCTime (intToNominalDiffTime (d * 86400)) currentTime
  let newPersonPass =
        Domain.Types.PurchasedPass.PurchasedPass
          { id = newId,
            shortId = shortId,
            personId = personId,
            passId = passId,
            validTripsLeft = pass.trips,
            status = Domain.Types.PurchasedPass.Pending,
            expiryDate = expiryDate,
            merchantId = merchantId,
            merchantOperatingCityId = pass.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  _ <- QPurchasedPass.create newPersonPass

  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  personEmail <- mapM decrypt person.email
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity merchantId

  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = newId.getId,
            orderShortId = shortId.getShortId,
            amount = pass.amount,
            customerId = person.id.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
            customerPhone = personPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing,
            splitSettlementDetails = Nothing
          }

  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
      createOrderCall = Payment.createOrder merchantId person.merchantOperatingCityId Nothing Payment.Passes
  mCreateOrderRes <-
    DPayment.createOrderService commonMerchantId (Just $ Kernel.Types.Id.cast merchantOpCity.id) commonPersonId createOrderReq createOrderCall
      >>= fromMaybeM (InternalError "Order creation failed")
  let paymentOrderShortId :: ShortId DOrder.PaymentOrder = coerce newPersonPass.shortId
  newStatus <- webhookHandlerPasses paymentOrderShortId mCreateOrderRes.status

  case newStatus of
    Domain.Types.PurchasedPass.Active -> do
      QPurchasedPass.updateStatusByPersonId Domain.Types.PurchasedPass.Active personId
      pure $
        API.Types.UI.PassService.PassSelectionAPIEntity
          { purchasedPassId = newPersonPass.id,
            paymentOrder = Nothing
          }
    Domain.Types.PurchasedPass.Expired -> do
      QPurchasedPass.updateStatusByPersonId Domain.Types.PurchasedPass.Expired personId
      pure $
        API.Types.UI.PassService.PassSelectionAPIEntity
          { purchasedPassId = newPersonPass.id,
            paymentOrder = Nothing
          }
    Domain.Types.PurchasedPass.Pending -> do
      QPurchasedPass.updateStatusByPersonId Domain.Types.PurchasedPass.Pending personId
      pure $
        API.Types.UI.PassService.PassSelectionAPIEntity
          { purchasedPassId = newPersonPass.id,
            paymentOrder = Just mCreateOrderRes
          }
    Domain.Types.PurchasedPass.Refunded -> do
      QPurchasedPass.updateStatusByPersonId Domain.Types.PurchasedPass.Refunded personId
      pure $
        API.Types.UI.PassService.PassSelectionAPIEntity
          { purchasedPassId = newPersonPass.id,
            paymentOrder = Nothing
          }
    _ -> do
      QPurchasedPass.updateStatusByPersonId Domain.Types.PurchasedPass.Failed personId
      throwError $ InternalError "Payment failed during order creation"

getPurchasedDetail ::
  ( ( (Maybe (Kernel.Types.Id.Id DP.Person)),
      Kernel.Types.Id.Id DM.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass ->
    Environment.Flow API.Types.UI.PassService.PurchasedPassAPIEntity
  )
getPurchasedDetail (mbPersonId, merchantId) purchasedPassId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  purchasedPass <- QPurchasedPass.findById purchasedPassId >>= fromMaybeM (PurchasedPassNotFound $ getId purchasedPassId)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)

  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId person.currentCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> " ,city: " <> show person.currentCity)
  let paymentOrderShortId :: ShortId DOrder.PaymentOrder = ShortId purchasedPass.shortId.getShortId
  order <- QOrder.findByShortId paymentOrderShortId >>= fromMaybeM (PaymentOrderNotFound $ getShortId purchasedPass.shortId)
  let commonPersonId = cast @DP.Person @DPayment.Person personId
      orderStatusCall = Payment.orderStatus merchantId merchantOpCity.id Nothing Payment.Passes
  paymentStatusResp <- DPayment.orderStatusService commonPersonId order.id orderStatusCall
  mappedStatus <- webhookHandlerPasses paymentOrderShortId paymentStatusResp.status
  pass <- QPass.findById purchasedPass.passId >>= fromMaybeM (PassNotFound $ getId purchasedPass.passId)
  passType <- QPassType.findById pass.passTypeId >>= fromMaybeM (PassTypeNotFound $ getId pass.passTypeId)
  passCategory <- QPassCategory.findById passType.passCategoryId >>= fromMaybeM (PassCategoryNotFound $ getId passType.passCategoryId)

  let passTypeAPIEntity =
        API.Types.UI.PassService.PassTypeAPIEntity
          { id = passType.id,
            name = passType.name,
            catchline = passType.catchline,
            title = passType.title,
            description = passType.description
          }
      passCategoryAPIEntity =
        API.Types.UI.PassService.PassCategoryAPIEntity
          { id = passCategory.id,
            name = passCategory.name,
            description = passCategory.description
          }
      passInfo =
        API.Types.UI.PassService.PassInfoAPIEntity
          { id = pass.id,
            amount = pass.amount,
            savings = pass.savings,
            benefit = pass.benefit,
            vehicleServiceTierType = pass.vehicleServiceTierType,
            -- aadhaarVerificationRequired = pass.aadhaarVerificationRequired,
            trips = pass.trips,
            days = pass.days
          }
      passEntity =
        API.Types.UI.PassService.PassAPIEntity
          { entityType = passTypeAPIEntity,
            category = passCategoryAPIEntity,
            info = [passInfo]
          }
  return
    API.Types.UI.PassService.PurchasedPassAPIEntity
      { id = purchasedPass.id,
        passEntity = passEntity,
        tripsLeft = purchasedPass.validTripsLeft,
        status = mappedStatus,
        purchaseDate = purchasedPass.createdAt,
        expiryDate = purchasedPass.expiryDate
      }

getPurchasedList ::
  ( ( Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id DM.Merchant
    ) ->
    Kernel.Prelude.Int ->
    Kernel.Prelude.Int ->
    Environment.Flow [API.Types.UI.PassService.PurchasedPassAPIEntity]
  )
getPurchasedList (mbPersonId, _) limit offset = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  personPasses <- QPurchasedPass.findAllByPersonId (Just limit) (Just offset) personId

  purchasedPassEntities <-
    mapM
      ( \purchasedPass -> do
          pass <- QPass.findById purchasedPass.passId >>= fromMaybeM (PassNotFound $ getId purchasedPass.passId)
          passType <- QPassType.findById pass.passTypeId >>= fromMaybeM (PassTypeNotFound $ getId pass.passTypeId)
          passCategory <- QPassCategory.findById passType.passCategoryId >>= fromMaybeM (PassCategoryNotFound $ getId passType.passCategoryId)

          let passTypeAPIEntity =
                API.Types.UI.PassService.PassTypeAPIEntity
                  { id = passType.id,
                    name = passType.name,
                    catchline = passType.catchline,
                    title = passType.title,
                    description = passType.description
                  }

          let passCategoryAPIEntity =
                API.Types.UI.PassService.PassCategoryAPIEntity
                  { id = passCategory.id,
                    name = passCategory.name,
                    description = passCategory.description
                  }

          let passInfo =
                API.Types.UI.PassService.PassInfoAPIEntity
                  { id = pass.id,
                    amount = pass.amount,
                    savings = pass.savings,
                    benefit = pass.benefit,
                    vehicleServiceTierType = pass.vehicleServiceTierType,
                    trips = pass.trips,
                    days = pass.days
                  }

              passEntity =
                API.Types.UI.PassService.PassAPIEntity
                  { entityType = passTypeAPIEntity,
                    category = passCategoryAPIEntity,
                    info = [passInfo]
                  }

          return
            API.Types.UI.PassService.PurchasedPassAPIEntity
              { id = purchasedPass.id,
                passEntity = passEntity,
                tripsLeft = purchasedPass.validTripsLeft,
                status = purchasedPass.status,
                purchaseDate = purchasedPass.createdAt,
                expiryDate = purchasedPass.expiryDate
              }
      )
      personPasses

  return purchasedPassEntities

webhookHandlerPasses :: ShortId DOrder.PaymentOrder -> Kernel.External.Payment.Juspay.Types.Common.TransactionStatus -> Environment.Flow Domain.Types.PurchasedPass.StatusType
webhookHandlerPasses orderShortId transactionStatus = do
  let mappedStatus = case transactionStatus of
        NEW -> Domain.Types.PurchasedPass.Pending
        PENDING_VBV -> Domain.Types.PurchasedPass.Pending
        CHARGED -> Domain.Types.PurchasedPass.Active
        AUTHENTICATION_FAILED -> Domain.Types.PurchasedPass.Failed
        AUTHORIZATION_FAILED -> Domain.Types.PurchasedPass.Failed
        JUSPAY_DECLINED -> Domain.Types.PurchasedPass.Failed
        AUTHORIZING -> Domain.Types.PurchasedPass.Pending
        COD_INITIATED -> Domain.Types.PurchasedPass.Refunded
        STARTED -> Domain.Types.PurchasedPass.Pending
        AUTO_REFUNDED -> Domain.Types.PurchasedPass.Refunded
        CLIENT_AUTH_TOKEN_EXPIRED -> Domain.Types.PurchasedPass.Failed
        CANCELLED -> Domain.Types.PurchasedPass.Failed

  Redis.whenWithLockRedis (mkOrderStatusCheckKey orderShortId.getShortId transactionStatus) 60 $ do
    -- TODO: handle cases of overwrite if this get delayed in fetching response
    QPurchasedPass.updateStatusByShortId mappedStatus (ShortId orderShortId.getShortId)
  return mappedStatus

mkOrderStatusCheckKey :: Text -> Payment.TransactionStatus -> Text
mkOrderStatusCheckKey orderId status = "lockKey:orderId:" <> orderId <> ":status" <> show status
