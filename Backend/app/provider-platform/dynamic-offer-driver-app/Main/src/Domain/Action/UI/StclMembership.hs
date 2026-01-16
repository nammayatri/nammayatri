{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.StclMembership (postSubmitApplication) where

import qualified API.Types.UI.StclMembership as APITypes
import qualified Data.Text as T
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.StclMembership as Domain
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.StclMembership as QStclMembership
import Tools.Auth

postSubmitApplication ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    APITypes.MembershipApplicationReq ->
    Environment.Flow APITypes.MembershipApplicationResp
  )
postSubmitApplication (mbDriverId, merchantId, merchantOperatingCityId) req = do
  -- Extract and validate driver ID
  driverId <- mbDriverId & fromMaybeM (InvalidRequest "Driver ID not found in authentication context")

  -- Validate that request driverId matches authenticated driver
  let requestDriverId = Kernel.Types.Id.Id req.driverId
  unless (driverId == requestDriverId) $
    throwError $ InvalidRequest "Driver ID in request does not match authenticated driver"

  -- Check for existing applications to prevent duplicates
  existingApplications <- QStclMembership.findByDriverId driverId
  unless (null existingApplications) $
    throwError $ InvalidRequest "An application already exists for this driver"

  -- Encrypt sensitive fields
  encryptedAadhar <- encrypt req.aadharNumber
  encryptedPAN <- encrypt req.panNumber
  encryptedMobile <- encrypt req.mobileNumber
  encryptedAccountNumber <- encrypt req.bankDetails.accountNumber
  encryptedIFSC <- encrypt req.bankDetails.ifscCode
  encryptedNomineeAadhar <- encrypt req.nomineeInfo.nomineeAadhar

  -- Generate application ID and record ID
  applicationId <- generateGUIDText
  recordId :: Kernel.Types.Id.Id Domain.StclMembership <- generateGUID
  now <- getCurrentTime

  -- Convert VehicleType enum to Text format for storage
  let vehicleTypeText = case req.vehicleInfo.vehicleType of
        APITypes.TwoWheeler -> "2Wheeler"
        APITypes.ThreeWheeler -> "3Wheeler"
        APITypes.FourWheeler -> "4Wheeler"

  -- Convert FuelType list to Text list
  let stripParentheses :: String -> String
      stripParentheses = filter (\c -> c `notElem` ['(', ')'])
  let fuelTypesText = map (T.pack . stripParentheses . show) req.vehicleInfo.fuelTypes

  -- Create domain type
  let membership =
        Domain.StclMembership
          { Domain.id = recordId,
            Domain.driverId = driverId,
            Domain.applicationId = applicationId,
            Domain.firstName = req.firstName,
            Domain.lastName = req.lastName,
            Domain.memberCategory = req.memberCategory,
            Domain.numberOfShares = req.numberOfShares,
            Domain.dateOfBirth = req.dateOfBirth,
            Domain.aadharNumber = encryptedAadhar,
            Domain.panNumber = encryptedPAN,
            Domain.mobileNumber = encryptedMobile,
            Domain.emailId = req.emailId,
            Domain.fatherMotherName = req.fatherMotherName,
            Domain.addressStreetAddress1 = req.address.streetAddress1,
            Domain.addressStreetAddress2 = req.address.streetAddress2,
            Domain.addressCity = req.address.city,
            Domain.addressState = req.address.stateName,
            Domain.addressPostalCode = req.address.postalCode,
            Domain.bankName = req.bankDetails.bankName,
            Domain.bankBranch = req.bankDetails.branch,
            Domain.accountNumber = encryptedAccountNumber,
            Domain.ifscCode = encryptedIFSC,
            Domain.vehicleType = vehicleTypeText,
            Domain.fuelTypes = fuelTypesText,
            Domain.nomineeName = req.nomineeInfo.nomineeName,
            Domain.nomineeAadhar = encryptedNomineeAadhar,
            Domain.declarationPlace = req.declaration.place,
            Domain.declarationDate = req.declaration.date,
            Domain.declarationSignature = req.declaration.signature,
            Domain.termsAccepted = req.declaration.termsAccepted,
            Domain.status = Domain.SUBMITTED,
            Domain.merchantId = merchantId,
            Domain.merchantOperatingCityId = merchantOperatingCityId,
            Domain.createdAt = now,
            Domain.updatedAt = now
          }

  -- Save to database
  QStclMembership.create membership

  -- Return response
  return $
    APITypes.MembershipApplicationResp
      { APITypes.applicationId = applicationId,
        APITypes.status = Domain.SUBMITTED,
        APITypes.message = "Your application has been submitted successfully",
        APITypes.submittedAt = now
      }
