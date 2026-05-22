{-# LANGUAGE OverloadedStrings #-}

module Dashboard
  ( registrationUnitTests,
    vehicleAssociationUnitTests,
    driverDocumentUploadUnitTests,
    driverOperationHubUnitTests,
    emailOtpUnitTests,
    dashboardUnitTests,
  )
where

import AddVehicleUnitTests (addVehicleUnitTests)
import CreateRequestUnitTests (createRequestUnitTests)
import DriverDocumentUploadUnitTests (driverDocumentUploadUnitTests)
import DriverOperationHubUnitTests (driverOperationHubUnitTests)
import EmailOtpUnitTests (emailOtpUnitTests)
import RegistrationUnitTests (registrationUnitTests)
import Test.Tasty (TestTree, testGroup)
import VehicleAssociationUnitTests (vehicleAssociationUnitTests)

-- Main test suite for all dashboard unit tests
dashboardUnitTests :: TestTree
dashboardUnitTests =
  testGroup
    "Dashboard Unit Tests"
    [ registrationUnitTests,
      vehicleAssociationUnitTests,
      driverDocumentUploadUnitTests,
      driverOperationHubUnitTests,
      addVehicleUnitTests,
      createRequestUnitTests,
      emailOtpUnitTests
    ]
