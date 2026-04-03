{-# LANGUAGE OverloadedStrings #-}

module Dashboard
  ( registrationUnitTests,
    vehicleAssociationUnitTests,
    driverDocumentUploadUnitTests,
    driverOperationHubUnitTests,
    llmIntegrationUnitTests,
    dashboardUnitTests,
  )
where

import AddVehicleUnitTests (addVehicleUnitTests)
import CreateRequestUnitTests (createRequestUnitTests)
import DriverDocumentUploadUnitTests (driverDocumentUploadUnitTests)
import DriverOperationHubUnitTests (driverOperationHubUnitTests)
import LLMIntegrationUnitTests (allLLMIntegrationTests)
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
      allLLMIntegrationTests
    ]

-- Export LLM integration tests separately for focused testing
llmIntegrationUnitTests :: TestTree
llmIntegrationUnitTests = allLLMIntegrationTests
