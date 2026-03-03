{-# LANGUAGE OverloadedStrings #-}

module Dashboard
  ( registrationUnitTests,
    vehicleAssociationUnitTests,
    driverDocumentUploadUnitTests,
    driverOperationHubUnitTests,
    rolesHierarchyUnitTests,
    dashboardUnitTests,
  )
where

import AddVehicleUnitTests (addVehicleUnitTests)
import CreateRequestUnitTests (createRequestUnitTests)
import DriverDocumentUploadUnitTests (driverDocumentUploadUnitTests)
import DriverOperationHubUnitTests (driverOperationHubUnitTests)
import RegistrationUnitTests (registrationUnitTests)
import RolesHierarchyUnitTests (rolesHierarchyUnitTests)
import Test.Tasty (TestTree, testGroup)
import VehicleAssociationUnitTests (vehicleAssociationUnitTests)

-- Main test suite for all dashboard unit tests
dashboardUnitTests :: TestTree
dashboardUnitTests =
  testGroup
    "Dashboard Unit Tests"
    [ registrationUnitTests,
      rolesHierarchyUnitTests,
      vehicleAssociationUnitTests,
      driverDocumentUploadUnitTests,
      driverOperationHubUnitTests,
      addVehicleUnitTests,
      createRequestUnitTests
    ]
