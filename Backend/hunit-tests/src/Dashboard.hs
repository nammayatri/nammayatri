{-# LANGUAGE OverloadedStrings #-}

module Dashboard
  ( registrationUnitTests,
    vehicleAssociationUnitTests,
    driverDocumentUploadUnitTests,
    driverOperationHubUnitTests,
    dashboardUnitTests,
  )
where

import AddVehicleUnitTests (addVehicleUnitTests)
import CancellationDuesUnitTests (cancellationDuesUnitTests)
import CreateRequestUnitTests (createRequestUnitTests)
import DriverDocumentUploadUnitTests (driverDocumentUploadUnitTests)
import DriverOperationHubUnitTests (driverOperationHubUnitTests)
import FleetRoleUnitTests (fleetRoleUnitTests)
import RegistrationUnitTests (registrationUnitTests)
import StclMembershipUnitTests (stclMembershipUnitTests)
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
      cancellationDuesUnitTests,
      stclMembershipUnitTests,
      fleetRoleUnitTests
    ]
