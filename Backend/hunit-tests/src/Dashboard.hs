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
import CreateRequestUnitTests (createRequestUnitTests)
import DriverDocumentUploadUnitTests (driverDocumentUploadUnitTests)
import DriverOperationHubUnitTests (driverOperationHubUnitTests)
import FleetDriverStatsUnitTests (fleetDriverStatsUnitTests)
import FleetEarningsUnitTests (fleetEarningsUnitTests)
import FleetManagementUnitTests (fleetManagementUnitTests)
import FleetVehicleStatsUnitTests (fleetVehicleStatsUnitTests)
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
      fleetManagementUnitTests,
      fleetEarningsUnitTests,
      fleetDriverStatsUnitTests,
      fleetVehicleStatsUnitTests
    ]
