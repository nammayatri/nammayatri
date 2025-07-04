Directory structure:
└── nammayatri-nammayatri/
    ├── README.md
    ├── crowdin.yml
    ├── crowdin_python_generator_configuration.json
    ├── crowdinTranslationGenerator.py
    ├── flake.lock
    ├── flake.nix
    ├── LICENSE
    ├── NammaTagConcept.md
    ├── om.yaml
    ├── .envrc.backend
    ├── .envrc.frontend
    ├── .hlint.yaml
    ├── .imgbotconfig
    ├── Backend/
    │   ├── README.md
    │   ├── cabal.project
    │   ├── CHANGELOG.md
    │   ├── db-check.sh
    │   ├── default.nix
    │   ├── dslLibs.yaml
    │   ├── dump.rdb
    │   ├── hie.yaml
    │   ├── Profiling.md
    │   ├── .gitignore
    │   ├── .hlint.yaml
    │   ├── app/
    │   │   ├── alchemist/
    │   │   │   ├── README.md
    │   │   │   ├── alchemist.cabal
    │   │   │   ├── package.yaml
    │   │   │   └── src/
    │   │   │       └── Main.hs
    │   │   ├── beckn-cli/
    │   │   │   ├── README.md
    │   │   │   ├── beckn-cli.cabal
    │   │   │   ├── package.yaml
    │   │   │   ├── app/
    │   │   │   │   └── Main.hs
    │   │   │   └── src/
    │   │   │       ├── GenerateKeyPair.hs
    │   │   │       └── PrepareDataForLoadTest.hs
    │   │   ├── dashboard/
    │   │   │   ├── CommonAPIs/
    │   │   │   │   ├── dashboard-helper-api.cabal
    │   │   │   │   ├── package.yaml
    │   │   │   │   ├── spec/
    │   │   │   │   │   ├── ProviderPlatform/
    │   │   │   │   │   │   ├── provider-dashboard-common.dhall
    │   │   │   │   │   │   ├── AppManagement/
    │   │   │   │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   │   │   │   └── API/
    │   │   │   │   │   │   │       ├── Driver.yaml
    │   │   │   │   │   │   │       ├── DriverSubscription.yaml
    │   │   │   │   │   │   │       ├── Overlay.yaml
    │   │   │   │   │   │   │       └── Subscription.yaml
    │   │   │   │   │   │   ├── Fleet/
    │   │   │   │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   │   │   │   └── API/
    │   │   │   │   │   │   │       ├── Driver.yaml
    │   │   │   │   │   │   │       ├── Onboarding.yaml
    │   │   │   │   │   │   │       └── RegistrationV2.yaml
    │   │   │   │   │   │   ├── Management/
    │   │   │   │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   │   │   │   └── API/
    │   │   │   │   │   │   │       ├── Account.yaml
    │   │   │   │   │   │   │       ├── Booking.yaml
    │   │   │   │   │   │   │       ├── CoinsConfig.yaml
    │   │   │   │   │   │   │       ├── Driver.yaml
    │   │   │   │   │   │   │       ├── DriverCoins.yaml
    │   │   │   │   │   │   │       ├── DriverGoHome.yaml
    │   │   │   │   │   │   │       ├── DriverReferral.yaml
    │   │   │   │   │   │   │       ├── DriverRegistration.yaml
    │   │   │   │   │   │   │       ├── Media.yaml
    │   │   │   │   │   │   │       ├── Merchant.yaml
    │   │   │   │   │   │   │       ├── Message.yaml
    │   │   │   │   │   │   │       ├── NammaTag.yaml
    │   │   │   │   │   │   │       ├── Payout.yaml
    │   │   │   │   │   │   │       ├── Revenue.yaml
    │   │   │   │   │   │   │       ├── Ride.yaml
    │   │   │   │   │   │   │       ├── System.yaml
    │   │   │   │   │   │   │       └── VehicleInfo.yaml
    │   │   │   │   │   │   ├── Operator/
    │   │   │   │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   │   │   │   └── API/
    │   │   │   │   │   │   │       ├── Driver.yaml
    │   │   │   │   │   │   │       ├── FleetManagement.yaml
    │   │   │   │   │   │   │       └── Registration.yaml
    │   │   │   │   │   │   └── RideBooking/
    │   │   │   │   │   │       ├── dsl-config.dhall
    │   │   │   │   │   │       └── API/
    │   │   │   │   │   │           ├── Driver.yaml
    │   │   │   │   │   │           ├── DriverRegistration.yaml
    │   │   │   │   │   │           ├── Maps.yaml
    │   │   │   │   │   │           ├── MeterRide.yaml
    │   │   │   │   │   │           ├── Ride.yaml
    │   │   │   │   │   │           ├── SearchRequest.yaml
    │   │   │   │   │   │           └── Volunteer.yaml
    │   │   │   │   │   └── RiderPlatform/
    │   │   │   │   │       ├── rider-dashboard-common.dhall
    │   │   │   │   │       ├── AppManagement/
    │   │   │   │   │       │   ├── dsl-config.dhall
    │   │   │   │   │       │   └── API/
    │   │   │   │   │       │       ├── Customer.yaml
    │   │   │   │   │       │       ├── EventManagement.yaml
    │   │   │   │   │       │       ├── MerchantOnboarding.yaml
    │   │   │   │   │       │       ├── TicketDashboard.yaml
    │   │   │   │   │       │       └── Tickets.yaml
    │   │   │   │   │       ├── Management/
    │   │   │   │   │       │   ├── dsl-config.dhall
    │   │   │   │   │       │   └── API/
    │   │   │   │   │       │       ├── Booking.yaml
    │   │   │   │   │       │       ├── Customer.yaml
    │   │   │   │   │       │       ├── FRFSTicket.yaml
    │   │   │   │   │       │       ├── Invoice.yaml
    │   │   │   │   │       │       ├── Merchant.yaml
    │   │   │   │   │       │       ├── NammaTag.yaml
    │   │   │   │   │       │       ├── Ride.yaml
    │   │   │   │   │       │       └── System.yaml
    │   │   │   │   │       └── RideBooking/
    │   │   │   │   │           ├── dsl-config.dhall
    │   │   │   │   │           └── API/
    │   │   │   │   │               ├── Booking.yaml
    │   │   │   │   │               ├── Cancel.yaml
    │   │   │   │   │               ├── Confirm.yaml
    │   │   │   │   │               ├── Frontend.yaml
    │   │   │   │   │               ├── Maps.yaml
    │   │   │   │   │               ├── NotifyRideInfo.yaml
    │   │   │   │   │               ├── Profile.yaml
    │   │   │   │   │               ├── Quote.yaml
    │   │   │   │   │               ├── Registration.yaml
    │   │   │   │   │               ├── Search.yaml
    │   │   │   │   │               └── Select.yaml
    │   │   │   │   ├── src/
    │   │   │   │   │   └── Dashboard/
    │   │   │   │   │       ├── Common.hs
    │   │   │   │   │       ├── SafetyPlatform.hs
    │   │   │   │   │       ├── Common/
    │   │   │   │   │       │   ├── Booking.hs
    │   │   │   │   │       │   ├── Driver.hs
    │   │   │   │   │       │   ├── DriverCoins.hs
    │   │   │   │   │       │   ├── Exotel.hs
    │   │   │   │   │       │   ├── Merchant.hs
    │   │   │   │   │       │   ├── Ride.hs
    │   │   │   │   │       │   └── SpecialZone.hs
    │   │   │   │   │       ├── ProviderPlatform/
    │   │   │   │   │       │   ├── CacAuth.hs
    │   │   │   │   │       │   ├── Fleet/
    │   │   │   │   │       │   │   ├── Driver.hs
    │   │   │   │   │       │   │   └── RegistrationV2.hs
    │   │   │   │   │       │   ├── Management/
    │   │   │   │   │       │   │   ├── Driver.hs
    │   │   │   │   │       │   │   ├── DriverCoins.hs
    │   │   │   │   │       │   │   ├── DriverReferral.hs
    │   │   │   │   │       │   │   ├── DriverRegistration.hs
    │   │   │   │   │       │   │   ├── Merchant.hs
    │   │   │   │   │       │   │   ├── Message.hs
    │   │   │   │   │       │   │   └── Ride.hs
    │   │   │   │   │       │   └── Operator/
    │   │   │   │   │       │       └── Registration.hs
    │   │   │   │   │       └── RiderPlatform/
    │   │   │   │   │           └── Management/
    │   │   │   │   │               ├── FRFSTicket.hs
    │   │   │   │   │               ├── Merchant.hs
    │   │   │   │   │               └── Ride.hs
    │   │   │   │   └── src-read-only/
    │   │   │   │       └── API/
    │   │   │   │           └── Types/
    │   │   │   │               ├── ProviderPlatform/
    │   │   │   │               │   ├── Fleet.hs
    │   │   │   │               │   ├── Management.hs
    │   │   │   │               │   ├── Operator.hs
    │   │   │   │               │   ├── Fleet/
    │   │   │   │               │   │   ├── Driver.hs
    │   │   │   │               │   │   ├── Onboarding.hs
    │   │   │   │               │   │   ├── RegistrationV2.hs
    │   │   │   │               │   │   └── Endpoints/
    │   │   │   │               │   │       ├── Driver.hs
    │   │   │   │               │   │       ├── Onboarding.hs
    │   │   │   │               │   │       └── RegistrationV2.hs
    │   │   │   │               │   ├── Management/
    │   │   │   │               │   │   ├── Account.hs
    │   │   │   │               │   │   ├── Booking.hs
    │   │   │   │               │   │   ├── CoinsConfig.hs
    │   │   │   │               │   │   ├── Driver.hs
    │   │   │   │               │   │   ├── DriverCoins.hs
    │   │   │   │               │   │   ├── DriverGoHome.hs
    │   │   │   │               │   │   ├── DriverReferral.hs
    │   │   │   │               │   │   ├── DriverRegistration.hs
    │   │   │   │               │   │   ├── Media.hs
    │   │   │   │               │   │   ├── Merchant.hs
    │   │   │   │               │   │   ├── Message.hs
    │   │   │   │               │   │   ├── NammaTag.hs
    │   │   │   │               │   │   ├── Payout.hs
    │   │   │   │               │   │   ├── Revenue.hs
    │   │   │   │               │   │   ├── Ride.hs
    │   │   │   │               │   │   ├── System.hs
    │   │   │   │               │   │   ├── VehicleInfo.hs
    │   │   │   │               │   │   └── Endpoints/
    │   │   │   │               │   │       ├── Account.hs
    │   │   │   │               │   │       ├── Booking.hs
    │   │   │   │               │   │       ├── CoinsConfig.hs
    │   │   │   │               │   │       ├── Driver.hs
    │   │   │   │               │   │       ├── DriverCoins.hs
    │   │   │   │               │   │       ├── DriverGoHome.hs
    │   │   │   │               │   │       ├── DriverReferral.hs
    │   │   │   │               │   │       ├── DriverRegistration.hs
    │   │   │   │               │   │       ├── Media.hs
    │   │   │   │               │   │       ├── Merchant.hs
    │   │   │   │               │   │       ├── Message.hs
    │   │   │   │               │   │       ├── NammaTag.hs
    │   │   │   │               │   │       ├── Payout.hs
    │   │   │   │               │   │       ├── Revenue.hs
    │   │   │   │               │   │       ├── Ride.hs
    │   │   │   │               │   │       ├── System.hs
    │   │   │   │               │   │       └── VehicleInfo.hs
    │   │   │   │               │   └── Operator/
    │   │   │   │               │       ├── Driver.hs
    │   │   │   │               │       ├── FleetManagement.hs
    │   │   │   │               │       ├── Registration.hs
    │   │   │   │               │       └── Endpoints/
    │   │   │   │               │           ├── Driver.hs
    │   │   │   │               │           ├── FleetManagement.hs
    │   │   │   │               │           └── Registration.hs
    │   │   │   │               └── RiderPlatform/
    │   │   │   │                   ├── Management.hs
    │   │   │   │                   └── Management/
    │   │   │   │                       ├── Booking.hs
    │   │   │   │                       ├── Customer.hs
    │   │   │   │                       ├── FRFSTicket.hs
    │   │   │   │                       ├── Invoice.hs
    │   │   │   │                       ├── Merchant.hs
    │   │   │   │                       ├── NammaTag.hs
    │   │   │   │                       ├── Ride.hs
    │   │   │   │                       ├── System.hs
    │   │   │   │                       └── Endpoints/
    │   │   │   │                           ├── Booking.hs
    │   │   │   │                           ├── Customer.hs
    │   │   │   │                           ├── FRFSTicket.hs
    │   │   │   │                           ├── Invoice.hs
    │   │   │   │                           ├── Merchant.hs
    │   │   │   │                           ├── NammaTag.hs
    │   │   │   │                           ├── Ride.hs
    │   │   │   │                           └── System.hs
    │   │   │   ├── Lib/
    │   │   │   │   ├── lib-dashboard.cabal
    │   │   │   │   ├── package.yaml
    │   │   │   │   └── src/
    │   │   │   │       ├── Environment.hs
    │   │   │   │       ├── API/
    │   │   │   │       │   ├── Dashboard.hs
    │   │   │   │       │   └── Dashboard/
    │   │   │   │       │       ├── AccessMatrix.hs
    │   │   │   │       │       ├── Merchant.hs
    │   │   │   │       │       ├── Person.hs
    │   │   │   │       │       ├── Registration.hs
    │   │   │   │       │       └── Roles.hs
    │   │   │   │       ├── Domain/
    │   │   │   │       │   ├── Action/
    │   │   │   │       │   │   └── Dashboard/
    │   │   │   │       │   │       ├── AccessMatrix.hs
    │   │   │   │       │   │       ├── Merchant.hs
    │   │   │   │       │   │       ├── Person.hs
    │   │   │   │       │   │       ├── Registration.hs
    │   │   │   │       │   │       ├── Roles.hs
    │   │   │   │       │   │       └── Transaction.hs
    │   │   │   │       │   └── Types/
    │   │   │   │       │       ├── AccessMatrix.hs
    │   │   │   │       │       ├── Merchant.hs
    │   │   │   │       │       ├── MerchantAccess.hs
    │   │   │   │       │       ├── Person.hs
    │   │   │   │       │       ├── RegistrationToken.hs
    │   │   │   │       │       ├── Role.hs
    │   │   │   │       │       ├── ServerName.hs
    │   │   │   │       │       ├── Transaction.hs
    │   │   │   │       │       └── Person/
    │   │   │   │       │           ├── API.hs
    │   │   │   │       │           └── Type.hs
    │   │   │   │       ├── SharedLogic/
    │   │   │   │       │   └── Transaction.hs
    │   │   │   │       ├── Storage/
    │   │   │   │       │   ├── Beam/
    │   │   │   │       │   │   ├── AccessMatrix.hs
    │   │   │   │       │   │   ├── BeamFlow.hs
    │   │   │   │       │   │   ├── Common.hs
    │   │   │   │       │   │   ├── Merchant.hs
    │   │   │   │       │   │   ├── MerchantAccess.hs
    │   │   │   │       │   │   ├── Person.hs
    │   │   │   │       │   │   ├── RegistrationToken.hs
    │   │   │   │       │   │   ├── Role.hs
    │   │   │   │       │   │   └── Transaction.hs
    │   │   │   │       │   └── Queries/
    │   │   │   │       │       ├── AccessMatrix.hs
    │   │   │   │       │       ├── Merchant.hs
    │   │   │   │       │       ├── MerchantAccess.hs
    │   │   │   │       │       ├── Person.hs
    │   │   │   │       │       ├── RegistrationToken.hs
    │   │   │   │       │       ├── Role.hs
    │   │   │   │       │       └── Transaction.hs
    │   │   │   │       └── Tools/
    │   │   │   │           ├── Auth.hs
    │   │   │   │           ├── Client.hs
    │   │   │   │           ├── Error.hs
    │   │   │   │           ├── Metrics.hs
    │   │   │   │           ├── Utils.hs
    │   │   │   │           ├── Auth/
    │   │   │   │           │   ├── Api.hs
    │   │   │   │           │   ├── Common.hs
    │   │   │   │           │   ├── Dashboard.hs
    │   │   │   │           │   └── Merchant.hs
    │   │   │   │           ├── Servant/
    │   │   │   │           │   └── HeaderAuth.hs
    │   │   │   │           └── Streaming/
    │   │   │   │               └── Kafka/
    │   │   │   │                   └── Environment.hs
    │   │   │   ├── provider-dashboard/
    │   │   │   │   ├── README.md
    │   │   │   │   ├── package.yaml
    │   │   │   │   ├── provider-dashboard.cabal
    │   │   │   │   ├── server/
    │   │   │   │   │   └── Main.hs
    │   │   │   │   ├── src/
    │   │   │   │   │   ├── API.hs
    │   │   │   │   │   ├── App.hs
    │   │   │   │   │   ├── API/
    │   │   │   │   │   │   ├── Exotel.hs
    │   │   │   │   │   │   ├── ProviderPlatform.hs
    │   │   │   │   │   │   ├── SpecialZone.hs
    │   │   │   │   │   │   ├── Fleet/
    │   │   │   │   │   │   │   └── Registration.hs
    │   │   │   │   │   │   └── ProviderPlatform/
    │   │   │   │   │   │       ├── DynamicOfferDriver.hs
    │   │   │   │   │   │       └── DynamicOfferDriver/
    │   │   │   │   │   │           ├── CacAuth.hs
    │   │   │   │   │   │           └── InternalAuth.hs
    │   │   │   │   │   ├── Domain/
    │   │   │   │   │   │   └── Action/
    │   │   │   │   │   │       └── ProviderPlatform/
    │   │   │   │   │   │           ├── CheckVerification.hs
    │   │   │   │   │   │           ├── AppManagement/
    │   │   │   │   │   │           │   ├── Driver.hs
    │   │   │   │   │   │           │   ├── DriverSubscription.hs
    │   │   │   │   │   │           │   ├── Overlay.hs
    │   │   │   │   │   │           │   └── Subscription.hs
    │   │   │   │   │   │           ├── Fleet/
    │   │   │   │   │   │           │   ├── Driver.hs
    │   │   │   │   │   │           │   ├── Onboarding.hs
    │   │   │   │   │   │           │   └── RegistrationV2.hs
    │   │   │   │   │   │           ├── IssueManagement/
    │   │   │   │   │   │           │   └── Issue.hs
    │   │   │   │   │   │           ├── Management/
    │   │   │   │   │   │           │   ├── Account.hs
    │   │   │   │   │   │           │   ├── Booking.hs
    │   │   │   │   │   │           │   ├── CoinsConfig.hs
    │   │   │   │   │   │           │   ├── Driver.hs
    │   │   │   │   │   │           │   ├── DriverCoins.hs
    │   │   │   │   │   │           │   ├── DriverGoHome.hs
    │   │   │   │   │   │           │   ├── DriverReferral.hs
    │   │   │   │   │   │           │   ├── DriverRegistration.hs
    │   │   │   │   │   │           │   ├── Media.hs
    │   │   │   │   │   │           │   ├── Merchant.hs
    │   │   │   │   │   │           │   ├── Message.hs
    │   │   │   │   │   │           │   ├── NammaTag.hs
    │   │   │   │   │   │           │   ├── Payout.hs
    │   │   │   │   │   │           │   ├── Revenue.hs
    │   │   │   │   │   │           │   ├── Ride.hs
    │   │   │   │   │   │           │   ├── System.hs
    │   │   │   │   │   │           │   └── VehicleInfo.hs
    │   │   │   │   │   │           ├── Operator/
    │   │   │   │   │   │           │   ├── Driver.hs
    │   │   │   │   │   │           │   ├── FleetManagement.hs
    │   │   │   │   │   │           │   └── Registration.hs
    │   │   │   │   │   │           └── RideBooking/
    │   │   │   │   │   │               ├── Driver.hs
    │   │   │   │   │   │               ├── DriverRegistration.hs
    │   │   │   │   │   │               ├── Maps.hs
    │   │   │   │   │   │               ├── MeterRide.hs
    │   │   │   │   │   │               ├── Ride.hs
    │   │   │   │   │   │               ├── SearchRequest.hs
    │   │   │   │   │   │               └── Volunteer.hs
    │   │   │   │   │   ├── ProviderPlatformClient/
    │   │   │   │   │   │   ├── SpecialZone.hs
    │   │   │   │   │   │   └── DynamicOfferDriver/
    │   │   │   │   │   │       ├── Exotel.hs
    │   │   │   │   │   │       └── Fleet.hs
    │   │   │   │   │   ├── RiderPlatformClient/
    │   │   │   │   │   │   └── RiderApp.hs
    │   │   │   │   │   ├── Storage/
    │   │   │   │   │   │   └── Beam/
    │   │   │   │   │   │       └── CommonInstances.hs
    │   │   │   │   │   └── Tools/
    │   │   │   │   │       └── Beam/
    │   │   │   │   │           └── UtilsTH.hs
    │   │   │   │   └── src-read-only/
    │   │   │   │       └── API/
    │   │   │   │           ├── Action/
    │   │   │   │           │   └── ProviderPlatform/
    │   │   │   │           │       ├── AppManagement.hs
    │   │   │   │           │       ├── Fleet.hs
    │   │   │   │           │       ├── IssueManagement.hs
    │   │   │   │           │       ├── Management.hs
    │   │   │   │           │       ├── Operator.hs
    │   │   │   │           │       ├── RideBooking.hs
    │   │   │   │           │       ├── AppManagement/
    │   │   │   │           │       │   ├── Driver.hs
    │   │   │   │           │       │   ├── DriverSubscription.hs
    │   │   │   │           │       │   ├── Overlay.hs
    │   │   │   │           │       │   └── Subscription.hs
    │   │   │   │           │       ├── Fleet/
    │   │   │   │           │       │   ├── Driver.hs
    │   │   │   │           │       │   ├── Onboarding.hs
    │   │   │   │           │       │   └── RegistrationV2.hs
    │   │   │   │           │       ├── IssueManagement/
    │   │   │   │           │       │   └── Issue.hs
    │   │   │   │           │       ├── Management/
    │   │   │   │           │       │   ├── Account.hs
    │   │   │   │           │       │   ├── Booking.hs
    │   │   │   │           │       │   ├── CoinsConfig.hs
    │   │   │   │           │       │   ├── Driver.hs
    │   │   │   │           │       │   ├── DriverCoins.hs
    │   │   │   │           │       │   ├── DriverGoHome.hs
    │   │   │   │           │       │   ├── DriverReferral.hs
    │   │   │   │           │       │   ├── DriverRegistration.hs
    │   │   │   │           │       │   ├── Media.hs
    │   │   │   │           │       │   ├── Merchant.hs
    │   │   │   │           │       │   ├── Message.hs
    │   │   │   │           │       │   ├── NammaTag.hs
    │   │   │   │           │       │   ├── Payout.hs
    │   │   │   │           │       │   ├── Revenue.hs
    │   │   │   │           │       │   ├── Ride.hs
    │   │   │   │           │       │   ├── System.hs
    │   │   │   │           │       │   └── VehicleInfo.hs
    │   │   │   │           │       ├── Operator/
    │   │   │   │           │       │   ├── Driver.hs
    │   │   │   │           │       │   ├── FleetManagement.hs
    │   │   │   │           │       │   └── Registration.hs
    │   │   │   │           │       └── RideBooking/
    │   │   │   │           │           ├── Driver.hs
    │   │   │   │           │           ├── DriverRegistration.hs
    │   │   │   │           │           ├── Maps.hs
    │   │   │   │           │           ├── MeterRide.hs
    │   │   │   │           │           ├── Ride.hs
    │   │   │   │           │           ├── SearchRequest.hs
    │   │   │   │           │           └── Volunteer.hs
    │   │   │   │           └── Client/
    │   │   │   │               └── ProviderPlatform/
    │   │   │   │                   ├── AppManagement.hs
    │   │   │   │                   ├── Fleet.hs
    │   │   │   │                   ├── IssueManagement.hs
    │   │   │   │                   ├── Management.hs
    │   │   │   │                   ├── Operator.hs
    │   │   │   │                   └── RideBooking.hs
    │   │   │   └── rider-dashboard/
    │   │   │       ├── README.md
    │   │   │       ├── package.yaml
    │   │   │       ├── rider-dashboard.cabal
    │   │   │       ├── server/
    │   │   │       │   └── Main.hs
    │   │   │       ├── src/
    │   │   │       │   ├── API.hs
    │   │   │       │   ├── App.hs
    │   │   │       │   ├── API/
    │   │   │       │   │   └── RiderPlatform.hs
    │   │   │       │   ├── Domain/
    │   │   │       │   │   └── Action/
    │   │   │       │   │       └── RiderPlatform/
    │   │   │       │   │           ├── AppManagement/
    │   │   │       │   │           │   ├── Customer.hs
    │   │   │       │   │           │   ├── EventManagement.hs
    │   │   │       │   │           │   ├── MerchantOnboarding.hs
    │   │   │       │   │           │   ├── TicketDashboard.hs
    │   │   │       │   │           │   └── Tickets.hs
    │   │   │       │   │           ├── IssueManagement/
    │   │   │       │   │           │   ├── Issue.hs
    │   │   │       │   │           │   └── IssueList.hs
    │   │   │       │   │           ├── Management/
    │   │   │       │   │           │   ├── Booking.hs
    │   │   │       │   │           │   ├── Customer.hs
    │   │   │       │   │           │   ├── FRFSTicket.hs
    │   │   │       │   │           │   ├── Invoice.hs
    │   │   │       │   │           │   ├── Merchant.hs
    │   │   │       │   │           │   ├── NammaTag.hs
    │   │   │       │   │           │   ├── Ride.hs
    │   │   │       │   │           │   └── System.hs
    │   │   │       │   │           └── RideBooking/
    │   │   │       │   │               ├── Booking.hs
    │   │   │       │   │               ├── Cancel.hs
    │   │   │       │   │               ├── Confirm.hs
    │   │   │       │   │               ├── Frontend.hs
    │   │   │       │   │               ├── Maps.hs
    │   │   │       │   │               ├── NotifyRideInfo.hs
    │   │   │       │   │               ├── Profile.hs
    │   │   │       │   │               ├── Quote.hs
    │   │   │       │   │               ├── Registration.hs
    │   │   │       │   │               ├── Search.hs
    │   │   │       │   │               └── Select.hs
    │   │   │       │   └── Storage/
    │   │   │       │       └── Beam/
    │   │   │       │           └── CommonInstances.hs
    │   │   │       └── src-read-only/
    │   │   │           └── API/
    │   │   │               ├── Action/
    │   │   │               │   └── RiderPlatform/
    │   │   │               │       ├── AppManagement.hs
    │   │   │               │       ├── IssueManagement.hs
    │   │   │               │       ├── Management.hs
    │   │   │               │       ├── RideBooking.hs
    │   │   │               │       ├── AppManagement/
    │   │   │               │       │   ├── Customer.hs
    │   │   │               │       │   ├── EventManagement.hs
    │   │   │               │       │   ├── MerchantOnboarding.hs
    │   │   │               │       │   ├── TicketDashboard.hs
    │   │   │               │       │   └── Tickets.hs
    │   │   │               │       ├── IssueManagement/
    │   │   │               │       │   ├── Issue.hs
    │   │   │               │       │   └── IssueList.hs
    │   │   │               │       ├── Management/
    │   │   │               │       │   ├── Booking.hs
    │   │   │               │       │   ├── Customer.hs
    │   │   │               │       │   ├── FRFSTicket.hs
    │   │   │               │       │   ├── Invoice.hs
    │   │   │               │       │   ├── Merchant.hs
    │   │   │               │       │   ├── NammaTag.hs
    │   │   │               │       │   ├── Ride.hs
    │   │   │               │       │   └── System.hs
    │   │   │               │       └── RideBooking/
    │   │   │               │           ├── Booking.hs
    │   │   │               │           ├── Cancel.hs
    │   │   │               │           ├── Confirm.hs
    │   │   │               │           ├── Frontend.hs
    │   │   │               │           ├── Maps.hs
    │   │   │               │           ├── NotifyRideInfo.hs
    │   │   │               │           ├── Profile.hs
    │   │   │               │           ├── Quote.hs
    │   │   │               │           ├── Registration.hs
    │   │   │               │           ├── Search.hs
    │   │   │               │           └── Select.hs
    │   │   │               └── Client/
    │   │   │                   └── RiderPlatform/
    │   │   │                       ├── AppManagement.hs
    │   │   │                       ├── IssueManagement.hs
    │   │   │                       ├── Management.hs
    │   │   │                       └── RideBooking.hs
    │   │   ├── example-service/
    │   │   │   ├── README.md
    │   │   │   ├── example-service.cabal
    │   │   │   ├── package.yaml
    │   │   │   ├── server/
    │   │   │   │   └── Main.hs
    │   │   │   └── src/
    │   │   │       ├── App.hs
    │   │   │       ├── Environment.hs
    │   │   │       ├── API/
    │   │   │       │   ├── Handler.hs
    │   │   │       │   └── Types.hs
    │   │   │       └── Tools/
    │   │   │           └── Metrics.hs
    │   │   ├── kafka-consumers/
    │   │   │   ├── kafka-consumers.cabal
    │   │   │   ├── package.yaml
    │   │   │   ├── app/
    │   │   │   │   └── Main.hs
    │   │   │   ├── src/
    │   │   │   │   ├── App.hs
    │   │   │   │   ├── Environment.hs
    │   │   │   │   ├── SystemConfigsOverride.hs
    │   │   │   │   ├── Consumer/
    │   │   │   │   │   ├── Flow.hs
    │   │   │   │   │   ├── AvailabilityTime/
    │   │   │   │   │   │   ├── Processor.hs
    │   │   │   │   │   │   ├── Types.hs
    │   │   │   │   │   │   └── Storage/
    │   │   │   │   │   │       ├── Queries.hs
    │   │   │   │   │   │       ├── Tables.hs
    │   │   │   │   │   │       └── Beam/
    │   │   │   │   │   │           └── Tables.hs
    │   │   │   │   │   ├── BroadcastMessage/
    │   │   │   │   │   │   └── Processor.hs
    │   │   │   │   │   ├── CustomerStats/
    │   │   │   │   │   │   └── Processor.hs
    │   │   │   │   │   └── LocationUpdate/
    │   │   │   │   │       ├── Processor.hs
    │   │   │   │   │       └── Types.hs
    │   │   │   │   └── DriverTrackingHealthCheck/
    │   │   │   │       ├── API.hs
    │   │   │   │       └── Service/
    │   │   │   │           └── Runner.hs
    │   │   │   └── test/
    │   │   │       └── Spec.hs
    │   │   ├── mocks/
    │   │   │   ├── fcm/
    │   │   │   │   ├── mock-fcm.cabal
    │   │   │   │   ├── package.yaml
    │   │   │   │   ├── server/
    │   │   │   │   │   └── Main.hs
    │   │   │   │   └── src/
    │   │   │   │       ├── App.hs
    │   │   │   │       ├── App/
    │   │   │   │       │   ├── Routes.hs
    │   │   │   │       │   └── Types.hs
    │   │   │   │       ├── Product/
    │   │   │   │       │   └── Fcm.hs
    │   │   │   │       └── Types/
    │   │   │   │           └── API/
    │   │   │   │               └── Fcm.hs
    │   │   │   ├── google/
    │   │   │   │   ├── README.md
    │   │   │   │   ├── mock-google.cabal
    │   │   │   │   ├── package.yaml
    │   │   │   │   ├── mock-data/
    │   │   │   │   │   └── snap-to-road/
    │   │   │   │   │       ├── requests.json
    │   │   │   │   │       └── response/
    │   │   │   │   │           ├── 1.json
    │   │   │   │   │           ├── 10.json
    │   │   │   │   │           ├── 11.json
    │   │   │   │   │           ├── 12.json
    │   │   │   │   │           ├── 13.json
    │   │   │   │   │           ├── 14.json
    │   │   │   │   │           ├── 2.json
    │   │   │   │   │           ├── 3.json
    │   │   │   │   │           ├── 4.json
    │   │   │   │   │           ├── 5.json
    │   │   │   │   │           ├── 6.json
    │   │   │   │   │           ├── 7.json
    │   │   │   │   │           ├── 8.json
    │   │   │   │   │           └── 9.json
    │   │   │   │   ├── server/
    │   │   │   │   │   └── Main.hs
    │   │   │   │   └── src/
    │   │   │   │       ├── API.hs
    │   │   │   │       ├── App.hs
    │   │   │   │       ├── Environment.hs
    │   │   │   │       ├── API/
    │   │   │   │       │   ├── Directions.hs
    │   │   │   │       │   ├── DistanceMatrix.hs
    │   │   │   │       │   ├── PlaceName.hs
    │   │   │   │       │   └── SnapToRoad.hs
    │   │   │   │       ├── Domain/
    │   │   │   │       │   └── Types/
    │   │   │   │       │       ├── MockPlace.hs
    │   │   │   │       │       └── MockRoute.hs
    │   │   │   │       ├── Lib/
    │   │   │   │       │   ├── GoogleConfig.hs
    │   │   │   │       │   └── IntegrationTests/
    │   │   │   │       │       └── Environment.hs
    │   │   │   │       ├── MockData/
    │   │   │   │       │   ├── Common.hs
    │   │   │   │       │   ├── Directions.hs
    │   │   │   │       │   ├── DistanceMatrix.hs
    │   │   │   │       │   ├── PlaceName.hs
    │   │   │   │       │   └── SnapToRoad.hs
    │   │   │   │       └── Tools/
    │   │   │   │           ├── Client.hs
    │   │   │   │           ├── Error.hs
    │   │   │   │           └── Metrics.hs
    │   │   │   ├── idfy/
    │   │   │   │   ├── mock-idfy.cabal
    │   │   │   │   ├── package.yaml
    │   │   │   │   ├── server/
    │   │   │   │   │   └── Main.hs
    │   │   │   │   └── src/
    │   │   │   │       ├── App.hs
    │   │   │   │       ├── Common.hs
    │   │   │   │       ├── App/
    │   │   │   │       │   ├── Routes.hs
    │   │   │   │       │   └── Types.hs
    │   │   │   │       ├── Product/
    │   │   │   │       │   └── Idfy.hs
    │   │   │   │       ├── Tools/
    │   │   │   │       │   └── FlowHandling.hs
    │   │   │   │       └── Types/
    │   │   │   │           ├── Common.hs
    │   │   │   │           └── Webhook.hs
    │   │   │   ├── public-transport-provider-platform/
    │   │   │   │   ├── mock-public-transport-provider-platform.cabal
    │   │   │   │   ├── package.yaml
    │   │   │   │   ├── server/
    │   │   │   │   │   └── Main.hs
    │   │   │   │   └── src/
    │   │   │   │       ├── App.hs
    │   │   │   │       ├── Environment.hs
    │   │   │   │       ├── ExternalAPI.hs
    │   │   │   │       ├── Redis.hs
    │   │   │   │       ├── API/
    │   │   │   │       │   ├── Confirm.hs
    │   │   │   │       │   ├── Search.hs
    │   │   │   │       │   ├── Status.hs
    │   │   │   │       │   ├── Types.hs
    │   │   │   │       │   └── Utils.hs
    │   │   │   │       └── MockData/
    │   │   │   │           ├── OnConfirm.hs
    │   │   │   │           └── OnSearch.hs
    │   │   │   ├── rider-platform/
    │   │   │   │   ├── README.md
    │   │   │   │   ├── mock-rider-platform.cabal
    │   │   │   │   ├── package.yaml
    │   │   │   │   ├── server/
    │   │   │   │   │   └── Main.hs
    │   │   │   │   └── src/
    │   │   │   │       ├── App.hs
    │   │   │   │       ├── Environment.hs
    │   │   │   │       └── API/
    │   │   │   │           ├── Handler.hs
    │   │   │   │           └── Types.hs
    │   │   │   └── sms/
    │   │   │       ├── mock-sms.cabal
    │   │   │       ├── package.yaml
    │   │   │       ├── server/
    │   │   │       │   └── Main.hs
    │   │   │       └── src/
    │   │   │           ├── App.hs
    │   │   │           ├── App/
    │   │   │           │   ├── Routes.hs
    │   │   │           │   └── Types.hs
    │   │   │           ├── Product/
    │   │   │           │   └── Sms.hs
    │   │   │           └── Types/
    │   │   │               └── API/
    │   │   │                   └── Sms.hs
    │   │   ├── provider-platform/
    │   │   │   ├── dynamic-offer-driver-app/
    │   │   │   │   ├── Allocator/
    │   │   │   │   │   ├── driver-offer-allocator.cabal
    │   │   │   │   │   ├── package.yaml
    │   │   │   │   │   ├── server/
    │   │   │   │   │   │   └── Main.hs
    │   │   │   │   │   └── src/
    │   │   │   │   │       ├── App.hs
    │   │   │   │   │       ├── Environment.hs
    │   │   │   │   │       └── Tools/
    │   │   │   │   │           └── Metrics.hs
    │   │   │   │   └── Main/
    │   │   │   │       ├── dynamic-offer-driver-app.cabal
    │   │   │   │       ├── package.yaml
    │   │   │   │       ├── server/
    │   │   │   │       │   └── Main.hs
    │   │   │   │       ├── spec/
    │   │   │   │       │   ├── dsl-config.dhall
    │   │   │   │       │   ├── API/
    │   │   │   │       │   │   ├── Cac.yaml
    │   │   │   │       │   │   ├── CallFeedback.yaml
    │   │   │   │       │   │   ├── DemandHotspots.yaml
    │   │   │   │       │   │   ├── DriverOnboarding.yaml
    │   │   │   │       │   │   ├── DriverProfileQuestions.yaml
    │   │   │   │       │   │   ├── EditBooking.yaml
    │   │   │   │       │   │   ├── fareCalculate.yaml
    │   │   │   │       │   │   ├── Insurance.yaml
    │   │   │   │       │   │   ├── invoice.yaml
    │   │   │   │       │   │   ├── LMS.yaml
    │   │   │   │       │   │   ├── Merchant.yaml
    │   │   │   │       │   │   ├── MeterRide.yaml
    │   │   │   │       │   │   ├── OperationHub.yaml
    │   │   │   │       │   │   ├── Operator.yaml
    │   │   │   │       │   │   ├── PriceBreakup.yaml
    │   │   │   │       │   │   ├── Reels.yaml
    │   │   │   │       │   │   ├── ReferralPayout.yaml
    │   │   │   │       │   │   ├── SocialLogin.yaml
    │   │   │   │       │   │   ├── specialLocation.yaml
    │   │   │   │       │   │   ├── SpecialLocWarrior.yaml
    │   │   │   │       │   │   ├── Tokenization.yaml
    │   │   │   │       │   │   ├── VehicleDetails.yaml
    │   │   │   │       │   │   ├── VehicleInfo.yaml
    │   │   │   │       │   │   └── wmb.yaml
    │   │   │   │       │   └── Storage/
    │   │   │   │       │       ├── AlertRequest.yaml
    │   │   │   │       │       ├── BapMetadata.yaml
    │   │   │   │       │       ├── BecknConfig.yaml
    │   │   │   │       │       ├── BlackListOrg.yaml
    │   │   │   │       │       ├── BlockedRoute.yaml
    │   │   │   │       │       ├── Booking.yaml
    │   │   │   │       │       ├── BookingCancellationReason.yaml
    │   │   │   │       │       ├── BookingUpdateRequest.yaml
    │   │   │   │       │       ├── BusinessEvent.yaml
    │   │   │   │       │       ├── CallFeedback.yaml
    │   │   │   │       │       ├── CallFeedbackOptions.yaml
    │   │   │   │       │       ├── CallStatus.yaml
    │   │   │   │       │       ├── cancellationCharges.yaml
    │   │   │   │       │       ├── CancellationReason.yaml
    │   │   │   │       │       ├── client.yaml
    │   │   │   │       │       ├── ConditionalCharges.yaml
    │   │   │   │       │       ├── configs.yaml
    │   │   │   │       │       ├── DailyStats.yaml
    │   │   │   │       │       ├── DriverBlocked.yaml
    │   │   │   │       │       ├── DriverBlockReason.yaml
    │   │   │   │       │       ├── DriverFee.yaml
    │   │   │   │       │       ├── DriverGoHome.yaml
    │   │   │   │       │       ├── DriverGullakAssociation.yaml
    │   │   │   │       │       ├── DriverInformation.yaml
    │   │   │   │       │       ├── DriverOnboarding.yaml
    │   │   │   │       │       ├── DriverOperatorAssociation.yaml
    │   │   │   │       │       ├── DriverPlan.yaml
    │   │   │   │       │       ├── DriverProfileQuestions.yaml
    │   │   │   │       │       ├── DriverQuote.yaml
    │   │   │   │       │       ├── DriverReferral.yaml
    │   │   │   │       │       ├── DriverStats.yaml
    │   │   │   │       │       ├── Estimate.yaml
    │   │   │   │       │       ├── Exophone.yaml
    │   │   │   │       │       ├── FareProduct.yaml
    │   │   │   │       │       ├── FeedBack.yaml
    │   │   │   │       │       ├── FleetBadge.yaml
    │   │   │   │       │       ├── FleetBadgeAssociation.yaml
    │   │   │   │       │       ├── FleetDriverAssociation.yaml
    │   │   │   │       │       ├── FleetMemberAssociation.yaml
    │   │   │   │       │       ├── FleetOperatorAssociation.yaml
    │   │   │   │       │       ├── FleetOwnerInformation.yaml
    │   │   │   │       │       ├── FRFSStaticData.yaml
    │   │   │   │       │       ├── FullFarePolicyProgressiveDetailsPerMinRateSection.yaml
    │   │   │   │       │       ├── Invoice.yaml
    │   │   │   │       │       ├── KioskLocation.yaml
    │   │   │   │       │       ├── KioskLocationTranslation.yaml
    │   │   │   │       │       ├── LeaderBoardConfigs.yaml
    │   │   │   │       │       ├── LMS.yaml
    │   │   │   │       │       ├── Location.yaml
    │   │   │   │       │       ├── Mandate.yaml
    │   │   │   │       │       ├── Merchant.yaml
    │   │   │   │       │       ├── Message.yaml
    │   │   │   │       │       ├── MetaData.yaml
    │   │   │   │       │       ├── Notification.yaml
    │   │   │   │       │       ├── OperationHub.yaml
    │   │   │   │       │       ├── Person.yaml
    │   │   │   │       │       ├── PlaceNameCache.yaml
    │   │   │   │       │       ├── Plan.yaml
    │   │   │   │       │       ├── PlanTranslation.yaml
    │   │   │   │       │       ├── PurchaseHistory.yaml
    │   │   │   │       │       ├── Rating.yaml
    │   │   │   │       │       ├── Reels.yaml
    │   │   │   │       │       ├── RegistrationToken.yaml
    │   │   │   │       │       ├── RegistryMapFallback.yaml
    │   │   │   │       │       ├── ride.yaml
    │   │   │   │       │       ├── RideDetails.yaml
    │   │   │   │       │       ├── RiderDetails.yaml
    │   │   │   │       │       ├── RiderDriverCorrelation.yaml
    │   │   │   │       │       ├── SearchRequest.yaml
    │   │   │   │       │       ├── SearchRequestForDriver.yaml
    │   │   │   │       │       ├── SearchRequestLocation.yaml
    │   │   │   │       │       ├── SearchTry.yaml
    │   │   │   │       │       ├── StopInformation.yaml
    │   │   │   │       │       ├── SubscriptionConfig.yaml
    │   │   │   │       │       ├── Toll.yaml
    │   │   │   │       │       ├── Translations.yaml
    │   │   │   │       │       ├── UiDriverConfig.yaml
    │   │   │   │       │       ├── Vehicle.yaml
    │   │   │   │       │       ├── VehicleDetails.yaml
    │   │   │   │       │       ├── VehicleInfo.yaml
    │   │   │   │       │       ├── VehicleServiceTier.yaml
    │   │   │   │       │       ├── VendorFee.yaml
    │   │   │   │       │       ├── VendorSplitDetails.yaml
    │   │   │   │       │       ├── Volunteer.yaml
    │   │   │   │       │       ├── WhereIsMyBus.yaml
    │   │   │   │       │       └── WhiteListOrg.yaml
    │   │   │   │       ├── src/
    │   │   │   │       │   ├── API.hs
    │   │   │   │       │   ├── App.hs
    │   │   │   │       │   ├── Environment.hs
    │   │   │   │       │   ├── API/
    │   │   │   │       │   │   ├── Beckn.hs
    │   │   │   │       │   │   ├── Dashboard.hs
    │   │   │   │       │   │   ├── IGM.hs
    │   │   │   │       │   │   ├── Internal.hs
    │   │   │   │       │   │   ├── UI.hs
    │   │   │   │       │   │   ├── Beckn/
    │   │   │   │       │   │   │   ├── Cancel.hs
    │   │   │   │       │   │   │   ├── Confirm.hs
    │   │   │   │       │   │   │   ├── Init.hs
    │   │   │   │       │   │   │   ├── Rating.hs
    │   │   │   │       │   │   │   ├── Search.hs
    │   │   │   │       │   │   │   ├── Select.hs
    │   │   │   │       │   │   │   ├── Status.hs
    │   │   │   │       │   │   │   ├── Track.hs
    │   │   │   │       │   │   │   ├── Update.hs
    │   │   │   │       │   │   │   └── IGM/
    │   │   │   │       │   │   │       └── Issue.hs
    │   │   │   │       │   │   ├── Dashboard/
    │   │   │   │       │   │   │   ├── Exotel.hs
    │   │   │   │       │   │   │   ├── Fleet.hs
    │   │   │   │       │   │   │   └── Fleet/
    │   │   │   │       │   │   │       └── Registration.hs
    │   │   │   │       │   │   ├── External/
    │   │   │   │       │   │   │   └── LiveEKD.hs
    │   │   │   │       │   │   ├── Internal/
    │   │   │   │       │   │   │   ├── Auth.hs
    │   │   │   │       │   │   │   ├── BulkLocUpdate.hs
    │   │   │   │       │   │   │   ├── Cac.hs
    │   │   │   │       │   │   │   ├── CallCustomerFCM.hs
    │   │   │   │       │   │   │   ├── CustomerCancellationDues.hs
    │   │   │   │       │   │   │   ├── DriverCoordinates.hs
    │   │   │   │       │   │   │   ├── DriverInactiveFCM.hs
    │   │   │   │       │   │   │   ├── DriverMode.hs
    │   │   │   │       │   │   │   ├── DriverReachedDestination.hs
    │   │   │   │       │   │   │   ├── DriverReferee.hs
    │   │   │   │       │   │   │   ├── DriverSourceDeparted.hs
    │   │   │   │       │   │   │   ├── FavouriteDrivers.hs
    │   │   │   │       │   │   │   ├── FeedbackForm.hs
    │   │   │   │       │   │   │   ├── KnowYourDriver.hs
    │   │   │   │       │   │   │   ├── Multimodal.hs
    │   │   │   │       │   │   │   ├── PopulateTipAmount.hs
    │   │   │   │       │   │   │   ├── ProdLoopStatus.hs
    │   │   │   │       │   │   │   ├── ReportACIssue.hs
    │   │   │   │       │   │   │   ├── ReportIssue.hs
    │   │   │   │       │   │   │   ├── Ride.hs
    │   │   │   │       │   │   │   ├── StopDetection.hs
    │   │   │   │       │   │   │   └── ViolationDetection.hs
    │   │   │   │       │   │   └── UI/
    │   │   │   │       │   │       ├── Call.hs
    │   │   │   │       │   │       ├── CallEvent.hs
    │   │   │   │       │   │       ├── CancellationReason.hs
    │   │   │   │       │   │       ├── City.hs
    │   │   │   │       │   │       ├── Driver.hs
    │   │   │   │       │   │       ├── DriverCoins.hs
    │   │   │   │       │   │       ├── DriverOnboarding.hs
    │   │   │   │       │   │       ├── DriverProfileSummary.hs
    │   │   │   │       │   │       ├── DriverReferral.hs
    │   │   │   │       │   │       ├── ExotelEndRide.hs
    │   │   │   │       │   │       ├── Issue.hs
    │   │   │   │       │   │       ├── KioskLocation.hs
    │   │   │   │       │   │       ├── LeaderBoard.hs
    │   │   │   │       │   │       ├── Maps.hs
    │   │   │   │       │   │       ├── Message.hs
    │   │   │   │       │   │       ├── OnMessage.hs
    │   │   │   │       │   │       ├── OrgAdmin.hs
    │   │   │   │       │   │       ├── Payment.hs
    │   │   │   │       │   │       ├── Performance.hs
    │   │   │   │       │   │       ├── Plan.hs
    │   │   │   │       │   │       ├── Rating.hs
    │   │   │   │       │   │       ├── Registration.hs
    │   │   │   │       │   │       ├── Ride.hs
    │   │   │   │       │   │       ├── RideRoute.hs
    │   │   │   │       │   │       ├── RideSummary.hs
    │   │   │   │       │   │       ├── Route.hs
    │   │   │   │       │   │       ├── Transporter.hs
    │   │   │   │       │   │       └── Whatsapp.hs
    │   │   │   │       │   ├── App/
    │   │   │   │       │   │   └── Server.hs
    │   │   │   │       │   ├── Beckn/
    │   │   │   │       │   │   ├── ACL/
    │   │   │   │       │   │   │   ├── Cancel.hs
    │   │   │   │       │   │   │   ├── Common.hs
    │   │   │   │       │   │   │   ├── Confirm.hs
    │   │   │   │       │   │   │   ├── Init.hs
    │   │   │   │       │   │   │   ├── OnCancel.hs
    │   │   │   │       │   │   │   ├── OnConfirm.hs
    │   │   │   │       │   │   │   ├── OnInit.hs
    │   │   │   │       │   │   │   ├── OnSearch.hs
    │   │   │   │       │   │   │   ├── OnSelect.hs
    │   │   │   │       │   │   │   ├── OnStatus.hs
    │   │   │   │       │   │   │   ├── OnTrack.hs
    │   │   │   │       │   │   │   ├── OnUpdate.hs
    │   │   │   │       │   │   │   ├── Rating.hs
    │   │   │   │       │   │   │   ├── Search.hs
    │   │   │   │       │   │   │   ├── Select.hs
    │   │   │   │       │   │   │   ├── Status.hs
    │   │   │   │       │   │   │   ├── Track.hs
    │   │   │   │       │   │   │   ├── Update.hs
    │   │   │   │       │   │   │   ├── Common/
    │   │   │   │       │   │   │   │   └── Order.hs
    │   │   │   │       │   │   │   └── IGM/
    │   │   │   │       │   │   │       └── Utils.hs
    │   │   │   │       │   │   └── OnDemand/
    │   │   │   │       │   │       ├── Transformer/
    │   │   │   │       │   │       │   ├── Init.hs
    │   │   │   │       │   │       │   ├── OnSearch.hs
    │   │   │   │       │   │       │   ├── OnUpdate.hs
    │   │   │   │       │   │       │   └── Search.hs
    │   │   │   │       │   │       └── Utils/
    │   │   │   │       │   │           ├── Callback.hs
    │   │   │   │       │   │           ├── Common.hs
    │   │   │   │       │   │           ├── Init.hs
    │   │   │   │       │   │           ├── OnSearch.hs
    │   │   │   │       │   │           ├── OnUpdate.hs
    │   │   │   │       │   │           └── Search.hs
    │   │   │   │       │   ├── Domain/
    │   │   │   │       │   │   ├── Action/
    │   │   │   │       │   │   │   ├── Beckn/
    │   │   │   │       │   │   │   │   ├── Cancel.hs
    │   │   │   │       │   │   │   │   ├── Confirm.hs
    │   │   │   │       │   │   │   │   ├── Init.hs
    │   │   │   │       │   │   │   │   ├── Rating.hs
    │   │   │   │       │   │   │   │   ├── Search.hs
    │   │   │   │       │   │   │   │   ├── Select.hs
    │   │   │   │       │   │   │   │   ├── Status.hs
    │   │   │   │       │   │   │   │   ├── Track.hs
    │   │   │   │       │   │   │   │   └── Update.hs
    │   │   │   │       │   │   │   ├── Dashboard/
    │   │   │   │       │   │   │   │   ├── Common.hs
    │   │   │   │       │   │   │   │   ├── Exotel.hs
    │   │   │   │       │   │   │   │   ├── Ride.hs
    │   │   │   │       │   │   │   │   ├── AppManagement/
    │   │   │   │       │   │   │   │   │   ├── Driver.hs
    │   │   │   │       │   │   │   │   │   ├── DriverSubscription.hs
    │   │   │   │       │   │   │   │   │   ├── Overlay.hs
    │   │   │   │       │   │   │   │   │   └── Subscription.hs
    │   │   │   │       │   │   │   │   ├── Driver/
    │   │   │   │       │   │   │   │   │   └── Notification.hs
    │   │   │   │       │   │   │   │   ├── Fleet/
    │   │   │   │       │   │   │   │   │   ├── Driver.hs
    │   │   │   │       │   │   │   │   │   ├── Onboarding.hs
    │   │   │   │       │   │   │   │   │   ├── Referral.hs
    │   │   │   │       │   │   │   │   │   ├── Registration.hs
    │   │   │   │       │   │   │   │   │   └── RegistrationV2.hs
    │   │   │   │       │   │   │   │   ├── IssueManagement/
    │   │   │   │       │   │   │   │   │   └── Issue.hs
    │   │   │   │       │   │   │   │   ├── Management/
    │   │   │   │       │   │   │   │   │   ├── Account.hs
    │   │   │   │       │   │   │   │   │   ├── Booking.hs
    │   │   │   │       │   │   │   │   │   ├── CoinsConfig.hs
    │   │   │   │       │   │   │   │   │   ├── Driver.hs
    │   │   │   │       │   │   │   │   │   ├── DriverCoins.hs
    │   │   │   │       │   │   │   │   │   ├── DriverGoHome.hs
    │   │   │   │       │   │   │   │   │   ├── DriverReferral.hs
    │   │   │   │       │   │   │   │   │   ├── DriverRegistration.hs
    │   │   │   │       │   │   │   │   │   ├── Media.hs
    │   │   │   │       │   │   │   │   │   ├── Merchant.hs
    │   │   │   │       │   │   │   │   │   ├── Message.hs
    │   │   │   │       │   │   │   │   │   ├── NammaTag.hs
    │   │   │   │       │   │   │   │   │   ├── Payout.hs
    │   │   │   │       │   │   │   │   │   ├── Revenue.hs
    │   │   │   │       │   │   │   │   │   ├── Ride.hs
    │   │   │   │       │   │   │   │   │   ├── System.hs
    │   │   │   │       │   │   │   │   │   └── VehicleInfo.hs
    │   │   │   │       │   │   │   │   ├── Operator/
    │   │   │   │       │   │   │   │   │   ├── Driver.hs
    │   │   │   │       │   │   │   │   │   ├── FleetManagement.hs
    │   │   │   │       │   │   │   │   │   └── Registration.hs
    │   │   │   │       │   │   │   │   └── RideBooking/
    │   │   │   │       │   │   │   │       ├── Driver.hs
    │   │   │   │       │   │   │   │       ├── DriverRegistration.hs
    │   │   │   │       │   │   │   │       ├── Maps.hs
    │   │   │   │       │   │   │   │       ├── MeterRide.hs
    │   │   │   │       │   │   │   │       ├── Ride.hs
    │   │   │   │       │   │   │   │       ├── SearchRequest.hs
    │   │   │   │       │   │   │   │       └── Volunteer.hs
    │   │   │   │       │   │   │   ├── External/
    │   │   │   │       │   │   │   │   └── LiveEKD.hs
    │   │   │   │       │   │   │   ├── Internal/
    │   │   │   │       │   │   │   │   ├── Auth.hs
    │   │   │   │       │   │   │   │   ├── BulkLocUpdate.hs
    │   │   │   │       │   │   │   │   ├── Cac.hs
    │   │   │   │       │   │   │   │   ├── CallCustomerFCM.hs
    │   │   │   │       │   │   │   │   ├── CustomerCancellationDues.hs
    │   │   │   │       │   │   │   │   ├── DriverCoordinates.hs
    │   │   │   │       │   │   │   │   ├── DriverInactiveFCM.hs
    │   │   │   │       │   │   │   │   ├── DriverMode.hs
    │   │   │   │       │   │   │   │   ├── DriverReachedDestination.hs
    │   │   │   │       │   │   │   │   ├── DriverReferee.hs
    │   │   │   │       │   │   │   │   ├── DriverSourceDeparted.hs
    │   │   │   │       │   │   │   │   ├── FavouriteDrivers.hs
    │   │   │   │       │   │   │   │   ├── FeedbackForm.hs
    │   │   │   │       │   │   │   │   ├── KnowYourDriver.hs
    │   │   │   │       │   │   │   │   ├── PopulateTipAmount.hs
    │   │   │   │       │   │   │   │   ├── ProdLoopStatus.hs
    │   │   │   │       │   │   │   │   ├── ReportACIssue.hs
    │   │   │   │       │   │   │   │   ├── ReportIssue.hs
    │   │   │   │       │   │   │   │   ├── Ride.hs
    │   │   │   │       │   │   │   │   ├── StopDetection.hs
    │   │   │   │       │   │   │   │   └── ViolationDetection.hs
    │   │   │   │       │   │   │   └── UI/
    │   │   │   │       │   │   │       ├── Cac.hs
    │   │   │   │       │   │   │       ├── Call.hs
    │   │   │   │       │   │   │       ├── CallEvent.hs
    │   │   │   │       │   │   │       ├── CallFeedback.hs
    │   │   │   │       │   │   │       ├── CallFeedbackFCM.hs
    │   │   │   │       │   │   │       ├── CallStatus.hs
    │   │   │   │       │   │   │       ├── CancellationReason.hs
    │   │   │   │       │   │   │       ├── City.hs
    │   │   │   │       │   │   │       ├── DemandHotspots.hs
    │   │   │   │       │   │   │       ├── Driver.hs
    │   │   │   │       │   │   │       ├── DriverCoin.hs
    │   │   │   │       │   │   │       ├── DriverGoHomeRequest.hs
    │   │   │   │       │   │   │       ├── DriverHomeLocation.hs
    │   │   │   │       │   │   │       ├── DriverOnboardingV2.hs
    │   │   │   │       │   │   │       ├── DriverProfileQuestions.hs
    │   │   │   │       │   │   │       ├── DriverProfileSummary.hs
    │   │   │   │       │   │   │       ├── DriverReferral.hs
    │   │   │   │       │   │   │       ├── EditBooking.hs
    │   │   │   │       │   │   │       ├── ExotelEndRide.hs
    │   │   │   │       │   │   │       ├── FareCalculator.hs
    │   │   │   │       │   │   │       ├── FleetDriverAssociation.hs
    │   │   │   │       │   │   │       ├── Insurance.hs
    │   │   │   │       │   │   │       ├── Invoice.hs
    │   │   │   │       │   │   │       ├── KioskLocation.hs
    │   │   │   │       │   │   │       ├── LeaderBoard.hs
    │   │   │   │       │   │   │       ├── LmsModule.hs
    │   │   │   │       │   │   │       ├── Location.hs
    │   │   │   │       │   │   │       ├── Maps.hs
    │   │   │   │       │   │   │       ├── Merchant.hs
    │   │   │   │       │   │   │       ├── MerchantServiceConfig.hs
    │   │   │   │       │   │   │       ├── Message.hs
    │   │   │   │       │   │   │       ├── MeterRide.hs
    │   │   │   │       │   │   │       ├── OnMessage.hs
    │   │   │   │       │   │   │       ├── OperationHub.hs
    │   │   │   │       │   │   │       ├── Operator.hs
    │   │   │   │       │   │   │       ├── OrgAdmin.hs
    │   │   │   │       │   │   │       ├── Payment.hs
    │   │   │   │       │   │   │       ├── Payout.hs
    │   │   │   │       │   │   │       ├── Performance.hs
    │   │   │   │       │   │   │       ├── Person.hs
    │   │   │   │       │   │   │       ├── PlaceNameCache.hs
    │   │   │   │       │   │   │       ├── Plan.hs
    │   │   │   │       │   │   │       ├── PriceBreakup.hs
    │   │   │   │       │   │   │       ├── Rating.hs
    │   │   │   │       │   │   │       ├── Reels.hs
    │   │   │   │       │   │   │       ├── ReferralPayout.hs
    │   │   │   │       │   │   │       ├── Registration.hs
    │   │   │   │       │   │   │       ├── Ride.hs
    │   │   │   │       │   │   │       ├── RideDetails.hs
    │   │   │   │       │   │   │       ├── RideRoute.hs
    │   │   │   │       │   │   │       ├── RideSummary.hs
    │   │   │   │       │   │   │       ├── Route.hs
    │   │   │   │       │   │   │       ├── SafetyWebhook.hs
    │   │   │   │       │   │   │       ├── SearchRequestForDriver.hs
    │   │   │   │       │   │   │       ├── SearchRequestLocation.hs
    │   │   │   │       │   │   │       ├── SocialLogin.hs
    │   │   │   │       │   │   │       ├── SpecialLocation.hs
    │   │   │   │       │   │   │       ├── SpecialLocationWarrior.hs
    │   │   │   │       │   │   │       ├── Tokenization.hs
    │   │   │   │       │   │   │       ├── Transporter.hs
    │   │   │   │       │   │   │       ├── VehicleDetails.hs
    │   │   │   │       │   │   │       ├── VehicleInfo.hs
    │   │   │   │       │   │   │       ├── Whatsapp.hs
    │   │   │   │       │   │   │       ├── WMB.hs
    │   │   │   │       │   │   │       ├── DriverOnboarding/
    │   │   │   │       │   │   │       │   ├── AadhaarVerification.hs
    │   │   │   │       │   │   │       │   ├── DriverLicense.hs
    │   │   │   │       │   │   │       │   ├── HyperVergeWebhook.hs
    │   │   │   │       │   │   │       │   ├── IdfyWebhook.hs
    │   │   │   │       │   │   │       │   ├── Image.hs
    │   │   │   │       │   │   │       │   ├── Referral.hs
    │   │   │   │       │   │   │       │   ├── Status.hs
    │   │   │   │       │   │   │       │   └── VehicleRegistrationCertificate.hs
    │   │   │   │       │   │   │       └── Ride/
    │   │   │   │       │   │   │           ├── CancelRide.hs
    │   │   │   │       │   │   │           ├── EndRide.hs
    │   │   │   │       │   │   │           ├── StartRide.hs
    │   │   │   │       │   │   │           ├── CancelRide/
    │   │   │   │       │   │   │           │   └── Internal.hs
    │   │   │   │       │   │   │           ├── EndRide/
    │   │   │   │       │   │   │           │   └── Internal.hs
    │   │   │   │       │   │   │           └── StartRide/
    │   │   │   │       │   │   │               └── Internal.hs
    │   │   │   │       │   │   └── Types/
    │   │   │   │       │   │       ├── Cac.hs
    │   │   │   │       │   │       ├── CacType.hs
    │   │   │   │       │   │       ├── City.hs
    │   │   │   │       │   │       ├── Common.hs
    │   │   │   │       │   │       ├── DeliveryDetails.hs
    │   │   │   │       │   │       ├── DeliveryPersonDetails.hs
    │   │   │   │       │   │       ├── DriverLocation.hs
    │   │   │   │       │   │       ├── FareParameters.hs
    │   │   │   │       │   │       ├── FarePolicy.hs
    │   │   │   │       │   │       ├── Geometry.hs
    │   │   │   │       │   │       ├── InterCityTravelCities.hs
    │   │   │   │       │   │       ├── LmsEnumTypes.hs
    │   │   │   │       │   │       ├── OnCancel.hs
    │   │   │   │       │   │       ├── OnUpdate.hs
    │   │   │   │       │   │       ├── Quote.hs
    │   │   │   │       │   │       ├── RideRoute.hs
    │   │   │   │       │   │       ├── UtilsTH.hs
    │   │   │   │       │   │       ├── Yudhishthira.hs
    │   │   │   │       │   │       ├── Beckn/
    │   │   │   │       │   │       │   └── Status.hs
    │   │   │   │       │   │       ├── Coins/
    │   │   │   │       │   │       │   ├── CoinHistory.hs
    │   │   │   │       │   │       │   └── CoinsConfig.hs
    │   │   │   │       │   │       ├── External/
    │   │   │   │       │   │       │   └── LiveEKD.hs
    │   │   │   │       │   │       ├── Extra/
    │   │   │   │       │   │       │   ├── ConditionalCharges.hs
    │   │   │   │       │   │       │   ├── DriverFee.hs
    │   │   │   │       │   │       │   ├── DriverHomeLocation.hs
    │   │   │   │       │   │       │   ├── DriverPlan.hs
    │   │   │   │       │   │       │   ├── Feedback.hs
    │   │   │   │       │   │       │   ├── Location.hs
    │   │   │   │       │   │       │   ├── MerchantMessage.hs
    │   │   │   │       │   │       │   ├── MerchantPaymentMethod.hs
    │   │   │   │       │   │       │   ├── MerchantServiceConfig.hs
    │   │   │   │       │   │       │   ├── MessageReport.hs
    │   │   │   │       │   │       │   ├── Notification.hs
    │   │   │   │       │   │       │   ├── Plan.hs
    │   │   │   │       │   │       │   └── TransporterConfig.hs
    │   │   │   │       │   │       ├── FarePolicy/
    │   │   │   │       │   │       │   ├── Common.hs
    │   │   │   │       │   │       │   ├── DriverExtraFeeBounds.hs
    │   │   │   │       │   │       │   ├── FarePolicyAmbulanceDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicyInterCityDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicyInterCityDetailsPricingSlabs.hs
    │   │   │   │       │   │       │   ├── FarePolicyProgressiveDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicyRentalDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicySlabsDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicyProgressiveDetails/
    │   │   │   │       │   │       │   │   ├── FarePolicyProgressiveDetailsPerExtraKmRateSection.hs
    │   │   │   │       │   │       │   │   └── FarePolicyProgressiveDetailsPerMinRateSection.hs
    │   │   │   │       │   │       │   ├── FarePolicyRentalDetails/
    │   │   │   │       │   │       │   │   ├── FarePolicyRentalDetailsDistanceBuffer.hs
    │   │   │   │       │   │       │   │   └── FarePolicyRentalDetailsPricingSlabs.hs
    │   │   │   │       │   │       │   └── FarePolicySlabsDetails/
    │   │   │   │       │   │       │       └── FarePolicySlabsDetailsSlab.hs
    │   │   │   │       │   │       └── Feedback/
    │   │   │   │       │   │           └── FeedbackForm.hs
    │   │   │   │       │   ├── Lib/
    │   │   │   │       │   │   ├── DriverScore.hs
    │   │   │   │       │   │   ├── LocationUpdates.hs
    │   │   │   │       │   │   ├── Schema.hs
    │   │   │   │       │   │   ├── DriverCoins/
    │   │   │   │       │   │   │   ├── Coins.hs
    │   │   │   │       │   │   │   └── Types.hs
    │   │   │   │       │   │   └── DriverScore/
    │   │   │   │       │   │       └── Types.hs
    │   │   │   │       │   ├── SharedLogic/
    │   │   │   │       │   │   ├── Allocator.hs
    │   │   │   │       │   │   ├── BlockedRouteDetector.hs
    │   │   │   │       │   │   ├── Booking.hs
    │   │   │   │       │   │   ├── Cac.hs
    │   │   │   │       │   │   ├── CallBAP.hs
    │   │   │   │       │   │   ├── CallBAPInternal.hs
    │   │   │   │       │   │   ├── CallIGMBAP.hs
    │   │   │   │       │   │   ├── Cancel.hs
    │   │   │   │       │   │   ├── DeleteDriver.hs
    │   │   │   │       │   │   ├── DriverFee.hs
    │   │   │   │       │   │   ├── DriverFleetOperatorAssociation.hs
    │   │   │   │       │   │   ├── DriverOnboarding.hs
    │   │   │   │       │   │   ├── DriverPool.hs
    │   │   │   │       │   │   ├── DynamicPricing.hs
    │   │   │   │       │   │   ├── EventTracking.hs
    │   │   │   │       │   │   ├── FareCalculator.hs
    │   │   │   │       │   │   ├── FarePolicy.hs
    │   │   │   │       │   │   ├── FareProduct.hs
    │   │   │   │       │   │   ├── Fleet.hs
    │   │   │   │       │   │   ├── GoogleMaps.hs
    │   │   │   │       │   │   ├── GoogleTranslate.hs
    │   │   │   │       │   │   ├── LocationMapping.hs
    │   │   │   │       │   │   ├── Merchant.hs
    │   │   │   │       │   │   ├── MerchantPaymentMethod.hs
    │   │   │   │       │   │   ├── MessageBuilder.hs
    │   │   │   │       │   │   ├── Payment.hs
    │   │   │   │       │   │   ├── Person.hs
    │   │   │   │       │   │   ├── Pricing.hs
    │   │   │   │       │   │   ├── Ride.hs
    │   │   │   │       │   │   ├── RiderDetails.hs
    │   │   │   │       │   │   ├── ScheduledNotifications.hs
    │   │   │   │       │   │   ├── SearchTry.hs
    │   │   │   │       │   │   ├── SearchTryLocker.hs
    │   │   │   │       │   │   ├── SyncRide.hs
    │   │   │   │       │   │   ├── TollsDetector.hs
    │   │   │   │       │   │   ├── VehicleServiceTier.hs
    │   │   │   │       │   │   ├── WMB.hs
    │   │   │   │       │   │   ├── Allocator/
    │   │   │   │       │   │   │   └── Jobs/
    │   │   │   │       │   │   │       ├── SendSearchRequestToDrivers.hs
    │   │   │   │       │   │   │       ├── Cautio/
    │   │   │   │       │   │   │       │   └── InstallationStatus.hs
    │   │   │   │       │   │   │       ├── CongestionCharge/
    │   │   │   │       │   │   │       │   └── CongestionChargeAvg.hs
    │   │   │   │       │   │   │       ├── Document/
    │   │   │   │       │   │   │       │   └── VerificationRetry.hs
    │   │   │   │       │   │   │       ├── DriverFeeUpdates/
    │   │   │   │       │   │   │       │   ├── BadDebtCalculationScheduler.hs
    │   │   │   │       │   │   │       │   └── DriverFee.hs
    │   │   │   │       │   │   │       ├── FCM/
    │   │   │   │       │   │   │       │   ├── RunScheduledFCMS.hs
    │   │   │   │       │   │   │       │   └── SoftBlockNotification.hs
    │   │   │   │       │   │   │       ├── FleetAlert/
    │   │   │   │       │   │   │       │   └── SendFleetAlert.hs
    │   │   │   │       │   │   │       ├── Mandate/
    │   │   │   │       │   │   │       │   ├── Execution.hs
    │   │   │   │       │   │   │       │   ├── Notification.hs
    │   │   │   │       │   │   │       │   └── OrderAndNotificationStatusUpdate.hs
    │   │   │   │       │   │   │       ├── Overlay/
    │   │   │   │       │   │   │       │   └── SendOverlay.hs
    │   │   │   │       │   │   │       ├── Payout/
    │   │   │   │       │   │   │       │   └── DriverReferralPayout.hs
    │   │   │   │       │   │   │       ├── ScheduledRides/
    │   │   │   │       │   │   │       │   ├── CheckExotelCallStatusAndNotifyBAP.hs
    │   │   │   │       │   │   │       │   ├── ScheduledRideAssignedOnUpdate.hs
    │   │   │   │       │   │   │       │   └── ScheduledRideNotificationsToDriver.hs
    │   │   │   │       │   │   │       ├── SendSearchRequestToDrivers/
    │   │   │   │       │   │   │       │   ├── Handle.hs
    │   │   │   │       │   │   │       │   └── Handle/
    │   │   │   │       │   │   │       │       ├── Internal.hs
    │   │   │   │       │   │   │       │       └── Internal/
    │   │   │   │       │   │   │       │           ├── DriverPool.hs
    │   │   │   │       │   │   │       │           ├── DriverPoolUnified.hs
    │   │   │   │       │   │   │       │           ├── SendSearchRequestToDrivers.hs
    │   │   │   │       │   │   │       │           └── DriverPool/
    │   │   │   │       │   │   │       │               └── Config.hs
    │   │   │   │       │   │   │       ├── SupplyDemand/
    │   │   │   │       │   │   │       │   └── SupplyDemandRatio.hs
    │   │   │   │       │   │   │       ├── UnblockDriverUpdate/
    │   │   │   │       │   │   │       │   └── UnblockDriver.hs
    │   │   │   │       │   │   │       └── Webhook/
    │   │   │   │       │   │   │           └── Webhook.hs
    │   │   │   │       │   │   ├── Beckn/
    │   │   │   │       │   │   │   └── Common.hs
    │   │   │   │       │   │   ├── BehaviourManagement/
    │   │   │   │       │   │   │   ├── CancellationRate.hs
    │   │   │   │       │   │   │   ├── IssueBreach.hs
    │   │   │   │       │   │   │   └── IssueBreachMitigation.hs
    │   │   │   │       │   │   ├── DriverOnboarding/
    │   │   │   │       │   │   │   └── Status.hs
    │   │   │   │       │   │   ├── DriverPool/
    │   │   │   │       │   │   │   ├── Config.hs
    │   │   │   │       │   │   │   └── Types.hs
    │   │   │   │       │   │   ├── External/
    │   │   │   │       │   │   │   └── LocationTrackingService/
    │   │   │   │       │   │   │       ├── Flow.hs
    │   │   │   │       │   │   │       ├── Types.hs
    │   │   │   │       │   │   │       └── API/
    │   │   │   │       │   │   │           ├── DriverBlockTill.hs
    │   │   │   │       │   │   │           ├── DriverLocation.hs
    │   │   │   │       │   │   │           ├── DriversLocation.hs
    │   │   │   │       │   │   │           ├── EndRide.hs
    │   │   │   │       │   │   │           ├── NearBy.hs
    │   │   │   │       │   │   │           ├── RideDetails.hs
    │   │   │   │       │   │   │           └── StartRide.hs
    │   │   │   │       │   │   └── KaalChakra/
    │   │   │   │       │   │       ├── Actions.hs
    │   │   │   │       │   │       └── Chakras.hs
    │   │   │   │       │   ├── Storage/
    │   │   │   │       │   │   ├── Beam/
    │   │   │   │       │   │   │   ├── BecknRequest.hs
    │   │   │   │       │   │   │   ├── Common.hs
    │   │   │   │       │   │   │   ├── FareParameters.hs
    │   │   │   │       │   │   │   ├── FarePolicy.hs
    │   │   │   │       │   │   │   ├── Geometry.hs
    │   │   │   │       │   │   │   ├── GovtDataRC.hs
    │   │   │   │       │   │   │   ├── InterCityTravelCities.hs
    │   │   │   │       │   │   │   ├── IssueManagement.hs
    │   │   │   │       │   │   │   ├── Payment.hs
    │   │   │   │       │   │   │   ├── Quote.hs
    │   │   │   │       │   │   │   ├── SchedulerJob.hs
    │   │   │   │       │   │   │   ├── SystemConfigs.hs
    │   │   │   │       │   │   │   ├── Webhook.hs
    │   │   │   │       │   │   │   ├── Yudhishthira.hs
    │   │   │   │       │   │   │   ├── Coins/
    │   │   │   │       │   │   │   │   ├── CoinHistory.hs
    │   │   │   │       │   │   │   │   └── CoinsConfig.hs
    │   │   │   │       │   │   │   ├── FareParameters/
    │   │   │   │       │   │   │   │   ├── FareParametersAmbulanceDetails.hs
    │   │   │   │       │   │   │   │   ├── FareParametersInterCityDetails.hs
    │   │   │   │       │   │   │   │   ├── FareParametersProgressiveDetails.hs
    │   │   │   │       │   │   │   │   ├── FareParametersRentalDetails.hs
    │   │   │   │       │   │   │   │   └── FareParametersSlabDetails.hs
    │   │   │   │       │   │   │   ├── FarePolicy/
    │   │   │   │       │   │   │   │   ├── DriverExtraFeeBounds.hs
    │   │   │   │       │   │   │   │   ├── FarePolicyAmbulanceDetailsSlab.hs
    │   │   │   │       │   │   │   │   ├── FarePolicyInterCityDetails.hs
    │   │   │   │       │   │   │   │   ├── FarePolicyInterCityDetailsPricingSlabs.hs
    │   │   │   │       │   │   │   │   ├── FarePolicyProgressiveDetails.hs
    │   │   │   │       │   │   │   │   ├── FarePolicyRentalDetails.hs
    │   │   │   │       │   │   │   │   ├── FarePolicyProgressiveDetails/
    │   │   │   │       │   │   │   │   │   └── FarePolicyProgressiveDetailsPerExtraKmRateSection.hs
    │   │   │   │       │   │   │   │   ├── FarePolicyRentalDetails/
    │   │   │   │       │   │   │   │   │   ├── FarePolicyRentalDetailsDistanceBuffers.hs
    │   │   │   │       │   │   │   │   │   └── FarePolicyRentalDetailsPricingSlabs.hs
    │   │   │   │       │   │   │   │   └── FarePolicySlabDetails/
    │   │   │   │       │   │   │   │       └── FarePolicySlabDetailsSlab.hs
    │   │   │   │       │   │   │   ├── Feedback/
    │   │   │   │       │   │   │   │   ├── FeedbackBadge.hs
    │   │   │   │       │   │   │   │   └── FeedbackForm.hs
    │   │   │   │       │   │   │   ├── Geometry/
    │   │   │   │       │   │   │   │   ├── Geometry.hs
    │   │   │   │       │   │   │   │   └── GeometryGeom.hs
    │   │   │   │       │   │   │   └── InterCityTravelCities/
    │   │   │   │       │   │   │       ├── InterCityTravelCities.hs
    │   │   │   │       │   │   │       └── InterCityTravelCitiesGeom.hs
    │   │   │   │       │   │   ├── Cac/
    │   │   │   │       │   │   │   ├── DriverIntelligentPoolConfig.hs
    │   │   │   │       │   │   │   ├── DriverPoolConfig.hs
    │   │   │   │       │   │   │   ├── FarePolicy.hs
    │   │   │   │       │   │   │   ├── GoHomeConfig.hs
    │   │   │   │       │   │   │   ├── MerchantServiceUsageConfig.hs
    │   │   │   │       │   │   │   ├── TransporterConfig.hs
    │   │   │   │       │   │   │   └── FarePolicy/
    │   │   │   │       │   │   │       ├── DriverExtraFeeBounds.hs
    │   │   │   │       │   │   │       ├── FarePolicyAmbulanceDetails.hs
    │   │   │   │       │   │   │       ├── FarePolicyInterCityDetails.hs
    │   │   │   │       │   │   │       ├── FarePolicyInterCityDetailsPricingSlabs.hs
    │   │   │   │       │   │   │       ├── FarePolicyProgressiveDetails.hs
    │   │   │   │       │   │   │       ├── FarePolicyRentalDetails.hs
    │   │   │   │       │   │   │       ├── FarePolicyProgressiveDetails/
    │   │   │   │       │   │   │       │   ├── FarePolicyProgressiveDetailsPerExtraKmRateSection.hs
    │   │   │   │       │   │   │       │   └── FarePolicyProgressiveDetailsPerMinRateSection.hs
    │   │   │   │       │   │   │       ├── FarePolicyRentalDetails/
    │   │   │   │       │   │   │       │   ├── FarePolicyRentalDetailsDistanceBuffers.hs
    │   │   │   │       │   │   │       │   └── FarePolicyRentalDetailsPricingSlabs.hs
    │   │   │   │       │   │   │       └── FarePolicySlabsDetails/
    │   │   │   │       │   │   │           └── FarePolicySlabsDetailsSlab.hs
    │   │   │   │       │   │   ├── CachedQueries/
    │   │   │   │       │   │   │   ├── BapMetadata.hs
    │   │   │   │       │   │   │   ├── BecknConfig.hs
    │   │   │   │       │   │   │   ├── BlackListOrg.hs
    │   │   │   │       │   │   │   ├── BlockedRoute.hs
    │   │   │   │       │   │   │   ├── CoinsConfig.hs
    │   │   │   │       │   │   │   ├── DocumentVerificationConfig.hs
    │   │   │   │       │   │   │   ├── DriverBlockReason.hs
    │   │   │   │       │   │   │   ├── DriverReferral.hs
    │   │   │   │       │   │   │   ├── Exophone.hs
    │   │   │   │       │   │   │   ├── FarePolicy.hs
    │   │   │   │       │   │   │   ├── FareProduct.hs
    │   │   │   │       │   │   │   ├── FleetOwnerDocumentVerificationConfig.hs
    │   │   │   │       │   │   │   ├── GoHomeConfig.hs
    │   │   │   │       │   │   │   ├── InterCityTravelCities.hs
    │   │   │   │       │   │   │   ├── KioskLocation.hs
    │   │   │   │       │   │   │   ├── KioskLocationTranslation.hs
    │   │   │   │       │   │   │   ├── Lms.hs
    │   │   │   │       │   │   │   ├── Merchant.hs
    │   │   │   │       │   │   │   ├── PlanExtra.hs
    │   │   │   │       │   │   │   ├── PlanTranslation.hs
    │   │   │   │       │   │   │   ├── ReelsData.hs
    │   │   │   │       │   │   │   ├── RegistryMapFallback.hs
    │   │   │   │       │   │   │   ├── RideRelatedNotificationConfig.hs
    │   │   │   │       │   │   │   ├── SubscriptionConfig.hs
    │   │   │   │       │   │   │   ├── Toll.hs
    │   │   │   │       │   │   │   ├── UiDriverConfig.hs
    │   │   │   │       │   │   │   ├── ValueAddNP.hs
    │   │   │   │       │   │   │   ├── VehicleDetails.hs
    │   │   │   │       │   │   │   ├── VehicleServiceTier.hs
    │   │   │   │       │   │   │   ├── WhiteListOrg.hs
    │   │   │   │       │   │   │   ├── Driver/
    │   │   │   │       │   │   │   │   ├── DriverImage.hs
    │   │   │   │       │   │   │   │   ├── GoHomeRequest.hs
    │   │   │   │       │   │   │   │   └── OnBoarding.hs
    │   │   │   │       │   │   │   ├── LLMPrompt/
    │   │   │   │       │   │   │   │   └── LLMPrompt.hs
    │   │   │   │       │   │   │   ├── Maps/
    │   │   │   │       │   │   │   │   └── PlaceNameCache.hs
    │   │   │   │       │   │   │   └── Merchant/
    │   │   │   │       │   │   │       ├── DriverIntelligentPoolConfig.hs
    │   │   │   │       │   │   │       ├── DriverPoolConfig.hs
    │   │   │   │       │   │   │       ├── LeaderBoardConfig.hs
    │   │   │   │       │   │   │       ├── MerchantMessage.hs
    │   │   │   │       │   │   │       ├── MerchantOperatingCity.hs
    │   │   │   │       │   │   │       ├── MerchantPaymentMethod.hs
    │   │   │   │       │   │   │       ├── MerchantPushNotification.hs
    │   │   │   │       │   │   │       ├── MerchantServiceConfig.hs
    │   │   │   │       │   │   │       ├── MerchantServiceUsageConfig.hs
    │   │   │   │       │   │   │       ├── MerchantState.hs
    │   │   │   │       │   │   │       ├── Overlay.hs
    │   │   │   │       │   │   │       ├── PayoutConfig.hs
    │   │   │   │       │   │   │       └── TransporterConfig.hs
    │   │   │   │       │   │   ├── Clickhouse/
    │   │   │   │       │   │   │   ├── Booking.hs
    │   │   │   │       │   │   │   ├── BppTransactionJoin.hs
    │   │   │   │       │   │   │   ├── DailyStats.hs
    │   │   │   │       │   │   │   ├── DriverEdaKafka.hs
    │   │   │   │       │   │   │   ├── DriverFee.hs
    │   │   │   │       │   │   │   ├── Estimate.hs
    │   │   │   │       │   │   │   ├── Location.hs
    │   │   │   │       │   │   │   ├── Ride.hs
    │   │   │   │       │   │   │   ├── RideDetails.hs
    │   │   │   │       │   │   │   └── SearchRequestForDriver.hs
    │   │   │   │       │   │   └── Queries/
    │   │   │   │       │   │       ├── AadhaarCardExtra.hs
    │   │   │   │       │   │       ├── BackgroundVerificationExtra.hs
    │   │   │   │       │   │       ├── BookingCancellationReasonExtra.hs
    │   │   │   │       │   │       ├── BookingExtra.hs
    │   │   │   │       │   │       ├── BusinessEventExtra.hs
    │   │   │   │       │   │       ├── CallStatusExtra.hs
    │   │   │   │       │   │       ├── DailyStatsExtra.hs
    │   │   │   │       │   │       ├── DocumentVerificationConfigExtra.hs
    │   │   │   │       │   │       ├── DriverBankAccountExtra.hs
    │   │   │   │       │   │       ├── DriverBlockReasonExtra.hs
    │   │   │   │       │   │       ├── DriverFeeExtra.hs
    │   │   │   │       │   │       ├── DriverGoHomeRequestExtra.hs
    │   │   │   │       │   │       ├── DriverGstinExtra.hs
    │   │   │   │       │   │       ├── DriverHomeLocationExtra.hs
    │   │   │   │       │   │       ├── DriverInformationExtra.hs
    │   │   │   │       │   │       ├── DriverLicenseExtra.hs
    │   │   │   │       │   │       ├── DriverOperatorAssociationExtra.hs
    │   │   │   │       │   │       ├── DriverPanCardExtra.hs
    │   │   │   │       │   │       ├── DriverPlanExtra.hs
    │   │   │   │       │   │       ├── DriverProfileQuestionsExtra.hs
    │   │   │   │       │   │       ├── DriverQuoteExtra.hs
    │   │   │   │       │   │       ├── DriverRCAssociationExtra.hs
    │   │   │   │       │   │       ├── DriverReferralExtra.hs
    │   │   │   │       │   │       ├── DriverSSNExtra.hs
    │   │   │   │       │   │       ├── DriverStatsExtra.hs
    │   │   │   │       │   │       ├── ExophoneExtra.hs
    │   │   │   │       │   │       ├── FareParameters.hs
    │   │   │   │       │   │       ├── FarePolicy.hs
    │   │   │   │       │   │       ├── FareProductExtra.hs
    │   │   │   │       │   │       ├── FeedbackExtra.hs
    │   │   │   │       │   │       ├── FleetBadgeAssociationExtra.hs
    │   │   │   │       │   │       ├── FleetBadgeExtra.hs
    │   │   │   │       │   │       ├── FleetDriverAssociationExtra.hs
    │   │   │   │       │   │       ├── FleetMemberAssociationExtra.hs
    │   │   │   │       │   │       ├── FleetOperatorAssociationExtra.hs
    │   │   │   │       │   │       ├── FleetOwnerInformationExtra.hs
    │   │   │   │       │   │       ├── FleetRCAssociationExtra.hs
    │   │   │   │       │   │       ├── Geometry.hs
    │   │   │   │       │   │       ├── ImageExtra.hs
    │   │   │   │       │   │       ├── InterCityTravelCities.hs
    │   │   │   │       │   │       ├── InvoiceExtra.hs
    │   │   │   │       │   │       ├── KioskLocationExtra.hs
    │   │   │   │       │   │       ├── LocationExtra.hs
    │   │   │   │       │   │       ├── LocationMappingExtra.hs
    │   │   │   │       │   │       ├── MandateExtra.hs
    │   │   │   │       │   │       ├── MerchantExtra.hs
    │   │   │   │       │   │       ├── MerchantPaymentMethodExtra.hs
    │   │   │   │       │   │       ├── MerchantServiceConfigExtra.hs
    │   │   │   │       │   │       ├── MessageExtra.hs
    │   │   │   │       │   │       ├── MessageReportExtra.hs
    │   │   │   │       │   │       ├── NotificationExtra.hs
    │   │   │   │       │   │       ├── OperationHubExtra.hs
    │   │   │   │       │   │       ├── OperationHubRequestsExtra.hs
    │   │   │   │       │   │       ├── PayoutConfigExtra.hs
    │   │   │   │       │   │       ├── PersonExtra.hs
    │   │   │   │       │   │       ├── PlanExtra.hs
    │   │   │   │       │   │       ├── PurchaseHistoryExtra.hs
    │   │   │   │       │   │       ├── Quote.hs
    │   │   │   │       │   │       ├── RatingExtra.hs
    │   │   │   │       │   │       ├── RegistrationTokenExtra.hs
    │   │   │   │       │   │       ├── RideExtra.hs
    │   │   │   │       │   │       ├── RiderDetailsExtra.hs
    │   │   │   │       │   │       ├── RouteExtra.hs
    │   │   │   │       │   │       ├── SearchRequestExtra.hs
    │   │   │   │       │   │       ├── SearchRequestForDriverExtra.hs
    │   │   │   │       │   │       ├── SearchTryExtra.hs
    │   │   │   │       │   │       ├── StationExtra.hs
    │   │   │   │       │   │       ├── Status.hs
    │   │   │   │       │   │       ├── TranslationsExtra.hs
    │   │   │   │       │   │       ├── TransporterConfigExtra.hs
    │   │   │   │       │   │       ├── TripAlertRequestExtra.hs
    │   │   │   │       │   │       ├── TripTransactionExtra.hs
    │   │   │   │       │   │       ├── UiDriverConfigExtra.hs
    │   │   │   │       │   │       ├── VehicleDetailsExtra.hs
    │   │   │   │       │   │       ├── VehicleExtra.hs
    │   │   │   │       │   │       ├── VehicleInsuranceExtra.hs
    │   │   │   │       │   │       ├── VehicleRegistrationCertificateExtra.hs
    │   │   │   │       │   │       ├── VehicleRouteMappingExtra.hs
    │   │   │   │       │   │       ├── VendorFeeExtra.hs
    │   │   │   │       │   │       ├── WhiteListOrgExtra.hs
    │   │   │   │       │   │       ├── Booking/
    │   │   │   │       │   │       │   └── Internal.hs
    │   │   │   │       │   │       ├── Coins/
    │   │   │   │       │   │       │   ├── CoinHistory.hs
    │   │   │   │       │   │       │   └── CoinsConfig.hs
    │   │   │   │       │   │       ├── Driver/
    │   │   │   │       │   │       │   └── GoHomeFeature/
    │   │   │   │       │   │       │       └── DriverGoHomeRequest/
    │   │   │   │       │   │       │           └── Internal.hs
    │   │   │   │       │   │       ├── DriverInformation/
    │   │   │   │       │   │       │   └── Internal.hs
    │   │   │   │       │   │       ├── DriverLocation/
    │   │   │   │       │   │       │   └── Internal.hs
    │   │   │   │       │   │       ├── DriverQuote/
    │   │   │   │       │   │       │   └── Internal.hs
    │   │   │   │       │   │       ├── FareParameters/
    │   │   │   │       │   │       │   ├── FareParametersAmbulanceDetails.hs
    │   │   │   │       │   │       │   ├── FareParametersInterCityDetails.hs
    │   │   │   │       │   │       │   ├── FareParametersProgressiveDetails.hs
    │   │   │   │       │   │       │   ├── FareParametersRentalDetails.hs
    │   │   │   │       │   │       │   └── FareParametersSlabDetails.hs
    │   │   │   │       │   │       ├── FarePolicy/
    │   │   │   │       │   │       │   ├── DriverExtraFeeBounds.hs
    │   │   │   │       │   │       │   ├── FarePolicyAmbulanceDetailsSlab.hs
    │   │   │   │       │   │       │   ├── FarePolicyInterCityDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicyInterCityDetailsPricingSlabs.hs
    │   │   │   │       │   │       │   ├── FarePolicyProgressiveDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicyRentalDetails.hs
    │   │   │   │       │   │       │   ├── FarePolicyProgressiveDetails/
    │   │   │   │       │   │       │   │   ├── FarePolicyProgressiveDetailsPerExtraKmRateSection.hs
    │   │   │   │       │   │       │   │   └── FarePolicyProgressiveDetailsPerMinRateSection.hs
    │   │   │   │       │   │       │   ├── FarePolicyRentalDetails/
    │   │   │   │       │   │       │   │   ├── FarePolicyRentalDetailsDistanceBuffers.hs
    │   │   │   │       │   │       │   │   └── FarePolicyRentalDetailsPricingSlabs.hs
    │   │   │   │       │   │       │   └── FarePolicySlabsDetails/
    │   │   │   │       │   │       │       └── FarePolicySlabsDetailsSlab.hs
    │   │   │   │       │   │       ├── Feedback/
    │   │   │   │       │   │       │   ├── FeedbackBadge.hs
    │   │   │   │       │   │       │   └── FeedbackForm.hs
    │   │   │   │       │   │       ├── Person/
    │   │   │   │       │   │       │   ├── GetNearestDrivers.hs
    │   │   │   │       │   │       │   ├── GetNearestDriversCurrentlyOnRide.hs
    │   │   │   │       │   │       │   ├── GetNearestGoHomeDrivers.hs
    │   │   │   │       │   │       │   └── Internal.hs
    │   │   │   │       │   │       ├── QueriesExtra/
    │   │   │   │       │   │       │   ├── BookingLite.hs
    │   │   │   │       │   │       │   └── RideLite.hs
    │   │   │   │       │   │       ├── Transformers/
    │   │   │   │       │   │       │   ├── Booking.hs
    │   │   │   │       │   │       │   ├── BookingLocation.hs
    │   │   │   │       │   │       │   ├── CancellationReason.hs
    │   │   │   │       │   │       │   ├── DailyStats.hs
    │   │   │   │       │   │       │   ├── DocumentVerificationConfig.hs
    │   │   │   │       │   │       │   ├── DriverFee.hs
    │   │   │   │       │   │       │   ├── DriverInformation.hs
    │   │   │   │       │   │       │   ├── DriverPlan.hs
    │   │   │   │       │   │       │   ├── DriverQuote.hs
    │   │   │   │       │   │       │   ├── DriverStats.hs
    │   │   │   │       │   │       │   ├── FleetOwnerInformation.hs
    │   │   │   │       │   │       │   ├── Invoice.hs
    │   │   │   │       │   │       │   ├── Location.hs
    │   │   │   │       │   │       │   ├── MerchantMessage.hs
    │   │   │   │       │   │       │   ├── MerchantPaymentMethod.hs
    │   │   │   │       │   │       │   ├── MerchantServiceConfig.hs
    │   │   │   │       │   │       │   ├── Message.hs
    │   │   │   │       │   │       │   ├── MessageReport.hs
    │   │   │   │       │   │       │   ├── Notification.hs
    │   │   │   │       │   │       │   ├── Person.hs
    │   │   │   │       │   │       │   ├── Plan.hs
    │   │   │   │       │   │       │   ├── QuestionInformation.hs
    │   │   │   │       │   │       │   ├── ReelsData.hs
    │   │   │   │       │   │       │   ├── RegistrationToken.hs
    │   │   │   │       │   │       │   ├── Ride.hs
    │   │   │   │       │   │       │   ├── SearchRequest.hs
    │   │   │   │       │   │       │   ├── SearchRequestForDriver.hs
    │   │   │   │       │   │       │   └── SearchTry.hs
    │   │   │   │       │   │       └── Vehicle/
    │   │   │   │       │   │           └── Internal.hs
    │   │   │   │       │   └── Tools/
    │   │   │   │       │       ├── AadhaarVerification.hs
    │   │   │   │       │       ├── Auth.hs
    │   │   │   │       │       ├── BackgroundVerification.hs
    │   │   │   │       │       ├── Call.hs
    │   │   │   │       │       ├── ChatCompletion.hs
    │   │   │   │       │       ├── ConfigPilot.hs
    │   │   │   │       │       ├── Constants.hs
    │   │   │   │       │       ├── Csv.hs
    │   │   │   │       │       ├── Dashcam.hs
    │   │   │   │       │       ├── DriverBackgroundVerification.hs
    │   │   │   │       │       ├── Encryption.hs
    │   │   │   │       │       ├── Error.hs
    │   │   │   │       │       ├── Event.hs
    │   │   │   │       │       ├── Maps.hs
    │   │   │   │       │       ├── MarketingEvents.hs
    │   │   │   │       │       ├── Metrics.hs
    │   │   │   │       │       ├── Notifications.hs
    │   │   │   │       │       ├── Payment.hs
    │   │   │   │       │       ├── PaymentNudge.hs
    │   │   │   │       │       ├── Payout.hs
    │   │   │   │       │       ├── SMS.hs
    │   │   │   │       │       ├── Ticket.hs
    │   │   │   │       │       ├── Utils.hs
    │   │   │   │       │       ├── Verification.hs
    │   │   │   │       │       ├── Whatsapp.hs
    │   │   │   │       │       ├── Beam/
    │   │   │   │       │       │   └── UtilsTH.hs
    │   │   │   │       │       └── Metrics/
    │   │   │   │       │           ├── ARDUBPPMetrics.hs
    │   │   │   │       │           ├── SendSearchRequestToDriverMetrics.hs
    │   │   │   │       │           ├── ARDUBPPMetrics/
    │   │   │   │       │           │   └── Types.hs
    │   │   │   │       │           └── SendSearchRequestToDriverMetrics/
    │   │   │   │       │               └── Types.hs
    │   │   │   │       └── src-read-only/
    │   │   │   │           ├── API/
    │   │   │   │           │   ├── Action/
    │   │   │   │           │   │   ├── Dashboard/
    │   │   │   │           │   │   │   ├── AppManagement.hs
    │   │   │   │           │   │   │   ├── Fleet.hs
    │   │   │   │           │   │   │   ├── IssueManagement.hs
    │   │   │   │           │   │   │   ├── Management.hs
    │   │   │   │           │   │   │   ├── Operator.hs
    │   │   │   │           │   │   │   ├── RideBooking.hs
    │   │   │   │           │   │   │   ├── AppManagement/
    │   │   │   │           │   │   │   │   ├── Driver.hs
    │   │   │   │           │   │   │   │   ├── DriverSubscription.hs
    │   │   │   │           │   │   │   │   ├── Overlay.hs
    │   │   │   │           │   │   │   │   └── Subscription.hs
    │   │   │   │           │   │   │   ├── Fleet/
    │   │   │   │           │   │   │   │   ├── Driver.hs
    │   │   │   │           │   │   │   │   ├── Onboarding.hs
    │   │   │   │           │   │   │   │   └── RegistrationV2.hs
    │   │   │   │           │   │   │   ├── IssueManagement/
    │   │   │   │           │   │   │   │   └── Issue.hs
    │   │   │   │           │   │   │   ├── Management/
    │   │   │   │           │   │   │   │   ├── Account.hs
    │   │   │   │           │   │   │   │   ├── Booking.hs
    │   │   │   │           │   │   │   │   ├── CoinsConfig.hs
    │   │   │   │           │   │   │   │   ├── Driver.hs
    │   │   │   │           │   │   │   │   ├── DriverCoins.hs
    │   │   │   │           │   │   │   │   ├── DriverGoHome.hs
    │   │   │   │           │   │   │   │   ├── DriverReferral.hs
    │   │   │   │           │   │   │   │   ├── DriverRegistration.hs
    │   │   │   │           │   │   │   │   ├── Media.hs
    │   │   │   │           │   │   │   │   ├── Merchant.hs
    │   │   │   │           │   │   │   │   ├── Message.hs
    │   │   │   │           │   │   │   │   ├── NammaTag.hs
    │   │   │   │           │   │   │   │   ├── Payout.hs
    │   │   │   │           │   │   │   │   ├── Revenue.hs
    │   │   │   │           │   │   │   │   ├── Ride.hs
    │   │   │   │           │   │   │   │   ├── System.hs
    │   │   │   │           │   │   │   │   └── VehicleInfo.hs
    │   │   │   │           │   │   │   ├── Operator/
    │   │   │   │           │   │   │   │   ├── Driver.hs
    │   │   │   │           │   │   │   │   ├── FleetManagement.hs
    │   │   │   │           │   │   │   │   └── Registration.hs
    │   │   │   │           │   │   │   └── RideBooking/
    │   │   │   │           │   │   │       ├── Driver.hs
    │   │   │   │           │   │   │       ├── DriverRegistration.hs
    │   │   │   │           │   │   │       ├── Maps.hs
    │   │   │   │           │   │   │       ├── MeterRide.hs
    │   │   │   │           │   │   │       ├── Ride.hs
    │   │   │   │           │   │   │       ├── SearchRequest.hs
    │   │   │   │           │   │   │       └── Volunteer.hs
    │   │   │   │           │   │   └── UI/
    │   │   │   │           │   │       ├── Cac.hs
    │   │   │   │           │   │       ├── CallFeedback.hs
    │   │   │   │           │   │       ├── DemandHotspots.hs
    │   │   │   │           │   │       ├── DriverOnboardingV2.hs
    │   │   │   │           │   │       ├── DriverProfileQuestions.hs
    │   │   │   │           │   │       ├── EditBooking.hs
    │   │   │   │           │   │       ├── FareCalculator.hs
    │   │   │   │           │   │       ├── Insurance.hs
    │   │   │   │           │   │       ├── Invoice.hs
    │   │   │   │           │   │       ├── LmsModule.hs
    │   │   │   │           │   │       ├── Merchant.hs
    │   │   │   │           │   │       ├── MeterRide.hs
    │   │   │   │           │   │       ├── OperationHub.hs
    │   │   │   │           │   │       ├── Operator.hs
    │   │   │   │           │   │       ├── PriceBreakup.hs
    │   │   │   │           │   │       ├── Reels.hs
    │   │   │   │           │   │       ├── ReferralPayout.hs
    │   │   │   │           │   │       ├── SocialLogin.hs
    │   │   │   │           │   │       ├── SpecialLocation.hs
    │   │   │   │           │   │       ├── SpecialLocationWarrior.hs
    │   │   │   │           │   │       ├── Tokenization.hs
    │   │   │   │           │   │       ├── VehicleDetails.hs
    │   │   │   │           │   │       ├── VehicleInfo.hs
    │   │   │   │           │   │       └── WMB.hs
    │   │   │   │           │   └── Types/
    │   │   │   │           │       ├── Dashboard/
    │   │   │   │           │       │   ├── AppManagement.hs
    │   │   │   │           │       │   ├── RideBooking.hs
    │   │   │   │           │       │   ├── AppManagement/
    │   │   │   │           │       │   │   ├── Driver.hs
    │   │   │   │           │       │   │   ├── DriverSubscription.hs
    │   │   │   │           │       │   │   ├── Overlay.hs
    │   │   │   │           │       │   │   ├── Subscription.hs
    │   │   │   │           │       │   │   └── Endpoints/
    │   │   │   │           │       │   │       ├── Driver.hs
    │   │   │   │           │       │   │       ├── DriverSubscription.hs
    │   │   │   │           │       │   │       ├── Overlay.hs
    │   │   │   │           │       │   │       └── Subscription.hs
    │   │   │   │           │       │   └── RideBooking/
    │   │   │   │           │       │       ├── Driver.hs
    │   │   │   │           │       │       ├── DriverRegistration.hs
    │   │   │   │           │       │       ├── Maps.hs
    │   │   │   │           │       │       ├── MeterRide.hs
    │   │   │   │           │       │       ├── Ride.hs
    │   │   │   │           │       │       ├── SearchRequest.hs
    │   │   │   │           │       │       ├── Volunteer.hs
    │   │   │   │           │       │       └── Endpoints/
    │   │   │   │           │       │           ├── Driver.hs
    │   │   │   │           │       │           ├── DriverRegistration.hs
    │   │   │   │           │       │           ├── Maps.hs
    │   │   │   │           │       │           ├── MeterRide.hs
    │   │   │   │           │       │           ├── Ride.hs
    │   │   │   │           │       │           ├── SearchRequest.hs
    │   │   │   │           │       │           └── Volunteer.hs
    │   │   │   │           │       └── UI/
    │   │   │   │           │           ├── CallFeedback.hs
    │   │   │   │           │           ├── DemandHotspots.hs
    │   │   │   │           │           ├── DriverOnboardingV2.hs
    │   │   │   │           │           ├── DriverProfileQuestions.hs
    │   │   │   │           │           ├── EditBooking.hs
    │   │   │   │           │           ├── FareCalculator.hs
    │   │   │   │           │           ├── Insurance.hs
    │   │   │   │           │           ├── Invoice.hs
    │   │   │   │           │           ├── LmsModule.hs
    │   │   │   │           │           ├── Merchant.hs
    │   │   │   │           │           ├── MeterRide.hs
    │   │   │   │           │           ├── OperationHub.hs
    │   │   │   │           │           ├── PriceBreakup.hs
    │   │   │   │           │           ├── Reels.hs
    │   │   │   │           │           ├── ReferralPayout.hs
    │   │   │   │           │           ├── SocialLogin.hs
    │   │   │   │           │           ├── SpecialLocationWarrior.hs
    │   │   │   │           │           ├── Tokenization.hs
    │   │   │   │           │           ├── VehicleDetails.hs
    │   │   │   │           │           ├── VehicleInfo.hs
    │   │   │   │           │           └── WMB.hs
    │   │   │   │           ├── Domain/
    │   │   │   │           │   └── Types/
    │   │   │   │           │       ├── AadhaarCard.hs
    │   │   │   │           │       ├── AadhaarOtpReq.hs
    │   │   │   │           │       ├── AadhaarOtpVerify.hs
    │   │   │   │           │       ├── AlertRequest.hs
    │   │   │   │           │       ├── BackgroundVerification.hs
    │   │   │   │           │       ├── BapMetadata.hs
    │   │   │   │           │       ├── BecknConfig.hs
    │   │   │   │           │       ├── BlackListOrg.hs
    │   │   │   │           │       ├── BlockedRoute.hs
    │   │   │   │           │       ├── Booking.hs
    │   │   │   │           │       ├── BookingCancellationReason.hs
    │   │   │   │           │       ├── BookingLocation.hs
    │   │   │   │           │       ├── BookingUpdateRequest.hs
    │   │   │   │           │       ├── BusinessEvent.hs
    │   │   │   │           │       ├── BusinessLicense.hs
    │   │   │   │           │       ├── CallFeedback.hs
    │   │   │   │           │       ├── CallFeedbackOptions.hs
    │   │   │   │           │       ├── CallStatus.hs
    │   │   │   │           │       ├── CancellationCharges.hs
    │   │   │   │           │       ├── CancellationFarePolicy.hs
    │   │   │   │           │       ├── CancellationReason.hs
    │   │   │   │           │       ├── Client.hs
    │   │   │   │           │       ├── ConditionalCharges.hs
    │   │   │   │           │       ├── DailyStats.hs
    │   │   │   │           │       ├── DocumentVerificationConfig.hs
    │   │   │   │           │       ├── DriverBankAccount.hs
    │   │   │   │           │       ├── DriverBlockReason.hs
    │   │   │   │           │       ├── DriverBlockTransactions.hs
    │   │   │   │           │       ├── DriverFee.hs
    │   │   │   │           │       ├── DriverGoHomeRequest.hs
    │   │   │   │           │       ├── DriverGstin.hs
    │   │   │   │           │       ├── DriverGullakAssociation.hs
    │   │   │   │           │       ├── DriverHomeLocation.hs
    │   │   │   │           │       ├── DriverInformation.hs
    │   │   │   │           │       ├── DriverIntelligentPoolConfig.hs
    │   │   │   │           │       ├── DriverLicense.hs
    │   │   │   │           │       ├── DriverModuleCompletion.hs
    │   │   │   │           │       ├── DriverOperatorAssociation.hs
    │   │   │   │           │       ├── DriverPanCard.hs
    │   │   │   │           │       ├── DriverPlan.hs
    │   │   │   │           │       ├── DriverPoolConfig.hs
    │   │   │   │           │       ├── DriverProfileQuestions.hs
    │   │   │   │           │       ├── DriverQuote.hs
    │   │   │   │           │       ├── DriverRCAssociation.hs
    │   │   │   │           │       ├── DriverReferral.hs
    │   │   │   │           │       ├── DriverSSN.hs
    │   │   │   │           │       ├── DriverStats.hs
    │   │   │   │           │       ├── Estimate.hs
    │   │   │   │           │       ├── Exophone.hs
    │   │   │   │           │       ├── FareProduct.hs
    │   │   │   │           │       ├── Feedback.hs
    │   │   │   │           │       ├── FleetBadge.hs
    │   │   │   │           │       ├── FleetBadgeAssociation.hs
    │   │   │   │           │       ├── FleetConfig.hs
    │   │   │   │           │       ├── FleetDriverAssociation.hs
    │   │   │   │           │       ├── FleetMemberAssociation.hs
    │   │   │   │           │       ├── FleetOperatorAssociation.hs
    │   │   │   │           │       ├── FleetOwnerDocumentVerificationConfig.hs
    │   │   │   │           │       ├── FleetOwnerInformation.hs
    │   │   │   │           │       ├── FleetRCAssociation.hs
    │   │   │   │           │       ├── FleetRouteAssociation.hs
    │   │   │   │           │       ├── FullFarePolicyProgressiveDetailsPerMinRateSection.hs
    │   │   │   │           │       ├── GoHomeConfig.hs
    │   │   │   │           │       ├── HyperVergeSdkLogs.hs
    │   │   │   │           │       ├── HyperVergeVerification.hs
    │   │   │   │           │       ├── IdfyVerification.hs
    │   │   │   │           │       ├── Image.hs
    │   │   │   │           │       ├── Invoice.hs
    │   │   │   │           │       ├── KioskLocation.hs
    │   │   │   │           │       ├── KioskLocationTranslation.hs
    │   │   │   │           │       ├── LeaderBoardConfigs.hs
    │   │   │   │           │       ├── LlmPrompt.hs
    │   │   │   │           │       ├── LmsCertificate.hs
    │   │   │   │           │       ├── LmsModule.hs
    │   │   │   │           │       ├── LmsModuleTranslation.hs
    │   │   │   │           │       ├── LmsModuleVideoInformation.hs
    │   │   │   │           │       ├── LmsVideoTranslation.hs
    │   │   │   │           │       ├── Location.hs
    │   │   │   │           │       ├── LocationMapping.hs
    │   │   │   │           │       ├── Mandate.hs
    │   │   │   │           │       ├── Merchant.hs
    │   │   │   │           │       ├── MerchantMessage.hs
    │   │   │   │           │       ├── MerchantOperatingCity.hs
    │   │   │   │           │       ├── MerchantPaymentMethod.hs
    │   │   │   │           │       ├── MerchantPushNotification.hs
    │   │   │   │           │       ├── MerchantServiceConfig.hs
    │   │   │   │           │       ├── MerchantServiceUsageConfig.hs
    │   │   │   │           │       ├── MerchantState.hs
    │   │   │   │           │       ├── Message.hs
    │   │   │   │           │       ├── MessageReport.hs
    │   │   │   │           │       ├── MessageTranslation.hs
    │   │   │   │           │       ├── MetaData.hs
    │   │   │   │           │       ├── ModuleCompletionInformation.hs
    │   │   │   │           │       ├── Notification.hs
    │   │   │   │           │       ├── OperationHub.hs
    │   │   │   │           │       ├── OperationHubRequests.hs
    │   │   │   │           │       ├── Overlay.hs
    │   │   │   │           │       ├── PayoutConfig.hs
    │   │   │   │           │       ├── Person.hs
    │   │   │   │           │       ├── PlaceNameCache.hs
    │   │   │   │           │       ├── Plan.hs
    │   │   │   │           │       ├── PlanTranslation.hs
    │   │   │   │           │       ├── PurchaseHistory.hs
    │   │   │   │           │       ├── QuestionInformation.hs
    │   │   │   │           │       ├── QuestionModuleMapping.hs
    │   │   │   │           │       ├── Rating.hs
    │   │   │   │           │       ├── RCValidationRules.hs
    │   │   │   │           │       ├── ReelsData.hs
    │   │   │   │           │       ├── RegistrationToken.hs
    │   │   │   │           │       ├── RegistryMapFallback.hs
    │   │   │   │           │       ├── Ride.hs
    │   │   │   │           │       ├── RideDetails.hs
    │   │   │   │           │       ├── RiderDetails.hs
    │   │   │   │           │       ├── RiderDriverCorrelation.hs
    │   │   │   │           │       ├── RideRelatedNotificationConfig.hs
    │   │   │   │           │       ├── Route.hs
    │   │   │   │           │       ├── RouteTripStopMapping.hs
    │   │   │   │           │       ├── SearchReqLocation.hs
    │   │   │   │           │       ├── SearchRequest.hs
    │   │   │   │           │       ├── SearchRequestForDriver.hs
    │   │   │   │           │       ├── SearchTry.hs
    │   │   │   │           │       ├── Station.hs
    │   │   │   │           │       ├── StopInformation.hs
    │   │   │   │           │       ├── SubscriptionConfig.hs
    │   │   │   │           │       ├── SurgePricing.hs
    │   │   │   │           │       ├── Toll.hs
    │   │   │   │           │       ├── Translations.hs
    │   │   │   │           │       ├── TransporterConfig.hs
    │   │   │   │           │       ├── TripAlertRequest.hs
    │   │   │   │           │       ├── TripTransaction.hs
    │   │   │   │           │       ├── UiDriverConfig.hs
    │   │   │   │           │       ├── ValueAddNP.hs
    │   │   │   │           │       ├── Vehicle.hs
    │   │   │   │           │       ├── VehicleDetails.hs
    │   │   │   │           │       ├── VehicleFitnessCertificate.hs
    │   │   │   │           │       ├── VehicleInfo.hs
    │   │   │   │           │       ├── VehicleInsurance.hs
    │   │   │   │           │       ├── VehicleNOC.hs
    │   │   │   │           │       ├── VehiclePermit.hs
    │   │   │   │           │       ├── VehiclePUC.hs
    │   │   │   │           │       ├── VehicleRegistrationCertificate.hs
    │   │   │   │           │       ├── VehicleRouteMapping.hs
    │   │   │   │           │       ├── VehicleServiceTier.hs
    │   │   │   │           │       ├── VendorFee.hs
    │   │   │   │           │       ├── VendorSplitDetails.hs
    │   │   │   │           │       ├── Volunteer.hs
    │   │   │   │           │       └── WhiteListOrg.hs
    │   │   │   │           └── Storage/
    │   │   │   │               ├── Beam/
    │   │   │   │               │   ├── AadhaarCard.hs
    │   │   │   │               │   ├── AadhaarOtpReq.hs
    │   │   │   │               │   ├── AadhaarOtpVerify.hs
    │   │   │   │               │   ├── AlertRequest.hs
    │   │   │   │               │   ├── BackgroundVerification.hs
    │   │   │   │               │   ├── BapMetadata.hs
    │   │   │   │               │   ├── BecknConfig.hs
    │   │   │   │               │   ├── BlackListOrg.hs
    │   │   │   │               │   ├── BlockedRoute.hs
    │   │   │   │               │   ├── Booking.hs
    │   │   │   │               │   ├── BookingCancellationReason.hs
    │   │   │   │               │   ├── BookingLocation.hs
    │   │   │   │               │   ├── BookingUpdateRequest.hs
    │   │   │   │               │   ├── BusinessEvent.hs
    │   │   │   │               │   ├── BusinessLicense.hs
    │   │   │   │               │   ├── CallFeedback.hs
    │   │   │   │               │   ├── CallFeedbackOptions.hs
    │   │   │   │               │   ├── CallStatus.hs
    │   │   │   │               │   ├── CancellationCharges.hs
    │   │   │   │               │   ├── CancellationFarePolicy.hs
    │   │   │   │               │   ├── CancellationReason.hs
    │   │   │   │               │   ├── Client.hs
    │   │   │   │               │   ├── ConditionalCharges.hs
    │   │   │   │               │   ├── DailyStats.hs
    │   │   │   │               │   ├── DocumentVerificationConfig.hs
    │   │   │   │               │   ├── DriverBankAccount.hs
    │   │   │   │               │   ├── DriverBlockReason.hs
    │   │   │   │               │   ├── DriverBlockTransactions.hs
    │   │   │   │               │   ├── DriverFee.hs
    │   │   │   │               │   ├── DriverGoHomeRequest.hs
    │   │   │   │               │   ├── DriverGstin.hs
    │   │   │   │               │   ├── DriverGullakAssociation.hs
    │   │   │   │               │   ├── DriverHomeLocation.hs
    │   │   │   │               │   ├── DriverInformation.hs
    │   │   │   │               │   ├── DriverIntelligentPoolConfig.hs
    │   │   │   │               │   ├── DriverLicense.hs
    │   │   │   │               │   ├── DriverModuleCompletion.hs
    │   │   │   │               │   ├── DriverOperatorAssociation.hs
    │   │   │   │               │   ├── DriverPanCard.hs
    │   │   │   │               │   ├── DriverPlan.hs
    │   │   │   │               │   ├── DriverPoolConfig.hs
    │   │   │   │               │   ├── DriverProfileQuestions.hs
    │   │   │   │               │   ├── DriverQuote.hs
    │   │   │   │               │   ├── DriverRCAssociation.hs
    │   │   │   │               │   ├── DriverReferral.hs
    │   │   │   │               │   ├── DriverSSN.hs
    │   │   │   │               │   ├── DriverStats.hs
    │   │   │   │               │   ├── Estimate.hs
    │   │   │   │               │   ├── Exophone.hs
    │   │   │   │               │   ├── FareProduct.hs
    │   │   │   │               │   ├── Feedback.hs
    │   │   │   │               │   ├── FleetBadge.hs
    │   │   │   │               │   ├── FleetBadgeAssociation.hs
    │   │   │   │               │   ├── FleetConfig.hs
    │   │   │   │               │   ├── FleetDriverAssociation.hs
    │   │   │   │               │   ├── FleetMemberAssociation.hs
    │   │   │   │               │   ├── FleetOperatorAssociation.hs
    │   │   │   │               │   ├── FleetOwnerDocumentVerificationConfig.hs
    │   │   │   │               │   ├── FleetOwnerInformation.hs
    │   │   │   │               │   ├── FleetRCAssociation.hs
    │   │   │   │               │   ├── FleetRouteAssociation.hs
    │   │   │   │               │   ├── FullFarePolicyProgressiveDetailsPerMinRateSection.hs
    │   │   │   │               │   ├── GoHomeConfig.hs
    │   │   │   │               │   ├── HyperVergeSdkLogs.hs
    │   │   │   │               │   ├── HyperVergeVerification.hs
    │   │   │   │               │   ├── IdfyVerification.hs
    │   │   │   │               │   ├── Image.hs
    │   │   │   │               │   ├── Invoice.hs
    │   │   │   │               │   ├── KioskLocation.hs
    │   │   │   │               │   ├── KioskLocationTranslation.hs
    │   │   │   │               │   ├── LeaderBoardConfigs.hs
    │   │   │   │               │   ├── LlmPrompt.hs
    │   │   │   │               │   ├── LmsCertificate.hs
    │   │   │   │               │   ├── LmsModule.hs
    │   │   │   │               │   ├── LmsModuleTranslation.hs
    │   │   │   │               │   ├── LmsModuleVideoInformation.hs
    │   │   │   │               │   ├── LmsVideoTranslation.hs
    │   │   │   │               │   ├── Location.hs
    │   │   │   │               │   ├── LocationMapping.hs
    │   │   │   │               │   ├── Mandate.hs
    │   │   │   │               │   ├── Merchant.hs
    │   │   │   │               │   ├── MerchantMessage.hs
    │   │   │   │               │   ├── MerchantOperatingCity.hs
    │   │   │   │               │   ├── MerchantPaymentMethod.hs
    │   │   │   │               │   ├── MerchantPushNotification.hs
    │   │   │   │               │   ├── MerchantServiceConfig.hs
    │   │   │   │               │   ├── MerchantServiceUsageConfig.hs
    │   │   │   │               │   ├── MerchantState.hs
    │   │   │   │               │   ├── Message.hs
    │   │   │   │               │   ├── MessageReport.hs
    │   │   │   │               │   ├── MessageTranslation.hs
    │   │   │   │               │   ├── MetaData.hs
    │   │   │   │               │   ├── ModuleCompletionInformation.hs
    │   │   │   │               │   ├── Notification.hs
    │   │   │   │               │   ├── OperationHub.hs
    │   │   │   │               │   ├── OperationHubRequests.hs
    │   │   │   │               │   ├── Overlay.hs
    │   │   │   │               │   ├── PayoutConfig.hs
    │   │   │   │               │   ├── Person.hs
    │   │   │   │               │   ├── PlaceNameCache.hs
    │   │   │   │               │   ├── Plan.hs
    │   │   │   │               │   ├── PlanTranslation.hs
    │   │   │   │               │   ├── PurchaseHistory.hs
    │   │   │   │               │   ├── QuestionInformation.hs
    │   │   │   │               │   ├── QuestionModuleMapping.hs
    │   │   │   │               │   ├── Rating.hs
    │   │   │   │               │   ├── RCValidationRules.hs
    │   │   │   │               │   ├── ReelsData.hs
    │   │   │   │               │   ├── RegistrationToken.hs
    │   │   │   │               │   ├── RegistryMapFallback.hs
    │   │   │   │               │   ├── Ride.hs
    │   │   │   │               │   ├── RideDetails.hs
    │   │   │   │               │   ├── RiderDetails.hs
    │   │   │   │               │   ├── RiderDriverCorrelation.hs
    │   │   │   │               │   ├── RideRelatedNotificationConfig.hs
    │   │   │   │               │   ├── Route.hs
    │   │   │   │               │   ├── RouteTripStopMapping.hs
    │   │   │   │               │   ├── SearchReqLocation.hs
    │   │   │   │               │   ├── SearchRequest.hs
    │   │   │   │               │   ├── SearchRequestForDriver.hs
    │   │   │   │               │   ├── SearchTry.hs
    │   │   │   │               │   ├── Station.hs
    │   │   │   │               │   ├── StopInformation.hs
    │   │   │   │               │   ├── SubscriptionConfig.hs
    │   │   │   │               │   ├── SurgePricing.hs
    │   │   │   │               │   ├── Toll.hs
    │   │   │   │               │   ├── Translations.hs
    │   │   │   │               │   ├── TransporterConfig.hs
    │   │   │   │               │   ├── TripAlertRequest.hs
    │   │   │   │               │   ├── TripTransaction.hs
    │   │   │   │               │   ├── UiDriverConfig.hs
    │   │   │   │               │   ├── ValueAddNP.hs
    │   │   │   │               │   ├── Vehicle.hs
    │   │   │   │               │   ├── VehicleDetails.hs
    │   │   │   │               │   ├── VehicleFitnessCertificate.hs
    │   │   │   │               │   ├── VehicleInfo.hs
    │   │   │   │               │   ├── VehicleInsurance.hs
    │   │   │   │               │   ├── VehicleNOC.hs
    │   │   │   │               │   ├── VehiclePermit.hs
    │   │   │   │               │   ├── VehiclePUC.hs
    │   │   │   │               │   ├── VehicleRegistrationCertificate.hs
    │   │   │   │               │   ├── VehicleRouteMapping.hs
    │   │   │   │               │   ├── VehicleServiceTier.hs
    │   │   │   │               │   ├── VendorFee.hs
    │   │   │   │               │   ├── VendorSplitDetails.hs
    │   │   │   │               │   ├── Volunteer.hs
    │   │   │   │               │   └── WhiteListOrg.hs
    │   │   │   │               ├── CachedQueries/
    │   │   │   │               │   ├── CancellationFarePolicy.hs
    │   │   │   │               │   ├── Plan.hs
    │   │   │   │               │   └── SurgePricing.hs
    │   │   │   │               └── Queries/
    │   │   │   │                   ├── AadhaarCard.hs
    │   │   │   │                   ├── AadhaarOtpReq.hs
    │   │   │   │                   ├── AadhaarOtpVerify.hs
    │   │   │   │                   ├── AlertRequest.hs
    │   │   │   │                   ├── BackgroundVerification.hs
    │   │   │   │                   ├── BapMetadata.hs
    │   │   │   │                   ├── BecknConfig.hs
    │   │   │   │                   ├── BlackListOrg.hs
    │   │   │   │                   ├── BlockedRoute.hs
    │   │   │   │                   ├── Booking.hs
    │   │   │   │                   ├── BookingCancellationReason.hs
    │   │   │   │                   ├── BookingLocation.hs
    │   │   │   │                   ├── BookingUpdateRequest.hs
    │   │   │   │                   ├── BusinessEvent.hs
    │   │   │   │                   ├── BusinessLicense.hs
    │   │   │   │                   ├── CallFeedback.hs
    │   │   │   │                   ├── CallFeedbackOptions.hs
    │   │   │   │                   ├── CallStatus.hs
    │   │   │   │                   ├── CancellationCharges.hs
    │   │   │   │                   ├── CancellationFarePolicy.hs
    │   │   │   │                   ├── CancellationReason.hs
    │   │   │   │                   ├── Client.hs
    │   │   │   │                   ├── ConditionalCharges.hs
    │   │   │   │                   ├── DailyStats.hs
    │   │   │   │                   ├── DocumentVerificationConfig.hs
    │   │   │   │                   ├── DriverBankAccount.hs
    │   │   │   │                   ├── DriverBlockReason.hs
    │   │   │   │                   ├── DriverBlockTransactions.hs
    │   │   │   │                   ├── DriverFee.hs
    │   │   │   │                   ├── DriverGoHomeRequest.hs
    │   │   │   │                   ├── DriverGstin.hs
    │   │   │   │                   ├── DriverGullakAssociation.hs
    │   │   │   │                   ├── DriverHomeLocation.hs
    │   │   │   │                   ├── DriverInformation.hs
    │   │   │   │                   ├── DriverIntelligentPoolConfig.hs
    │   │   │   │                   ├── DriverLicense.hs
    │   │   │   │                   ├── DriverModuleCompletion.hs
    │   │   │   │                   ├── DriverOperatorAssociation.hs
    │   │   │   │                   ├── DriverPanCard.hs
    │   │   │   │                   ├── DriverPlan.hs
    │   │   │   │                   ├── DriverPoolConfig.hs
    │   │   │   │                   ├── DriverProfileQuestions.hs
    │   │   │   │                   ├── DriverQuote.hs
    │   │   │   │                   ├── DriverRCAssociation.hs
    │   │   │   │                   ├── DriverReferral.hs
    │   │   │   │                   ├── DriverSSN.hs
    │   │   │   │                   ├── DriverStats.hs
    │   │   │   │                   ├── Estimate.hs
    │   │   │   │                   ├── Exophone.hs
    │   │   │   │                   ├── FareProduct.hs
    │   │   │   │                   ├── Feedback.hs
    │   │   │   │                   ├── FleetBadge.hs
    │   │   │   │                   ├── FleetBadgeAssociation.hs
    │   │   │   │                   ├── FleetConfig.hs
    │   │   │   │                   ├── FleetDriverAssociation.hs
    │   │   │   │                   ├── FleetMemberAssociation.hs
    │   │   │   │                   ├── FleetOperatorAssociation.hs
    │   │   │   │                   ├── FleetOwnerDocumentVerificationConfig.hs
    │   │   │   │                   ├── FleetOwnerInformation.hs
    │   │   │   │                   ├── FleetRCAssociation.hs
    │   │   │   │                   ├── FleetRouteAssociation.hs
    │   │   │   │                   ├── FullFarePolicyProgressiveDetailsPerMinRateSection.hs
    │   │   │   │                   ├── GoHomeConfig.hs
    │   │   │   │                   ├── HyperVergeSdkLogs.hs
    │   │   │   │                   ├── HyperVergeVerification.hs
    │   │   │   │                   ├── IdfyVerification.hs
    │   │   │   │                   ├── Image.hs
    │   │   │   │                   ├── Invoice.hs
    │   │   │   │                   ├── KioskLocation.hs
    │   │   │   │                   ├── KioskLocationTranslation.hs
    │   │   │   │                   ├── LeaderBoardConfigs.hs
    │   │   │   │                   ├── LlmPrompt.hs
    │   │   │   │                   ├── LmsCertificate.hs
    │   │   │   │                   ├── LmsModule.hs
    │   │   │   │                   ├── LmsModuleTranslation.hs
    │   │   │   │                   ├── LmsModuleVideoInformation.hs
    │   │   │   │                   ├── LmsVideoTranslation.hs
    │   │   │   │                   ├── Location.hs
    │   │   │   │                   ├── LocationMapping.hs
    │   │   │   │                   ├── Mandate.hs
    │   │   │   │                   ├── Merchant.hs
    │   │   │   │                   ├── MerchantMessage.hs
    │   │   │   │                   ├── MerchantOperatingCity.hs
    │   │   │   │                   ├── MerchantPaymentMethod.hs
    │   │   │   │                   ├── MerchantPushNotification.hs
    │   │   │   │                   ├── MerchantServiceConfig.hs
    │   │   │   │                   ├── MerchantServiceUsageConfig.hs
    │   │   │   │                   ├── MerchantState.hs
    │   │   │   │                   ├── Message.hs
    │   │   │   │                   ├── MessageReport.hs
    │   │   │   │                   ├── MessageTranslation.hs
    │   │   │   │                   ├── MetaData.hs
    │   │   │   │                   ├── ModuleCompletionInformation.hs
    │   │   │   │                   ├── Notification.hs
    │   │   │   │                   ├── OperationHub.hs
    │   │   │   │                   ├── OperationHubRequests.hs
    │   │   │   │                   ├── Overlay.hs
    │   │   │   │                   ├── PayoutConfig.hs
    │   │   │   │                   ├── Person.hs
    │   │   │   │                   ├── PlaceNameCache.hs
    │   │   │   │                   ├── Plan.hs
    │   │   │   │                   ├── PlanTranslation.hs
    │   │   │   │                   ├── PurchaseHistory.hs
    │   │   │   │                   ├── QuestionInformation.hs
    │   │   │   │                   ├── QuestionModuleMapping.hs
    │   │   │   │                   ├── Rating.hs
    │   │   │   │                   ├── RCValidationRules.hs
    │   │   │   │                   ├── ReelsData.hs
    │   │   │   │                   ├── RegistrationToken.hs
    │   │   │   │                   ├── RegistryMapFallback.hs
    │   │   │   │                   ├── Ride.hs
    │   │   │   │                   ├── RideDetails.hs
    │   │   │   │                   ├── RiderDetails.hs
    │   │   │   │                   ├── RiderDriverCorrelation.hs
    │   │   │   │                   ├── RideRelatedNotificationConfig.hs
    │   │   │   │                   ├── Route.hs
    │   │   │   │                   ├── RouteTripStopMapping.hs
    │   │   │   │                   ├── SearchReqLocation.hs
    │   │   │   │                   ├── SearchRequest.hs
    │   │   │   │                   ├── SearchRequestForDriver.hs
    │   │   │   │                   ├── SearchTry.hs
    │   │   │   │                   ├── Station.hs
    │   │   │   │                   ├── StopInformation.hs
    │   │   │   │                   ├── SubscriptionConfig.hs
    │   │   │   │                   ├── SurgePricing.hs
    │   │   │   │                   ├── Toll.hs
    │   │   │   │                   ├── Translations.hs
    │   │   │   │                   ├── TransporterConfig.hs
    │   │   │   │                   ├── TripAlertRequest.hs
    │   │   │   │                   ├── TripTransaction.hs
    │   │   │   │                   ├── UiDriverConfig.hs
    │   │   │   │                   ├── ValueAddNP.hs
    │   │   │   │                   ├── Vehicle.hs
    │   │   │   │                   ├── VehicleDetails.hs
    │   │   │   │                   ├── VehicleFitnessCertificate.hs
    │   │   │   │                   ├── VehicleInfo.hs
    │   │   │   │                   ├── VehicleInsurance.hs
    │   │   │   │                   ├── VehicleNOC.hs
    │   │   │   │                   ├── VehiclePermit.hs
    │   │   │   │                   ├── VehiclePUC.hs
    │   │   │   │                   ├── VehicleRegistrationCertificate.hs
    │   │   │   │                   ├── VehicleRouteMapping.hs
    │   │   │   │                   ├── VehicleServiceTier.hs
    │   │   │   │                   ├── VendorFee.hs
    │   │   │   │                   ├── VendorSplitDetails.hs
    │   │   │   │                   ├── Volunteer.hs
    │   │   │   │                   ├── WhiteListOrg.hs
    │   │   │   │                   └── OrphanInstances/
    │   │   │   │                       ├── AadhaarCard.hs
    │   │   │   │                       ├── AlertRequest.hs
    │   │   │   │                       ├── BackgroundVerification.hs
    │   │   │   │                       ├── Booking.hs
    │   │   │   │                       ├── BookingCancellationReason.hs
    │   │   │   │                       ├── BusinessEvent.hs
    │   │   │   │                       ├── CallStatus.hs
    │   │   │   │                       ├── CancellationReason.hs
    │   │   │   │                       ├── DailyStats.hs
    │   │   │   │                       ├── DocumentVerificationConfig.hs
    │   │   │   │                       ├── DriverBankAccount.hs
    │   │   │   │                       ├── DriverBlockReason.hs
    │   │   │   │                       ├── DriverFee.hs
    │   │   │   │                       ├── DriverGoHomeRequest.hs
    │   │   │   │                       ├── DriverGstin.hs
    │   │   │   │                       ├── DriverHomeLocation.hs
    │   │   │   │                       ├── DriverInformation.hs
    │   │   │   │                       ├── DriverLicense.hs
    │   │   │   │                       ├── DriverOperatorAssociation.hs
    │   │   │   │                       ├── DriverPanCard.hs
    │   │   │   │                       ├── DriverPlan.hs
    │   │   │   │                       ├── DriverProfileQuestions.hs
    │   │   │   │                       ├── DriverQuote.hs
    │   │   │   │                       ├── DriverRCAssociation.hs
    │   │   │   │                       ├── DriverReferral.hs
    │   │   │   │                       ├── DriverSSN.hs
    │   │   │   │                       ├── DriverStats.hs
    │   │   │   │                       ├── Exophone.hs
    │   │   │   │                       ├── FareProduct.hs
    │   │   │   │                       ├── Feedback.hs
    │   │   │   │                       ├── FleetBadge.hs
    │   │   │   │                       ├── FleetBadgeAssociation.hs
    │   │   │   │                       ├── FleetDriverAssociation.hs
    │   │   │   │                       ├── FleetMemberAssociation.hs
    │   │   │   │                       ├── FleetOperatorAssociation.hs
    │   │   │   │                       ├── FleetOwnerInformation.hs
    │   │   │   │                       ├── FleetRCAssociation.hs
    │   │   │   │                       ├── Image.hs
    │   │   │   │                       ├── Invoice.hs
    │   │   │   │                       ├── KioskLocation.hs
    │   │   │   │                       ├── Location.hs
    │   │   │   │                       ├── LocationMapping.hs
    │   │   │   │                       ├── Mandate.hs
    │   │   │   │                       ├── Merchant.hs
    │   │   │   │                       ├── MerchantPaymentMethod.hs
    │   │   │   │                       ├── MerchantServiceConfig.hs
    │   │   │   │                       ├── Message.hs
    │   │   │   │                       ├── MessageReport.hs
    │   │   │   │                       ├── Notification.hs
    │   │   │   │                       ├── OperationHub.hs
    │   │   │   │                       ├── OperationHubRequests.hs
    │   │   │   │                       ├── PayoutConfig.hs
    │   │   │   │                       ├── Person.hs
    │   │   │   │                       ├── Plan.hs
    │   │   │   │                       ├── PurchaseHistory.hs
    │   │   │   │                       ├── Rating.hs
    │   │   │   │                       ├── RegistrationToken.hs
    │   │   │   │                       ├── Ride.hs
    │   │   │   │                       ├── RiderDetails.hs
    │   │   │   │                       ├── RiderDriverCorrelation.hs
    │   │   │   │                       ├── Route.hs
    │   │   │   │                       ├── SearchRequest.hs
    │   │   │   │                       ├── SearchRequestForDriver.hs
    │   │   │   │                       ├── SearchTry.hs
    │   │   │   │                       ├── Station.hs
    │   │   │   │                       ├── Translations.hs
    │   │   │   │                       ├── TransporterConfig.hs
    │   │   │   │                       ├── TripAlertRequest.hs
    │   │   │   │                       ├── TripTransaction.hs
    │   │   │   │                       ├── UiDriverConfig.hs
    │   │   │   │                       ├── Vehicle.hs
    │   │   │   │                       ├── VehicleDetails.hs
    │   │   │   │                       ├── VehicleInsurance.hs
    │   │   │   │                       ├── VehicleRegistrationCertificate.hs
    │   │   │   │                       ├── VehicleRouteMapping.hs
    │   │   │   │                       ├── VendorFee.hs
    │   │   │   │                       ├── VendorSplitDetails.hs
    │   │   │   │                       └── WhiteListOrg.hs
    │   │   │   └── dynamic-offer-driver-drainer/
    │   │   │       ├── dynamic-offer-driver-drainer.cabal
    │   │   │       ├── package.yaml
    │   │   │       ├── server/
    │   │   │       │   └── Main.hs
    │   │   │       └── src/
    │   │   │           ├── Constants.hs
    │   │   │           ├── Config/
    │   │   │           │   └── Env.hs
    │   │   │           ├── DBQuery/
    │   │   │           │   ├── Functions.hs
    │   │   │           │   └── Types.hs
    │   │   │           ├── DBSync/
    │   │   │           │   ├── Create.hs
    │   │   │           │   ├── DBSync.hs
    │   │   │           │   ├── Delete.hs
    │   │   │           │   └── Update.hs
    │   │   │           ├── Event/
    │   │   │           │   └── Event.hs
    │   │   │           ├── Types/
    │   │   │           │   ├── Config.hs
    │   │   │           │   ├── DBSync.hs
    │   │   │           │   ├── Event.hs
    │   │   │           │   └── DBSync/
    │   │   │           │       ├── Create.hs
    │   │   │           │       ├── DBModel.hs
    │   │   │           │       ├── Delete.hs
    │   │   │           │       └── Update.hs
    │   │   │           └── Utils/
    │   │   │               ├── Config.hs
    │   │   │               ├── Event.hs
    │   │   │               ├── Parse.hs
    │   │   │               ├── Redis.hs
    │   │   │               └── Utils.hs
    │   │   ├── rider-platform/
    │   │   │   ├── public-transport-rider-platform/
    │   │   │   │   ├── Main/
    │   │   │   │   │   ├── README.md
    │   │   │   │   │   ├── package.yaml
    │   │   │   │   │   ├── public-transport-rider-platform.cabal
    │   │   │   │   │   ├── server/
    │   │   │   │   │   │   └── Main.hs
    │   │   │   │   │   └── src/
    │   │   │   │   │       ├── App.hs
    │   │   │   │   │       ├── Environment.hs
    │   │   │   │   │       ├── API/
    │   │   │   │   │       │   ├── Handler.hs
    │   │   │   │   │       │   ├── Types.hs
    │   │   │   │   │       │   ├── Beckn/
    │   │   │   │   │       │   │   ├── Handler.hs
    │   │   │   │   │       │   │   ├── Types.hs
    │   │   │   │   │       │   │   ├── OnCancel/
    │   │   │   │   │       │   │   │   └── Handler.hs
    │   │   │   │   │       │   │   ├── OnConfirm/
    │   │   │   │   │       │   │   │   └── Handler.hs
    │   │   │   │   │       │   │   ├── OnSearch/
    │   │   │   │   │       │   │   │   └── Handler.hs
    │   │   │   │   │       │   │   └── OnStatus/
    │   │   │   │   │       │   │       └── Handler.hs
    │   │   │   │   │       │   ├── Swagger/
    │   │   │   │   │       │   │   ├── Handler.hs
    │   │   │   │   │       │   │   └── Types.hs
    │   │   │   │   │       │   └── UI/
    │   │   │   │   │       │       ├── Handler.hs
    │   │   │   │   │       │       ├── Types.hs
    │   │   │   │   │       │       ├── Booking/
    │   │   │   │   │       │       │   ├── Handler.hs
    │   │   │   │   │       │       │   ├── Types.hs
    │   │   │   │   │       │       │   ├── BookingId/
    │   │   │   │   │       │       │   │   ├── Handler.hs
    │   │   │   │   │       │       │   │   ├── Types.hs
    │   │   │   │   │       │       │   │   └── TriggerStatus/
    │   │   │   │   │       │       │   │       ├── Handler.hs
    │   │   │   │   │       │       │   │       └── Types.hs
    │   │   │   │   │       │       │   └── BookingList/
    │   │   │   │   │       │       │       ├── Handler.hs
    │   │   │   │   │       │       │       └── Types.hs
    │   │   │   │   │       │       ├── QuoteConfirm/
    │   │   │   │   │       │       │   ├── Handler.hs
    │   │   │   │   │       │       │   └── Types.hs
    │   │   │   │   │       │       └── SearchId/
    │   │   │   │   │       │           └── Quotes/
    │   │   │   │   │       │               ├── Handler.hs
    │   │   │   │   │       │               └── Types.hs
    │   │   │   │   │       ├── Beckn/
    │   │   │   │   │       │   ├── Context.hs
    │   │   │   │   │       │   ├── ACL/
    │   │   │   │   │       │   │   ├── Confirm.hs
    │   │   │   │   │       │   │   ├── OnConfirm.hs
    │   │   │   │   │       │   │   ├── OnSearch.hs
    │   │   │   │   │       │   │   ├── OnStatus.hs
    │   │   │   │   │       │   │   ├── Status.hs
    │   │   │   │   │       │   │   └── Common/
    │   │   │   │   │       │   │       └── MakeStatus.hs
    │   │   │   │   │       │   └── Spec/
    │   │   │   │   │       │       ├── Common.hs
    │   │   │   │   │       │       ├── Confirm.hs
    │   │   │   │   │       │       ├── OnCancel.hs
    │   │   │   │   │       │       ├── OnConfirm.hs
    │   │   │   │   │       │       ├── OnSearch.hs
    │   │   │   │   │       │       ├── OnStatus.hs
    │   │   │   │   │       │       ├── Search.hs
    │   │   │   │   │       │       ├── Status.hs
    │   │   │   │   │       │       ├── API/
    │   │   │   │   │       │       │   ├── Confirm.hs
    │   │   │   │   │       │       │   ├── OnCancel.hs
    │   │   │   │   │       │       │   ├── OnConfirm.hs
    │   │   │   │   │       │       │   ├── OnSearch.hs
    │   │   │   │   │       │       │   ├── OnStatus.hs
    │   │   │   │   │       │       │   ├── Search.hs
    │   │   │   │   │       │       │   └── Status.hs
    │   │   │   │   │       │       ├── Common/
    │   │   │   │   │       │       │   ├── Billing.hs
    │   │   │   │   │       │       │   ├── Context.hs
    │   │   │   │   │       │       │   ├── DecimalValue.hs
    │   │   │   │   │       │       │   ├── Domain.hs
    │   │   │   │   │       │       │   ├── Duration.hs
    │   │   │   │   │       │       │   ├── Gps.hs
    │   │   │   │   │       │       │   ├── OrderState.hs
    │   │   │   │   │       │       │   ├── Payment.hs
    │   │   │   │   │       │       │   ├── Price.hs
    │   │   │   │   │       │       │   ├── ProviderId.hs
    │   │   │   │   │       │       │   └── Quotation.hs
    │   │   │   │   │       │       ├── Confirm/
    │   │   │   │   │       │       │   └── Item.hs
    │   │   │   │   │       │       ├── OnConfirm/
    │   │   │   │   │       │       │   ├── Descriptor.hs
    │   │   │   │   │       │       │   ├── Item.hs
    │   │   │   │   │       │       │   ├── Order.hs
    │   │   │   │   │       │       │   ├── Params.hs
    │   │   │   │   │       │       │   ├── Quantity.hs
    │   │   │   │   │       │       │   └── Time.hs
    │   │   │   │   │       │       ├── OnSearch/
    │   │   │   │   │       │       │   ├── Departure.hs
    │   │   │   │   │       │       │   ├── Descriptor.hs
    │   │   │   │   │       │       │   ├── Fare.hs
    │   │   │   │   │       │       │   ├── Image.hs
    │   │   │   │   │       │       │   ├── Item.hs
    │   │   │   │   │       │       │   ├── LocationDetails.hs
    │   │   │   │   │       │       │   ├── Provider.hs
    │   │   │   │   │       │       │   └── Route.hs
    │   │   │   │   │       │       ├── OnStatus/
    │   │   │   │   │       │       │   ├── Descriptor.hs
    │   │   │   │   │       │       │   ├── Item.hs
    │   │   │   │   │       │       │   ├── Order.hs
    │   │   │   │   │       │       │   ├── Params.hs
    │   │   │   │   │       │       │   └── Time.hs
    │   │   │   │   │       │       └── Search/
    │   │   │   │   │       │           ├── Fulfillment.hs
    │   │   │   │   │       │           └── LocationGps.hs
    │   │   │   │   │       ├── Domain/
    │   │   │   │   │       │   ├── Action/
    │   │   │   │   │       │   │   ├── Beckn/
    │   │   │   │   │       │   │   │   ├── OnConfirm.hs
    │   │   │   │   │       │   │   │   ├── OnSearch.hs
    │   │   │   │   │       │   │   │   └── OnStatus.hs
    │   │   │   │   │       │   │   └── UI/
    │   │   │   │   │       │   │       ├── BookingList.hs
    │   │   │   │   │       │   │       ├── QuoteConfirm.hs
    │   │   │   │   │       │   │       ├── Quotes.hs
    │   │   │   │   │       │   │       ├── Status.hs
    │   │   │   │   │       │   │       └── TriggerStatus.hs
    │   │   │   │   │       │   └── Types/
    │   │   │   │   │       │       ├── Booking.hs
    │   │   │   │   │       │       ├── PaymentTransaction.hs
    │   │   │   │   │       │       ├── Quote.hs
    │   │   │   │   │       │       ├── Search.hs
    │   │   │   │   │       │       ├── TransportStation.hs
    │   │   │   │   │       │       └── Booking/
    │   │   │   │   │       │           ├── API.hs
    │   │   │   │   │       │           └── Type.hs
    │   │   │   │   │       ├── ExternalAPI/
    │   │   │   │   │       │   └── Flow.hs
    │   │   │   │   │       ├── Storage/
    │   │   │   │   │       │   ├── Beam/
    │   │   │   │   │       │   │   └── BecknRequest.hs
    │   │   │   │   │       │   ├── Queries/
    │   │   │   │   │       │   │   ├── Booking.hs
    │   │   │   │   │       │   │   ├── PaymentTransaction.hs
    │   │   │   │   │       │   │   ├── Quote.hs
    │   │   │   │   │       │   │   ├── Search.hs
    │   │   │   │   │       │   │   └── TransportStation.hs
    │   │   │   │   │       │   └── Tabular/
    │   │   │   │   │       │       ├── Booking.hs
    │   │   │   │   │       │       ├── PaymentTransaction.hs
    │   │   │   │   │       │       ├── Quote.hs
    │   │   │   │   │       │       ├── Search.hs
    │   │   │   │   │       │       └── TransportStation.hs
    │   │   │   │   │       └── Tools/
    │   │   │   │   │           ├── Auth.hs
    │   │   │   │   │           ├── Error.hs
    │   │   │   │   │           ├── Metrics.hs
    │   │   │   │   │           ├── Beam/
    │   │   │   │   │           │   └── UtilsTH.hs
    │   │   │   │   │           ├── Metrics/
    │   │   │   │   │           │   └── Types.hs
    │   │   │   │   │           └── Streaming/
    │   │   │   │   │               └── Kafka/
    │   │   │   │   │                   ├── Environment.hs
    │   │   │   │   │                   └── Topic/
    │   │   │   │   │                       └── BusinessEvent.hs
    │   │   │   │   └── search-consumer/
    │   │   │   │       ├── README.md
    │   │   │   │       ├── package.yaml
    │   │   │   │       ├── public-transport-search-consumer.cabal
    │   │   │   │       ├── server/
    │   │   │   │       │   └── Main.hs
    │   │   │   │       └── src/
    │   │   │   │           ├── App.hs
    │   │   │   │           ├── Environment.hs
    │   │   │   │           ├── Beckn/
    │   │   │   │           │   └── ACL/
    │   │   │   │           │       └── Search.hs
    │   │   │   │           ├── Domain/
    │   │   │   │           │   └── Action/
    │   │   │   │           │       └── Search.hs
    │   │   │   │           ├── ExternalAPI/
    │   │   │   │           │   └── Flow.hs
    │   │   │   │           ├── Service/
    │   │   │   │           │   └── Runner.hs
    │   │   │   │           └── Tools/
    │   │   │   │               ├── Metrics.hs
    │   │   │   │               └── Streaming/
    │   │   │   │                   └── Kafka.hs
    │   │   │   ├── rider-app/
    │   │   │   │   ├── Main/
    │   │   │   │   │   ├── package.yaml
    │   │   │   │   │   ├── rider-app.cabal
    │   │   │   │   │   ├── server/
    │   │   │   │   │   │   └── Main.hs
    │   │   │   │   │   ├── spec/
    │   │   │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   │   │   ├── API/
    │   │   │   │   │   │   │   ├── AttractionRecommend.yaml
    │   │   │   │   │   │   │   ├── BBPS.yaml
    │   │   │   │   │   │   │   ├── Cac.yaml
    │   │   │   │   │   │   │   ├── CRIS.yaml
    │   │   │   │   │   │   │   ├── CustomerReferral.yaml
    │   │   │   │   │   │   │   ├── DeletedPerson.yaml
    │   │   │   │   │   │   │   ├── EditLocation.yaml
    │   │   │   │   │   │   │   ├── estimateBP.yaml
    │   │   │   │   │   │   │   ├── Favourites.yaml
    │   │   │   │   │   │   │   ├── followRide.yaml
    │   │   │   │   │   │   │   ├── FrfsTicket.yaml
    │   │   │   │   │   │   │   ├── Insurance.yaml
    │   │   │   │   │   │   │   ├── InsuranceInternal.yaml
    │   │   │   │   │   │   │   ├── invoice.yaml
    │   │   │   │   │   │   │   ├── MeterRideInternal.yaml
    │   │   │   │   │   │   │   ├── miscellaneous.yaml
    │   │   │   │   │   │   │   ├── MultiModal.yaml
    │   │   │   │   │   │   │   ├── Nearby.yaml
    │   │   │   │   │   │   │   ├── NearbyBuses.yaml
    │   │   │   │   │   │   │   ├── Payment.yaml
    │   │   │   │   │   │   │   ├── Places.yaml
    │   │   │   │   │   │   │   ├── PriceBreakup.yaml
    │   │   │   │   │   │   │   ├── SocialLogin.yaml
    │   │   │   │   │   │   │   ├── sos.yaml
    │   │   │   │   │   │   │   ├── ticket.yaml
    │   │   │   │   │   │   │   ├── TicketKapture.yaml
    │   │   │   │   │   │   │   ├── TrackRoute.yaml
    │   │   │   │   │   │   │   └── TriggerFCM.yaml
    │   │   │   │   │   │   └── Storage/
    │   │   │   │   │   │       ├── Aadhaar.yaml
    │   │   │   │   │   │       ├── AppInstalls.yaml
    │   │   │   │   │   │       ├── BBPS.yaml
    │   │   │   │   │   │       ├── BecknConfig.yaml
    │   │   │   │   │   │       ├── BlackListOrg.yaml
    │   │   │   │   │   │       ├── Booking.yaml
    │   │   │   │   │   │       ├── BookingCancellationReason.yaml
    │   │   │   │   │   │       ├── BookingUpdateRequest.yaml
    │   │   │   │   │   │       ├── BppDetails.yaml
    │   │   │   │   │   │       ├── CallBackRequest.yaml
    │   │   │   │   │   │       ├── CallStatus.yaml
    │   │   │   │   │   │       ├── CancellationReason.yaml
    │   │   │   │   │   │       ├── client.yaml
    │   │   │   │   │   │       ├── ClientPersonInfo.yaml
    │   │   │   │   │   │       ├── configs.yaml
    │   │   │   │   │   │       ├── DeletedPerson.yaml
    │   │   │   │   │   │       ├── DriverOffer.yaml
    │   │   │   │   │   │       ├── estimate.yaml
    │   │   │   │   │   │       ├── exophone.yaml
    │   │   │   │   │   │       ├── FareBreakUp.yaml
    │   │   │   │   │   │       ├── FeedbackForm.yaml
    │   │   │   │   │   │       ├── FrfsTicket.yaml
    │   │   │   │   │   │       ├── HotSpotConfig.yaml
    │   │   │   │   │   │       ├── Insurance.yaml
    │   │   │   │   │   │       ├── InterCityDetails.yaml
    │   │   │   │   │   │       ├── issue.yaml
    │   │   │   │   │   │       ├── Location.yaml
    │   │   │   │   │   │       ├── LocationMapping.yaml
    │   │   │   │   │   │       ├── Maps.yaml
    │   │   │   │   │   │       ├── Merchant.yaml
    │   │   │   │   │   │       ├── MerchantConfig.yaml
    │   │   │   │   │   │       ├── MerchantOnboarding.yaml
    │   │   │   │   │   │       ├── MultiModal.yaml
    │   │   │   │   │   │       ├── NotificationSoundConfig.yaml
    │   │   │   │   │   │       ├── OnSearchEvent.yaml
    │   │   │   │   │   │       ├── ParcelDetails.yaml
    │   │   │   │   │   │       ├── PartnerOrganization.yaml
    │   │   │   │   │   │       ├── PaymentManagement.yaml
    │   │   │   │   │   │       ├── Person.yaml
    │   │   │   │   │   │       ├── PersonDefaultEmergencyNumber.yaml
    │   │   │   │   │   │       ├── PersonDisability.yaml
    │   │   │   │   │   │       ├── PersonStats.yaml
    │   │   │   │   │   │       ├── PlaceBasedServiceConfig.yaml
    │   │   │   │   │   │       ├── PopularLocation.yaml
    │   │   │   │   │   │       ├── quote.yaml
    │   │   │   │   │   │       ├── quoteBreakup.yaml
    │   │   │   │   │   │       ├── rating.yaml
    │   │   │   │   │   │       ├── RecentLocation.yaml
    │   │   │   │   │   │       ├── RegistrationToken.yaml
    │   │   │   │   │   │       ├── RentalDetails.yaml
    │   │   │   │   │   │       ├── ride.yaml
    │   │   │   │   │   │       ├── RiderConfig.yaml
    │   │   │   │   │   │       ├── RoutePolylines.yaml
    │   │   │   │   │   │       ├── SafetySettings.yaml
    │   │   │   │   │   │       ├── SavedReqLocation.yaml
    │   │   │   │   │   │       ├── SearchReqLocation.yaml
    │   │   │   │   │   │       ├── SearchRequest.yaml
    │   │   │   │   │   │       ├── sos.yaml
    │   │   │   │   │   │       ├── SpecialZoneQuote.yaml
    │   │   │   │   │   │       ├── StopInformation.yaml
    │   │   │   │   │   │       ├── ticket.yaml
    │   │   │   │   │   │       ├── TripTerms.yaml
    │   │   │   │   │   │       ├── UiRiderConfig.yaml
    │   │   │   │   │   │       ├── VehicleConfig.yaml
    │   │   │   │   │   │       ├── VehicleRouteMapping.yaml
    │   │   │   │   │   │       ├── VendorDetail.yaml
    │   │   │   │   │   │       └── WhiteListOrg.yaml
    │   │   │   │   │   ├── src/
    │   │   │   │   │   │   ├── API.hs
    │   │   │   │   │   │   ├── App.hs
    │   │   │   │   │   │   ├── Environment.hs
    │   │   │   │   │   │   ├── API/
    │   │   │   │   │   │   │   ├── Beckn.hs
    │   │   │   │   │   │   │   ├── Dashboard.hs
    │   │   │   │   │   │   │   ├── FRFS.hs
    │   │   │   │   │   │   │   ├── IGM.hs
    │   │   │   │   │   │   │   ├── Internal.hs
    │   │   │   │   │   │   │   ├── UI.hs
    │   │   │   │   │   │   │   ├── Beckn/
    │   │   │   │   │   │   │   │   ├── OnCancel.hs
    │   │   │   │   │   │   │   │   ├── OnConfirm.hs
    │   │   │   │   │   │   │   │   ├── OnInit.hs
    │   │   │   │   │   │   │   │   ├── OnSearch.hs
    │   │   │   │   │   │   │   │   ├── OnSelect.hs
    │   │   │   │   │   │   │   │   ├── OnStatus.hs
    │   │   │   │   │   │   │   │   ├── OnTrack.hs
    │   │   │   │   │   │   │   │   ├── OnUpdate.hs
    │   │   │   │   │   │   │   │   ├── FRFS/
    │   │   │   │   │   │   │   │   │   ├── OnCancel.hs
    │   │   │   │   │   │   │   │   │   ├── OnConfirm.hs
    │   │   │   │   │   │   │   │   │   ├── OnInit.hs
    │   │   │   │   │   │   │   │   │   ├── OnSearch.hs
    │   │   │   │   │   │   │   │   │   ├── OnSelect.hs
    │   │   │   │   │   │   │   │   │   ├── OnStatus.hs
    │   │   │   │   │   │   │   │   │   └── OnUpdate.hs
    │   │   │   │   │   │   │   │   └── IGM/
    │   │   │   │   │   │   │   │       └── Issue.hs
    │   │   │   │   │   │   │   ├── Dashboard/
    │   │   │   │   │   │   │   │   └── Exotel.hs
    │   │   │   │   │   │   │   ├── Internal/
    │   │   │   │   │   │   │   │   ├── Cac.hs
    │   │   │   │   │   │   │   │   ├── DriverArrivalNotf.hs
    │   │   │   │   │   │   │   │   ├── FrequentLocUser.hs
    │   │   │   │   │   │   │   │   ├── FRFS.hs
    │   │   │   │   │   │   │   │   ├── Rating.hs
    │   │   │   │   │   │   │   │   ├── StopEvents.hs
    │   │   │   │   │   │   │   │   └── ViolationDetection.hs
    │   │   │   │   │   │   │   ├── Types/
    │   │   │   │   │   │   │   │   └── Dashboard/
    │   │   │   │   │   │   │   │       └── AppManagement/
    │   │   │   │   │   │   │   │           └── OrphanInstances/
    │   │   │   │   │   │   │   │               ├── MerchantOnboarding.hs
    │   │   │   │   │   │   │   │               └── TicketDashboard.hs
    │   │   │   │   │   │   │   └── UI/
    │   │   │   │   │   │   │       ├── AadhaarVerification.hs
    │   │   │   │   │   │   │       ├── AppInstalls.hs
    │   │   │   │   │   │   │       ├── Booking.hs
    │   │   │   │   │   │   │       ├── Call.hs
    │   │   │   │   │   │   │       ├── CallEvent.hs
    │   │   │   │   │   │   │       ├── Cancel.hs
    │   │   │   │   │   │   │       ├── CancellationReason.hs
    │   │   │   │   │   │   │       ├── Confirm.hs
    │   │   │   │   │   │   │       ├── Disability.hs
    │   │   │   │   │   │   │       ├── FeedbackForm.hs
    │   │   │   │   │   │   │       ├── Frontend.hs
    │   │   │   │   │   │   │       ├── GoogleTranslate.hs
    │   │   │   │   │   │   │       ├── HotSpot.hs
    │   │   │   │   │   │   │       ├── Issue.hs
    │   │   │   │   │   │   │       ├── Maps.hs
    │   │   │   │   │   │   │       ├── PartnerOrganizationFRFS.hs
    │   │   │   │   │   │   │       ├── Payment.hs
    │   │   │   │   │   │   │       ├── PersonStats.hs
    │   │   │   │   │   │   │       ├── Profile.hs
    │   │   │   │   │   │   │       ├── Quote.hs
    │   │   │   │   │   │   │       ├── Rating.hs
    │   │   │   │   │   │   │       ├── Registration.hs
    │   │   │   │   │   │   │       ├── RentalsIntercityCache.hs
    │   │   │   │   │   │   │       ├── Ride.hs
    │   │   │   │   │   │   │       ├── Route.hs
    │   │   │   │   │   │   │       ├── SavedReqLocation.hs
    │   │   │   │   │   │   │       ├── Search.hs
    │   │   │   │   │   │   │       ├── Select.hs
    │   │   │   │   │   │   │       ├── Serviceability.hs
    │   │   │   │   │   │   │       ├── Sos.hs
    │   │   │   │   │   │   │       ├── Support.hs
    │   │   │   │   │   │   │       └── Whatsapp.hs
    │   │   │   │   │   │   ├── App/
    │   │   │   │   │   │   │   └── Server.hs
    │   │   │   │   │   │   ├── Beckn/
    │   │   │   │   │   │   │   ├── Core.hs
    │   │   │   │   │   │   │   ├── ACL/
    │   │   │   │   │   │   │   │   ├── Cancel.hs
    │   │   │   │   │   │   │   │   ├── Common.hs
    │   │   │   │   │   │   │   │   ├── Confirm.hs
    │   │   │   │   │   │   │   │   ├── Init.hs
    │   │   │   │   │   │   │   │   ├── OnCancel.hs
    │   │   │   │   │   │   │   │   ├── OnConfirm.hs
    │   │   │   │   │   │   │   │   ├── OnInit.hs
    │   │   │   │   │   │   │   │   ├── OnSearch.hs
    │   │   │   │   │   │   │   │   ├── OnSelect.hs
    │   │   │   │   │   │   │   │   ├── OnStatus.hs
    │   │   │   │   │   │   │   │   ├── OnTrack.hs
    │   │   │   │   │   │   │   │   ├── OnUpdate.hs
    │   │   │   │   │   │   │   │   ├── Rating.hs
    │   │   │   │   │   │   │   │   ├── Search.hs
    │   │   │   │   │   │   │   │   ├── Select.hs
    │   │   │   │   │   │   │   │   ├── Status.hs
    │   │   │   │   │   │   │   │   ├── Track.hs
    │   │   │   │   │   │   │   │   ├── Update.hs
    │   │   │   │   │   │   │   │   ├── FRFS/
    │   │   │   │   │   │   │   │   │   ├── Cancel.hs
    │   │   │   │   │   │   │   │   │   ├── Confirm.hs
    │   │   │   │   │   │   │   │   │   ├── Init.hs
    │   │   │   │   │   │   │   │   │   ├── OnCancel.hs
    │   │   │   │   │   │   │   │   │   ├── OnConfirm.hs
    │   │   │   │   │   │   │   │   │   ├── OnInit.hs
    │   │   │   │   │   │   │   │   │   ├── OnSearch.hs
    │   │   │   │   │   │   │   │   │   ├── OnSelect.hs
    │   │   │   │   │   │   │   │   │   ├── OnStatus.hs
    │   │   │   │   │   │   │   │   │   ├── OnUpdate.hs
    │   │   │   │   │   │   │   │   │   ├── Search.hs
    │   │   │   │   │   │   │   │   │   ├── Select.hs
    │   │   │   │   │   │   │   │   │   ├── Status.hs
    │   │   │   │   │   │   │   │   │   └── Utils.hs
    │   │   │   │   │   │   │   │   └── IGM/
    │   │   │   │   │   │   │   │       ├── Issue.hs
    │   │   │   │   │   │   │   │       ├── IssueStatus.hs
    │   │   │   │   │   │   │   │       └── Utils.hs
    │   │   │   │   │   │   │   └── OnDemand/
    │   │   │   │   │   │   │       ├── Transformer/
    │   │   │   │   │   │   │       │   ├── Init.hs
    │   │   │   │   │   │   │       │   ├── OnSearch.hs
    │   │   │   │   │   │   │       │   └── Search.hs
    │   │   │   │   │   │   │       └── Utils/
    │   │   │   │   │   │   │           ├── Common.hs
    │   │   │   │   │   │   │           ├── Init.hs
    │   │   │   │   │   │   │           └── OnSearch.hs
    │   │   │   │   │   │   ├── Domain/
    │   │   │   │   │   │   │   ├── Action/
    │   │   │   │   │   │   │   │   ├── Beckn/
    │   │   │   │   │   │   │   │   │   ├── Common.hs
    │   │   │   │   │   │   │   │   │   ├── OnCancel.hs
    │   │   │   │   │   │   │   │   │   ├── OnConfirm.hs
    │   │   │   │   │   │   │   │   │   ├── OnInit.hs
    │   │   │   │   │   │   │   │   │   ├── OnSearch.hs
    │   │   │   │   │   │   │   │   │   ├── OnSelect.hs
    │   │   │   │   │   │   │   │   │   ├── OnStatus.hs
    │   │   │   │   │   │   │   │   │   ├── OnTrack.hs
    │   │   │   │   │   │   │   │   │   ├── OnUpdate.hs
    │   │   │   │   │   │   │   │   │   └── FRFS/
    │   │   │   │   │   │   │   │   │       ├── Common.hs
    │   │   │   │   │   │   │   │   │       ├── GWLink.hs
    │   │   │   │   │   │   │   │   │       ├── OnCancel.hs
    │   │   │   │   │   │   │   │   │       ├── OnConfirm.hs
    │   │   │   │   │   │   │   │   │       ├── OnInit.hs
    │   │   │   │   │   │   │   │   │       ├── OnSearch.hs
    │   │   │   │   │   │   │   │   │       ├── OnSelect.hs
    │   │   │   │   │   │   │   │   │       ├── OnStatus.hs
    │   │   │   │   │   │   │   │   │       └── OnUpdate.hs
    │   │   │   │   │   │   │   │   ├── Dashboard/
    │   │   │   │   │   │   │   │   │   ├── Booking.hs
    │   │   │   │   │   │   │   │   │   ├── Customer.hs
    │   │   │   │   │   │   │   │   │   ├── Exotel.hs
    │   │   │   │   │   │   │   │   │   ├── FRFSTicket.hs
    │   │   │   │   │   │   │   │   │   ├── Invoice.hs
    │   │   │   │   │   │   │   │   │   ├── Merchant.hs
    │   │   │   │   │   │   │   │   │   ├── NammaTag.hs
    │   │   │   │   │   │   │   │   │   ├── Ride.hs
    │   │   │   │   │   │   │   │   │   ├── Route.hs
    │   │   │   │   │   │   │   │   │   ├── System.hs
    │   │   │   │   │   │   │   │   │   ├── AppManagement/
    │   │   │   │   │   │   │   │   │   │   ├── Customer.hs
    │   │   │   │   │   │   │   │   │   │   ├── EventManagement.hs
    │   │   │   │   │   │   │   │   │   │   ├── MerchantOnboarding.hs
    │   │   │   │   │   │   │   │   │   │   ├── TicketDashboard.hs
    │   │   │   │   │   │   │   │   │   │   ├── Tickets.hs
    │   │   │   │   │   │   │   │   │   │   ├── EventManagement/
    │   │   │   │   │   │   │   │   │   │   │   └── Utils.hs
    │   │   │   │   │   │   │   │   │   │   └── MerchantOnboarding/
    │   │   │   │   │   │   │   │   │   │       ├── Handlers.hs
    │   │   │   │   │   │   │   │   │   │       └── TicketHandlers.hs
    │   │   │   │   │   │   │   │   │   ├── IssueManagement/
    │   │   │   │   │   │   │   │   │   │   ├── Issue.hs
    │   │   │   │   │   │   │   │   │   │   └── IssueList.hs
    │   │   │   │   │   │   │   │   │   └── RideBooking/
    │   │   │   │   │   │   │   │   │       ├── Booking.hs
    │   │   │   │   │   │   │   │   │       ├── Cancel.hs
    │   │   │   │   │   │   │   │   │       ├── Confirm.hs
    │   │   │   │   │   │   │   │   │       ├── Frontend.hs
    │   │   │   │   │   │   │   │   │       ├── Maps.hs
    │   │   │   │   │   │   │   │   │       ├── NotifyRideInfo.hs
    │   │   │   │   │   │   │   │   │       ├── Profile.hs
    │   │   │   │   │   │   │   │   │       ├── Quote.hs
    │   │   │   │   │   │   │   │   │       ├── Registration.hs
    │   │   │   │   │   │   │   │   │       ├── Search.hs
    │   │   │   │   │   │   │   │   │       └── Select.hs
    │   │   │   │   │   │   │   │   ├── Internal/
    │   │   │   │   │   │   │   │   │   ├── Cac.hs
    │   │   │   │   │   │   │   │   │   ├── DriverArrivalNotf.hs
    │   │   │   │   │   │   │   │   │   ├── FrequentLocUser.hs
    │   │   │   │   │   │   │   │   │   ├── FRFS.hs
    │   │   │   │   │   │   │   │   │   ├── Payout.hs
    │   │   │   │   │   │   │   │   │   ├── Rating.hs
    │   │   │   │   │   │   │   │   │   ├── StopEvents.hs
    │   │   │   │   │   │   │   │   │   └── ViolationDetection.hs
    │   │   │   │   │   │   │   │   └── UI/
    │   │   │   │   │   │   │   │       ├── AadhaarVerification.hs
    │   │   │   │   │   │   │   │       ├── AppInstalls.hs
    │   │   │   │   │   │   │   │       ├── AttractionRecommend.hs
    │   │   │   │   │   │   │   │       ├── BBPS.hs
    │   │   │   │   │   │   │   │       ├── Booking.hs
    │   │   │   │   │   │   │   │       ├── Cac.hs
    │   │   │   │   │   │   │   │       ├── Call.hs
    │   │   │   │   │   │   │   │       ├── CallEvent.hs
    │   │   │   │   │   │   │   │       ├── Cancel.hs
    │   │   │   │   │   │   │   │       ├── CancellationReason.hs
    │   │   │   │   │   │   │   │       ├── Confirm.hs
    │   │   │   │   │   │   │   │       ├── CRIS.hs
    │   │   │   │   │   │   │   │       ├── CustomerReferral.hs
    │   │   │   │   │   │   │   │       ├── DeletedPerson.hs
    │   │   │   │   │   │   │   │       ├── Disability.hs
    │   │   │   │   │   │   │   │       ├── DriverOffer.hs
    │   │   │   │   │   │   │   │       ├── EditLocation.hs
    │   │   │   │   │   │   │   │       ├── Estimate.hs
    │   │   │   │   │   │   │   │       ├── EstimateBP.hs
    │   │   │   │   │   │   │   │       ├── FareBreakup.hs
    │   │   │   │   │   │   │   │       ├── FavouriteDriver.hs
    │   │   │   │   │   │   │   │       ├── Feedback.hs
    │   │   │   │   │   │   │   │       ├── FeedbackForm.hs
    │   │   │   │   │   │   │   │       ├── FollowRide.hs
    │   │   │   │   │   │   │   │       ├── FRFSTicketService.hs
    │   │   │   │   │   │   │   │       ├── Frontend.hs
    │   │   │   │   │   │   │   │       ├── HotSpot.hs
    │   │   │   │   │   │   │   │       ├── IGM.hs
    │   │   │   │   │   │   │   │       ├── Insurance.hs
    │   │   │   │   │   │   │   │       ├── InsuranceInternal.hs
    │   │   │   │   │   │   │   │       ├── InterCityDetails.hs
    │   │   │   │   │   │   │   │       ├── Invoice.hs
    │   │   │   │   │   │   │   │       ├── Location.hs
    │   │   │   │   │   │   │   │       ├── Maps.hs
    │   │   │   │   │   │   │   │       ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │   │   │       ├── MeterRideInternal.hs
    │   │   │   │   │   │   │   │       ├── Miscellaneous.hs
    │   │   │   │   │   │   │   │       ├── MultimodalConfirm.hs
    │   │   │   │   │   │   │   │       ├── NearbyBuses.hs
    │   │   │   │   │   │   │   │       ├── NearbyDrivers.hs
    │   │   │   │   │   │   │   │       ├── PartnerOrganizationFRFS.hs
    │   │   │   │   │   │   │   │       ├── Payment.hs
    │   │   │   │   │   │   │   │       ├── Person.hs
    │   │   │   │   │   │   │   │       ├── PersonDefaultEmergencyNumber.hs
    │   │   │   │   │   │   │   │       ├── PersonStats.hs
    │   │   │   │   │   │   │   │       ├── Places.hs
    │   │   │   │   │   │   │   │       ├── PriceBreakup.hs
    │   │   │   │   │   │   │   │       ├── Profile.hs
    │   │   │   │   │   │   │   │       ├── Quote.hs
    │   │   │   │   │   │   │   │       ├── Registration.hs
    │   │   │   │   │   │   │   │       ├── RentalDetails.hs
    │   │   │   │   │   │   │   │       ├── RentalsIntercityCache.hs
    │   │   │   │   │   │   │   │       ├── Ride.hs
    │   │   │   │   │   │   │   │       ├── RidePayment.hs
    │   │   │   │   │   │   │   │       ├── Route.hs
    │   │   │   │   │   │   │   │       ├── SavedReqLocation.hs
    │   │   │   │   │   │   │   │       ├── Search.hs
    │   │   │   │   │   │   │   │       ├── Select.hs
    │   │   │   │   │   │   │   │       ├── Serviceability.hs
    │   │   │   │   │   │   │   │       ├── SocialLogin.hs
    │   │   │   │   │   │   │   │       ├── Sos.hs
    │   │   │   │   │   │   │   │       ├── SpecialZoneQuote.hs
    │   │   │   │   │   │   │   │       ├── Support.hs
    │   │   │   │   │   │   │   │       ├── TicketDashboard.hs
    │   │   │   │   │   │   │   │       ├── TicketKapture.hs
    │   │   │   │   │   │   │   │       ├── TicketService.hs
    │   │   │   │   │   │   │   │       ├── TrackRoute.hs
    │   │   │   │   │   │   │   │       ├── TriggerFCM.hs
    │   │   │   │   │   │   │   │       └── Whatsapp.hs
    │   │   │   │   │   │   │   └── Types/
    │   │   │   │   │   │   │       ├── CacType.hs
    │   │   │   │   │   │   │       ├── Common.hs
    │   │   │   │   │   │   │       ├── DeliveryDetails.hs
    │   │   │   │   │   │   │       ├── EventManagement.hs
    │   │   │   │   │   │   │       ├── Geometry.hs
    │   │   │   │   │   │   │       ├── HotSpot.hs
    │   │   │   │   │   │   │       ├── LocationAddress.hs
    │   │   │   │   │   │   │       ├── RentalsIntercityCache.hs
    │   │   │   │   │   │   │       ├── TicketDashboard.hs
    │   │   │   │   │   │   │       ├── UtilsTH.hs
    │   │   │   │   │   │   │       ├── Booking/
    │   │   │   │   │   │   │       │   └── API.hs
    │   │   │   │   │   │   │       ├── EventManagement/
    │   │   │   │   │   │   │       │   └── Permissions.hs
    │   │   │   │   │   │   │       ├── Extra/
    │   │   │   │   │   │   │       │   ├── Booking.hs
    │   │   │   │   │   │   │       │   ├── CancellationReason.hs
    │   │   │   │   │   │   │       │   ├── FeedbackForm.hs
    │   │   │   │   │   │   │       │   ├── FRFSCachedQuote.hs
    │   │   │   │   │   │   │       │   ├── IntegratedBPPConfig.hs
    │   │   │   │   │   │   │       │   ├── InterCityDetails.hs
    │   │   │   │   │   │   │       │   ├── MerchantMessage.hs
    │   │   │   │   │   │   │       │   ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │   │       │   ├── MerchantServiceConfig.hs
    │   │   │   │   │   │   │       │   ├── PartnerOrgConfig.hs
    │   │   │   │   │   │   │       │   ├── PersonFlowStatus.hs
    │   │   │   │   │   │   │       │   ├── PlaceNameCache.hs
    │   │   │   │   │   │   │       │   ├── RecentLocation.hs
    │   │   │   │   │   │   │       │   ├── RentalDetails.hs
    │   │   │   │   │   │   │       │   ├── Ride.hs
    │   │   │   │   │   │   │       │   ├── RiderConfig.hs
    │   │   │   │   │   │   │       │   └── TicketBooking.hs
    │   │   │   │   │   │   │       ├── FarePolicy/
    │   │   │   │   │   │   │       │   └── FareProductType.hs
    │   │   │   │   │   │   │       └── MerchantOnboarding/
    │   │   │   │   │   │   │           └── Handler.hs
    │   │   │   │   │   │   ├── ExternalBPP/
    │   │   │   │   │   │   │   ├── CallAPI.hs
    │   │   │   │   │   │   │   ├── Flow.hs
    │   │   │   │   │   │   │   └── ExternalAPI/
    │   │   │   │   │   │   │       ├── CallAPI.hs
    │   │   │   │   │   │   │       ├── Types.hs
    │   │   │   │   │   │   │       ├── Bus/
    │   │   │   │   │   │   │       │   └── EBIX/
    │   │   │   │   │   │   │       │       ├── Auth.hs
    │   │   │   │   │   │   │       │       ├── Order.hs
    │   │   │   │   │   │   │       │       ├── Payment.hs
    │   │   │   │   │   │   │       │       └── Status.hs
    │   │   │   │   │   │   │       ├── Direct/
    │   │   │   │   │   │   │       │   ├── Order.hs
    │   │   │   │   │   │   │       │   ├── Status.hs
    │   │   │   │   │   │   │       │   ├── Utils.hs
    │   │   │   │   │   │   │       │   └── Verify.hs
    │   │   │   │   │   │   │       ├── Metro/
    │   │   │   │   │   │   │       │   └── CMRL/
    │   │   │   │   │   │   │       │       ├── Auth.hs
    │   │   │   │   │   │   │       │       ├── BusinessHour.hs
    │   │   │   │   │   │   │       │       ├── DurationDetails.hs
    │   │   │   │   │   │   │       │       ├── Error.hs
    │   │   │   │   │   │   │       │       ├── FareByOriginDest.hs
    │   │   │   │   │   │   │       │       ├── FareMatrix.hs
    │   │   │   │   │   │   │       │       ├── Order.hs
    │   │   │   │   │   │   │       │       ├── PassengerViewStatus.hs
    │   │   │   │   │   │   │       │       ├── StationList.hs
    │   │   │   │   │   │   │       │       └── TicketStatus.hs
    │   │   │   │   │   │   │       └── Subway/
    │   │   │   │   │   │   │           └── CRIS/
    │   │   │   │   │   │   │               ├── Auth.hs
    │   │   │   │   │   │   │               ├── BookJourney.hs
    │   │   │   │   │   │   │               ├── ChangeDevice.hs
    │   │   │   │   │   │   │               ├── Encryption.hs
    │   │   │   │   │   │   │               ├── Error.hs
    │   │   │   │   │   │   │               ├── OtpGeneration.hs
    │   │   │   │   │   │   │               ├── RouteFare.hs
    │   │   │   │   │   │   │               ├── SDKData.hs
    │   │   │   │   │   │   │               └── Uts.hs
    │   │   │   │   │   │   ├── Lib/
    │   │   │   │   │   │   │   ├── JourneyLeg/
    │   │   │   │   │   │   │   │   ├── Bus.hs
    │   │   │   │   │   │   │   │   ├── Interface.hs
    │   │   │   │   │   │   │   │   ├── Metro.hs
    │   │   │   │   │   │   │   │   ├── Subway.hs
    │   │   │   │   │   │   │   │   ├── Taxi.hs
    │   │   │   │   │   │   │   │   ├── Types.hs
    │   │   │   │   │   │   │   │   ├── Walk.hs
    │   │   │   │   │   │   │   │   ├── Common/
    │   │   │   │   │   │   │   │   │   └── FRFS.hs
    │   │   │   │   │   │   │   │   └── Types/
    │   │   │   │   │   │   │   │       ├── Bus.hs
    │   │   │   │   │   │   │   │       ├── Metro.hs
    │   │   │   │   │   │   │   │       ├── Subway.hs
    │   │   │   │   │   │   │   │       ├── Taxi.hs
    │   │   │   │   │   │   │   │       └── Walk.hs
    │   │   │   │   │   │   │   └── JourneyModule/
    │   │   │   │   │   │   │       ├── Base.hs
    │   │   │   │   │   │   │       ├── Location.hs
    │   │   │   │   │   │   │       ├── Types.hs
    │   │   │   │   │   │   │       └── Utils.hs
    │   │   │   │   │   │   ├── SharedLogic/
    │   │   │   │   │   │   │   ├── Booking.hs
    │   │   │   │   │   │   │   ├── Cac.hs
    │   │   │   │   │   │   │   ├── CallBPP.hs
    │   │   │   │   │   │   │   ├── CallBPPInternal.hs
    │   │   │   │   │   │   │   ├── CallFRFSBPP.hs
    │   │   │   │   │   │   │   ├── CallIGMBPP.hs
    │   │   │   │   │   │   │   ├── Confirm.hs
    │   │   │   │   │   │   │   ├── CreateFareForMultiModal.hs
    │   │   │   │   │   │   │   ├── FRFSUtils.hs
    │   │   │   │   │   │   │   ├── GoogleTranslate.hs
    │   │   │   │   │   │   │   ├── Insurance.hs
    │   │   │   │   │   │   │   ├── JobScheduler.hs
    │   │   │   │   │   │   │   ├── LocationMapping.hs
    │   │   │   │   │   │   │   ├── Merchant.hs
    │   │   │   │   │   │   │   ├── MerchantConfig.hs
    │   │   │   │   │   │   │   ├── MessageBuilder.hs
    │   │   │   │   │   │   │   ├── MetroOffer.hs
    │   │   │   │   │   │   │   ├── Payment.hs
    │   │   │   │   │   │   │   ├── Person.hs
    │   │   │   │   │   │   │   ├── PersonDefaultEmergencyNumber.hs
    │   │   │   │   │   │   │   ├── PublicTransport.hs
    │   │   │   │   │   │   │   ├── Referral.hs
    │   │   │   │   │   │   │   ├── Ride.hs
    │   │   │   │   │   │   │   ├── ScheduledNotifications.hs
    │   │   │   │   │   │   │   ├── Search.hs
    │   │   │   │   │   │   │   ├── Serviceability.hs
    │   │   │   │   │   │   │   ├── TicketUtils.hs
    │   │   │   │   │   │   │   ├── External/
    │   │   │   │   │   │   │   │   ├── BbpsService/
    │   │   │   │   │   │   │   │   │   └── Flow.hs
    │   │   │   │   │   │   │   │   ├── LocationTrackingService/
    │   │   │   │   │   │   │   │   │   ├── Flow.hs
    │   │   │   │   │   │   │   │   │   ├── Types.hs
    │   │   │   │   │   │   │   │   │   └── API/
    │   │   │   │   │   │   │   │   │       ├── NearbyDrivers.hs
    │   │   │   │   │   │   │   │   │       └── VehicleTrackingOnRoute.hs
    │   │   │   │   │   │   │   │   └── Nandi/
    │   │   │   │   │   │   │   │       ├── Flow.hs
    │   │   │   │   │   │   │   │       ├── Types.hs
    │   │   │   │   │   │   │   │       └── API/
    │   │   │   │   │   │   │   │           └── Nandi.hs
    │   │   │   │   │   │   │   ├── KaalChakra/
    │   │   │   │   │   │   │   │   └── Actions.hs
    │   │   │   │   │   │   │   ├── Scheduler/
    │   │   │   │   │   │   │   │   └── Jobs/
    │   │   │   │   │   │   │   │       ├── CallPoliceApi.hs
    │   │   │   │   │   │   │   │       ├── Chakras.hs
    │   │   │   │   │   │   │   │       ├── CheckExotelCallStatusAndNotifyBPP.hs
    │   │   │   │   │   │   │   │       ├── CheckPNAndSendSMS.hs
    │   │   │   │   │   │   │   │       ├── ExecutePaymentIntent.hs
    │   │   │   │   │   │   │   │       ├── MetroBusinessHour.hs
    │   │   │   │   │   │   │   │       ├── PostRideSafetyNotification.hs
    │   │   │   │   │   │   │   │       ├── SafetyCSAlert.hs
    │   │   │   │   │   │   │   │       ├── SafetyIVR.hs
    │   │   │   │   │   │   │   │       ├── ScheduledRideNotificationsToRider.hs
    │   │   │   │   │   │   │   │       ├── ScheduledRidePopupToRider.hs
    │   │   │   │   │   │   │   │       ├── UpdateCrisUtsData.hs
    │   │   │   │   │   │   │   │       └── Payout/
    │   │   │   │   │   │   │   │           └── MetroIncentivePayout.hs
    │   │   │   │   │   │   │   └── TicketRule/
    │   │   │   │   │   │   │       ├── Apply.hs
    │   │   │   │   │   │   │       └── Core.hs
    │   │   │   │   │   │   ├── Storage/
    │   │   │   │   │   │   │   ├── Beam/
    │   │   │   │   │   │   │   │   ├── BecknRequest.hs
    │   │   │   │   │   │   │   │   ├── Common.hs
    │   │   │   │   │   │   │   │   ├── EstimateBreakup.hs
    │   │   │   │   │   │   │   │   ├── Geometry.hs
    │   │   │   │   │   │   │   │   ├── IssueManagement.hs
    │   │   │   │   │   │   │   │   ├── Payment.hs
    │   │   │   │   │   │   │   │   ├── SchedulerJob.hs
    │   │   │   │   │   │   │   │   ├── SystemConfigs.hs
    │   │   │   │   │   │   │   │   ├── Yudhishthira.hs
    │   │   │   │   │   │   │   │   └── Geometry/
    │   │   │   │   │   │   │   │       ├── Geometry.hs
    │   │   │   │   │   │   │   │       └── GeometryGeom.hs
    │   │   │   │   │   │   │   ├── CachedQueries/
    │   │   │   │   │   │   │   │   ├── BecknConfig.hs
    │   │   │   │   │   │   │   │   ├── BlackListOrg.hs
    │   │   │   │   │   │   │   │   ├── BppDetails.hs
    │   │   │   │   │   │   │   │   ├── Exophone.hs
    │   │   │   │   │   │   │   │   ├── FollowRide.hs
    │   │   │   │   │   │   │   │   ├── FRFSConfig.hs
    │   │   │   │   │   │   │   │   ├── FRFSGtfsStageFare.hs
    │   │   │   │   │   │   │   │   ├── GTFSFeedInfo.hs
    │   │   │   │   │   │   │   │   ├── HotSpotConfig.hs
    │   │   │   │   │   │   │   │   ├── Merchant.hs
    │   │   │   │   │   │   │   │   ├── MerchantConfig.hs
    │   │   │   │   │   │   │   │   ├── PartnerOrganization.hs
    │   │   │   │   │   │   │   │   ├── PartnerOrgConfig.hs
    │   │   │   │   │   │   │   │   ├── PartnerOrgStation.hs
    │   │   │   │   │   │   │   │   ├── Person.hs
    │   │   │   │   │   │   │   │   ├── PlaceBasedServiceConfig.hs
    │   │   │   │   │   │   │   │   ├── RideRelatedNotificationConfig.hs
    │   │   │   │   │   │   │   │   ├── RouteStopTimeTable.hs
    │   │   │   │   │   │   │   │   ├── SavedReqLocationExtra.hs
    │   │   │   │   │   │   │   │   ├── Sos.hs
    │   │   │   │   │   │   │   │   ├── Station.hs
    │   │   │   │   │   │   │   │   ├── UiRiderConfig.hs
    │   │   │   │   │   │   │   │   ├── ValueAddNP.hs
    │   │   │   │   │   │   │   │   ├── VehicleConfig.hs
    │   │   │   │   │   │   │   │   ├── WhiteListOrg.hs
    │   │   │   │   │   │   │   │   ├── Maps/
    │   │   │   │   │   │   │   │   │   ├── LocationMapCache.hs
    │   │   │   │   │   │   │   │   │   └── PlaceNameCache.hs
    │   │   │   │   │   │   │   │   ├── Merchant/
    │   │   │   │   │   │   │   │   │   ├── MerchantMessage.hs
    │   │   │   │   │   │   │   │   │   ├── MerchantOperatingCity.hs
    │   │   │   │   │   │   │   │   │   ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │   │   │   │   ├── MerchantPushNotification.hs
    │   │   │   │   │   │   │   │   │   ├── MerchantServiceConfig.hs
    │   │   │   │   │   │   │   │   │   ├── MerchantServiceUsageConfig.hs
    │   │   │   │   │   │   │   │   │   ├── MerchantState.hs
    │   │   │   │   │   │   │   │   │   ├── MultiModalBus.hs
    │   │   │   │   │   │   │   │   │   ├── MultiModalSuburban.hs
    │   │   │   │   │   │   │   │   │   ├── PayoutConfig.hs
    │   │   │   │   │   │   │   │   │   └── RiderConfig.hs
    │   │   │   │   │   │   │   │   ├── OTPRest/
    │   │   │   │   │   │   │   │   │   └── OTPRest.hs
    │   │   │   │   │   │   │   │   └── Person/
    │   │   │   │   │   │   │   │       └── PersonFlowStatus.hs
    │   │   │   │   │   │   │   ├── Clickhouse/
    │   │   │   │   │   │   │   │   ├── Booking.hs
    │   │   │   │   │   │   │   │   ├── BookingCancellationReason.hs
    │   │   │   │   │   │   │   │   ├── EstimateBreakup.hs
    │   │   │   │   │   │   │   │   ├── FareBreakup.hs
    │   │   │   │   │   │   │   │   ├── Location.hs
    │   │   │   │   │   │   │   │   ├── Person.hs
    │   │   │   │   │   │   │   │   ├── QuoteBreakup.hs
    │   │   │   │   │   │   │   │   ├── Ride.hs
    │   │   │   │   │   │   │   │   └── Sos.hs
    │   │   │   │   │   │   │   ├── GraphqlQueries/
    │   │   │   │   │   │   │   │   ├── Client.hs
    │   │   │   │   │   │   │   │   ├── RouteStopTimeTable.hs
    │   │   │   │   │   │   │   │   └── Types.hs
    │   │   │   │   │   │   │   └── Queries/
    │   │   │   │   │   │   │       ├── AppInstallsExtra.hs
    │   │   │   │   │   │   │       ├── BBPSExtra.hs
    │   │   │   │   │   │   │       ├── BecknConfigExtra.hs
    │   │   │   │   │   │   │       ├── BookingCancellationReasonExtra.hs
    │   │   │   │   │   │   │       ├── BookingExtra.hs
    │   │   │   │   │   │   │       ├── BookingPartiesLinkExtra.hs
    │   │   │   │   │   │   │       ├── BusinessHourExtra.hs
    │   │   │   │   │   │   │       ├── CallStatusExtra.hs
    │   │   │   │   │   │   │       ├── CancellationReasonExtra.hs
    │   │   │   │   │   │   │       ├── DisabilityExtra.hs
    │   │   │   │   │   │   │       ├── EstimateBreakup.hs
    │   │   │   │   │   │   │       ├── EstimateExtra.hs
    │   │   │   │   │   │   │       ├── ExophoneExtra.hs
    │   │   │   │   │   │   │       ├── FareBreakupExtra.hs
    │   │   │   │   │   │   │       ├── FeedbackFormExtra.hs
    │   │   │   │   │   │   │       ├── FRFSQuoteExtra.hs
    │   │   │   │   │   │   │       ├── FRFSSearchExtra.hs
    │   │   │   │   │   │   │       ├── FRFSTicketBokingPayment.hs
    │   │   │   │   │   │   │       ├── FRFSTicketBookingExtra.hs
    │   │   │   │   │   │   │       ├── Geometry.hs
    │   │   │   │   │   │   │       ├── IssueExtra.hs
    │   │   │   │   │   │   │       ├── JourneyExtra.hs
    │   │   │   │   │   │   │       ├── JourneyLegExtra.hs
    │   │   │   │   │   │   │       ├── LocationMappingExtra.hs
    │   │   │   │   │   │   │       ├── MerchantExtra.hs
    │   │   │   │   │   │   │       ├── MerchantPaymentMethodExtra.hs
    │   │   │   │   │   │   │       ├── MerchantServiceConfigExtra.hs
    │   │   │   │   │   │   │       ├── MerchantServiceUsageConfigExtra.hs
    │   │   │   │   │   │   │       ├── PartnerOrganizationExtra.hs
    │   │   │   │   │   │   │       ├── PartnerOrgConfigExtra.hs
    │   │   │   │   │   │   │       ├── PersonDefaultEmergencyNumberExtra.hs
    │   │   │   │   │   │   │       ├── PersonExtra.hs
    │   │   │   │   │   │   │       ├── PersonStatsExtra.hs
    │   │   │   │   │   │   │       ├── PlaceBasedServiceConfigExtra.hs
    │   │   │   │   │   │   │       ├── PopularLocationExtra.hs
    │   │   │   │   │   │   │       ├── QuoteExtra.hs
    │   │   │   │   │   │   │       ├── RatingExtra.hs
    │   │   │   │   │   │   │       ├── RecentLocationExtra.hs
    │   │   │   │   │   │   │       ├── RegistrationTokenExtra.hs
    │   │   │   │   │   │   │       ├── RideExtra.hs
    │   │   │   │   │   │   │       ├── RouteExtra.hs
    │   │   │   │   │   │   │       ├── RoutePolylinesExtra.hs
    │   │   │   │   │   │   │       ├── SafetySettingsExtra.hs
    │   │   │   │   │   │   │       ├── SavedReqLocationExtra.hs
    │   │   │   │   │   │   │       ├── SearchRequestExtra.hs
    │   │   │   │   │   │   │       ├── ServiceCategoryExtra.hs
    │   │   │   │   │   │   │       ├── ServicePeopleCategoryExtra.hs
    │   │   │   │   │   │   │       ├── StationExtra.hs
    │   │   │   │   │   │   │       ├── StationsExtraInformationExtra.hs
    │   │   │   │   │   │   │       ├── TicketBookingExtra.hs
    │   │   │   │   │   │   │       ├── TicketBookingPeopleCategoryExtra.hs
    │   │   │   │   │   │   │       ├── TicketBookingServiceCategoryExtra.hs
    │   │   │   │   │   │   │       ├── TicketBookingServiceExtra.hs
    │   │   │   │   │   │   │       ├── TicketServiceExtra.hs
    │   │   │   │   │   │   │       ├── UiRiderConfigExtra.hs
    │   │   │   │   │   │   │       ├── VehicleRouteMappingExtra.hs
    │   │   │   │   │   │   │       ├── WalkLegMultimodalExtra.hs
    │   │   │   │   │   │   │       ├── WhiteListOrgExtra.hs
    │   │   │   │   │   │   │       ├── Extra/
    │   │   │   │   │   │   │       │   └── Transformers/
    │   │   │   │   │   │   │       │       └── Ride.hs
    │   │   │   │   │   │   │       ├── QueriesExtra/
    │   │   │   │   │   │   │       │   └── RideLite.hs
    │   │   │   │   │   │   │       └── Transformers/
    │   │   │   │   │   │   │           ├── AppInstalls.hs
    │   │   │   │   │   │   │           ├── BlackListOrg.hs
    │   │   │   │   │   │   │           ├── Booking.hs
    │   │   │   │   │   │   │           ├── BookingCancellationReason.hs
    │   │   │   │   │   │   │           ├── CancellationReason.hs
    │   │   │   │   │   │   │           ├── Estimate.hs
    │   │   │   │   │   │   │           ├── FRFSSearch.hs
    │   │   │   │   │   │   │           ├── IntegratedBPPConfig.hs
    │   │   │   │   │   │   │           ├── Merchant.hs
    │   │   │   │   │   │   │           ├── MerchantConfig.hs
    │   │   │   │   │   │   │           ├── MerchantMessage.hs
    │   │   │   │   │   │   │           ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │   │           ├── MerchantServiceConfig.hs
    │   │   │   │   │   │   │           ├── MultiModal.hs
    │   │   │   │   │   │   │           ├── PartnerOrgConfig.hs
    │   │   │   │   │   │   │           ├── Person.hs
    │   │   │   │   │   │   │           ├── Quote.hs
    │   │   │   │   │   │   │           ├── RentalDetails.hs
    │   │   │   │   │   │   │           ├── RouteDetails.hs
    │   │   │   │   │   │   │           ├── SearchReqLocation.hs
    │   │   │   │   │   │   │           ├── SearchRequest.hs
    │   │   │   │   │   │   │           ├── ServicePeopleCategory.hs
    │   │   │   │   │   │   │           ├── SpecialZoneQuote.hs
    │   │   │   │   │   │   │           ├── TicketMerchantDetails.hs
    │   │   │   │   │   │   │           ├── TripTerms.hs
    │   │   │   │   │   │   │           └── WhiteListOrg.hs
    │   │   │   │   │   │   └── Tools/
    │   │   │   │   │   │       ├── AadhaarVerification.hs
    │   │   │   │   │   │       ├── Auth.hs
    │   │   │   │   │   │       ├── Call.hs
    │   │   │   │   │   │       ├── ConfigPilot.hs
    │   │   │   │   │   │       ├── Constants.hs
    │   │   │   │   │   │       ├── Error.hs
    │   │   │   │   │   │       ├── Event.hs
    │   │   │   │   │   │       ├── Insurance.hs
    │   │   │   │   │   │       ├── JSON.hs
    │   │   │   │   │   │       ├── Maps.hs
    │   │   │   │   │   │       ├── Metrics.hs
    │   │   │   │   │   │       ├── MultiModal.hs
    │   │   │   │   │   │       ├── Notifications.hs
    │   │   │   │   │   │       ├── Payment.hs
    │   │   │   │   │   │       ├── Payout.hs
    │   │   │   │   │   │       ├── Schema.hs
    │   │   │   │   │   │       ├── SignatureAuth.hs
    │   │   │   │   │   │       ├── SMS.hs
    │   │   │   │   │   │       ├── Ticket.hs
    │   │   │   │   │   │       ├── Whatsapp.hs
    │   │   │   │   │   │       ├── Beam/
    │   │   │   │   │   │       │   └── UtilsTH.hs
    │   │   │   │   │   │       ├── Metrics/
    │   │   │   │   │   │       │   ├── BAPMetrics.hs
    │   │   │   │   │   │       │   └── BAPMetrics/
    │   │   │   │   │   │       │       └── Types.hs
    │   │   │   │   │   │       └── Streaming/
    │   │   │   │   │   │           └── Kafka.hs
    │   │   │   │   │   ├── src-read-only/
    │   │   │   │   │   │   ├── API/
    │   │   │   │   │   │   │   ├── Action/
    │   │   │   │   │   │   │   │   ├── Dashboard/
    │   │   │   │   │   │   │   │   │   ├── AppManagement.hs
    │   │   │   │   │   │   │   │   │   ├── IssueManagement.hs
    │   │   │   │   │   │   │   │   │   ├── Management.hs
    │   │   │   │   │   │   │   │   │   ├── RideBooking.hs
    │   │   │   │   │   │   │   │   │   ├── AppManagement/
    │   │   │   │   │   │   │   │   │   │   ├── Customer.hs
    │   │   │   │   │   │   │   │   │   │   ├── EventManagement.hs
    │   │   │   │   │   │   │   │   │   │   ├── MerchantOnboarding.hs
    │   │   │   │   │   │   │   │   │   │   ├── TicketDashboard.hs
    │   │   │   │   │   │   │   │   │   │   └── Tickets.hs
    │   │   │   │   │   │   │   │   │   ├── IssueManagement/
    │   │   │   │   │   │   │   │   │   │   ├── Issue.hs
    │   │   │   │   │   │   │   │   │   │   └── IssueList.hs
    │   │   │   │   │   │   │   │   │   ├── Management/
    │   │   │   │   │   │   │   │   │   │   ├── Booking.hs
    │   │   │   │   │   │   │   │   │   │   ├── Customer.hs
    │   │   │   │   │   │   │   │   │   │   ├── FRFSTicket.hs
    │   │   │   │   │   │   │   │   │   │   ├── Invoice.hs
    │   │   │   │   │   │   │   │   │   │   ├── Merchant.hs
    │   │   │   │   │   │   │   │   │   │   ├── NammaTag.hs
    │   │   │   │   │   │   │   │   │   │   ├── Ride.hs
    │   │   │   │   │   │   │   │   │   │   └── System.hs
    │   │   │   │   │   │   │   │   │   └── RideBooking/
    │   │   │   │   │   │   │   │   │       ├── Booking.hs
    │   │   │   │   │   │   │   │   │       ├── Cancel.hs
    │   │   │   │   │   │   │   │   │       ├── Confirm.hs
    │   │   │   │   │   │   │   │   │       ├── Frontend.hs
    │   │   │   │   │   │   │   │   │       ├── Maps.hs
    │   │   │   │   │   │   │   │   │       ├── NotifyRideInfo.hs
    │   │   │   │   │   │   │   │   │       ├── Profile.hs
    │   │   │   │   │   │   │   │   │       ├── Quote.hs
    │   │   │   │   │   │   │   │   │       ├── Registration.hs
    │   │   │   │   │   │   │   │   │       ├── Search.hs
    │   │   │   │   │   │   │   │   │       └── Select.hs
    │   │   │   │   │   │   │   │   └── UI/
    │   │   │   │   │   │   │   │       ├── AttractionRecommend.hs
    │   │   │   │   │   │   │   │       ├── BBPS.hs
    │   │   │   │   │   │   │   │       ├── Cac.hs
    │   │   │   │   │   │   │   │       ├── CRIS.hs
    │   │   │   │   │   │   │   │       ├── CustomerReferral.hs
    │   │   │   │   │   │   │   │       ├── DeletedPerson.hs
    │   │   │   │   │   │   │   │       ├── EditLocation.hs
    │   │   │   │   │   │   │   │       ├── EstimateBP.hs
    │   │   │   │   │   │   │   │       ├── FavouriteDriver.hs
    │   │   │   │   │   │   │   │       ├── FollowRide.hs
    │   │   │   │   │   │   │   │       ├── FRFSTicketService.hs
    │   │   │   │   │   │   │   │       ├── Insurance.hs
    │   │   │   │   │   │   │   │       ├── InsuranceInternal.hs
    │   │   │   │   │   │   │   │       ├── Invoice.hs
    │   │   │   │   │   │   │   │       ├── MeterRideInternal.hs
    │   │   │   │   │   │   │   │       ├── Miscellaneous.hs
    │   │   │   │   │   │   │   │       ├── MultimodalConfirm.hs
    │   │   │   │   │   │   │   │       ├── NearbyBuses.hs
    │   │   │   │   │   │   │   │       ├── NearbyDrivers.hs
    │   │   │   │   │   │   │   │       ├── Places.hs
    │   │   │   │   │   │   │   │       ├── PriceBreakup.hs
    │   │   │   │   │   │   │   │       ├── RidePayment.hs
    │   │   │   │   │   │   │   │       ├── SocialLogin.hs
    │   │   │   │   │   │   │   │       ├── Sos.hs
    │   │   │   │   │   │   │   │       ├── TicketKapture.hs
    │   │   │   │   │   │   │   │       ├── TicketService.hs
    │   │   │   │   │   │   │   │       ├── TrackRoute.hs
    │   │   │   │   │   │   │   │       └── TriggerFCM.hs
    │   │   │   │   │   │   │   └── Types/
    │   │   │   │   │   │   │       ├── Dashboard/
    │   │   │   │   │   │   │       │   ├── AppManagement.hs
    │   │   │   │   │   │   │       │   ├── RideBooking.hs
    │   │   │   │   │   │   │       │   ├── AppManagement/
    │   │   │   │   │   │   │       │   │   ├── Customer.hs
    │   │   │   │   │   │   │       │   │   ├── EventManagement.hs
    │   │   │   │   │   │   │       │   │   ├── MerchantOnboarding.hs
    │   │   │   │   │   │   │       │   │   ├── TicketDashboard.hs
    │   │   │   │   │   │   │       │   │   ├── Tickets.hs
    │   │   │   │   │   │   │       │   │   └── Endpoints/
    │   │   │   │   │   │   │       │   │       ├── Customer.hs
    │   │   │   │   │   │   │       │   │       ├── EventManagement.hs
    │   │   │   │   │   │   │       │   │       ├── MerchantOnboarding.hs
    │   │   │   │   │   │   │       │   │       ├── TicketDashboard.hs
    │   │   │   │   │   │   │       │   │       └── Tickets.hs
    │   │   │   │   │   │   │       │   └── RideBooking/
    │   │   │   │   │   │   │       │       ├── Booking.hs
    │   │   │   │   │   │   │       │       ├── Cancel.hs
    │   │   │   │   │   │   │       │       ├── Confirm.hs
    │   │   │   │   │   │   │       │       ├── Frontend.hs
    │   │   │   │   │   │   │       │       ├── Maps.hs
    │   │   │   │   │   │   │       │       ├── NotifyRideInfo.hs
    │   │   │   │   │   │   │       │       ├── Profile.hs
    │   │   │   │   │   │   │       │       ├── Quote.hs
    │   │   │   │   │   │   │       │       ├── Registration.hs
    │   │   │   │   │   │   │       │       ├── Search.hs
    │   │   │   │   │   │   │       │       ├── Select.hs
    │   │   │   │   │   │   │       │       └── Endpoints/
    │   │   │   │   │   │   │       │           ├── Booking.hs
    │   │   │   │   │   │   │       │           ├── Cancel.hs
    │   │   │   │   │   │   │       │           ├── Confirm.hs
    │   │   │   │   │   │   │       │           ├── Frontend.hs
    │   │   │   │   │   │   │       │           ├── Maps.hs
    │   │   │   │   │   │   │       │           ├── NotifyRideInfo.hs
    │   │   │   │   │   │   │       │           ├── Profile.hs
    │   │   │   │   │   │   │       │           ├── Quote.hs
    │   │   │   │   │   │   │       │           ├── Registration.hs
    │   │   │   │   │   │   │       │           ├── Search.hs
    │   │   │   │   │   │   │       │           └── Select.hs
    │   │   │   │   │   │   │       └── UI/
    │   │   │   │   │   │   │           ├── AttractionRecommend.hs
    │   │   │   │   │   │   │           ├── BBPS.hs
    │   │   │   │   │   │   │           ├── CRIS.hs
    │   │   │   │   │   │   │           ├── CustomerReferral.hs
    │   │   │   │   │   │   │           ├── DeletedPerson.hs
    │   │   │   │   │   │   │           ├── EditLocation.hs
    │   │   │   │   │   │   │           ├── EstimateBP.hs
    │   │   │   │   │   │   │           ├── FavouriteDriver.hs
    │   │   │   │   │   │   │           ├── FollowRide.hs
    │   │   │   │   │   │   │           ├── FRFSTicketService.hs
    │   │   │   │   │   │   │           ├── Insurance.hs
    │   │   │   │   │   │   │           ├── Invoice.hs
    │   │   │   │   │   │   │           ├── MeterRideInternal.hs
    │   │   │   │   │   │   │           ├── Miscellaneous.hs
    │   │   │   │   │   │   │           ├── MultimodalConfirm.hs
    │   │   │   │   │   │   │           ├── NearbyBuses.hs
    │   │   │   │   │   │   │           ├── NearbyDrivers.hs
    │   │   │   │   │   │   │           ├── Places.hs
    │   │   │   │   │   │   │           ├── PriceBreakup.hs
    │   │   │   │   │   │   │           ├── RidePayment.hs
    │   │   │   │   │   │   │           ├── SocialLogin.hs
    │   │   │   │   │   │   │           ├── Sos.hs
    │   │   │   │   │   │   │           ├── TicketKapture.hs
    │   │   │   │   │   │   │           ├── TicketService.hs
    │   │   │   │   │   │   │           ├── TrackRoute.hs
    │   │   │   │   │   │   │           └── TriggerFCM.hs
    │   │   │   │   │   │   ├── Domain/
    │   │   │   │   │   │   │   └── Types/
    │   │   │   │   │   │   │       ├── AadhaarOtpReq.hs
    │   │   │   │   │   │   │       ├── AadhaarOtpVerify.hs
    │   │   │   │   │   │   │       ├── AadhaarVerification.hs
    │   │   │   │   │   │   │       ├── AppInstalls.hs
    │   │   │   │   │   │   │       ├── BBPS.hs
    │   │   │   │   │   │   │       ├── BBPSConfig.hs
    │   │   │   │   │   │   │       ├── BecknConfig.hs
    │   │   │   │   │   │   │       ├── BlackListOrg.hs
    │   │   │   │   │   │   │       ├── Booking.hs
    │   │   │   │   │   │   │       ├── BookingCancellationReason.hs
    │   │   │   │   │   │   │       ├── BookingLocation.hs
    │   │   │   │   │   │   │       ├── BookingPartiesLink.hs
    │   │   │   │   │   │   │       ├── BookingUpdateRequest.hs
    │   │   │   │   │   │   │       ├── BppDetails.hs
    │   │   │   │   │   │   │       ├── BusinessHour.hs
    │   │   │   │   │   │   │       ├── CallbackRequest.hs
    │   │   │   │   │   │   │       ├── CallStatus.hs
    │   │   │   │   │   │   │       ├── CancellationReason.hs
    │   │   │   │   │   │   │       ├── Client.hs
    │   │   │   │   │   │   │       ├── ClientPersonInfo.hs
    │   │   │   │   │   │   │       ├── DeletedPerson.hs
    │   │   │   │   │   │   │       ├── Disability.hs
    │   │   │   │   │   │   │       ├── DisabilityTranslation.hs
    │   │   │   │   │   │   │       ├── DraftTicketChange.hs
    │   │   │   │   │   │   │       ├── DraftTicketChangeHistory.hs
    │   │   │   │   │   │   │       ├── DriverOffer.hs
    │   │   │   │   │   │   │       ├── Estimate.hs
    │   │   │   │   │   │   │       ├── Exophone.hs
    │   │   │   │   │   │   │       ├── FareBreakup.hs
    │   │   │   │   │   │   │       ├── FeedbackForm.hs
    │   │   │   │   │   │   │       ├── FRFSConfig.hs
    │   │   │   │   │   │   │       ├── FRFSFarePolicy.hs
    │   │   │   │   │   │   │       ├── FRFSGtfsStageFare.hs
    │   │   │   │   │   │   │       ├── FRFSQuote.hs
    │   │   │   │   │   │   │       ├── FRFSRecon.hs
    │   │   │   │   │   │   │       ├── FRFSRouteFareProduct.hs
    │   │   │   │   │   │   │       ├── FRFSRouteStopStageFare.hs
    │   │   │   │   │   │   │       ├── FRFSSearch.hs
    │   │   │   │   │   │   │       ├── FRFSStageFare.hs
    │   │   │   │   │   │   │       ├── FRFSTicket.hs
    │   │   │   │   │   │   │       ├── FRFSTicketBooking.hs
    │   │   │   │   │   │   │       ├── FRFSTicketBookingPayment.hs
    │   │   │   │   │   │   │       ├── FRFSTicketDiscount.hs
    │   │   │   │   │   │   │       ├── FRFSVehicleServiceTier.hs
    │   │   │   │   │   │   │       ├── GTFSFeedInfo.hs
    │   │   │   │   │   │   │       ├── HotSpotConfig.hs
    │   │   │   │   │   │   │       ├── Insurance.hs
    │   │   │   │   │   │   │       ├── InsuranceConfig.hs
    │   │   │   │   │   │   │       ├── IntegratedBPPConfig.hs
    │   │   │   │   │   │   │       ├── InterCityDetails.hs
    │   │   │   │   │   │   │       ├── Issue.hs
    │   │   │   │   │   │   │       ├── Journey.hs
    │   │   │   │   │   │   │       ├── JourneyBooking.hs
    │   │   │   │   │   │   │       ├── JourneyFeedback.hs
    │   │   │   │   │   │   │       ├── JourneyLeg.hs
    │   │   │   │   │   │   │       ├── JourneyLegsFeedbacks.hs
    │   │   │   │   │   │   │       ├── JourneyRouteDetails.hs
    │   │   │   │   │   │   │       ├── Location.hs
    │   │   │   │   │   │   │       ├── LocationMapping.hs
    │   │   │   │   │   │   │       ├── Merchant.hs
    │   │   │   │   │   │   │       ├── MerchantConfig.hs
    │   │   │   │   │   │   │       ├── MerchantMessage.hs
    │   │   │   │   │   │   │       ├── MerchantOnboarding.hs
    │   │   │   │   │   │   │       ├── MerchantOnboardingStep.hs
    │   │   │   │   │   │   │       ├── MerchantOnboardingStepConfig.hs
    │   │   │   │   │   │   │       ├── MerchantOperatingCity.hs
    │   │   │   │   │   │   │       ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │   │       ├── MerchantPushNotification.hs
    │   │   │   │   │   │   │       ├── MerchantServiceConfig.hs
    │   │   │   │   │   │   │       ├── MerchantServiceUsageConfig.hs
    │   │   │   │   │   │   │       ├── MerchantState.hs
    │   │   │   │   │   │   │       ├── MultimodalPreferences.hs
    │   │   │   │   │   │   │       ├── NotificationSoundsConfig.hs
    │   │   │   │   │   │   │       ├── OnSearchEvent.hs
    │   │   │   │   │   │   │       ├── ParcelDetails.hs
    │   │   │   │   │   │   │       ├── PartnerOrganization.hs
    │   │   │   │   │   │   │       ├── PartnerOrgConfig.hs
    │   │   │   │   │   │   │       ├── PartnerOrgStation.hs
    │   │   │   │   │   │   │       ├── PassengerDetails.hs
    │   │   │   │   │   │   │       ├── PaymentCustomer.hs
    │   │   │   │   │   │   │       ├── PayoutConfig.hs
    │   │   │   │   │   │   │       ├── Person.hs
    │   │   │   │   │   │   │       ├── PersonDefaultEmergencyNumber.hs
    │   │   │   │   │   │   │       ├── PersonDisability.hs
    │   │   │   │   │   │   │       ├── PersonFlowStatus.hs
    │   │   │   │   │   │   │       ├── PersonStats.hs
    │   │   │   │   │   │   │       ├── PlaceBasedServiceConfig.hs
    │   │   │   │   │   │   │       ├── PlaceNameCache.hs
    │   │   │   │   │   │   │       ├── PopularLocation.hs
    │   │   │   │   │   │   │       ├── Quote.hs
    │   │   │   │   │   │   │       ├── QuoteBreakup.hs
    │   │   │   │   │   │   │       ├── Rating.hs
    │   │   │   │   │   │   │       ├── RecentLocation.hs
    │   │   │   │   │   │   │       ├── RegistrationToken.hs
    │   │   │   │   │   │   │       ├── RentalDetails.hs
    │   │   │   │   │   │   │       ├── Ride.hs
    │   │   │   │   │   │   │       ├── RiderConfig.hs
    │   │   │   │   │   │   │       ├── RideRelatedNotificationConfig.hs
    │   │   │   │   │   │   │       ├── Route.hs
    │   │   │   │   │   │   │       ├── RouteDetails.hs
    │   │   │   │   │   │   │       ├── RoutePolylines.hs
    │   │   │   │   │   │   │       ├── RouteStopCalender.hs
    │   │   │   │   │   │   │       ├── RouteStopFare.hs
    │   │   │   │   │   │   │       ├── RouteStopMapping.hs
    │   │   │   │   │   │   │       ├── RouteStopTimeTable.hs
    │   │   │   │   │   │   │       ├── RouteTripMapping.hs
    │   │   │   │   │   │   │       ├── SafetySettings.hs
    │   │   │   │   │   │   │       ├── SavedReqLocation.hs
    │   │   │   │   │   │   │       ├── SearchReqLocation.hs
    │   │   │   │   │   │   │       ├── SearchRequest.hs
    │   │   │   │   │   │   │       ├── SearchRequestPartiesLink.hs
    │   │   │   │   │   │   │       ├── SeatManagement.hs
    │   │   │   │   │   │   │       ├── ServiceCategory.hs
    │   │   │   │   │   │   │       ├── ServicePeopleCategory.hs
    │   │   │   │   │   │   │       ├── Sos.hs
    │   │   │   │   │   │   │       ├── SpecialOccasion.hs
    │   │   │   │   │   │   │       ├── SpecialZoneQuote.hs
    │   │   │   │   │   │   │       ├── Station.hs
    │   │   │   │   │   │   │       ├── StationsExtraInformation.hs
    │   │   │   │   │   │   │       ├── StopInformation.hs
    │   │   │   │   │   │   │       ├── TicketBooking.hs
    │   │   │   │   │   │   │       ├── TicketBookingPeopleCategory.hs
    │   │   │   │   │   │   │       ├── TicketBookingService.hs
    │   │   │   │   │   │   │       ├── TicketBookingServiceCategory.hs
    │   │   │   │   │   │   │       ├── TicketMerchantDetails.hs
    │   │   │   │   │   │   │       ├── TicketPlace.hs
    │   │   │   │   │   │   │       ├── TicketService.hs
    │   │   │   │   │   │   │       ├── TripTerms.hs
    │   │   │   │   │   │   │       ├── UiRiderConfig.hs
    │   │   │   │   │   │   │       ├── ValueAddNP.hs
    │   │   │   │   │   │   │       ├── VehicleConfig.hs
    │   │   │   │   │   │   │       ├── VehicleRouteMapping.hs
    │   │   │   │   │   │   │       ├── VendorSplitDetails.hs
    │   │   │   │   │   │   │       ├── WalkLegMultimodal.hs
    │   │   │   │   │   │   │       └── WhiteListOrg.hs
    │   │   │   │   │   │   └── Storage/
    │   │   │   │   │   │       ├── Beam/
    │   │   │   │   │   │       │   ├── AadhaarOtpReq.hs
    │   │   │   │   │   │       │   ├── AadhaarOtpVerify.hs
    │   │   │   │   │   │       │   ├── AadhaarVerification.hs
    │   │   │   │   │   │       │   ├── AmbulanceDetails.hs
    │   │   │   │   │   │       │   ├── AppInstalls.hs
    │   │   │   │   │   │       │   ├── BBPS.hs
    │   │   │   │   │   │       │   ├── BBPSConfig.hs
    │   │   │   │   │   │       │   ├── BecknConfig.hs
    │   │   │   │   │   │       │   ├── BlackListOrg.hs
    │   │   │   │   │   │       │   ├── Booking.hs
    │   │   │   │   │   │       │   ├── BookingCancellationReason.hs
    │   │   │   │   │   │       │   ├── BookingLocation.hs
    │   │   │   │   │   │       │   ├── BookingPartiesLink.hs
    │   │   │   │   │   │       │   ├── BookingUpdateRequest.hs
    │   │   │   │   │   │       │   ├── BppDetails.hs
    │   │   │   │   │   │       │   ├── BusinessHour.hs
    │   │   │   │   │   │       │   ├── CallbackRequest.hs
    │   │   │   │   │   │       │   ├── CallStatus.hs
    │   │   │   │   │   │       │   ├── CancellationReason.hs
    │   │   │   │   │   │       │   ├── Client.hs
    │   │   │   │   │   │       │   ├── ClientPersonInfo.hs
    │   │   │   │   │   │       │   ├── DeletedPerson.hs
    │   │   │   │   │   │       │   ├── Disability.hs
    │   │   │   │   │   │       │   ├── DisabilityTranslation.hs
    │   │   │   │   │   │       │   ├── DraftTicketChange.hs
    │   │   │   │   │   │       │   ├── DraftTicketChangeHistory.hs
    │   │   │   │   │   │       │   ├── DriverOffer.hs
    │   │   │   │   │   │       │   ├── Estimate.hs
    │   │   │   │   │   │       │   ├── Exophone.hs
    │   │   │   │   │   │       │   ├── FareBreakup.hs
    │   │   │   │   │   │       │   ├── FeedbackForm.hs
    │   │   │   │   │   │       │   ├── FRFSConfig.hs
    │   │   │   │   │   │       │   ├── FRFSFarePolicy.hs
    │   │   │   │   │   │       │   ├── FRFSGtfsStageFare.hs
    │   │   │   │   │   │       │   ├── FRFSQuote.hs
    │   │   │   │   │   │       │   ├── FRFSRecon.hs
    │   │   │   │   │   │       │   ├── FRFSRouteFareProduct.hs
    │   │   │   │   │   │       │   ├── FRFSRouteStopStageFare.hs
    │   │   │   │   │   │       │   ├── FRFSSearch.hs
    │   │   │   │   │   │       │   ├── FRFSStageFare.hs
    │   │   │   │   │   │       │   ├── FRFSTicket.hs
    │   │   │   │   │   │       │   ├── FRFSTicketBooking.hs
    │   │   │   │   │   │       │   ├── FRFSTicketBookingPayment.hs
    │   │   │   │   │   │       │   ├── FRFSTicketDiscount.hs
    │   │   │   │   │   │       │   ├── FRFSVehicleServiceTier.hs
    │   │   │   │   │   │       │   ├── GTFSFeedInfo.hs
    │   │   │   │   │   │       │   ├── HotSpotConfig.hs
    │   │   │   │   │   │       │   ├── Insurance.hs
    │   │   │   │   │   │       │   ├── InsuranceConfig.hs
    │   │   │   │   │   │       │   ├── IntegratedBPPConfig.hs
    │   │   │   │   │   │       │   ├── InterCityDetails.hs
    │   │   │   │   │   │       │   ├── Issue.hs
    │   │   │   │   │   │       │   ├── Journey.hs
    │   │   │   │   │   │       │   ├── JourneyBooking.hs
    │   │   │   │   │   │       │   ├── JourneyFeedback.hs
    │   │   │   │   │   │       │   ├── JourneyLeg.hs
    │   │   │   │   │   │       │   ├── JourneyLegsFeedbacks.hs
    │   │   │   │   │   │       │   ├── JourneyRouteDetails.hs
    │   │   │   │   │   │       │   ├── Location.hs
    │   │   │   │   │   │       │   ├── LocationMapping.hs
    │   │   │   │   │   │       │   ├── Merchant.hs
    │   │   │   │   │   │       │   ├── MerchantConfig.hs
    │   │   │   │   │   │       │   ├── MerchantMessage.hs
    │   │   │   │   │   │       │   ├── MerchantOnboarding.hs
    │   │   │   │   │   │       │   ├── MerchantOnboardingStep.hs
    │   │   │   │   │   │       │   ├── MerchantOnboardingStepConfig.hs
    │   │   │   │   │   │       │   ├── MerchantOperatingCity.hs
    │   │   │   │   │   │       │   ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │       │   ├── MerchantPushNotification.hs
    │   │   │   │   │   │       │   ├── MerchantServiceConfig.hs
    │   │   │   │   │   │       │   ├── MerchantServiceUsageConfig.hs
    │   │   │   │   │   │       │   ├── MerchantState.hs
    │   │   │   │   │   │       │   ├── MultimodalPreferences.hs
    │   │   │   │   │   │       │   ├── NotificationSoundsConfig.hs
    │   │   │   │   │   │       │   ├── OnSearchEvent.hs
    │   │   │   │   │   │       │   ├── ParcelDetails.hs
    │   │   │   │   │   │       │   ├── PartnerOrganization.hs
    │   │   │   │   │   │       │   ├── PartnerOrgConfig.hs
    │   │   │   │   │   │       │   ├── PartnerOrgStation.hs
    │   │   │   │   │   │       │   ├── PassengerDetails.hs
    │   │   │   │   │   │       │   ├── PaymentCustomer.hs
    │   │   │   │   │   │       │   ├── PayoutConfig.hs
    │   │   │   │   │   │       │   ├── Person.hs
    │   │   │   │   │   │       │   ├── PersonDefaultEmergencyNumber.hs
    │   │   │   │   │   │       │   ├── PersonDisability.hs
    │   │   │   │   │   │       │   ├── PersonStats.hs
    │   │   │   │   │   │       │   ├── PlaceBasedServiceConfig.hs
    │   │   │   │   │   │       │   ├── PlaceNameCache.hs
    │   │   │   │   │   │       │   ├── PopularLocation.hs
    │   │   │   │   │   │       │   ├── Quote.hs
    │   │   │   │   │   │       │   ├── QuoteBreakup.hs
    │   │   │   │   │   │       │   ├── Rating.hs
    │   │   │   │   │   │       │   ├── RecentLocation.hs
    │   │   │   │   │   │       │   ├── RegistrationToken.hs
    │   │   │   │   │   │       │   ├── RentalDetails.hs
    │   │   │   │   │   │       │   ├── Ride.hs
    │   │   │   │   │   │       │   ├── RiderConfig.hs
    │   │   │   │   │   │       │   ├── RideRelatedNotificationConfig.hs
    │   │   │   │   │   │       │   ├── Route.hs
    │   │   │   │   │   │       │   ├── RouteDetails.hs
    │   │   │   │   │   │       │   ├── RoutePolylines.hs
    │   │   │   │   │   │       │   ├── RouteStopCalender.hs
    │   │   │   │   │   │       │   ├── RouteStopFare.hs
    │   │   │   │   │   │       │   ├── RouteStopMapping.hs
    │   │   │   │   │   │       │   ├── RouteStopTimeTable.hs
    │   │   │   │   │   │       │   ├── RouteTripMapping.hs
    │   │   │   │   │   │       │   ├── SafetySettings.hs
    │   │   │   │   │   │       │   ├── SavedReqLocation.hs
    │   │   │   │   │   │       │   ├── SearchReqLocation.hs
    │   │   │   │   │   │       │   ├── SearchRequest.hs
    │   │   │   │   │   │       │   ├── SearchRequestPartiesLink.hs
    │   │   │   │   │   │       │   ├── SeatManagement.hs
    │   │   │   │   │   │       │   ├── ServiceCategory.hs
    │   │   │   │   │   │       │   ├── ServicePeopleCategory.hs
    │   │   │   │   │   │       │   ├── Sos.hs
    │   │   │   │   │   │       │   ├── SpecialOccasion.hs
    │   │   │   │   │   │       │   ├── SpecialZoneQuote.hs
    │   │   │   │   │   │       │   ├── Station.hs
    │   │   │   │   │   │       │   ├── StationsExtraInformation.hs
    │   │   │   │   │   │       │   ├── StopInformation.hs
    │   │   │   │   │   │       │   ├── TicketBooking.hs
    │   │   │   │   │   │       │   ├── TicketBookingPeopleCategory.hs
    │   │   │   │   │   │       │   ├── TicketBookingService.hs
    │   │   │   │   │   │       │   ├── TicketBookingServiceCategory.hs
    │   │   │   │   │   │       │   ├── TicketMerchantDetails.hs
    │   │   │   │   │   │       │   ├── TicketPlace.hs
    │   │   │   │   │   │       │   ├── TicketService.hs
    │   │   │   │   │   │       │   ├── TripTerms.hs
    │   │   │   │   │   │       │   ├── UiRiderConfig.hs
    │   │   │   │   │   │       │   ├── ValueAddNP.hs
    │   │   │   │   │   │       │   ├── VehicleConfig.hs
    │   │   │   │   │   │       │   ├── VehicleRouteMapping.hs
    │   │   │   │   │   │       │   ├── VendorSplitDetails.hs
    │   │   │   │   │   │       │   ├── WalkLegMultimodal.hs
    │   │   │   │   │   │       │   └── WhiteListOrg.hs
    │   │   │   │   │   │       ├── CachedQueries/
    │   │   │   │   │   │       │   ├── FeedbackForm.hs
    │   │   │   │   │   │       │   ├── InsuranceConfig.hs
    │   │   │   │   │   │       │   ├── IntegratedBPPConfig.hs
    │   │   │   │   │   │       │   └── SavedReqLocation.hs
    │   │   │   │   │   │       └── Queries/
    │   │   │   │   │   │           ├── AadhaarOtpReq.hs
    │   │   │   │   │   │           ├── AadhaarOtpVerify.hs
    │   │   │   │   │   │           ├── AadhaarVerification.hs
    │   │   │   │   │   │           ├── AppInstalls.hs
    │   │   │   │   │   │           ├── BBPS.hs
    │   │   │   │   │   │           ├── BBPSConfig.hs
    │   │   │   │   │   │           ├── BecknConfig.hs
    │   │   │   │   │   │           ├── BlackListOrg.hs
    │   │   │   │   │   │           ├── Booking.hs
    │   │   │   │   │   │           ├── BookingCancellationReason.hs
    │   │   │   │   │   │           ├── BookingLocation.hs
    │   │   │   │   │   │           ├── BookingPartiesLink.hs
    │   │   │   │   │   │           ├── BookingUpdateRequest.hs
    │   │   │   │   │   │           ├── BppDetails.hs
    │   │   │   │   │   │           ├── BusinessHour.hs
    │   │   │   │   │   │           ├── CallbackRequest.hs
    │   │   │   │   │   │           ├── CallStatus.hs
    │   │   │   │   │   │           ├── CancellationReason.hs
    │   │   │   │   │   │           ├── Client.hs
    │   │   │   │   │   │           ├── ClientPersonInfo.hs
    │   │   │   │   │   │           ├── DeletedPerson.hs
    │   │   │   │   │   │           ├── Disability.hs
    │   │   │   │   │   │           ├── DisabilityTranslation.hs
    │   │   │   │   │   │           ├── DraftTicketChange.hs
    │   │   │   │   │   │           ├── DraftTicketChangeHistory.hs
    │   │   │   │   │   │           ├── DriverOffer.hs
    │   │   │   │   │   │           ├── Estimate.hs
    │   │   │   │   │   │           ├── Exophone.hs
    │   │   │   │   │   │           ├── FareBreakup.hs
    │   │   │   │   │   │           ├── FeedbackForm.hs
    │   │   │   │   │   │           ├── FRFSConfig.hs
    │   │   │   │   │   │           ├── FRFSFarePolicy.hs
    │   │   │   │   │   │           ├── FRFSGtfsStageFare.hs
    │   │   │   │   │   │           ├── FRFSQuote.hs
    │   │   │   │   │   │           ├── FRFSRecon.hs
    │   │   │   │   │   │           ├── FRFSRouteFareProduct.hs
    │   │   │   │   │   │           ├── FRFSRouteStopStageFare.hs
    │   │   │   │   │   │           ├── FRFSSearch.hs
    │   │   │   │   │   │           ├── FRFSStageFare.hs
    │   │   │   │   │   │           ├── FRFSTicket.hs
    │   │   │   │   │   │           ├── FRFSTicketBooking.hs
    │   │   │   │   │   │           ├── FRFSTicketBookingPayment.hs
    │   │   │   │   │   │           ├── FRFSTicketDiscount.hs
    │   │   │   │   │   │           ├── FRFSVehicleServiceTier.hs
    │   │   │   │   │   │           ├── GTFSFeedInfo.hs
    │   │   │   │   │   │           ├── HotSpotConfig.hs
    │   │   │   │   │   │           ├── Insurance.hs
    │   │   │   │   │   │           ├── InsuranceConfig.hs
    │   │   │   │   │   │           ├── IntegratedBPPConfig.hs
    │   │   │   │   │   │           ├── InterCityDetails.hs
    │   │   │   │   │   │           ├── Issue.hs
    │   │   │   │   │   │           ├── Journey.hs
    │   │   │   │   │   │           ├── JourneyBooking.hs
    │   │   │   │   │   │           ├── JourneyFeedback.hs
    │   │   │   │   │   │           ├── JourneyLeg.hs
    │   │   │   │   │   │           ├── JourneyLegsFeedbacks.hs
    │   │   │   │   │   │           ├── JourneyRouteDetails.hs
    │   │   │   │   │   │           ├── Location.hs
    │   │   │   │   │   │           ├── LocationMapping.hs
    │   │   │   │   │   │           ├── Merchant.hs
    │   │   │   │   │   │           ├── MerchantConfig.hs
    │   │   │   │   │   │           ├── MerchantMessage.hs
    │   │   │   │   │   │           ├── MerchantOnboarding.hs
    │   │   │   │   │   │           ├── MerchantOnboardingStep.hs
    │   │   │   │   │   │           ├── MerchantOnboardingStepConfig.hs
    │   │   │   │   │   │           ├── MerchantOperatingCity.hs
    │   │   │   │   │   │           ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │           ├── MerchantPushNotification.hs
    │   │   │   │   │   │           ├── MerchantServiceConfig.hs
    │   │   │   │   │   │           ├── MerchantServiceUsageConfig.hs
    │   │   │   │   │   │           ├── MerchantState.hs
    │   │   │   │   │   │           ├── MultimodalPreferences.hs
    │   │   │   │   │   │           ├── NotificationSoundsConfig.hs
    │   │   │   │   │   │           ├── OnSearchEvent.hs
    │   │   │   │   │   │           ├── ParcelDetails.hs
    │   │   │   │   │   │           ├── PartnerOrganization.hs
    │   │   │   │   │   │           ├── PartnerOrgConfig.hs
    │   │   │   │   │   │           ├── PartnerOrgStation.hs
    │   │   │   │   │   │           ├── PassengerDetails.hs
    │   │   │   │   │   │           ├── PaymentCustomer.hs
    │   │   │   │   │   │           ├── PayoutConfig.hs
    │   │   │   │   │   │           ├── Person.hs
    │   │   │   │   │   │           ├── PersonDefaultEmergencyNumber.hs
    │   │   │   │   │   │           ├── PersonDisability.hs
    │   │   │   │   │   │           ├── PersonStats.hs
    │   │   │   │   │   │           ├── PlaceBasedServiceConfig.hs
    │   │   │   │   │   │           ├── PlaceNameCache.hs
    │   │   │   │   │   │           ├── PopularLocation.hs
    │   │   │   │   │   │           ├── Quote.hs
    │   │   │   │   │   │           ├── QuoteBreakup.hs
    │   │   │   │   │   │           ├── Rating.hs
    │   │   │   │   │   │           ├── RecentLocation.hs
    │   │   │   │   │   │           ├── RegistrationToken.hs
    │   │   │   │   │   │           ├── RentalDetails.hs
    │   │   │   │   │   │           ├── Ride.hs
    │   │   │   │   │   │           ├── RiderConfig.hs
    │   │   │   │   │   │           ├── RideRelatedNotificationConfig.hs
    │   │   │   │   │   │           ├── Route.hs
    │   │   │   │   │   │           ├── RouteDetails.hs
    │   │   │   │   │   │           ├── RoutePolylines.hs
    │   │   │   │   │   │           ├── RouteStopCalender.hs
    │   │   │   │   │   │           ├── RouteStopFare.hs
    │   │   │   │   │   │           ├── RouteStopMapping.hs
    │   │   │   │   │   │           ├── RouteStopTimeTable.hs
    │   │   │   │   │   │           ├── RouteTripMapping.hs
    │   │   │   │   │   │           ├── SafetySettings.hs
    │   │   │   │   │   │           ├── SavedReqLocation.hs
    │   │   │   │   │   │           ├── SearchReqLocation.hs
    │   │   │   │   │   │           ├── SearchRequest.hs
    │   │   │   │   │   │           ├── SearchRequestPartiesLink.hs
    │   │   │   │   │   │           ├── SeatManagement.hs
    │   │   │   │   │   │           ├── ServiceCategory.hs
    │   │   │   │   │   │           ├── ServicePeopleCategory.hs
    │   │   │   │   │   │           ├── Sos.hs
    │   │   │   │   │   │           ├── SpecialOccasion.hs
    │   │   │   │   │   │           ├── SpecialZoneQuote.hs
    │   │   │   │   │   │           ├── Station.hs
    │   │   │   │   │   │           ├── StationsExtraInformation.hs
    │   │   │   │   │   │           ├── StopInformation.hs
    │   │   │   │   │   │           ├── TicketBooking.hs
    │   │   │   │   │   │           ├── TicketBookingPeopleCategory.hs
    │   │   │   │   │   │           ├── TicketBookingService.hs
    │   │   │   │   │   │           ├── TicketBookingServiceCategory.hs
    │   │   │   │   │   │           ├── TicketMerchantDetails.hs
    │   │   │   │   │   │           ├── TicketPlace.hs
    │   │   │   │   │   │           ├── TicketService.hs
    │   │   │   │   │   │           ├── TripTerms.hs
    │   │   │   │   │   │           ├── UiRiderConfig.hs
    │   │   │   │   │   │           ├── ValueAddNP.hs
    │   │   │   │   │   │           ├── VehicleConfig.hs
    │   │   │   │   │   │           ├── VehicleRouteMapping.hs
    │   │   │   │   │   │           ├── VendorSplitDetails.hs
    │   │   │   │   │   │           ├── WalkLegMultimodal.hs
    │   │   │   │   │   │           ├── WhiteListOrg.hs
    │   │   │   │   │   │           └── OrphanInstances/
    │   │   │   │   │   │               ├── AppInstalls.hs
    │   │   │   │   │   │               ├── BBPS.hs
    │   │   │   │   │   │               ├── BecknConfig.hs
    │   │   │   │   │   │               ├── Booking.hs
    │   │   │   │   │   │               ├── BookingCancellationReason.hs
    │   │   │   │   │   │               ├── BookingPartiesLink.hs
    │   │   │   │   │   │               ├── BusinessHour.hs
    │   │   │   │   │   │               ├── CallStatus.hs
    │   │   │   │   │   │               ├── CancellationReason.hs
    │   │   │   │   │   │               ├── Disability.hs
    │   │   │   │   │   │               ├── Estimate.hs
    │   │   │   │   │   │               ├── Exophone.hs
    │   │   │   │   │   │               ├── FareBreakup.hs
    │   │   │   │   │   │               ├── FeedbackForm.hs
    │   │   │   │   │   │               ├── FRFSQuote.hs
    │   │   │   │   │   │               ├── FRFSSearch.hs
    │   │   │   │   │   │               ├── FRFSTicketBooking.hs
    │   │   │   │   │   │               ├── Issue.hs
    │   │   │   │   │   │               ├── Journey.hs
    │   │   │   │   │   │               ├── JourneyLeg.hs
    │   │   │   │   │   │               ├── LocationMapping.hs
    │   │   │   │   │   │               ├── Merchant.hs
    │   │   │   │   │   │               ├── MerchantPaymentMethod.hs
    │   │   │   │   │   │               ├── MerchantServiceConfig.hs
    │   │   │   │   │   │               ├── MerchantServiceUsageConfig.hs
    │   │   │   │   │   │               ├── PartnerOrganization.hs
    │   │   │   │   │   │               ├── PartnerOrgConfig.hs
    │   │   │   │   │   │               ├── Person.hs
    │   │   │   │   │   │               ├── PersonDefaultEmergencyNumber.hs
    │   │   │   │   │   │               ├── PersonStats.hs
    │   │   │   │   │   │               ├── PlaceBasedServiceConfig.hs
    │   │   │   │   │   │               ├── PopularLocation.hs
    │   │   │   │   │   │               ├── Quote.hs
    │   │   │   │   │   │               ├── Rating.hs
    │   │   │   │   │   │               ├── RecentLocation.hs
    │   │   │   │   │   │               ├── RegistrationToken.hs
    │   │   │   │   │   │               ├── Ride.hs
    │   │   │   │   │   │               ├── Route.hs
    │   │   │   │   │   │               ├── RoutePolylines.hs
    │   │   │   │   │   │               ├── SafetySettings.hs
    │   │   │   │   │   │               ├── SavedReqLocation.hs
    │   │   │   │   │   │               ├── SearchRequest.hs
    │   │   │   │   │   │               ├── ServiceCategory.hs
    │   │   │   │   │   │               ├── ServicePeopleCategory.hs
    │   │   │   │   │   │               ├── Station.hs
    │   │   │   │   │   │               ├── StationsExtraInformation.hs
    │   │   │   │   │   │               ├── TicketBooking.hs
    │   │   │   │   │   │               ├── TicketBookingPeopleCategory.hs
    │   │   │   │   │   │               ├── TicketBookingService.hs
    │   │   │   │   │   │               ├── TicketBookingServiceCategory.hs
    │   │   │   │   │   │               ├── TicketService.hs
    │   │   │   │   │   │               ├── UiRiderConfig.hs
    │   │   │   │   │   │               ├── VehicleRouteMapping.hs
    │   │   │   │   │   │               ├── WalkLegMultimodal.hs
    │   │   │   │   │   │               └── WhiteListOrg.hs
    │   │   │   │   │   └── test/
    │   │   │   │   │       ├── Main.hs
    │   │   │   │   │       └── FRFS/
    │   │   │   │   │           └── DirectQR.hs
    │   │   │   │   ├── Scheduler/
    │   │   │   │   │   ├── package.yaml
    │   │   │   │   │   ├── rider-app-scheduler.cabal
    │   │   │   │   │   ├── server/
    │   │   │   │   │   │   └── Main.hs
    │   │   │   │   │   └── src/
    │   │   │   │   │       ├── App.hs
    │   │   │   │   │       ├── Environment.hs
    │   │   │   │   │       └── Tools/
    │   │   │   │   │           └── Metrics.hs
    │   │   │   │   └── search-result-aggregator/
    │   │   │   │       ├── README.md
    │   │   │   │       ├── package.yaml
    │   │   │   │       ├── search-result-aggregator.cabal
    │   │   │   │       ├── server/
    │   │   │   │       │   └── Main.hs
    │   │   │   │       └── src/
    │   │   │   │           ├── App.hs
    │   │   │   │           ├── Environment.hs
    │   │   │   │           ├── Service/
    │   │   │   │           │   └── Runner.hs
    │   │   │   │           └── Tools/
    │   │   │   │               ├── Metrics.hs
    │   │   │   │               └── Streaming/
    │   │   │   │                   └── Kafka.hs
    │   │   │   └── rider-app-drainer/
    │   │   │       ├── package.yaml
    │   │   │       ├── rider-app-drainer.cabal
    │   │   │       ├── server/
    │   │   │       │   └── Main.hs
    │   │   │       └── src/
    │   │   │           ├── Constants.hs
    │   │   │           ├── Config/
    │   │   │           │   └── Env.hs
    │   │   │           ├── DBQuery/
    │   │   │           │   ├── Functions.hs
    │   │   │           │   └── Types.hs
    │   │   │           ├── DBSync/
    │   │   │           │   ├── Create.hs
    │   │   │           │   ├── DBSync.hs
    │   │   │           │   ├── Delete.hs
    │   │   │           │   └── Update.hs
    │   │   │           ├── Event/
    │   │   │           │   └── Event.hs
    │   │   │           ├── Types/
    │   │   │           │   ├── DBSync.hs
    │   │   │           │   ├── Event.hs
    │   │   │           │   └── DBSync/
    │   │   │           │       ├── Create.hs
    │   │   │           │       ├── DBModel.hs
    │   │   │           │       ├── Delete.hs
    │   │   │           │       └── Update.hs
    │   │   │           └── Utils/
    │   │   │               ├── Config.hs
    │   │   │               ├── Event.hs
    │   │   │               ├── Parse.hs
    │   │   │               ├── Redis.hs
    │   │   │               └── Utils.hs
    │   │   ├── safety-dashboard/
    │   │   │   ├── README.md
    │   │   │   ├── package.yaml
    │   │   │   ├── safety-dashboard.cabal
    │   │   │   ├── server/
    │   │   │   │   └── Main.hs
    │   │   │   ├── spec/
    │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   ├── API/
    │   │   │   │   │   ├── admin.yaml
    │   │   │   │   │   ├── flaggedCategory.yaml
    │   │   │   │   │   ├── merchant.yaml
    │   │   │   │   │   ├── notification.yaml
    │   │   │   │   │   ├── searchSuspect.yaml
    │   │   │   │   │   ├── suspect.yaml
    │   │   │   │   │   └── suspectFlagRequest.yaml
    │   │   │   │   └── Storage/
    │   │   │   │       └── suspect.yaml
    │   │   │   ├── src/
    │   │   │   │   ├── API.hs
    │   │   │   │   ├── App.hs
    │   │   │   │   ├── API/
    │   │   │   │   │   └── UI.hs
    │   │   │   │   ├── Domain/
    │   │   │   │   │   └── Action/
    │   │   │   │   │       └── UI/
    │   │   │   │   │           ├── Admin.hs
    │   │   │   │   │           ├── FlaggedCategory.hs
    │   │   │   │   │           ├── Merchant.hs
    │   │   │   │   │           ├── Notification.hs
    │   │   │   │   │           ├── SearchSuspect.hs
    │   │   │   │   │           ├── Suspect.hs
    │   │   │   │   │           ├── SuspectFlagRequest.hs
    │   │   │   │   │           └── Webhook.hs
    │   │   │   │   ├── Storage/
    │   │   │   │   │   ├── Beam/
    │   │   │   │   │   │   ├── CommonInstances.hs
    │   │   │   │   │   │   └── SystemConfigs.hs
    │   │   │   │   │   └── Queries/
    │   │   │   │   │       ├── FlaggedCategoryExtra.hs
    │   │   │   │   │       ├── SuspectExtra.hs
    │   │   │   │   │       ├── SuspectFlagRequestExtra.hs
    │   │   │   │   │       ├── SuspectStatusHistoryExtra.hs
    │   │   │   │   │       └── Transformers/
    │   │   │   │   │           └── Suspect.hs
    │   │   │   │   └── Tools/
    │   │   │   │       ├── Error.hs
    │   │   │   │       ├── Auth/
    │   │   │   │       │   └── Webhook.hs
    │   │   │   │       └── Beam/
    │   │   │   │           └── UtilsTH.hs
    │   │   │   └── src-read-only/
    │   │   │       ├── API/
    │   │   │       │   ├── Action/
    │   │   │       │   │   └── UI/
    │   │   │       │   │       ├── Admin.hs
    │   │   │       │   │       ├── FlaggedCategory.hs
    │   │   │       │   │       ├── Merchant.hs
    │   │   │       │   │       ├── Notification.hs
    │   │   │       │   │       ├── SearchSuspect.hs
    │   │   │       │   │       ├── Suspect.hs
    │   │   │       │   │       └── SuspectFlagRequest.hs
    │   │   │       │   └── Types/
    │   │   │       │       └── UI/
    │   │   │       │           ├── Admin.hs
    │   │   │       │           ├── FlaggedCategory.hs
    │   │   │       │           ├── Merchant.hs
    │   │   │       │           ├── Notification.hs
    │   │   │       │           ├── SearchSuspect.hs
    │   │   │       │           ├── Suspect.hs
    │   │   │       │           └── SuspectFlagRequest.hs
    │   │   │       ├── Domain/
    │   │   │       │   └── Types/
    │   │   │       │       ├── FlaggedCategory.hs
    │   │   │       │       ├── MerchantConfigs.hs
    │   │   │       │       ├── Notification.hs
    │   │   │       │       ├── PortalConfigs.hs
    │   │   │       │       ├── Suspect.hs
    │   │   │       │       ├── SuspectFlagRequest.hs
    │   │   │       │       ├── SuspectStatusChangeRequest.hs
    │   │   │       │       └── SuspectStatusHistory.hs
    │   │   │       └── Storage/
    │   │   │           ├── Beam/
    │   │   │           │   ├── FlaggedCategory.hs
    │   │   │           │   ├── MerchantConfigs.hs
    │   │   │           │   ├── Notification.hs
    │   │   │           │   ├── PortalConfigs.hs
    │   │   │           │   ├── Suspect.hs
    │   │   │           │   ├── SuspectFlagRequest.hs
    │   │   │           │   ├── SuspectStatusChangeRequest.hs
    │   │   │           │   └── SuspectStatusHistory.hs
    │   │   │           └── Queries/
    │   │   │               ├── FlaggedCategory.hs
    │   │   │               ├── MerchantConfigs.hs
    │   │   │               ├── Notification.hs
    │   │   │               ├── PortalConfigs.hs
    │   │   │               ├── Suspect.hs
    │   │   │               ├── SuspectFlagRequest.hs
    │   │   │               ├── SuspectStatusChangeRequest.hs
    │   │   │               ├── SuspectStatusHistory.hs
    │   │   │               └── OrphanInstances/
    │   │   │                   ├── FlaggedCategory.hs
    │   │   │                   ├── Suspect.hs
    │   │   │                   ├── SuspectFlagRequest.hs
    │   │   │                   └── SuspectStatusHistory.hs
    │   │   ├── sdk-event-pipeline/
    │   │   │   ├── package.yaml
    │   │   │   ├── sdk-event-pipeline.cabal
    │   │   │   ├── server/
    │   │   │   │   └── Main.hs
    │   │   │   └── src/
    │   │   │       ├── API.hs
    │   │   │       ├── App.hs
    │   │   │       ├── Environment.hs
    │   │   │       ├── Domain/
    │   │   │       │   ├── Action/
    │   │   │       │   │   └── SDKEvents.hs
    │   │   │       │   └── Types/
    │   │   │       │       └── SDKEvents.hs
    │   │   │       ├── External/
    │   │   │       │   ├── Flow.hs
    │   │   │       │   ├── Types.hs
    │   │   │       │   └── API/
    │   │   │       │       └── DriverAppAuth.hs
    │   │   │       └── Tools/
    │   │   │           └── Auth.hs
    │   │   ├── special-zone/
    │   │   │   ├── package.yaml
    │   │   │   ├── special-zone.cabal
    │   │   │   ├── server/
    │   │   │   │   └── Main.hs
    │   │   │   └── src/
    │   │   │       ├── App.hs
    │   │   │       ├── Environment.hs
    │   │   │       ├── API/
    │   │   │       │   ├── Handler.hs
    │   │   │       │   └── Types.hs
    │   │   │       ├── Domain/
    │   │   │       │   ├── Action/
    │   │   │       │   │   └── SpecialZone.hs
    │   │   │       │   └── Types/
    │   │   │       │       └── SpecialZone.hs
    │   │   │       ├── Storage/
    │   │   │       │   ├── Queries/
    │   │   │       │   │   ├── EntryExit.hs
    │   │   │       │   │   └── SpecialZone.hs
    │   │   │       │   └── Tabular/
    │   │   │       │       ├── EntryExit.hs
    │   │   │       │       └── SpecialZone.hs
    │   │   │       └── Tools/
    │   │   │           └── Auth.hs
    │   │   └── utils/
    │   │       ├── image-api-helper/
    │   │       │   ├── image-api-helper.cabal
    │   │       │   ├── package.yaml
    │   │       │   ├── server/
    │   │       │   │   └── Main.hs
    │   │       │   └── src/
    │   │       │       ├── App.hs
    │   │       │       ├── Environment.hs
    │   │       │       └── API/
    │   │       │           ├── DecodeFile.hs
    │   │       │           ├── EncodeFile.hs
    │   │       │           └── Total.hs
    │   │       └── route-extractor/
    │   │           ├── package.yaml
    │   │           ├── route-extractor.cabal
    │   │           ├── app/
    │   │           │   └── Main.hs
    │   │           └── src/
    │   │               ├── RenderTrack.hs
    │   │               └── RouteExtractor.hs
    │   ├── blacklist_geo_config/
    │   │   ├── howrah_railway_station.json
    │   │   ├── kolkata_railway_station.json
    │   │   ├── netaji_subash_chadra_bose_international_airport.json
    │   │   ├── santragachi_railway_station.json
    │   │   └── sealdah_railway_station.json
    │   ├── dev/
    │   │   ├── README.md
    │   │   ├── psql.sh
    │   │   ├── sql_dump.sh
    │   │   ├── stan.sh
    │   │   ├── clickhouse/
    │   │   │   ├── local-testing-data/
    │   │   │   │   ├── atlas-driver-offer-bpp.sql
    │   │   │   │   └── atlas-kafka.sql
    │   │   │   └── sql-seed/
    │   │   │       ├── atlas-driver-offer-bpp-seed.sql
    │   │   │       └── atlas-kafka-seed.sql
    │   │   ├── flatten-migrations/
    │   │   │   ├── README.md
    │   │   │   ├── clean.sh
    │   │   │   ├── diff_all_migrations.sh
    │   │   │   ├── fetch_inserts.sh
    │   │   │   ├── flatten.sh
    │   │   │   ├── flatten_all.sh
    │   │   │   ├── get_all_migrations.sh
    │   │   │   ├── get_migrations.sh
    │   │   │   ├── ordered_schema_tables.sql
    │   │   │   ├── remove_test_data.sh
    │   │   │   ├── restore_test_data.sh
    │   │   │   ├── return_migrations.sh
    │   │   │   ├── schema_info.sh
    │   │   │   ├── store_extra_migrations.sh
    │   │   │   └── tables_to_insert_fetch.sh
    │   │   ├── grafana/
    │   │   │   ├── config.ini
    │   │   │   ├── dashboards/
    │   │   │   │   ├── beckn_bap_metrics.json
    │   │   │   │   ├── beckn_bpp_metrics.json
    │   │   │   │   ├── beckn_metrics.json
    │   │   │   │   └── ghc_stats.json
    │   │   │   └── provisioning/
    │   │   │       ├── dashboards/
    │   │   │       │   └── config.yml
    │   │   │       └── datasources/
    │   │   │           └── config.yml
    │   │   ├── load-test/
    │   │   │   └── script.js
    │   │   ├── local-testing-data/
    │   │   │   ├── mock-registry.sql
    │   │   │   ├── person-location.sql
    │   │   │   ├── provider-dashboard.sql
    │   │   │   ├── public-transport-rider-platform.sql
    │   │   │   ├── rider-dashboard.sql
    │   │   │   ├── safety-dashboard.sql
    │   │   │   └── special-zone.sql
    │   │   ├── migrations/
    │   │   │   ├── run_migrations.sh
    │   │   │   ├── dynamic-offer-driver-app/
    │   │   │   │   ├── 0000-db-init.sql
    │   │   │   │   ├── 0001-local-testing-data.sql
    │   │   │   │   ├── 0003-fare.sql
    │   │   │   │   ├── 0005-VehicleRegistrationCert.sql
    │   │   │   │   ├── 0006-add-registered-to-person.sql
    │   │   │   │   ├── 0007-init-confirm.sql
    │   │   │   │   ├── 0009-add-table-beckn-request.sql
    │   │   │   │   ├── 0010-drivers-and-vehicles-always-linked.sql
    │   │   │   │   ├── 0011-on-update-flow.sql
    │   │   │   │   ├── 0014-change-beckn-request-field-type.sql
    │   │   │   │   ├── 0016-fare-policy.sql
    │   │   │   │   ├── 0018-renamed-auto-to-auto-rickshaw.sql
    │   │   │   │   ├── 0020-drop-not-null-constraints-feedbackDetails.sql
    │   │   │   │   ├── 0023-fare-rounding.sql
    │   │   │   │   ├── 0026-coordinatesCalculatedAt-field.sql
    │   │   │   │   ├── 0027-fare-policy-column-renamed.sql
    │   │   │   │   ├── 0032-adding-full-address-column-to-search-request-location.sql
    │   │   │   │   ├── 0033-remove-registered-from-person.sql
    │   │   │   │   ├── 0036-pickup-drop-loc-thresholds.sql
    │   │   │   │   ├── 0038-add-hash-documents.sql
    │   │   │   │   ├── 0041-optimize-tables.sql
    │   │   │   │   ├── 0042-drop-fullame--firstname-notnull.sql
    │   │   │   │   ├── 0043-trip-constraints.sql
    │   │   │   │   ├── 0045-geomerty.sql
    │   │   │   │   ├── 0047-expanding-karnataka-geometry.sql
    │   │   │   │   ├── 0050-rename-org-to-merchant.sql
    │   │   │   │   ├── 0051-merchant-maps-configs.sql
    │   │   │   │   ├── 0052-move-geofencing-to-db.sql
    │   │   │   │   ├── 0053-add-status-in-srfd.sql
    │   │   │   │   ├── 0054-merchant-config-changes.sql
    │   │   │   │   ├── 0056-add-ride-details.sql
    │   │   │   │   ├── 0057-add-versions-to-person.sql
    │   │   │   │   ├── 0060-search-request-driver-straight-line-distance-column-added.sql
    │   │   │   │   ├── 0061-add-fcm-configs-to-transporter-config.sql
    │   │   │   │   ├── 0063-merchant-mmi-config.sql
    │   │   │   │   ├── 0064-not-null-merchat-id-in-person.sql
    │   │   │   │   ├── 0065-job-scheduler.sql
    │   │   │   │   ├── 0067-add-phone-number-as-text-to-person.sql
    │   │   │   │   ├── 0068-added-driver-availability-table.sql
    │   │   │   │   ├── 0071-update-extra-fee-column.sql
    │   │   │   │   ├── 0074-add-driver-availability-in-ride.sql
    │   │   │   │   ├── 0075-rename-job-table.sql
    │   │   │   │   ├── 0080-merchant-sms-config.sql
    │   │   │   │   ├── 0081-add-waiting-threshold-in-fp.sql
    │   │   │   │   ├── 0087-create-driver-flow-status-table.sql
    │   │   │   │   ├── 0091-adding-uniq-constaint-on-driver-bucket.sql
    │   │   │   │   ├── 0092-ride-fare-parameters-id.sql
    │   │   │   │   ├── 0094-added-whatsapp-config.sql
    │   │   │   │   ├── 0096-added-notification-tables.sql
    │   │   │   │   ├── 0097-restricted-extra-fare.sql
    │   │   │   │   ├── 0098-refactored-minTripDistanceValue-in-restrictedFare.sql
    │   │   │   │   ├── 0102-add-referral-link-password-to-transporterConfig.sql
    │   │   │   │   ├── 0106-driver-referral-eligible-at.sql
    │   │   │   │   ├── 0107-add-deadKmFare-in-FP.sql
    │   │   │   │   ├── 0108-merchant-message-table.sql
    │   │   │   │   ├── 0112-add-exo-phones.sql
    │   │   │   │   ├── 0113-add-merchantid-in-constraints.sql
    │   │   │   │   ├── 0115-added-special-location-table.sql
    │   │   │   │   ├── 0117-alternate-phone-number.sql
    │   │   │   │   ├── 0118-merchant-multiple-exoPhones.sql
    │   │   │   │   ├── 0119-rename-jobs-table.sql
    │   │   │   │   ├── 0120-added-issue-tables.sql
    │   │   │   │   ├── 0121-merchant-verification-configs.sql
    │   │   │   │   ├── 0122-special-zone.sql
    │   │   │   │   ├── 0125-updated-transporter-config-table.sql
    │   │   │   │   ├── 0126-driver-pool-config-changes.sql
    │   │   │   │   ├── 0127-exophone-table-created.sql
    │   │   │   │   ├── 0128-add-onboarding-document-configs-and-add-columns-in-transporter-config.sql
    │   │   │   │   ├── 0129-add-search-repeat-counter-field.sql
    │   │   │   │   ├── 0131-added-shardId-in-jobs.sql
    │   │   │   │   ├── 0132-alter-transporter-config.sql
    │   │   │   │   ├── 0133-change-issue-table-properties.sql
    │   │   │   │   ├── 0134-added-slab-fare-policy.sql
    │   │   │   │   ├── 0135-added-fare-policy-type-in-fare-params.sql
    │   │   │   │   ├── 0136-exophone-initiate-call.sql
    │   │   │   │   ├── 0137-add-fare-policy-type-column-merchant.sql
    │   │   │   │   ├── 0140-onboarding-config-timestamps.sql
    │   │   │   │   ├── 0141-update-send-otp-template.sql
    │   │   │   │   ├── 0144-update-issue-category-logo-url.sql
    │   │   │   │   ├── 0147-add-extra-fee.sql
    │   │   │   │   ├── 0148-reuse-old-estimate-in-repetition.sql
    │   │   │   │   ├── 0154-add-door-in-location.sql
    │   │   │   │   ├── 0156-add-colums-to-driverStats.sql
    │   │   │   │   ├── 0158-refactor-fare-policy.sql
    │   │   │   │   ├── 0159-fare-policy-per-extra-km-rate-sections.sql
    │   │   │   │   ├── 0161-refactor-fare-policy-driver-extra-fee-bounds.sql
    │   │   │   │   ├── 0163-add-merchantId-in-registrationToken-table.sql
    │   │   │   │   ├── 0164-add-night-shift-multiplier-in-fp.sql
    │   │   │   │   ├── 0165-added-driver-stats-and-config.sql
    │   │   │   │   ├── 0166-move-free-waiting-time.sql
    │   │   │   │   ├── 0168-reuse-old-estimate-in-repetition.sql
    │   │   │   │   ├── 0169-add-merchant-id-column-location-table.sql
    │   │   │   │   ├── 0171-add_search_try_repeat_type_field.sql
    │   │   │   │   ├── 0172-add-FPSlab-platform-fee.sql
    │   │   │   │   ├── 0173-special-location-tag.sql
    │   │   │   │   ├── 0175-add-merchantId-to-tables.sql
    │   │   │   │   ├── 0178-add-of-locks-to-driver-information.sql
    │   │   │   │   ├── 0180-merchant-payment-method.sql
    │   │   │   │   ├── 0181-juspay-payments-integration.sql
    │   │   │   │   ├── 0183-add-place-name-cache.sql
    │   │   │   │   ├── 0184-add-aadhaar-verification.sql
    │   │   │   │   ├── 0185-onRide-inconsistency-resolve.sql
    │   │   │   │   ├── 0186-add-aadhar-verified-in-merchant.sql
    │   │   │   │   ├── 0187_txn_uuid_optional.sql
    │   │   │   │   ├── 0189_split_platform_fee.sql
    │   │   │   │   ├── 0190-mall-category.sql
    │   │   │   │   ├── 0191-driver-fee.sql
    │   │   │   │   ├── 0192-transporter-config-update.sql
    │   │   │   │   ├── 0193-add-vehicle-variant-in-config.sql
    │   │   │   │   ├── 0194-remove-driver-location-foriegn-key.sql
    │   │   │   │   ├── 0195-added-juspay-order-id.sql
    │   │   │   │   ├── 0196-add-missing-fields-in-payment-order.sql
    │   │   │   │   ├── 0198-add-address-in-gates-special-location.sql
    │   │   │   │   ├── 0200-subscriber-metadata.sql
    │   │   │   │   ├── 0203-added-payment-order-merchant-id.sql
    │   │   │   │   ├── 0205-add-feedback-badges.sql
    │   │   │   │   ├── 0207-roadblock-category.sql
    │   │   │   │   ├── 0208-add-aadhaar-hash.sql
    │   │   │   │   ├── 0211-place-id-update.sql
    │   │   │   │   ├── 0212-add-feedbackform.sql
    │   │   │   │   ├── 0215-driver-block-reason.sql
    │   │   │   │   ├── 0216-payment-merchant-id-updated.sql
    │   │   │   │   ├── 0217-drop-aadhaar-flag-in-merchant.sql
    │   │   │   │   ├── 0218-add-isrcactive-flag.sql
    │   │   │   │   ├── 0221-add-gupshup-sms-config.sql
    │   │   │   │   ├── 0222-add-mediaId-to-person-and-add-fvConfigs.sql
    │   │   │   │   ├── 0224-add-new-messages-for-yatri-sathi.sql
    │   │   │   │   ├── 0226-subscription-plans.sql
    │   │   │   │   ├── 0227-payment-link-message.sql
    │   │   │   │   ├── 0228-plan-translations.sql
    │   │   │   │   ├── 0229-add-ticket-config.sql
    │   │   │   │   ├── 0231-adding-country-to-merchant-table.sql
    │   │   │   │   ├── 0232-update-fare-related-to-fare.sql
    │   │   │   │   ├── 0237-add-kioskLocations.sql
    │   │   │   │   ├── 0238-add-kiosk-location-translations.sql
    │   │   │   │   ├── 0240-added-parent-job-id.sql
    │   │   │   │   ├── 0242-added-table-GoHomeConfig.sql
    │   │   │   │   ├── 0243-add-call-service.sql
    │   │   │   │   ├── 0247-adding-aadhaar-image-and-aadhaar-image-resize-columns.sql
    │   │   │   │   ├── 0249-add-mandate-payment-flow-and-bank-error.sql
    │   │   │   │   ├── 0252-add-transporter-config-and-df-cols.sql
    │   │   │   │   ├── 0254-driver-id-in-invoice.sql
    │   │   │   │   ├── 0256-make-columns-in-payment-order-as-null.sql
    │   │   │   │   ├── 0257-backfill-offer-plan-details.sql
    │   │   │   │   ├── 0263-payment-nudge.sql
    │   │   │   │   ├── 0265-adding-location-and-location-mapping-tables.sql
    │   │   │   │   ├── 0267-drop-foreign-constraints.sql
    │   │   │   │   ├── 0268-volunteer-table.sql
    │   │   │   │   ├── 0271-backfill-rating.sql
    │   │   │   │   ├── 0273-update-version-to-latest.sql
    │   │   │   │   ├── 0275-add-fleet-driver-association.sql
    │   │   │   │   ├── 0276-add-time-base-charge-to-farepolicy.sql
    │   │   │   │   ├── 0277-add-averageVechileSpeed-to-transport-config.sql
    │   │   │   │   ├── 0278-populate_collected_at.sql
    │   │   │   │   ├── 0279-indexing-driver-fee.sql
    │   │   │   │   ├── 0285-dailyStats-table.sql
    │   │   │   │   ├── 0290-adding-new-issue-flow-related-data.sql
    │   │   │   │   ├── 0291-add-new-columns-in-merchant-overlay.sql
    │   │   │   │   ├── 0292-update-chats-in-issue-report.sql
    │   │   │   │   ├── 0293-add-columns-to-merchant-message-table.sql
    │   │   │   │   ├── 0294-merchant-operating-city-id-related-changes.sql
    │   │   │   │   ├── 0295-changed-rc-number-prefix.sql
    │   │   │   │   ├── 0296-adding-merchantOpCityId-to-fare-product.sql
    │   │   │   │   ├── 0297-adding-city-to-geometry.sql
    │   │   │   │   ├── 0298-ys-driver-fee-fix-migrations.sql
    │   │   │   │   ├── 0299-add-retarget-retries-feilds.sql
    │   │   │   │   ├── 0300-adding-priority-to-issue-category.sql
    │   │   │   │   ├── 0302-add-translated-lang.sql
    │   │   │   │   ├── 0307-coins-config.sql
    │   │   │   │   ├── 0308-update-column-type-of-driver-stats.sql
    │   │   │   │   ├── 0309-enable-yatri-saath-in-subscription-flow.sql
    │   │   │   │   ├── 0311-ys-merger-configs.sql
    │   │   │   │   ├── 0314-slab-fare-platform-charge.sql
    │   │   │   │   ├── 0318-ys-transporter-config-update.sql
    │   │   │   │   ├── 0320-cancellationDues-column-in-fareparams.sql
    │   │   │   │   ├── 0321-ys-query-fix.sql
    │   │   │   │   ├── 0324-update_secondary_actions_column_type_in_merchant_overlay.sql
    │   │   │   │   ├── 0327-added-kv-configs-table-and-default-values.sql
    │   │   │   │   ├── 0329-adding-merchantOpCityId-to-IssueReport.sql
    │   │   │   │   ├── 0330-backfilling-in-driver-info.sql
    │   │   │   │   ├── 0332-source-down-retry.sql
    │   │   │   │   ├── 0333-add-column-bulkuploadtitle-coin-history.sql
    │   │   │   │   ├── 0336-add-updatedAt-to-coin-history.sql
    │   │   │   │   ├── 0337-add-error-message-translations-table.sql
    │   │   │   │   ├── 0339-added-merchant-operating-city-in-driver-infromation.sql
    │   │   │   │   ├── 0340-add-search-req-to-driver-column.sql
    │   │   │   │   ├── 0341-coin-notification-translation.sql
    │   │   │   │   ├── 0342-adding-short-Id-to-IssueReport.sql
    │   │   │   │   ├── 0343-fix-value-in-translations.sql
    │   │   │   │   ├── 0344-rental-refactoring.sql
    │   │   │   │   ├── 0347-whitelisting-and-blacklisting.sql
    │   │   │   │   ├── 0349-inter-city.sql
    │   │   │   │   ├── 0350-whitelist-datatype.sql
    │   │   │   │   ├── 0351-driver-pool-config.sql
    │   │   │   │   ├── 0352-added-govtData-rc-table.sql
    │   │   │   │   ├── 0354-added-gate-into.sql
    │   │   │   │   ├── 0356-subscription-refactoring.sql
    │   │   │   │   ├── 0357-fare-params-rental-cols.sql
    │   │   │   │   ├── 0358-ondc-interoperability.sql
    │   │   │   │   ├── 0359-update-merchant-payment-method.sql
    │   │   │   │   ├── 0361-adds-value-add-np.sql
    │   │   │   │   ├── 0362-add-merchant-operating-city-in-notification-table.sql
    │   │   │   │   ├── 0365-beckn-config-mobility.sql
    │   │   │   │   ├── 0366-fake-otp-numbers.sql
    │   │   │   │   ├── 0368-fix-rename-column.sql
    │   │   │   │   ├── 0370-added txnId-in-payment-transaction copy.sql
    │   │   │   │   ├── 0372-update-grpc-config.sql
    │   │   │   │   ├── 0376-drop-unique_constraint_for_rc_table.sql
    │   │   │   │   ├── 0378-add-transaction-id-index-for-booking.sql
    │   │   │   │   ├── 0380-document-verification-config-data.sql
    │   │   │   │   ├── 0381-driver-onboarding-v2.sql
    │   │   │   │   ├── 0382-vehicle-service-tier.sql
    │   │   │   │   ├── 0383-toll-charge-addition.sql
    │   │   │   │   ├── 0384-add-enabled-variant-for-subscription.sql
    │   │   │   │   ├── 0386-congestion-charges.sql
    │   │   │   │   ├── 0387-add-safety-webhook-auth-token.sql
    │   │   │   │   ├── 0389-add-coin-translation-in-translation.sql
    │   │   │   │   ├── 0390-onboarding-document-language-configuration.sql
    │   │   │   │   ├── 0391-add-dl-verification.sql
    │   │   │   │   ├── 0393-adding-updatedAt-to-gateInfo.sql
    │   │   │   │   ├── 0395-book-any-feature.sql
    │   │   │   │   ├── 0396-added-placeNameCache-Expiry.sql
    │   │   │   │   ├── 0399-parking-charge.sql
    │   │   │   │   ├── 0401-add-average-speed-for-bike.sql
    │   │   │   │   ├── 0402-ac-checks.sql
    │   │   │   │   ├── 0404-blocked-route-toll-names.sql
    │   │   │   │   ├── 0405-create-driver-leaderBoard_Configs.sql
    │   │   │   │   ├── 0406-enable-rental-sz.sql
    │   │   │   │   ├── 0407-adding-kaptureQueue-to-transporterConfig.sql
    │   │   │   │   ├── 0409-adds-igm-category.sql
    │   │   │   │   ├── 0410-vehicle-details.sql
    │   │   │   │   ├── 0411-currencies.sql
    │   │   │   │   ├── 0413-transporter-config-type-email-otp-type-fix.sql
    │   │   │   │   ├── 0414-add-currency-in-coin-purchase-history.sql
    │   │   │   │   ├── 0415-auto-selected-vehicle-variant.sql
    │   │   │   │   ├── 0416-added-HyperVerge-Config.sql
    │   │   │   │   ├── 0419-rename-columns-to-tables.sql
    │   │   │   │   ├── 0420-indexes-on-imageId.sql
    │   │   │   │   ├── 0421-added-associatited-for-fleet.sql
    │   │   │   │   ├── 0422-air-conditioned-score-vst.sql
    │   │   │   │   ├── 0423-db-backfill.sql
    │   │   │   │   ├── 0424-add-merchantOpCityId-to-special-location-priority.sql
    │   │   │   │   ├── 0425-intercity-fare-policy.sql
    │   │   │   │   ├── 0427-add-waiting-charge-rentals.sql
    │   │   │   │   ├── 0428-add-bike-variant-onboarding-configs.sql
    │   │   │   │   ├── 0429-adding-avg-speed-in-transporter-config.sql
    │   │   │   │   ├── 0430-fleet-owner-info.sql
    │   │   │   │   ├── 0431-pickup-charge-in-rental.sql
    │   │   │   │   ├── 0433-distance-units-bpp.sql
    │   │   │   │   ├── 0434-ambulance.sql
    │   │   │   │   ├── 0435-add-message-for-fleet-join-otp.sql
    │   │   │   │   ├── 0436-doc-verification.sql
    │   │   │   │   ├── 0437-add-notification-config-values.sql
    │   │   │   │   ├── 0438-Hyperverge-onboarding-sdk-integration.sql
    │   │   │   │   ├── 0439-BackFill-AadharVerification-to-AadhaarCard.sql
    │   │   │   │   ├── 0440-adding-index-on-aadhaarHash-in-aadhaarCard.sql
    │   │   │   │   ├── 0441-special-zone-hybrid-flow.sql
    │   │   │   │   ├── 0442-crosscities.sql
    │   │   │   │   ├── 0443-add-download-app-key-in-message.sql
    │   │   │   │   ├── 0444-checkr-integration.sql
    │   │   │   │   ├── 0445-service-provider-payment-order.sql
    │   │   │   │   ├── 0446-updates-to-toll-crossed-push-notification.sql
    │   │   │   │   ├── 0447-avg-speed-black.sql
    │   │   │   │   ├── 0448-add-two-and-five-ride-events-in-coins.sql
    │   │   │   │   ├── 0449-ac-checks-translations.sql
    │   │   │   │   ├── 0450-enabling-toll-crossed-notifications.sql
    │   │   │   │   ├── 0452-verification-pending-on-user-input.sql
    │   │   │   │   ├── 0453-edit-pickup-overlay-fix.sql
    │   │   │   │   ├── 0455-added-HyperVerge-Verification-Config.sql
    │   │   │   │   ├── 0456-update-ac-tiers.sql
    │   │   │   │   ├── 0457-update-ac-tiers.sql
    │   │   │   │   ├── 0458-ac-checks-ambulance.sql
    │   │   │   │   ├── 0459-aading-to-panNumberHash-pacCard.sql
    │   │   │   │   ├── 0460-fixing-merchantId-issue-queries.sql
    │   │   │   │   ├── 0461-time-based-farepolicy.sql
    │   │   │   │   ├── 0462-avg-speed-suv-plus.sql
    │   │   │   │   ├── 0463-updare-inter-city-fare-policy.sql
    │   │   │   │   ├── 0464-issue-flow-update-queries.sql
    │   │   │   │   ├── 0465-Gullak-tokenization-config.sql
    │   │   │   │   ├── 0468-adding-twillio-sms-configs.sql
    │   │   │   │   ├── 0470-ambulance-fare-policy.sql
    │   │   │   │   ├── 0471-fixing-issue-message-foreign-key-constraints.sql
    │   │   │   │   ├── 0472-correct-kolkata-location.sql
    │   │   │   │   ├── 0473-update-merchant-service-usage-and-transporter-config.sql
    │   │   │   │   ├── 0474-adding-new-order-DVC-latest-figma.sql
    │   │   │   │   ├── 0475-add-stripe-fields-in-transactions.sql
    │   │   │   │   ├── 0476-add-new-message-to-pushnotification.sql
    │   │   │   │   ├── 0477-cancellation-fare-policy.sql
    │   │   │   │   ├── 0479-one-time-deposit-message-key.sql
    │   │   │   │   ├── 0480-add-ten-ride-events-in-coins.sql
    │   │   │   │   ├── 0481-added-google-route-merchant-service-config.sql
    │   │   │   │   ├── 0482-message-key-fix-one-time-security-deposit.sql
    │   │   │   │   ├── 0483-added-places-api-merchant-service-config.sql
    │   │   │   │   ├── 0484-indexing-on-rider-details.sql
    │   │   │   │   ├── 0485-added-pn-and-payout-vpa-status.sql
    │   │   │   │   ├── 0486-Adding-Index-To-Image-personId-docType.sql
    │   │   │   │   ├── 0487-driver-dynamic-pool-logic.sql
    │   │   │   │   ├── 0490-intercity-allowance-in-mins.sql
    │   │   │   │   ├── 0492-move-charges-at-fare-policy-level.sql
    │   │   │   │   ├── 0493-whiteListOrg-merchant-and-city-level.sql
    │   │   │   │   ├── 0494-add-gat-type-in-gate-info.sql
    │   │   │   │   ├── 0495-pricing-slabs.sql
    │   │   │   │   ├── 0496-ambualnce-base-fare.sql
    │   │   │   │   ├── 0499-twillio-sdk-config.sql
    │   │   │   │   ├── 0500-delivery.sql
    │   │   │   │   ├── 0502-add-entity_id-coin-history-table.sql
    │   │   │   │   ├── 0503-added-pn-for-referral.sql
    │   │   │   │   ├── 0504-rental-and-intercity-pricing-slabs.sql
    │   │   │   │   ├── 0505-delivery-image.sql
    │   │   │   │   ├── 0506-updating-backward-compatibility-flag-for-old-apks.sql
    │   │   │   │   ├── 0507-adds-igm-issue-id-to-issue-report.sql
    │   │   │   │   ├── 0508-kaal-chakra-user.sql
    │   │   │   │   ├── 0509-subscription-refactoring-2.sql
    │   │   │   │   ├── 0511-demand-hotspots.sql
    │   │   │   │   ├── 0512-update-pricing-slabs.sql
    │   │   │   │   ├── 0513-coin-x-ride-event.sql
    │   │   │   │   ├── 0514-fare-policy-stop-charges.sql
    │   │   │   │   ├── 0515-added-congestion-in-fareparameters.sql
    │   │   │   │   ├── 0516-adding-allowedRideStatuses-to-issue-category.sql
    │   │   │   │   ├── 0517-translations-for-nudge-and-unblock.sql
    │   │   │   │   ├── 0520-more-merchant-pn.sql
    │   │   │   │   ├── 0521-parcel-delivery-pn.sql
    │   │   │   │   ├── 0522-x-rides-coin-event.sql
    │   │   │   │   ├── 0523-arrival-time-buffer.sql
    │   │   │   │   ├── 0524-add-unique-constraint-to-Overlay-and-merchant-message.sql
    │   │   │   │   ├── 0525-nuding-and-blocking-configs.sql
    │   │   │   │   ├── 0535-merchant-pn-for-driver-unblock.sql
    │   │   │   │   ├── 0536-add-created-and-updated-at-to-issue-config.sql
    │   │   │   │   ├── 0537-driver-block.sql
    │   │   │   │   ├── 0538-add-table-igm-issue.sql
    │   │   │   │   ├── 0539-add-table-igm-config.sql
    │   │   │   │   ├── 0540-adds-igm-config.sql
    │   │   │   │   ├── 0541-update-vehicle-category-vehicleServiceTier.sql
    │   │   │   │   ├── 0542-vehicle-category-coins.sql
    │   │   │   │   ├── 0543-adding-Indexes-to-Hv-sdk-logs.sql
    │   │   │   │   ├── 0544-subscription-changes.sql
    │   │   │   │   ├── 0545-add-column-specialLocation.sql
    │   │   │   │   ├── 0546-vendor-split-details.sql
    │   │   │   │   ├── 0550-create-index-on-burt.sql
    │   │   │   │   ├── 0560-set-valid-cancellation-tags-stats-start-date.sql
    │   │   │   │   ├── 0561-add-vehicle-category-in-coin-purchase-history.sql
    │   │   │   │   ├── 0562-special-location-type.sql
    │   │   │   │   ├── 0563-hyperverge-rc-integration.sql
    │   │   │   │   ├── 0564-insert-llm-config-azure-openai.sql
    │   │   │   │   ├── 0565-stop-detection-translations-and-configs.sql
    │   │   │   │   ├── 0566-updated-kv-configs.sql
    │   │   │   │   ├── 0567-metro-warrior-coin-translations.sql
    │   │   │   │   ├── 0568-metro-warrior-notification.sql
    │   │   │   │   ├── 0569-add-column-mocid.sql
    │   │   │   │   ├── 0570-extra-fare-mitigation.sql
    │   │   │   │   ├── 0571-metrowarrior-coin-events.sql
    │   │   │   │   ├── 0572-tip-options-fare-policy.sql
    │   │   │   │   ├── 0573-add-mid-and-mocid-columns.sql
    │   │   │   │   ├── 0574-intercity-fare-policy-state-entry-permit-charges.sql
    │   │   │   │   ├── 0575-add-order-resp-dump.sql
    │   │   │   │   ├── 0576-mid-mocid-field-for-scheduler-job.sql
    │   │   │   │   ├── 0577-add-metro-coin-translations.sql
    │   │   │   │   ├── 0580-waiting-charge-inter-city.sql
    │   │   │   │   ├── 0581-adding-minThresholdFOrPassThrroughDestination-TransporterConfig.sql
    │   │   │   │   ├── 0582-addIndexes-tripTransaction-vehicleRouteMapping.sql
    │   │   │   │   ├── 0583-wheres-my-bus-dev-testng.sql
    │   │   │   │   ├── 0584-add-fleet-applet-id.sql
    │   │   │   │   ├── 0585-dashboard-wheres-my-bus.sql
    │   │   │   │   ├── 0595-heritage-cab.sql
    │   │   │   │   ├── 0598-insert-azure-openai-driver-profile-gen-prompt-1.sql
    │   │   │   │   ├── 0599-ev-auto-rickshaw.sql
    │   │   │   │   ├── 0600-add-mandatory-col-issue-opt.sql
    │   │   │   │   ├── 0601-base-fare-depreciation.sql
    │   │   │   │   ├── 0615-delivery-truck.sql
    │   │   │   │   ├── 0620-msil-docs.sql
    │   │   │   │   ├── 0621-rc-expiry-check.sql
    │   │   │   │   ├── 0625-ops-hub.sql
    │   │   │   │   ├── 0666-pooling-app-dynamic-logic-for-local.sql
    │   │   │   │   ├── 0667-reopened-count.sql
    │   │   │   │   ├── 0668.namma-tag-trigger.sql
    │   │   │   │   ├── 0669-upsert-special-location.sql
    │   │   │   │   ├── 0670-add-additional-charges-to-fare-params.sql
    │   │   │   │   ├── 0671-trip-alert-request.sql
    │   │   │   │   ├── 0672-add-gupshup-entityid-config.sql
    │   │   │   │   ├── 0673-add-waive-off-title-and-description.sql
    │   │   │   │   ├── 0674-adding-index-on-imagedID2-dl.sql
    │   │   │   │   ├── 0675-call-feedback-options.sql
    │   │   │   │   ├── 0676-updating-pan-and-gst-service-verification.sql
    │   │   │   │   ├── 0677-add-fleet-link-and-unlink-success-messages.sql
    │   │   │   │   ├── 0678-msil-notifications.sql
    │   │   │   │   ├── 0680-drunk-and-drive-overlay.sql
    │   │   │   │   ├── 0681-added-pet-charges.sql
    │   │   │   │   └── 378-forward-batch-migrations.sql
    │   │   │   ├── mock-registry/
    │   │   │   │   ├── 0000-init.sql
    │   │   │   │   └── 0001-update-cities-to-std-codes.sql
    │   │   │   ├── provider-dashboard/
    │   │   │   │   ├── 0000-moved-roles-to-db.sql
    │   │   │   │   ├── 0001-added-server-name-to-person-and-reg-token.sql
    │   │   │   │   ├── 0002-added-test-data-for-ardu-bpp.sql
    │   │   │   │   ├── 0003-create-merchant-table.sql
    │   │   │   │   ├── 0004-create-transaction-table.sql
    │   │   │   │   ├── 0005-merchant-access.sql
    │   │   │   │   ├── 0006-transaction-table-altered.sql
    │   │   │   │   ├── 0007-access-matrix-table-altered.sql
    │   │   │   │   ├── 0008-add-special-zone-merchant.sql
    │   │   │   │   ├── 0009-add-secret-key-in-person.sql
    │   │   │   │   ├── 0010-add-fare-policy-in-access-matrix-table.sql
    │   │   │   │   ├── 0011-get-merchant-configs-apis.sql
    │   │   │   │   ├── 0012-new-dashboard-access-buttons.sql
    │   │   │   │   ├── 0013-cleanup-stuck-rides-bookings-flow.sql
    │   │   │   │   ├── 0014-add-exempt-fee-button.sql
    │   │   │   │   ├── 0015-add-subscription-plan-accesses.sql
    │   │   │   │   ├── 0016-add-vehicle-driver-and-issue-access.sql
    │   │   │   │   ├── 0017-revenue-api-access.sql
    │   │   │   │   ├── 0018-fleet-management.sql
    │   │   │   │   ├── 0019-add-overlay-access.sql
    │   │   │   │   ├── 0020-collection-api.sql
    │   │   │   │   ├── 0021-driver-driverfee-and-invoice.sql
    │   │   │   │   ├── 0022-fleet-owner-access.sql
    │   │   │   │   ├── 0023-order-status.sql
    │   │   │   │   ├── 0025-add-send-sms-access.sql
    │   │   │   │   ├── 0026-added-supported-city-in-merchant.sql
    │   │   │   │   ├── 0027-city-level-access.sql
    │   │   │   │   ├── 0028-make-server-array-merchant.sql
    │   │   │   │   ├── 0029-make-server-array-merchant.sql
    │   │   │   │   ├── 0030-update-role-internal-admin.sql
    │   │   │   │   ├── 0031-add-send-dummy-notification-user-action-type.sql
    │   │   │   │   ├── 0032-generic-scheduler-trigger.sql
    │   │   │   │   ├── 0033-add-coin-access.sql
    │   │   │   │   ├── 0034-add-partners-data-in-merchant.sql
    │   │   │   │   ├── 0035-get-coin-history-access.sql
    │   │   │   │   ├── 0036-subscription-api-with-service.sql
    │   │   │   │   ├── 0037-create-operating-cty-api.sql
    │   │   │   │   ├── 0038-rc-review-apis.sql
    │   │   │   │   ├── 0039-added-enabled-in-registration-token.sql
    │   │   │   │   ├── 0040-removed-unused-column-in-merchant.sql
    │   │   │   │   ├── 0041-add-bulk-upload-v2-access.sql
    │   │   │   │   ├── 0042-added-merchant_auth_token.sql
    │   │   │   │   ├── 0043-added-fare-break-up.sql
    │   │   │   │   ├── 0044-remove-ac-usage-restriction.sql
    │   │   │   │   ├── 0045-added-driver-tag.sql
    │   │   │   │   ├── 0046-added-verified-in-person.sql
    │   │   │   │   ├── 0047-add-receive-notification.sql
    │   │   │   │   ├── 0048-add-update-fleet-owner-info.sql
    │   │   │   │   ├── 0049-added-fleet-driver-api.sql
    │   │   │   │   ├── 0050-add-indices-transaction.sql
    │   │   │   │   ├── 0051-added-ride-list-for-fleet.sql
    │   │   │   │   ├── 0052-link-rc-with-driver-fleet.sql
    │   │   │   │   ├── 0053-clear-fee.sql
    │   │   │   │   ├── 0054-add-history-api-to-plans.sql
    │   │   │   │   ├── 0055-adding-person-numbers-in-access-matrix.sql
    │   │   │   │   ├── 0056-adding-person-id-in-access-matrix.sql
    │   │   │   │   ├── 0057-add-apis-for-security-deposit.sql
    │   │   │   │   ├── 0058-namma-tag-access.sql
    │   │   │   │   ├── 0059-subscription-clear-cache-api.sql
    │   │   │   │   ├── 0060-get-aadhar-pan-info-access.sql
    │   │   │   │   ├── 0061-partial-cash-collection-and-exemption.sql
    │   │   │   │   ├── 0062-add-require-approval-in-merchant.sql
    │   │   │   │   ├── 0063-adding-Access-Matrix-for-getPanAadhaarSelfieDetailsListAPI.sql
    │   │   │   │   ├── 0064-add-config-manager-in-access-matrix.sql
    │   │   │   │   ├── 0065-add-rejection-reason-and-rejected-at-in-person.sql
    │   │   │   │   ├── 0066-added-dashboard-type.sql
    │   │   │   │   ├── 0067-msil-dashboard.sql
    │   │   │   │   └── 0068-operator-local-access.sql
    │   │   │   ├── public-transport-rider-platform/
    │   │   │   │   ├── 1000-add-from-date-to-search.sql
    │   │   │   │   ├── 1001-remove-from-date-from-search.sql
    │   │   │   │   ├── 1002-add-table-beckn-request.sql
    │   │   │   │   └── 1003-change-beckn-request-field-type.sql
    │   │   │   ├── rider-app/
    │   │   │   │   ├── 0000-db-init.sql
    │   │   │   │   ├── 0001-local-testing-data.sql
    │   │   │   │   ├── 1013-add-id-and-driverId-to-RCR-table.sql
    │   │   │   │   ├── 1014-transform-org-to-whitelisted-provider.sql
    │   │   │   │   ├── 1015-add-ride-booking-reallocationsCount.sql
    │   │   │   │   ├── 1017-call-status-alter-status.sql
    │   │   │   │   ├── 1018-remove-kochi-region.sql
    │   │   │   │   ├── 1019-unique-contraint-to-quotes.sql
    │   │   │   │   ├── 1020-change-person-fields.sql
    │   │   │   │   ├── 1021-saved-locations.sql
    │   │   │   │   ├── 1022-rental-packages.sql
    │   │   │   │   ├── 1025-add-table-beckn-request.sql
    │   │   │   │   ├── 1026-spec-changes.sql
    │   │   │   │   ├── 1027-fare-breakup.sql
    │   │   │   │   ├── 1028-merchant-based-solution.sql
    │   │   │   │   ├── 1029-ride-table-changes.sql
    │   │   │   │   ├── 1030-change-beckn-request-field-type.sql
    │   │   │   │   ├── 1031-merchant-exo-phone.sql
    │   │   │   │   ├── 1033-added-estimate-table.sql
    │   │   │   │   ├── 1034-add-email.sql
    │   │   │   │   ├── 1036-renamed-auto-to-driver-offer.sql
    │   │   │   │   ├── 1038-renamed-auto-to-auto-rickshaw.sql
    │   │   │   │   ├── 1039-move-fcm-config-to-db.sql
    │   │   │   │   ├── 1040-move-geofencing-config-to-db.sql
    │   │   │   │   ├── 1041-diff-gateway-n-registry-for-merchants.sql
    │   │   │   │   ├── 1043-fix-rideBooking-from-location-id-fkey-reference.sql
    │   │   │   │   ├── 1044-add-merchant-constaraint-for-person.sql
    │   │   │   │   ├── 1046-optimize-tables.sql
    │   │   │   │   ├── 1047-make-monetary-fields-consistent-numeric.sql
    │   │   │   │   ├── 1048-add-merchant-name.sql
    │   │   │   │   ├── 1050-karnataka-geom.sql
    │   │   │   │   ├── 1051-expand-Kerala-geometry.sql
    │   │   │   │   ├── 1052-rename-organization-to-bpp-blacklist.sql
    │   │   │   │   ├── 1053-expanding-karnataka-geometry.sql
    │   │   │   │   ├── 1055-merchant-maps-configs.sql
    │   │   │   │   ├── 1056-rename-cancellation-source.sql
    │   │   │   │   ├── 1057-merchant-config-changes.sql
    │   │   │   │   ├── 1058-webengage.sql
    │   │   │   │   ├── 1059-estimated-fare-range.sql
    │   │   │   │   ├── 1061-merchant-mmi-config.sql
    │   │   │   │   ├── 1062-move-fcm-account-file-to-db.sql
    │   │   │   │   ├── 1063-added-estimated-breakup-table.sql
    │   │   │   │   ├── 1064-added-placeid-column-for-saved-locations.sql
    │   │   │   │   ├── 1066-create-person-flow-status-table.sql
    │   │   │   │   ├── 1068-added-night-shift-rate-column.sql
    │   │   │   │   ├── 1070-fix-person-flow-status-json.sql
    │   │   │   │   ├── 1071-merchant-sms-config.sql
    │   │   │   │   ├── 1073-add_arrival_time_in_ride.sql
    │   │   │   │   ├── 1074-making-sms-service-config-as-priority-list.sql
    │   │   │   │   ├── 1077-added-whatsapp-config.sql
    │   │   │   │   ├── 1080-add-exoPhones-in-merchant.sql
    │   │   │   │   ├── 1083-add-referral-code.sql
    │   │   │   │   ├── 1084-added-driver-offer-base-url-and-api-key-in-merchant-table.sql
    │   │   │   │   ├── 1088-remove-duplicates-from-callStatus.sql
    │   │   │   │   ├── 1090-merchant-message-table.sql
    │   │   │   │   ├── 1092-merchant-multiple-exoPhones.sql
    │   │   │   │   ├── 1093-add_subscriber_id_to_merchant.sql
    │   │   │   │   ├── 1097-added-special-location-table.sql
    │   │   │   │   ├── 1098-exophone-table-created.sql
    │   │   │   │   ├── 1099-add-transaction_id.sql
    │   │   │   │   ├── 1101-auto-assign-estimate.sql
    │   │   │   │   ├── 1102-update-send-otp-template.sql
    │   │   │   │   ├── 1103-added-blocked-to-person.sql
    │   │   │   │   ├── 1108-not-null-estimate-status.sql
    │   │   │   │   ├── 1109-reuse-old-estimates-in-repetition.sql
    │   │   │   │   ├── 1111-merchant-signature.sql
    │   │   │   │   ├── 1112-added-merchant-configuration-table.sql
    │   │   │   │   ├── 1113-refactor-fare-policy.sql
    │   │   │   │   ├── 1114-notification-service.sql
    │   │   │   │   ├── 1115-added-merchant-config-rule-id-in-person.sql
    │   │   │   │   ├── 1118-reuse-old-estimates-in-repetition.sql
    │   │   │   │   ├── 1119-add-driver-cancel-details-and-distance-for-cancel-ride.sql
    │   │   │   │   ├── 1122-make-cipher-text-maybe.sql
    │   │   │   │   ├── 1125-add-merchantId-to-tables.sql
    │   │   │   │   ├── 1127-merchant-payment-method.sql
    │   │   │   │   ├── 1128-juspay-payments-integration.sql
    │   │   │   │   ├── 1129-renamed-paymentOrder-personId.sql
    │   │   │   │   ├── 1130_txn_uuid_optional.sql
    │   │   │   │   ├── 1132-added-juspay-order-id.sql
    │   │   │   │   ├── 1133-add-missing-fields-in-payment-order.sql
    │   │   │   │   ├── 1136-self-id-uri-merchant.sql
    │   │   │   │   ├── 1137-added-payment-order-merchant-id.sql
    │   │   │   │   ├── 1138-add-driver-mobile-country-code-ride-table.sql
    │   │   │   │   ├── 1139-payment-merchant-id-updated.sql
    │   │   │   │   ├── 1140-add-directions-cache.sql
    │   │   │   │   ├── 1141-add-Initialized-column-slot-merchant.sql
    │   │   │   │   ├── 1142-add-address-in-gates-special-location.sql
    │   │   │   │   ├── 1144-beckn-init-to-onconfirm-spec-change.sql
    │   │   │   │   ├── 1146-add-feedback-form.sql
    │   │   │   │   ├── 1147-create-table-hotspotconfig.sql
    │   │   │   │   ├── 1149-add-gupshup-sms-config.sql
    │   │   │   │   ├── 1150-add-new-messages-for-yatri-sathi.sql
    │   │   │   │   ├── 1151-configuration-update-whatsapp-gupshup.sql
    │   │   │   │   ├── 1152-subscription-plans.sql
    │   │   │   │   ├── 1153-add-person-stats-table.sql
    │   │   │   │   ├── 1154-add-time-diff-column-in-merchant.sql
    │   │   │   │   ├── 1155-add-ticket-config.sql
    │   │   │   │   ├── 1156-add-distance-weightage-in-merchant.sql
    │   │   │   │   ├── 1157-add-minimum-driver-rates-count-to-merchant.sql
    │   │   │   │   ├── 1160-create-disability-and-disability-translation.sql
    │   │   │   │   ├── 1162-add-call-service.sql
    │   │   │   │   ├── 1165-add-bank-error-in-payment-order-and-txn.sql
    │   │   │   │   ├── 1166-payment-order-table-change-fields-to-nullable.sql
    │   │   │   │   ├── 1167-adding-location-and-location-mapping-tables.sql
    │   │   │   │   ├── 1168-drop-foreign-constraint.sql
    │   │   │   │   ├── 1169-adding-aadhaar-otp-and-verification-detail-tables.sql
    │   │   │   │   ├── 1170-update-version-to-latest.sql
    │   │   │   │   ├── 1175-delete-directions-cache-table.sql
    │   │   │   │   ├── 1177-merchant-operating-city-table.sql
    │   │   │   │   ├── 1178-add-sos-emergency-message.sql
    │   │   │   │   ├── 1185-adding-new-issue-flow-related-data.sql
    │   │   │   │   ├── 1186-add-retarget-and-retry-fields.sql
    │   │   │   │   ├── 1187-adding-new-data-for-issue-flow.sql
    │   │   │   │   ├── 1189-zoo-booking-integration.sql
    │   │   │   │   ├── 1192-fixing-translations-and-adding-extra-options.sql
    │   │   │   │   ├── 1193-added-columns-shortDesc-iconUrl-mapImgUrl-in TicketPlace.sql
    │   │   │   │   ├── 1194-tickets.sql
    │   │   │   │   ├── 1197-modifying-ticket-booking-system-data.sql
    │   │   │   │   ├── 1199-changing-account-related-issue-message.sql
    │   │   │   │   ├── 1202-added-fallback-merchant.sql
    │   │   │   │   ├── 1203-added-kv-configs-for-table-and-default-values.sql
    │   │   │   │   ├── 1204-adding-merchantOpCityId-to-IssueReport.sql
    │   │   │   │   ├── 1206-add-and-remove-hotspot-columns.sql
    │   │   │   │   ├── 1207-adding-ShortId-to-IssueReport.sql
    │   │   │   │   ├── 1208-next-billion-service-config.sql
    │   │   │   │   ├── 1209-add-priority-in-emergency-contact-table.sql
    │   │   │   │   ├── 1211-add-beckn-config.sql
    │   │   │   │   ├── 1212-add-merchantId-in-emergency-contact.sql
    │   │   │   │   ├── 1213-chennai-metro-stations.sql
    │   │   │   │   ├── 1214-rental-refactoring.sql
    │   │   │   │   ├── 1218-white-list-org-table.sql
    │   │   │   │   ├── 1219-inter-city.sql
    │   │   │   │   ├── 1220-whitelist-datatype.sql
    │   │   │   │   ├── 1222-created-gate-info-table.sql
    │   │   │   │   ├── 1223-whitelist-new-queries.sql
    │   │   │   │   ├── 1224-notification-sound-config.sql
    │   │   │   │   ├── 1225-add-deep-link-in-payment-order.sql
    │   │   │   │   ├── 1226-ondc-interoperability.sql
    │   │   │   │   ├── 1227-update-merchant-payment-method.sql
    │   │   │   │   ├── 1229-adds-value-add-np.sql
    │   │   │   │   ├── 1230-drop-distance_weightage.sql
    │   │   │   │   ├── 1233-beckn-config-mobility.sql
    │   │   │   │   ├── 1234-fake-otp-numbers.sql
    │   │   │   │   ├── 1236-rickshaw-config-yatri.sql
    │   │   │   │   ├── 1237-payment-transaction.sql
    │   │   │   │   ├── 1238-kochi-metro-stations.sql
    │   │   │   │   ├── 1241-add-merchant-operating-city-in-merchant-service-config.sql
    │   │   │   │   ├── 1242-add-metro-payment-creds.sql
    │   │   │   │   ├── 1244-interop-changes.sql
    │   │   │   │   ├── 1245-price-currency.sql
    │   │   │   │   ├── 1246-add-transaction-id-index-for-booking.sql
    │   │   │   │   ├── 1247-added-referral-columns.sql
    │   │   │   │   ├── 1250-add-createdAt-updatedAt-to-tables.sql
    │   │   │   │   ├── 1252-add-merchant-id-to-whitelistorg.sql
    │   │   │   │   ├── 1253-expiry-to-placeNameCache.sql
    │   │   │   │   ├── 1255-adding-updated-at-to-gate-info.sql
    │   │   │   │   ├── 1256-adding-bookingSyncStatusCallSecondsDiffThreshold.sql
    │   │   │   │   ├── 1257-add-client-id.sql
    │   │   │   │   ├── 1258-change-pks-of-person-default-emergency-number.sql
    │   │   │   │   ├── 1259-drop-quote-constraint.sql
    │   │   │   │   ├── 1260-adding-kapture-queue-values.sql
    │   │   │   │   ├── 1261-frfs-config.sql
    │   │   │   │   ├── 1262-adding-ticket-booking-cancellation.sql
    │   │   │   │   ├── 1263-adding-fare-breakup-chat.sql
    │   │   │   │   ├── 1264-kapture-sos-queue-values.sql
    │   │   │   │   ├── 1265-add-notification-config-values.sql
    │   │   │   │   ├── 1266-frfs-recon-constraint-updates.sql
    │   │   │   │   ├── 1267-vehicle-variant.sql
    │   │   │   │   ├── 1268-partner-organization.sql
    │   │   │   │   ├── 1269-frfs-transaction-tables-backfill.sql
    │   │   │   │   ├── 1270-add-stripe-config.sql
    │   │   │   │   ├── 1272-updates-to-toll-crossed-push-notification.sql
    │   │   │   │   ├── 1273-indexing-on-device-id.sql
    │   │   │   │   ├── 1274-fixies-to-migrations-adding-merchantId-to-issue.sql
    │   │   │   │   ├── 1275-issue-flow-queries.sql
    │   │   │   │   ├── 1276-update-merchant-service-usage-and-rider-config.sql
    │   │   │   │   ├── 1277-add-ERSS-and-JM-config.sql
    │   │   │   │   ├── 1281-ambulance.sql
    │   │   │   │   ├── 1282-fixing-issue-message-foreign-key-constraints.sql
    │   │   │   │   ├── 1285-adding-twillio-sms-configs.sql
    │   │   │   │   ├── 1286-make-nicco-park-live.sql
    │   │   │   │   ├── 1287-add-stripe-fields-in-transactions.sql
    │   │   │   │   ├── 1288-add-metro-ticket-booked-sms.sql
    │   │   │   │   ├── 1289-add-new-message-to-pushnotifications.sql
    │   │   │   │   ├── 1292-merchant-message-end-otp.sql
    │   │   │   │   ├── 1293-added-google-route-config-merchant-service-config.sql
    │   │   │   │   ├── 1294-update-exotel-mapping-for-unattended-tickets.sql
    │   │   │   │   ├── 1295-merchant-message.sql
    │   │   │   │   ├── 1296-added-new-place-api-config-merchant-service-config.sql
    │   │   │   │   ├── 1297-added-message-for-post-ride-sos.sql
    │   │   │   │   ├── 1299-modifies-beckn-config.sql
    │   │   │   │   ├── 1300-whiteListOrg-city-level.sql
    │   │   │   │   ├── 1301-add-gate-type-in-gate-info.sql
    │   │   │   │   ├── 1302-adding-allowedRideStatuses-to-issue-flow.sql
    │   │   │   │   ├── 1320-delivery.sql
    │   │   │   │   ├── 1321-added-pn-config-for-first-ride-event.sql
    │   │   │   │   ├── 1322-post-ride-sos-header-addition.sql
    │   │   │   │   ├── 1323-filling-values-for-cancellation-reason-id.sql
    │   │   │   │   ├── 1326-adds-igm-issue-id-issue-report.sql
    │   │   │   │   ├── 1330-default-push-notis.sql
    │   │   │   │   ├── 1331-parcel-delivery-pns.sql
    │   │   │   │   ├── 1340-stripe-job-exec-delays-rider-config.sql
    │   │   │   │   ├── 1341-bus-booking-ebix.sql
    │   │   │   │   ├── 1342-add-created-and-updated-at-to-issue-config.sql
    │   │   │   │   ├── 1344-add-igm-config.sql
    │   │   │   │   ├── 1345-igm-config.sql
    │   │   │   │   ├── 1346-adding-multimodal-transit-configs.sql
    │   │   │   │   ├── 1347-add-post-ride-safety-applet-id.sql
    │   │   │   │   ├── 1348-cmrl-metro-config.sql
    │   │   │   │   ├── 1349-change-vp-in-tracking-url.sql
    │   │   │   │   ├── 1356-adding-l0feedback-queue-to-kapture-config.sql
    │   │   │   │   ├── 1357-add-column-special-location.sql
    │   │   │   │   ├── 1358-update-autocomplete-params.sql
    │   │   │   │   ├── 1359-add-mocid-in-payment-order.sql
    │   │   │   │   ├── 1360-special-location-type.sql
    │   │   │   │   ├── 1361-updated-kv-configs.sql
    │   │   │   │   ├── 1362-add-split-response-in-transaction.sql
    │   │   │   │   ├── 1363-added-index-in-payout-order-table.sql
    │   │   │   │   ├── 1364-add-mid-and-mocid-column.sql
    │   │   │   │   ├── 1365-add-order-resp-dump.sql
    │   │   │   │   ├── 1366-mid-mocid-field-for-scheduler-job.sql
    │   │   │   │   ├── 1367-multimodal.sql
    │   │   │   │   ├── 1368-service-account-config-google-wallet.sql
    │   │   │   │   ├── 1369-cancel-ticket-sms-merchant-messsage.sql
    │   │   │   │   ├── 1370-multimodal-indices.sql
    │   │   │   │   ├── 1371-updating_provider_id_and_name_frfs_config.sql
    │   │   │   │   ├── 1372-valid-till-frfs-config.sql
    │   │   │   │   ├── 1373-ticket-cancel-otp-sms-merchant-message.sql
    │   │   │   │   ├── 1390-cancellation-rate-blocking.sql
    │   │   │   │   ├── 1391-add-mandatory-col-issue-opt.sql
    │   │   │   │   ├── 1400-delivery-truck.sql
    │   │   │   │   ├── 1401-reopened-count.sql
    │   │   │   │   ├── 1402.namma-tag-trigger.sql
    │   │   │   │   ├── 1403-safety-reasons.sql
    │   │   │   │   ├── 1404-upsert-special-location.sql
    │   │   │   │   ├── 1405-add-nearby-driver-configs.sql
    │   │   │   │   ├── 1406-add-fare-cache-rentals-intercity-city-base-configs.sql
    │   │   │   │   ├── 1407-added-destination-data-in-fare-cache-config.sql
    │   │   │   │   ├── 1409-add-ticket-merchant-onboarding-config.sql
    │   │   │   │   ├── 1410-add-gupshup-entityid-config.sql
    │   │   │   │   ├── 1420-issue-chat.sql
    │   │   │   │   ├── 1421-unique-index.sql
    │   │   │   │   ├── 1422.added-pn-for-r2r.sql
    │   │   │   │   ├── 1423-add-sorted-weight-config.sql
    │   │   │   │   ├── 1424-added-pn-for-new-customer-payout-reward.sql
    │   │   │   │   ├── 1425-add-fraud-auth-check-columns-values-to-merchant-config.sql
    │   │   │   │   ├── 1426-update-suburban-station-translations.sql
    │   │   │   │   ├── 1427-added-pn-for-rewards-in-webhook.sql
    │   │   │   │   ├── 1428-added-aajuspay-service.sql
    │   │   │   │   ├── 1429-frfs-gtfs-fare-query-backfill.sql
    │   │   │   │   ├── 1430-feed-info-for-chennai.sql
    │   │   │   │   ├── 1431-update-merchant-service-config-for-base-url-for-nandi.sql
    │   │   │   │   ├── 1433-update-kapture-ticket-config.sql
    │   │   │   │   ├── 1434-gtfs-service-config-backfile.sql
    │   │   │   │   ├── 1435-rider-config.sql
    │   │   │   │   ├── 1436-polylines-bakfill.sql
    │   │   │   │   ├── 1440-remove-kapture-config-apikey.sql
    │   │   │   │   ├── 1441-remove-apiKey-kapture.sql
    │   │   │   │   ├── 1445-insurance-setup.sql
    │   │   │   │   ├── 1446-merchant-notification.sql
    │   │   │   │   └── 1447-add-encpryption-key-kapture.sql
    │   │   │   ├── rider-dashboard/
    │   │   │   │   ├── 0000-moved-roles-to-db.sql
    │   │   │   │   ├── 0001-added-server-name-to-person-and-reg-token.sql
    │   │   │   │   ├── 0002-create-merchant-table.sql
    │   │   │   │   ├── 0003-create-transaction-table.sql
    │   │   │   │   ├── 0004-merchant-access.sql
    │   │   │   │   ├── 0005-transaction-table-altered.sql
    │   │   │   │   ├── 0006-access-matrix-table-altered.sql
    │   │   │   │   ├── 0007-add-secret-key-in-table.sql
    │   │   │   │   ├── 0008-get-merchant-configs-apis.sql
    │   │   │   │   ├── 0009-ride-force-sync-api-added.sql
    │   │   │   │   ├── 0010-cleanup-stuck-rides-bookings-flow.sql
    │   │   │   │   ├── 0011-add-kapture-issue-access.sql
    │   │   │   │   ├── 0012-fleet-management.sql
    │   │   │   │   ├── 0015-added-default-operating-city.sql
    │   │   │   │   ├── 0016-city-level-access.sql
    │   │   │   │   ├── 0017-make-server-array-merchant.sql
    │   │   │   │   ├── 0018-customer-cancellation-dues.sql
    │   │   │   │   ├── 0019-add-partners-data-in-merchant.sql
    │   │   │   │   ├── 0020-create-operating-cty-api.sql
    │   │   │   │   ├── 0021-added-enabled-in-registration-token.sql
    │   │   │   │   ├── 0022-added-safety-center-blocking.sql
    │   │   │   │   ├── 0023-removed-unused-column-in-merchant.sql
    │   │   │   │   ├── 0024-added-merchant-auth-token.sql
    │   │   │   │   ├── 0025-added-auth-in-ride-list.sql
    │   │   │   │   ├── 0026-added-verified-in-person.sql
    │   │   │   │   ├── 0027-add-receive-notification.sql
    │   │   │   │   ├── 0028-add-indices-transaction.sql
    │   │   │   │   ├── 0029-adding-kochi-to-supported-cities.sql
    │   │   │   │   ├── 0030-adding-person-numbers-in-access-matrix.sql
    │   │   │   │   ├── 0031-adding-person-id-in-access-matrix.sql
    │   │   │   │   ├── 0032-access-for-dashboard-apis.sql
    │   │   │   │   ├── 0033-add-config-manager-in-access-matrix.sql
    │   │   │   │   ├── 0034-add-require-approval-in-merchant.sql
    │   │   │   │   ├── 0035-add-rejection-reason-and-rejected-at-in-person.sql
    │   │   │   │   ├── 0036-add-ticket-dashboard-enums-and-roles.sql
    │   │   │   │   ├── 0037-add-ticket-dashboard-api-endpoints.sql
    │   │   │   │   ├── 0038-add-ticket-dashboard-merchant-onboarding-access.sql
    │   │   │   │   ├── 0039-add-dashboard-type.sql
    │   │   │   │   └── 0040-event-management.sql
    │   │   │   ├── safety-dashboard/
    │   │   │   │   ├── 0000-safety-dashboard.sql
    │   │   │   │   ├── 0001-added-merchant-auth-token.sql
    │   │   │   │   ├── 0002-added-verified-in-person.sql
    │   │   │   │   ├── 0003-add-receive-notification.sql
    │   │   │   │   └── 0004-added-dashboard-type.sql
    │   │   │   ├── scheduler/
    │   │   │   │   └── 0001-rider-job-scheduler.sql
    │   │   │   ├── scheduler-example/
    │   │   │   │   └── 0001-job-scheduler.sql
    │   │   │   └── special-zone/
    │   │   │       └── special-zone.sql
    │   │   ├── migrations-after-release/
    │   │   │   ├── dynamic-offer-driver-app/
    │   │   │   │   ├── 0001-add-truck-onboarding.sql
    │   │   │   │   └── 0002-avg-speed-bus.sql
    │   │   │   └── rider-app/
    │   │   │       ├── 0001-remove-sms-template-from-partner-org-config.sql
    │   │   │       └── 0001-traking-url-change.sql
    │   │   ├── migrations-read-only/
    │   │   │   ├── dynamic-offer-driver-app/
    │   │   │   │   ├── aadhaar_card.sql
    │   │   │   │   ├── aadhaar_otp_req.sql
    │   │   │   │   ├── aadhaar_otp_verify.sql
    │   │   │   │   ├── aadhaar_verification.sql
    │   │   │   │   ├── app_dynamic_logic.sql
    │   │   │   │   ├── app_dynamic_logic_element.sql
    │   │   │   │   ├── app_dynamic_logic_rollout.sql
    │   │   │   │   ├── approval_request.sql
    │   │   │   │   ├── background_verification.sql
    │   │   │   │   ├── bap_metadata.sql
    │   │   │   │   ├── beckn_config.sql
    │   │   │   │   ├── black_list_org.sql
    │   │   │   │   ├── blocked_route.sql
    │   │   │   │   ├── booking.sql
    │   │   │   │   ├── booking_cancellation_reason.sql
    │   │   │   │   ├── booking_location.sql
    │   │   │   │   ├── booking_update_request.sql
    │   │   │   │   ├── business_event.sql
    │   │   │   │   ├── business_license.sql
    │   │   │   │   ├── call_feedback.sql
    │   │   │   │   ├── call_feedback_options.sql
    │   │   │   │   ├── call_status.sql
    │   │   │   │   ├── cancellation_charges.sql
    │   │   │   │   ├── cancellation_fare_policy.sql
    │   │   │   │   ├── cancellation_reason.sql
    │   │   │   │   ├── chakra_queries.sql
    │   │   │   │   ├── client.sql
    │   │   │   │   ├── conditional_charges.sql
    │   │   │   │   ├── daily_stats.sql
    │   │   │   │   ├── document_verification_config.sql
    │   │   │   │   ├── driver_bank_account.sql
    │   │   │   │   ├── driver_block_reason.sql
    │   │   │   │   ├── driver_block_transactions.sql
    │   │   │   │   ├── driver_fee.sql
    │   │   │   │   ├── driver_go_home_request.sql
    │   │   │   │   ├── driver_gstin.sql
    │   │   │   │   ├── driver_gullak_association.sql
    │   │   │   │   ├── driver_home_location.sql
    │   │   │   │   ├── driver_information.sql
    │   │   │   │   ├── driver_intelligent_pool_config.sql
    │   │   │   │   ├── driver_license.sql
    │   │   │   │   ├── driver_module_completion.sql
    │   │   │   │   ├── driver_operator_association.sql
    │   │   │   │   ├── driver_pan_card.sql
    │   │   │   │   ├── driver_plan.sql
    │   │   │   │   ├── driver_pool_config.sql
    │   │   │   │   ├── driver_profile_questions.sql
    │   │   │   │   ├── driver_quote.sql
    │   │   │   │   ├── driver_rc_association.sql
    │   │   │   │   ├── driver_referral.sql
    │   │   │   │   ├── driver_ssn.sql
    │   │   │   │   ├── driver_stats.sql
    │   │   │   │   ├── estimate.sql
    │   │   │   │   ├── exophone.sql
    │   │   │   │   ├── fare_policy_progressive_details_per_min_rate_section.sql
    │   │   │   │   ├── fare_product.sql
    │   │   │   │   ├── feedback.sql
    │   │   │   │   ├── fleet_badge.sql
    │   │   │   │   ├── fleet_badge_association.sql
    │   │   │   │   ├── fleet_config.sql
    │   │   │   │   ├── fleet_driver_association.sql
    │   │   │   │   ├── fleet_member_association.sql
    │   │   │   │   ├── fleet_operator_association.sql
    │   │   │   │   ├── fleet_owner_document_verification_config.sql
    │   │   │   │   ├── fleet_owner_information.sql
    │   │   │   │   ├── fleet_rc_association.sql
    │   │   │   │   ├── fleet_route_association.sql
    │   │   │   │   ├── go_home_config.sql
    │   │   │   │   ├── hyperverge_sdk_logs.sql
    │   │   │   │   ├── hyperverge_verification.sql
    │   │   │   │   ├── idfy_verification.sql
    │   │   │   │   ├── image.sql
    │   │   │   │   ├── inter_city_travel_cities.sql
    │   │   │   │   ├── invoice.sql
    │   │   │   │   ├── kiosk_location.sql
    │   │   │   │   ├── kiosk_location_translation.sql
    │   │   │   │   ├── leader_board_configs.sql
    │   │   │   │   ├── llm_prompt.sql
    │   │   │   │   ├── lms_certificate.sql
    │   │   │   │   ├── lms_module.sql
    │   │   │   │   ├── lms_module_translation.sql
    │   │   │   │   ├── lms_module_video_information.sql
    │   │   │   │   ├── lms_video_translation.sql
    │   │   │   │   ├── location.sql
    │   │   │   │   ├── location_mapping.sql
    │   │   │   │   ├── mandate.sql
    │   │   │   │   ├── merchant.sql
    │   │   │   │   ├── merchant_message.sql
    │   │   │   │   ├── merchant_operating_city.sql
    │   │   │   │   ├── merchant_overlay.sql
    │   │   │   │   ├── merchant_payment_method.sql
    │   │   │   │   ├── merchant_push_notification.sql
    │   │   │   │   ├── merchant_service_config.sql
    │   │   │   │   ├── merchant_service_usage_config.sql
    │   │   │   │   ├── merchant_state.sql
    │   │   │   │   ├── message.sql
    │   │   │   │   ├── message_report.sql
    │   │   │   │   ├── message_translation.sql
    │   │   │   │   ├── meta_data.sql
    │   │   │   │   ├── module_completion_information.sql
    │   │   │   │   ├── namma_tag.sql
    │   │   │   │   ├── namma_tag_trigger.sql
    │   │   │   │   ├── notification.sql
    │   │   │   │   ├── operation_hub.sql
    │   │   │   │   ├── operation_hub_requests.sql
    │   │   │   │   ├── payout_config.sql
    │   │   │   │   ├── payout_order.sql
    │   │   │   │   ├── payout_transaction.sql
    │   │   │   │   ├── person.sql
    │   │   │   │   ├── place_name_cache.sql
    │   │   │   │   ├── plan.sql
    │   │   │   │   ├── plan_translation.sql
    │   │   │   │   ├── purchase_history.sql
    │   │   │   │   ├── question_information.sql
    │   │   │   │   ├── question_module_mapping.sql
    │   │   │   │   ├── rating.sql
    │   │   │   │   ├── rc_validation_rules.sql
    │   │   │   │   ├── reels_data.sql
    │   │   │   │   ├── refunds.sql
    │   │   │   │   ├── registration_token.sql
    │   │   │   │   ├── registry_map_fallback.sql
    │   │   │   │   ├── ride.sql
    │   │   │   │   ├── ride_details.sql
    │   │   │   │   ├── ride_related_notification_config.sql
    │   │   │   │   ├── rider_details.sql
    │   │   │   │   ├── rider_driver_correlation.sql
    │   │   │   │   ├── route.sql
    │   │   │   │   ├── route_trip_stop_mapping.sql
    │   │   │   │   ├── search_request.sql
    │   │   │   │   ├── search_request_for_driver.sql
    │   │   │   │   ├── search_request_location.sql
    │   │   │   │   ├── search_try.sql
    │   │   │   │   ├── station.sql
    │   │   │   │   ├── stop_information.sql
    │   │   │   │   ├── subscription_config.sql
    │   │   │   │   ├── surge_pricing.sql
    │   │   │   │   ├── tag_action_notification_config.sql
    │   │   │   │   ├── time_bound_config.sql
    │   │   │   │   ├── toll.sql
    │   │   │   │   ├── translations.sql
    │   │   │   │   ├── transporter_config.sql
    │   │   │   │   ├── trip_alert_request.sql
    │   │   │   │   ├── trip_transaction.sql
    │   │   │   │   ├── ui_driver_config.sql
    │   │   │   │   ├── user_data.sql
    │   │   │   │   ├── value_add_np.sql
    │   │   │   │   ├── vehicle.sql
    │   │   │   │   ├── vehicle_details.sql
    │   │   │   │   ├── vehicle_fitness_certificate.sql
    │   │   │   │   ├── vehicle_info.sql
    │   │   │   │   ├── vehicle_insurance.sql
    │   │   │   │   ├── vehicle_noc.sql
    │   │   │   │   ├── vehicle_permit.sql
    │   │   │   │   ├── vehicle_puc.sql
    │   │   │   │   ├── vehicle_registration_certificate.sql
    │   │   │   │   ├── vehicle_route_mapping.sql
    │   │   │   │   ├── vehicle_service_tier.sql
    │   │   │   │   ├── vendor_fee.sql
    │   │   │   │   ├── vendor_split_details.sql
    │   │   │   │   ├── volunteer.sql
    │   │   │   │   ├── webhook.sql
    │   │   │   │   └── white_list_org.sql
    │   │   │   ├── provider-dashboard/
    │   │   │   │   ├── API_AppManagement_Driver.sql
    │   │   │   │   ├── API_AppManagement_DriverSubscription.sql
    │   │   │   │   ├── API_AppManagement_Overlay.sql
    │   │   │   │   ├── API_AppManagement_Subscription.sql
    │   │   │   │   ├── API_Fleet_Driver.sql
    │   │   │   │   ├── API_IssueManagement_Issue.sql
    │   │   │   │   ├── API_Management_Booking.sql
    │   │   │   │   ├── API_Management_Driver.sql
    │   │   │   │   ├── API_Management_DriverCoins.sql
    │   │   │   │   ├── API_Management_DriverGoHome.sql
    │   │   │   │   ├── API_Management_DriverReferral.sql
    │   │   │   │   ├── API_Management_DriverRegistration.sql
    │   │   │   │   ├── API_Management_Merchant.sql
    │   │   │   │   ├── API_Management_Message.sql
    │   │   │   │   ├── API_Management_NammaTag.sql
    │   │   │   │   ├── API_Management_Payout.sql
    │   │   │   │   ├── API_Management_Revenue.sql
    │   │   │   │   ├── API_Management_Ride.sql
    │   │   │   │   ├── API_Management_System.sql
    │   │   │   │   ├── API_RideBooking_Driver.sql
    │   │   │   │   ├── API_RideBooking_DriverRegistration.sql
    │   │   │   │   ├── API_RideBooking_Maps.sql
    │   │   │   │   ├── API_RideBooking_Ride.sql
    │   │   │   │   ├── API_RideBooking_SearchRequest.sql
    │   │   │   │   ├── API_RideBooking_Volunteer.sql
    │   │   │   │   ├── fleet_member_association.sql
    │   │   │   │   ├── Local_API_AppManagement_Driver.sql
    │   │   │   │   ├── Local_API_AppManagement_DriverSubscription.sql
    │   │   │   │   ├── Local_API_AppManagement_Overlay.sql
    │   │   │   │   ├── Local_API_AppManagement_Subscription.sql
    │   │   │   │   ├── Local_API_Fleet_Driver.sql
    │   │   │   │   ├── Local_API_Fleet_Onboarding.sql
    │   │   │   │   ├── Local_API_Fleet_RegistrationV2.sql
    │   │   │   │   ├── Local_API_IssueManagement_Issue.sql
    │   │   │   │   ├── Local_API_Management_Account.sql
    │   │   │   │   ├── Local_API_Management_Booking.sql
    │   │   │   │   ├── Local_API_Management_CoinsConfig.sql
    │   │   │   │   ├── Local_API_Management_Driver.sql
    │   │   │   │   ├── Local_API_Management_DriverCoins.sql
    │   │   │   │   ├── Local_API_Management_DriverGoHome.sql
    │   │   │   │   ├── Local_API_Management_DriverReferral.sql
    │   │   │   │   ├── Local_API_Management_DriverRegistration.sql
    │   │   │   │   ├── Local_API_Management_Media.sql
    │   │   │   │   ├── Local_API_Management_Merchant.sql
    │   │   │   │   ├── Local_API_Management_Message.sql
    │   │   │   │   ├── Local_API_Management_NammaTag.sql
    │   │   │   │   ├── Local_API_Management_Payout.sql
    │   │   │   │   ├── Local_API_Management_Revenue.sql
    │   │   │   │   ├── Local_API_Management_Ride.sql
    │   │   │   │   ├── Local_API_Management_System.sql
    │   │   │   │   ├── Local_API_Management_VehicleInfo.sql
    │   │   │   │   ├── Local_API_Operator_Driver.sql
    │   │   │   │   ├── Local_API_Operator_FleetManagement.sql
    │   │   │   │   ├── Local_API_Operator_Registration.sql
    │   │   │   │   ├── Local_API_RideBooking_Driver.sql
    │   │   │   │   ├── Local_API_RideBooking_DriverRegistration.sql
    │   │   │   │   ├── Local_API_RideBooking_Maps.sql
    │   │   │   │   ├── Local_API_RideBooking_MeterRide.sql
    │   │   │   │   ├── Local_API_RideBooking_Ride.sql
    │   │   │   │   ├── Local_API_RideBooking_SearchRequest.sql
    │   │   │   │   └── Local_API_RideBooking_Volunteer.sql
    │   │   │   ├── rider-app/
    │   │   │   │   ├── aadhaar_otp_req.sql
    │   │   │   │   ├── aadhaar_otp_verify.sql
    │   │   │   │   ├── aadhaar_verification.sql
    │   │   │   │   ├── app_dynamic_logic.sql
    │   │   │   │   ├── app_dynamic_logic_element.sql
    │   │   │   │   ├── app_dynamic_logic_rollout.sql
    │   │   │   │   ├── app_installs.sql
    │   │   │   │   ├── bbps.sql
    │   │   │   │   ├── bbps_config.sql
    │   │   │   │   ├── beckn_config.sql
    │   │   │   │   ├── beckn_vehicle_config.sql
    │   │   │   │   ├── black_list_org.sql
    │   │   │   │   ├── booking.sql
    │   │   │   │   ├── booking_cancellation_reason.sql
    │   │   │   │   ├── booking_location.sql
    │   │   │   │   ├── booking_parties_link.sql
    │   │   │   │   ├── booking_update_request.sql
    │   │   │   │   ├── bpp_details.sql
    │   │   │   │   ├── business_hour.sql
    │   │   │   │   ├── call_status.sql
    │   │   │   │   ├── callback_request.sql
    │   │   │   │   ├── cancellation_reason.sql
    │   │   │   │   ├── chakra_queries.sql
    │   │   │   │   ├── client.sql
    │   │   │   │   ├── client_person_info.sql
    │   │   │   │   ├── deleted_person.sql
    │   │   │   │   ├── disability.sql
    │   │   │   │   ├── disability_translation.sql
    │   │   │   │   ├── draft_ticket_change.sql
    │   │   │   │   ├── draft_ticket_change_history.sql
    │   │   │   │   ├── driver_offer.sql
    │   │   │   │   ├── estimate.sql
    │   │   │   │   ├── exophone.sql
    │   │   │   │   ├── fare_breakup.sql
    │   │   │   │   ├── feedback_form.sql
    │   │   │   │   ├── frfs_config.sql
    │   │   │   │   ├── frfs_fare_policy.sql
    │   │   │   │   ├── frfs_gtfs_stage_fare.sql
    │   │   │   │   ├── frfs_quote.sql
    │   │   │   │   ├── frfs_recon.sql
    │   │   │   │   ├── frfs_route_fare_product.sql
    │   │   │   │   ├── frfs_route_stop_stage_fare.sql
    │   │   │   │   ├── frfs_search.sql
    │   │   │   │   ├── frfs_stage_fare.sql
    │   │   │   │   ├── frfs_ticket.sql
    │   │   │   │   ├── frfs_ticket_booking.sql
    │   │   │   │   ├── frfs_ticket_booking_payment.sql
    │   │   │   │   ├── frfs_ticket_discount.sql
    │   │   │   │   ├── frfs_vehicle_service_tier.sql
    │   │   │   │   ├── gtfs_feed_info.sql
    │   │   │   │   ├── hot_spot_config.sql
    │   │   │   │   ├── igm_issue.sql
    │   │   │   │   ├── insurance.sql
    │   │   │   │   ├── insurance_config.sql
    │   │   │   │   ├── integrated_bpp_config.sql
    │   │   │   │   ├── inter_city_details.sql
    │   │   │   │   ├── issue.sql
    │   │   │   │   ├── journey.sql
    │   │   │   │   ├── journey_booking.sql
    │   │   │   │   ├── journey_feedback.sql
    │   │   │   │   ├── journey_leg.sql
    │   │   │   │   ├── journey_legs_feedbacks.sql
    │   │   │   │   ├── journey_route_details.sql
    │   │   │   │   ├── location.sql
    │   │   │   │   ├── location_mapping.sql
    │   │   │   │   ├── merchant.sql
    │   │   │   │   ├── merchant_config.sql
    │   │   │   │   ├── merchant_message.sql
    │   │   │   │   ├── merchant_onboarding.sql
    │   │   │   │   ├── merchant_onboarding_step.sql
    │   │   │   │   ├── merchant_onboarding_step_config.sql
    │   │   │   │   ├── merchant_operating_city.sql
    │   │   │   │   ├── merchant_payment_method.sql
    │   │   │   │   ├── merchant_push_notification.sql
    │   │   │   │   ├── merchant_service_config.sql
    │   │   │   │   ├── merchant_service_usage_config.sql
    │   │   │   │   ├── merchant_state.sql
    │   │   │   │   ├── multimodal_preferences.sql
    │   │   │   │   ├── namma_tag.sql
    │   │   │   │   ├── namma_tag_trigger.sql
    │   │   │   │   ├── notification_sounds_config.sql
    │   │   │   │   ├── on_search_event.sql
    │   │   │   │   ├── parcel_details.sql
    │   │   │   │   ├── partner_org_config.sql
    │   │   │   │   ├── partner_org_station.sql
    │   │   │   │   ├── partner_organization.sql
    │   │   │   │   ├── passenger_details.sql
    │   │   │   │   ├── payment_customer.sql
    │   │   │   │   ├── payout_config.sql
    │   │   │   │   ├── payout_order.sql
    │   │   │   │   ├── payout_transaction.sql
    │   │   │   │   ├── person.sql
    │   │   │   │   ├── person_default_emergency_number.sql
    │   │   │   │   ├── person_disability.sql
    │   │   │   │   ├── person_flow_status.sql
    │   │   │   │   ├── person_stats.sql
    │   │   │   │   ├── place_based_service_config.sql
    │   │   │   │   ├── place_name_cache.sql
    │   │   │   │   ├── popular_location.sql
    │   │   │   │   ├── quote.sql
    │   │   │   │   ├── quote_breakup.sql
    │   │   │   │   ├── rating.sql
    │   │   │   │   ├── recent_location.sql
    │   │   │   │   ├── refunds.sql
    │   │   │   │   ├── registration_token.sql
    │   │   │   │   ├── rental_details.sql
    │   │   │   │   ├── ride.sql
    │   │   │   │   ├── ride_related_notification_config.sql
    │   │   │   │   ├── rider_config.sql
    │   │   │   │   ├── route.sql
    │   │   │   │   ├── route_details.sql
    │   │   │   │   ├── route_polylines.sql
    │   │   │   │   ├── route_stop_calender.sql
    │   │   │   │   ├── route_stop_fare.sql
    │   │   │   │   ├── route_stop_mapping.sql
    │   │   │   │   ├── route_stop_time_table.sql
    │   │   │   │   ├── route_trip_mapping.sql
    │   │   │   │   ├── safety_settings.sql
    │   │   │   │   ├── saved_location.sql
    │   │   │   │   ├── search_request.sql
    │   │   │   │   ├── search_request_location.sql
    │   │   │   │   ├── search_request_parties_link.sql
    │   │   │   │   ├── seat_management.sql
    │   │   │   │   ├── service_category.sql
    │   │   │   │   ├── service_people_category.sql
    │   │   │   │   ├── sos.sql
    │   │   │   │   ├── special_occasion.sql
    │   │   │   │   ├── special_zone_quote.sql
    │   │   │   │   ├── station.sql
    │   │   │   │   ├── stations_extra_information.sql
    │   │   │   │   ├── stop_information.sql
    │   │   │   │   ├── tag_action_notification_config.sql
    │   │   │   │   ├── ticket_booking.sql
    │   │   │   │   ├── ticket_booking_people_category.sql
    │   │   │   │   ├── ticket_booking_service.sql
    │   │   │   │   ├── ticket_booking_service_category.sql
    │   │   │   │   ├── ticket_merchant_details.sql
    │   │   │   │   ├── ticket_place.sql
    │   │   │   │   ├── ticket_service.sql
    │   │   │   │   ├── time_bound_config.sql
    │   │   │   │   ├── trip_terms.sql
    │   │   │   │   ├── ui_rider_config.sql
    │   │   │   │   ├── user_data.sql
    │   │   │   │   ├── value_add_np.sql
    │   │   │   │   ├── vehicle_route_mapping.sql
    │   │   │   │   ├── vendor_split_details.sql
    │   │   │   │   ├── walk_leg_multimodal.sql
    │   │   │   │   ├── webhook.sql
    │   │   │   │   └── white_list_org.sql
    │   │   │   ├── rider-dashboard/
    │   │   │   │   ├── API_AppManagement_Customer.sql
    │   │   │   │   ├── API_AppManagement_HotSpot.sql
    │   │   │   │   ├── API_AppManagement_Tickets.sql
    │   │   │   │   ├── API_IssueManagement_Issue.sql
    │   │   │   │   ├── API_IssueManagement_IssueList.sql
    │   │   │   │   ├── API_Management_Booking.sql
    │   │   │   │   ├── API_Management_Customer.sql
    │   │   │   │   ├── API_Management_FRFSTicket.sql
    │   │   │   │   ├── API_Management_Invoice.sql
    │   │   │   │   ├── API_Management_Merchant.sql
    │   │   │   │   ├── API_Management_NammaTag.sql
    │   │   │   │   ├── API_Management_Ride.sql
    │   │   │   │   ├── API_Management_System.sql
    │   │   │   │   ├── API_RideBooking_Booking.sql
    │   │   │   │   ├── API_RideBooking_Cancel.sql
    │   │   │   │   ├── API_RideBooking_Confirm.sql
    │   │   │   │   ├── API_RideBooking_Frontend.sql
    │   │   │   │   ├── API_RideBooking_Maps.sql
    │   │   │   │   ├── API_RideBooking_NotifyRideInfo.sql
    │   │   │   │   ├── API_RideBooking_Profile.sql
    │   │   │   │   ├── API_RideBooking_Quote.sql
    │   │   │   │   ├── API_RideBooking_Registration.sql
    │   │   │   │   ├── API_RideBooking_Search.sql
    │   │   │   │   ├── API_RideBooking_Select.sql
    │   │   │   │   ├── Local_API_AppManagement_Customer.sql
    │   │   │   │   ├── Local_API_AppManagement_EventManagement.sql
    │   │   │   │   ├── Local_API_AppManagement_HotSpot.sql
    │   │   │   │   ├── Local_API_AppManagement_MerchantOnboarding.sql
    │   │   │   │   ├── Local_API_AppManagement_TicketDashboard.sql
    │   │   │   │   ├── Local_API_AppManagement_Tickets.sql
    │   │   │   │   ├── Local_API_IssueManagement_Issue.sql
    │   │   │   │   ├── Local_API_IssueManagement_IssueList.sql
    │   │   │   │   ├── Local_API_Management_Booking.sql
    │   │   │   │   ├── Local_API_Management_Customer.sql
    │   │   │   │   ├── Local_API_Management_FRFSTicket.sql
    │   │   │   │   ├── Local_API_Management_Invoice.sql
    │   │   │   │   ├── Local_API_Management_Merchant.sql
    │   │   │   │   ├── Local_API_Management_NammaTag.sql
    │   │   │   │   ├── Local_API_Management_Ride.sql
    │   │   │   │   ├── Local_API_Management_System.sql
    │   │   │   │   ├── Local_API_RideBooking_Booking.sql
    │   │   │   │   ├── Local_API_RideBooking_Cancel.sql
    │   │   │   │   ├── Local_API_RideBooking_Confirm.sql
    │   │   │   │   ├── Local_API_RideBooking_Frontend.sql
    │   │   │   │   ├── Local_API_RideBooking_Maps.sql
    │   │   │   │   ├── Local_API_RideBooking_NotifyRideInfo.sql
    │   │   │   │   ├── Local_API_RideBooking_Profile.sql
    │   │   │   │   ├── Local_API_RideBooking_Quote.sql
    │   │   │   │   ├── Local_API_RideBooking_Registration.sql
    │   │   │   │   ├── Local_API_RideBooking_Search.sql
    │   │   │   │   └── Local_API_RideBooking_Select.sql
    │   │   │   └── safety-dashboard/
    │   │   │       ├── flagged_category.sql
    │   │   │       ├── merchant_configs.sql
    │   │   │       ├── notification.sql
    │   │   │       ├── portal_configs.sql
    │   │   │       ├── suspect.sql
    │   │   │       ├── suspect_flag_request.sql
    │   │   │       ├── suspect_status_change_request.sql
    │   │   │       └── suspect_status_history.sql
    │   │   ├── nginx/
    │   │   │   └── nginx.conf
    │   │   ├── osrm/
    │   │   │   ├── Dockerfile.osrm
    │   │   │   └── README
    │   │   ├── passetto-dev/
    │   │   │   ├── geninis.hs
    │   │   │   └── README
    │   │   ├── pgadmin/
    │   │   │   └── servers.json
    │   │   ├── prometheus/
    │   │   │   └── config.yml
    │   │   ├── redis/
    │   │   │   ├── setup-notifications.sh
    │   │   │   └── test-notifications.redis
    │   │   ├── reset/
    │   │   │   ├── reset_app.sql
    │   │   │   └── reset_dobpp.sql
    │   │   ├── rest-client/
    │   │   │   ├── environment.json
    │   │   │   ├── clickhouse/
    │   │   │   │   ├── fleet.http
    │   │   │   │   ├── revenue.http
    │   │   │   │   ├── ride-route.http
    │   │   │   │   └── ride.http
    │   │   │   ├── dashboard/
    │   │   │   │   ├── bap-dashboard/
    │   │   │   │   │   ├── dashboard.http
    │   │   │   │   │   └── app-backend/
    │   │   │   │   │       ├── customer.http
    │   │   │   │   │       ├── ride.http
    │   │   │   │   │       ├── appManagement/
    │   │   │   │   │       │   ├── hotSpot.http
    │   │   │   │   │       │   └── tickets.http
    │   │   │   │   │       ├── issueManagement/
    │   │   │   │   │       │   ├── issue.http
    │   │   │   │   │       │   └── issueList.http
    │   │   │   │   │       ├── management/
    │   │   │   │   │       │   ├── booking.http
    │   │   │   │   │       │   ├── merchant.http
    │   │   │   │   │       │   └── namma-tag.http
    │   │   │   │   │       └── rideBooking/
    │   │   │   │   │           ├── booking.http
    │   │   │   │   │           ├── cancel.http
    │   │   │   │   │           ├── confirm.http
    │   │   │   │   │           ├── frontend.http
    │   │   │   │   │           ├── maps.http
    │   │   │   │   │           ├── profile.http
    │   │   │   │   │           ├── quote.http
    │   │   │   │   │           ├── registration.http
    │   │   │   │   │           ├── search.http
    │   │   │   │   │           └── select.http
    │   │   │   │   └── bpp-dashboard/
    │   │   │   │       ├── dashboard.http
    │   │   │   │       ├── exotel.http
    │   │   │   │       └── driver-offer-bpp/
    │   │   │   │           ├── documents-flow.http
    │   │   │   │           ├── AppManagement/
    │   │   │   │           │   ├── driver.http
    │   │   │   │           │   ├── driverSubscription.http
    │   │   │   │           │   ├── overlay.http
    │   │   │   │           │   └── subscription.http
    │   │   │   │           ├── fleet/
    │   │   │   │           │   ├── driver.http
    │   │   │   │           │   ├── onboarding.http
    │   │   │   │           │   ├── registration.http
    │   │   │   │           │   └── registrationV2.http
    │   │   │   │           ├── issueManagement/
    │   │   │   │           │   └── issue.http
    │   │   │   │           ├── management/
    │   │   │   │           │   ├── booking.http
    │   │   │   │           │   ├── driver.http
    │   │   │   │           │   ├── driverCoins.http
    │   │   │   │           │   ├── driverGoHome.http
    │   │   │   │           │   ├── driverReferral.http
    │   │   │   │           │   ├── driverRegistration.http
    │   │   │   │           │   ├── issue.http
    │   │   │   │           │   ├── media.http
    │   │   │   │           │   ├── merchant.http
    │   │   │   │           │   ├── message.http
    │   │   │   │           │   ├── nammaTag.http
    │   │   │   │           │   ├── payout.http
    │   │   │   │           │   ├── revenue.http
    │   │   │   │           │   ├── ride.http
    │   │   │   │           │   └── system.http
    │   │   │   │           ├── Operator/
    │   │   │   │           │   ├── driver.http
    │   │   │   │           │   ├── fleetManagement.http
    │   │   │   │           │   └── registration.http
    │   │   │   │           └── rideBooking/
    │   │   │   │               ├── driver.http
    │   │   │   │               ├── driverRegistration.http
    │   │   │   │               ├── maps.http
    │   │   │   │               ├── ride.http
    │   │   │   │               └── volunteer.http
    │   │   │   ├── dynamic-offer-driver-app/
    │   │   │   │   ├── auto-flow.http
    │   │   │   │   ├── coinsConfig.http
    │   │   │   │   ├── driver-offers-quote-twice.http
    │   │   │   │   ├── driver.http
    │   │   │   │   ├── fare_policy.http
    │   │   │   │   ├── go-home-flow.http
    │   │   │   │   ├── Invoice.http
    │   │   │   │   ├── onboarding.http
    │   │   │   │   ├── payment.http
    │   │   │   │   ├── repeatition.http
    │   │   │   │   └── second-driver-offers-after-confirm.http
    │   │   │   ├── mock-google/
    │   │   │   │   ├── google-distance-matrix.http
    │   │   │   │   ├── google-place-name.http
    │   │   │   │   ├── google-snap-to-road.http
    │   │   │   │   ├── mock-google-directions.http
    │   │   │   │   ├── mock-google-distance-matrix.http
    │   │   │   │   └── mock-google-snap-to-road.http
    │   │   │   ├── mock-public-transport-provider-platform/
    │   │   │   │   └── provider-platform.http
    │   │   │   ├── osrm/
    │   │   │   │   └── test.http
    │   │   │   ├── public-transport/
    │   │   │   │   ├── rider-platform-vim.http
    │   │   │   │   └── rider-platform-vscode.http
    │   │   │   ├── registry/
    │   │   │   │   └── registry.http
    │   │   │   └── rider-app/
    │   │   │       ├── auth.http
    │   │   │       ├── call.http
    │   │   │       ├── emergency_num.http
    │   │   │       ├── payment.http
    │   │   │       ├── rentalSearch.http
    │   │   │       ├── rideBooking.http
    │   │   │       ├── route.http
    │   │   │       ├── serviceability.http
    │   │   │       ├── support.http
    │   │   │       ├── ticket-service.http
    │   │   │       └── translation.http
    │   │   ├── sql-seed/
    │   │   │   ├── driver-location-seed.sql
    │   │   │   ├── dynamic-offer-driver-app-seed.sql
    │   │   │   ├── fmd-wrapper-backend-seed.sql
    │   │   │   ├── kaal-chakra-seed.sql
    │   │   │   ├── mock-registry-seed.sql
    │   │   │   ├── passetto-seed.sql
    │   │   │   ├── pre-init.sql
    │   │   │   ├── provider-dashboard-seed.sql
    │   │   │   ├── public-transport-rider-platform-seed.sql
    │   │   │   ├── rider-app-seed.sql
    │   │   │   ├── rider-dashboard-seed.sql
    │   │   │   ├── safety-dashboard-seed.sql
    │   │   │   ├── scheduler-example-seed.sql
    │   │   │   └── special-zone-seed.sql
    │   │   └── visualize-ride/
    │   │       ├── visualize-ride.sh
    │   │       └── visualizeTests.sh
    │   ├── dhall-configs/
    │   │   ├── dev/
    │   │   │   ├── beckn-gateway.dhall
    │   │   │   ├── broadcast-message.dhall
    │   │   │   ├── common.dhall
    │   │   │   ├── driver-availability-calculator.dhall
    │   │   │   ├── driver-drainer.dhall
    │   │   │   ├── driver-offer-allocator.dhall
    │   │   │   ├── driver-tracking-healthcheck-service.dhall
    │   │   │   ├── dynamic-offer-driver-app.dhall
    │   │   │   ├── integration-tests.dhall
    │   │   │   ├── location-update.dhall
    │   │   │   ├── location_tracking_service.dhall
    │   │   │   ├── mock-google.dhall
    │   │   │   ├── mock-public-transport-provider-platform.dhall
    │   │   │   ├── mock-registry.dhall
    │   │   │   ├── mock-rider-platform.dhall
    │   │   │   ├── person-stats.dhall
    │   │   │   ├── producer.dhall
    │   │   │   ├── provider-dashboard.dhall
    │   │   │   ├── public-transport-rider-platform.dhall
    │   │   │   ├── public-transport-search-consumer.dhall
    │   │   │   ├── rider-app-scheduler.dhall
    │   │   │   ├── rider-app.dhall
    │   │   │   ├── rider-dashboard.dhall
    │   │   │   ├── rider-drainer.dhall
    │   │   │   ├── rider-producer.dhall
    │   │   │   ├── safety-dashboard.dhall
    │   │   │   ├── sdk-event-pipeline.dhall
    │   │   │   ├── search-result-aggregator.dhall
    │   │   │   ├── special-zone.dhall
    │   │   │   └── secrets/
    │   │   │       ├── beckn-gateway.dhall
    │   │   │       ├── common.dhall
    │   │   │       ├── dynamic-offer-driver-app.dhall
    │   │   │       ├── integration-tests.dhall
    │   │   │       ├── kaal-chakra.dhall
    │   │   │       ├── mock-registry.dhall
    │   │   │       ├── provider-dashboard.dhall
    │   │   │       ├── public-transport-rider-platform.dhall
    │   │   │       ├── rider-app.dhall
    │   │   │       ├── rider-dashboard.dhall
    │   │   │       ├── safety-dashboard.dhall
    │   │   │       └── top-secret-template.dhall
    │   │   └── generic/
    │   │       └── common.dhall
    │   ├── doc/
    │   │   ├── architechture.md
    │   │   ├── auth.md
    │   │   ├── databases.md
    │   │   ├── error-handling.md
    │   │   ├── registry-encryption.md
    │   │   ├── rest-client.md
    │   │   ├── signatures.md
    │   │   └── validation.md
    │   ├── docs/
    │   │   └── database-schema.md
    │   ├── geo_config/
    │   │   ├── karnataka.json
    │   │   ├── kerala.json
    │   │   └── kolkata.json
    │   ├── lib/
    │   │   ├── beckn-services/
    │   │   │   ├── beckn-services.cabal
    │   │   │   ├── package.yaml
    │   │   │   └── src/
    │   │   │       └── AWS/
    │   │   │           ├── S3.hs
    │   │   │           └── S3/
    │   │   │               ├── Error.hs
    │   │   │               ├── Flow.hs
    │   │   │               ├── Init.hs
    │   │   │               ├── SignatureAuth.hs
    │   │   │               ├── Types.hs
    │   │   │               └── Utils.hs
    │   │   ├── beckn-spec/
    │   │   │   ├── beckn-spec.cabal
    │   │   │   ├── package.yaml
    │   │   │   └── src/
    │   │   │       ├── Beckn/
    │   │   │       │   └── Types/
    │   │   │       │       └── Core/
    │   │   │       │           ├── Metro/
    │   │   │       │           │   ├── Common.hs
    │   │   │       │           │   ├── OnSearch.hs
    │   │   │       │           │   ├── Search.hs
    │   │   │       │           │   ├── API/
    │   │   │       │           │   │   ├── OnSearch.hs
    │   │   │       │           │   │   └── Search.hs
    │   │   │       │           │   ├── Common/
    │   │   │       │           │   │   └── DecimalValue.hs
    │   │   │       │           │   ├── OnSearch/
    │   │   │       │           │   │   ├── Catalog.hs
    │   │   │       │           │   │   ├── Descriptor.hs
    │   │   │       │           │   │   ├── Fulfillment.hs
    │   │   │       │           │   │   ├── Gps.hs
    │   │   │       │           │   │   ├── Item.hs
    │   │   │       │           │   │   ├── Location.hs
    │   │   │       │           │   │   ├── Price.hs
    │   │   │       │           │   │   ├── Provider.hs
    │   │   │       │           │   │   ├── Time.hs
    │   │   │       │           │   │   └── Vehicle.hs
    │   │   │       │           │   └── Search/
    │   │   │       │           │       ├── Address.hs
    │   │   │       │           │       ├── Category.hs
    │   │   │       │           │       ├── Circle.hs
    │   │   │       │           │       ├── City.hs
    │   │   │       │           │       ├── Country.hs
    │   │   │       │           │       ├── Descriptor.hs
    │   │   │       │           │       ├── Duration.hs
    │   │   │       │           │       ├── Gps.hs
    │   │   │       │           │       ├── Image.hs
    │   │   │       │           │       ├── Intent.hs
    │   │   │       │           │       ├── Item.hs
    │   │   │       │           │       ├── Location.hs
    │   │   │       │           │       ├── Offer.hs
    │   │   │       │           │       ├── Payment.hs
    │   │   │       │           │       ├── Price.hs
    │   │   │       │           │       ├── Scalar.hs
    │   │   │       │           │       ├── Tags.hs
    │   │   │       │           │       ├── Time.hs
    │   │   │       │           │       └── Vehicle.hs
    │   │   │       │           └── Taxi/
    │   │   │       │               ├── Confirm.hs
    │   │   │       │               ├── Init.hs
    │   │   │       │               ├── OnConfirm.hs
    │   │   │       │               ├── OnInit.hs
    │   │   │       │               ├── OnSearch.hs
    │   │   │       │               ├── OnSelect.hs
    │   │   │       │               ├── OnStatus.hs
    │   │   │       │               ├── OnTrack.hs
    │   │   │       │               ├── OnUpdate.hs
    │   │   │       │               ├── Rating.hs
    │   │   │       │               ├── Search.hs
    │   │   │       │               ├── Select.hs
    │   │   │       │               ├── Status.hs
    │   │   │       │               ├── Track.hs
    │   │   │       │               ├── Update.hs
    │   │   │       │               ├── API/
    │   │   │       │               │   ├── Cancel.hs
    │   │   │       │               │   ├── Confirm.hs
    │   │   │       │               │   ├── Init.hs
    │   │   │       │               │   ├── OnCancel.hs
    │   │   │       │               │   ├── OnConfirm.hs
    │   │   │       │               │   ├── OnInit.hs
    │   │   │       │               │   ├── OnSearch.hs
    │   │   │       │               │   ├── OnSelect.hs
    │   │   │       │               │   ├── OnStatus.hs
    │   │   │       │               │   ├── OnTrack.hs
    │   │   │       │               │   ├── OnUpdate.hs
    │   │   │       │               │   ├── Rating.hs
    │   │   │       │               │   ├── Search.hs
    │   │   │       │               │   ├── Select.hs
    │   │   │       │               │   ├── Status.hs
    │   │   │       │               │   ├── Track.hs
    │   │   │       │               │   └── Update.hs
    │   │   │       │               ├── Cancel/
    │   │   │       │               │   └── Req.hs
    │   │   │       │               ├── Common/
    │   │   │       │               │   ├── Address.hs
    │   │   │       │               │   ├── Agent.hs
    │   │   │       │               │   ├── Authorization.hs
    │   │   │       │               │   ├── BreakupItem.hs
    │   │   │       │               │   ├── CancellationSource.hs
    │   │   │       │               │   ├── Customer.hs
    │   │   │       │               │   ├── DecimalValue.hs
    │   │   │       │               │   ├── Descriptor.hs
    │   │   │       │               │   ├── FareProductType.hs
    │   │   │       │               │   ├── FulfillmentInfo.hs
    │   │   │       │               │   ├── Gps.hs
    │   │   │       │               │   ├── ItemCode.hs
    │   │   │       │               │   ├── Location.hs
    │   │   │       │               │   ├── Payment.hs
    │   │   │       │               │   ├── PaymentCollector.hs
    │   │   │       │               │   ├── PaymentInstrument.hs
    │   │   │       │               │   ├── PaymentType.hs
    │   │   │       │               │   ├── Price.hs
    │   │   │       │               │   ├── Provider.hs
    │   │   │       │               │   ├── Quote.hs
    │   │   │       │               │   ├── RideCompletedQuote.hs
    │   │   │       │               │   ├── StartInfo.hs
    │   │   │       │               │   ├── StopInfo.hs
    │   │   │       │               │   ├── Tags.hs
    │   │   │       │               │   ├── TimeDuration.hs
    │   │   │       │               │   ├── TimeTimestamp.hs
    │   │   │       │               │   └── Vehicle.hs
    │   │   │       │               ├── Confirm/
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   └── Order.hs
    │   │   │       │               ├── Init/
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   └── Order.hs
    │   │   │       │               ├── OnConfirm/
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   └── Order.hs
    │   │   │       │               ├── OnInit/
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   ├── Order.hs
    │   │   │       │               │   └── OrderState.hs
    │   │   │       │               ├── OnSearch/
    │   │   │       │               │   ├── Addon.hs
    │   │   │       │               │   ├── Catalog.hs
    │   │   │       │               │   ├── Category.hs
    │   │   │       │               │   ├── Descriptor.hs
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   ├── Item.hs
    │   │   │       │               │   ├── Location.hs
    │   │   │       │               │   ├── Offer.hs
    │   │   │       │               │   ├── Price.hs
    │   │   │       │               │   ├── Provider.hs
    │   │   │       │               │   ├── ProviderLocation.hs
    │   │   │       │               │   ├── StartInfo.hs
    │   │   │       │               │   └── StopInfo.hs
    │   │   │       │               ├── OnSelect/
    │   │   │       │               │   ├── Addon.hs
    │   │   │       │               │   ├── Agent.hs
    │   │   │       │               │   ├── Category.hs
    │   │   │       │               │   ├── Descriptor.hs
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   ├── Item.hs
    │   │   │       │               │   ├── Location.hs
    │   │   │       │               │   ├── Offer.hs
    │   │   │       │               │   ├── Order.hs
    │   │   │       │               │   ├── Price.hs
    │   │   │       │               │   ├── Provider.hs
    │   │   │       │               │   ├── ProviderLocation.hs
    │   │   │       │               │   ├── Quote.hs
    │   │   │       │               │   ├── StartInfo.hs
    │   │   │       │               │   └── StopInfo.hs
    │   │   │       │               ├── OnStatus/
    │   │   │       │               │   ├── Order.hs
    │   │   │       │               │   └── Order/
    │   │   │       │               │       ├── BookingCancelledOrder.hs
    │   │   │       │               │       ├── BookingReallocationOrder.hs
    │   │   │       │               │       ├── NewBookingOrder.hs
    │   │   │       │               │       ├── OrderState.hs
    │   │   │       │               │       ├── RideAssignedOrder.hs
    │   │   │       │               │       ├── RideCompletedOrder.hs
    │   │   │       │               │       └── RideStartedOrder.hs
    │   │   │       │               ├── OnTrack/
    │   │   │       │               │   └── Tracking.hs
    │   │   │       │               ├── OnUpdate/
    │   │   │       │               │   ├── OnUpdateEvent.hs
    │   │   │       │               │   └── OnUpdateEvent/
    │   │   │       │               │       ├── BookingCancelledEvent.hs
    │   │   │       │               │       ├── BookingReallocationEvent.hs
    │   │   │       │               │       ├── DriverArrivedEvent.hs
    │   │   │       │               │       ├── EstimateRepetitionEvent.hs
    │   │   │       │               │       ├── NewMessageEvent.hs
    │   │   │       │               │       ├── OnUpdateEventType.hs
    │   │   │       │               │       ├── QuoteRepetitionEvent.hs
    │   │   │       │               │       ├── RideAssignedEvent.hs
    │   │   │       │               │       ├── RideCompletedEvent.hs
    │   │   │       │               │       ├── RideStartedEvent.hs
    │   │   │       │               │       ├── SafetyAlertEvent.hs
    │   │   │       │               │       └── StopArrivedEvent.hs
    │   │   │       │               ├── Rating/
    │   │   │       │               │   └── FeedbackForm.hs
    │   │   │       │               ├── Search/
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   ├── Intent.hs
    │   │   │       │               │   ├── Location.hs
    │   │   │       │               │   ├── StartInfo.hs
    │   │   │       │               │   ├── StopInfo.hs
    │   │   │       │               │   └── Time.hs
    │   │   │       │               ├── Select/
    │   │   │       │               │   ├── Fulfillment.hs
    │   │   │       │               │   ├── Order.hs
    │   │   │       │               │   ├── Quote.hs
    │   │   │       │               │   └── StartInfo.hs
    │   │   │       │               ├── Track/
    │   │   │       │               │   └── Order.hs
    │   │   │       │               └── Update/
    │   │   │       │                   ├── UpdateEvent.hs
    │   │   │       │                   └── UpdateEvent/
    │   │   │       │                       ├── AddStopEvent.hs
    │   │   │       │                       ├── EditLocationEvent.hs
    │   │   │       │                       ├── EditStopEvent.hs
    │   │   │       │                       ├── PaymentCompletedEvent.hs
    │   │   │       │                       ├── UpdateEventType.hs
    │   │   │       │                       └── PaymentCompletedEvent/
    │   │   │       │                           └── Payment.hs
    │   │   │       ├── BecknV2/
    │   │   │       │   ├── Utils.hs
    │   │   │       │   ├── FRFS/
    │   │   │       │   │   ├── APIs.hs
    │   │   │       │   │   ├── Enums.hs
    │   │   │       │   │   ├── Types.hs
    │   │   │       │   │   └── Utils.hs
    │   │   │       │   ├── IGM/
    │   │   │       │   │   └── APIs.hs
    │   │   │       │   └── OnDemand/
    │   │   │       │       ├── Enums.hs
    │   │   │       │       ├── Tags.hs
    │   │   │       │       ├── Types.hs
    │   │   │       │       └── Utils/
    │   │   │       │           ├── Common.hs
    │   │   │       │           ├── Context.hs
    │   │   │       │           └── Payment.hs
    │   │   │       ├── Domain/
    │   │   │       │   ├── Types.hs
    │   │   │       │   ├── Utils.hs
    │   │   │       │   └── Types/
    │   │   │       │       ├── Alert.hs
    │   │   │       │       ├── EmptyDynamicParam.hs
    │   │   │       │       ├── FareProductType.hs
    │   │   │       │       ├── FleetBadgeType.hs
    │   │   │       │       ├── FRFSRouteDetails.hs
    │   │   │       │       ├── RefereeLink.hs
    │   │   │       │       ├── ServiceTierType.hs
    │   │   │       │       ├── StationType.hs
    │   │   │       │       ├── Trip.hs
    │   │   │       │       ├── VehicleCategory.hs
    │   │   │       │       ├── VehicleVariant.hs
    │   │   │       │       └── Alert/
    │   │   │       │           ├── AlertRequestData.hs
    │   │   │       │           ├── AlertRequestStatus.hs
    │   │   │       │           ├── AlertRequestType.hs
    │   │   │       │           └── DetectionData.hs
    │   │   │       └── IGM/
    │   │   │           ├── Enums.hs
    │   │   │           ├── Types.hs
    │   │   │           └── Utils.hs
    │   │   ├── dashcam/
    │   │   │   ├── dashcam.cabal
    │   │   │   ├── package.yaml
    │   │   │   └── src/
    │   │   │       └── Lib/
    │   │   │           └── Dashcam/
    │   │   │               └── Domain/
    │   │   │                   ├── Interface.hs
    │   │   │                   ├── Types.hs
    │   │   │                   └── Cautio/
    │   │   │                       ├── Flow.hs
    │   │   │                       └── Types.hs
    │   │   ├── external/
    │   │   │   ├── external.cabal
    │   │   │   ├── package.yaml
    │   │   │   └── src/
    │   │   │       ├── ChatCompletion/
    │   │   │       │   ├── Interface.hs
    │   │   │       │   ├── Types.hs
    │   │   │       │   ├── AzureOpenAI/
    │   │   │       │   │   ├── API.hs
    │   │   │       │   │   ├── Config.hs
    │   │   │       │   │   └── Types.hs
    │   │   │       │   ├── Gemini/
    │   │   │       │   │   ├── API.hs
    │   │   │       │   │   ├── Config.hs
    │   │   │       │   │   └── Types.hs
    │   │   │       │   └── Interface/
    │   │   │       │       ├── AzureOpenAI.hs
    │   │   │       │       ├── Gemini.hs
    │   │   │       │       └── Types.hs
    │   │   │       ├── Email/
    │   │   │       │   ├── Types.hs
    │   │   │       │   └── AWS/
    │   │   │       │       └── Flow.hs
    │   │   │       ├── KafkaLogs/
    │   │   │       │   └── TransactionLogs.hs
    │   │   │       ├── Slack/
    │   │   │       │   └── AWS/
    │   │   │       │       └── Flow.hs
    │   │   │       └── TransactionLogs/
    │   │   │           ├── Interface.hs
    │   │   │           ├── PushLogs.hs
    │   │   │           ├── Types.hs
    │   │   │           ├── Interface/
    │   │   │           │   ├── ONDC.hs
    │   │   │           │   └── Types.hs
    │   │   │           └── ONDC/
    │   │   │               ├── Flow.hs
    │   │   │               └── Types.hs
    │   │   ├── location-updates/
    │   │   │   ├── location-updates.cabal
    │   │   │   ├── package.yaml
    │   │   │   ├── src/
    │   │   │   │   └── Lib/
    │   │   │   │       ├── LocationUpdates.hs
    │   │   │   │       └── LocationUpdates/
    │   │   │   │           └── Internal.hs
    │   │   │   └── test/
    │   │   │       ├── app/
    │   │   │       │   └── Main.hs
    │   │   │       └── src/
    │   │   │           ├── API.hs
    │   │   │           ├── RedisAlgorithm.hs
    │   │   │           ├── Routes.hs
    │   │   │           └── Utils.hs
    │   │   ├── payment/
    │   │   │   ├── package.yaml
    │   │   │   ├── payment.cabal
    │   │   │   ├── spec/
    │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   └── Storage/
    │   │   │   │       ├── Payouts.yaml
    │   │   │   │       └── Refunds.yaml
    │   │   │   ├── src/
    │   │   │   │   └── Lib/
    │   │   │   │       └── Payment/
    │   │   │   │           ├── API.hs
    │   │   │   │           ├── Domain/
    │   │   │   │           │   ├── Action.hs
    │   │   │   │           │   └── Types/
    │   │   │   │           │       ├── Common.hs
    │   │   │   │           │       ├── PaymentOrder.hs
    │   │   │   │           │       └── PaymentTransaction.hs
    │   │   │   │           └── Storage/
    │   │   │   │               ├── Beam/
    │   │   │   │               │   ├── BeamFlow.hs
    │   │   │   │               │   ├── PaymentOrder.hs
    │   │   │   │               │   └── PaymentTransaction.hs
    │   │   │   │               └── Queries/
    │   │   │   │                   ├── PaymentOrder.hs
    │   │   │   │                   ├── PaymentTransaction.hs
    │   │   │   │                   └── PayoutOrderExtra.hs
    │   │   │   └── src-read-only/
    │   │   │       └── Lib/
    │   │   │           └── Payment/
    │   │   │               ├── Domain/
    │   │   │               │   └── Types/
    │   │   │               │       ├── PayoutOrder.hs
    │   │   │               │       ├── PayoutTransaction.hs
    │   │   │               │       └── Refunds.hs
    │   │   │               └── Storage/
    │   │   │                   ├── Beam/
    │   │   │                   │   ├── PayoutOrder.hs
    │   │   │                   │   ├── PayoutTransaction.hs
    │   │   │                   │   └── Refunds.hs
    │   │   │                   └── Queries/
    │   │   │                       ├── PayoutOrder.hs
    │   │   │                       ├── PayoutTransaction.hs
    │   │   │                       ├── Refunds.hs
    │   │   │                       └── OrphanInstances/
    │   │   │                           ├── PayoutOrder.hs
    │   │   │                           └── Refunds.hs
    │   │   ├── producer/
    │   │   │   ├── package.yaml
    │   │   │   ├── producer.cabal
    │   │   │   ├── app/
    │   │   │   │   └── Main.hs
    │   │   │   └── src/
    │   │   │       ├── App.hs
    │   │   │       ├── Environment.hs
    │   │   │       └── Producer/
    │   │   │           ├── Flow.hs
    │   │   │           └── SchedulerJob.hs
    │   │   ├── scheduler/
    │   │   │   ├── package.yaml
    │   │   │   ├── scheduler.cabal
    │   │   │   └── src/
    │   │   │       └── Lib/
    │   │   │           ├── Scheduler.hs
    │   │   │           └── Scheduler/
    │   │   │               ├── App.hs
    │   │   │               ├── Environment.hs
    │   │   │               ├── Error.hs
    │   │   │               ├── Handler.hs
    │   │   │               ├── JobHandler.hs
    │   │   │               ├── Metrics.hs
    │   │   │               ├── ScheduleJob.hs
    │   │   │               ├── Types.hs
    │   │   │               └── JobStorageType/
    │   │   │                   ├── SchedulerType.hs
    │   │   │                   ├── DB/
    │   │   │                   │   ├── Queries.hs
    │   │   │                   │   └── Table.hs
    │   │   │                   └── Redis/
    │   │   │                       └── Queries.hs
    │   │   ├── sessionizer-metrics/
    │   │   │   ├── package.yaml
    │   │   │   ├── sessionizer-metrics.cabal
    │   │   │   └── src/
    │   │   │       └── Lib/
    │   │   │           └── SessionizerMetrics/
    │   │   │               ├── EventStream.hs
    │   │   │               ├── Kafka/
    │   │   │               │   ├── Config.hs
    │   │   │               │   └── Internal.hs
    │   │   │               ├── Prometheus/
    │   │   │               │   └── Internal.hs
    │   │   │               └── Types/
    │   │   │                   └── Event.hs
    │   │   ├── shared-services/
    │   │   │   ├── package.yaml
    │   │   │   ├── shared-services.cabal
    │   │   │   ├── spec/
    │   │   │   │   └── IssueManagement/
    │   │   │   │       ├── issue-management-common.dhall
    │   │   │   │       ├── ProviderPlatform/
    │   │   │   │       │   ├── dsl-config.dhall
    │   │   │   │       │   └── API/
    │   │   │   │       │       └── Issue.yaml
    │   │   │   │       └── RiderPlatform/
    │   │   │   │           ├── dsl-config.dhall
    │   │   │   │           └── API/
    │   │   │   │               ├── Issue.yaml
    │   │   │   │               └── IssueList.yaml
    │   │   │   ├── src/
    │   │   │   │   ├── ConfigPilotFrontend/
    │   │   │   │   │   ├── API.hs
    │   │   │   │   │   ├── Common.hs
    │   │   │   │   │   ├── Flow.hs
    │   │   │   │   │   └── Types.hs
    │   │   │   │   ├── IssueManagement/
    │   │   │   │   │   ├── Common.hs
    │   │   │   │   │   ├── API/
    │   │   │   │   │   │   ├── Beckn/
    │   │   │   │   │   │   │   └── Issue.hs
    │   │   │   │   │   │   └── UI/
    │   │   │   │   │   │       └── Issue.hs
    │   │   │   │   │   ├── Beckn/
    │   │   │   │   │   │   └── ACL/
    │   │   │   │   │   │       ├── Issue.hs
    │   │   │   │   │   │       ├── IssueStatus.hs
    │   │   │   │   │   │       ├── OnIssue.hs
    │   │   │   │   │   │       ├── OnIssueStatus.hs
    │   │   │   │   │   │       └── IGM/
    │   │   │   │   │   │           └── Utils.hs
    │   │   │   │   │   ├── Common/
    │   │   │   │   │   │   ├── Beckn/
    │   │   │   │   │   │   │   └── Issue.hs
    │   │   │   │   │   │   ├── Dashboard/
    │   │   │   │   │   │   │   └── Issue.hs
    │   │   │   │   │   │   └── UI/
    │   │   │   │   │   │       └── Issue.hs
    │   │   │   │   │   ├── Domain/
    │   │   │   │   │   │   ├── Action/
    │   │   │   │   │   │   │   ├── Beckn/
    │   │   │   │   │   │   │   │   ├── Issue.hs
    │   │   │   │   │   │   │   │   ├── IssueStatus.hs
    │   │   │   │   │   │   │   │   ├── OnIssue.hs
    │   │   │   │   │   │   │   │   └── OnIssueStatus.hs
    │   │   │   │   │   │   │   ├── Dashboard/
    │   │   │   │   │   │   │   │   └── Issue.hs
    │   │   │   │   │   │   │   └── UI/
    │   │   │   │   │   │   │       └── Issue.hs
    │   │   │   │   │   │   └── Types/
    │   │   │   │   │   │       ├── MediaFile.hs
    │   │   │   │   │   │       └── Issue/
    │   │   │   │   │   │           ├── Comment.hs
    │   │   │   │   │   │           ├── IGMConfig.hs
    │   │   │   │   │   │           ├── IGMIssue.hs
    │   │   │   │   │   │           ├── IssueCategory.hs
    │   │   │   │   │   │           ├── IssueChat.hs
    │   │   │   │   │   │           ├── IssueConfig.hs
    │   │   │   │   │   │           ├── IssueMessage.hs
    │   │   │   │   │   │           ├── IssueOption.hs
    │   │   │   │   │   │           ├── IssueReport.hs
    │   │   │   │   │   │           └── IssueTranslation.hs
    │   │   │   │   │   ├── SharedLogic/
    │   │   │   │   │   │   └── CallAPI.hs
    │   │   │   │   │   ├── Storage/
    │   │   │   │   │   │   ├── BeamFlow.hs
    │   │   │   │   │   │   ├── Beam/
    │   │   │   │   │   │   │   ├── MediaFile.hs
    │   │   │   │   │   │   │   └── Issue/
    │   │   │   │   │   │   │       ├── Comment.hs
    │   │   │   │   │   │   │       ├── IGMConfig.hs
    │   │   │   │   │   │   │       ├── IGMIssue.hs
    │   │   │   │   │   │   │       ├── IssueCategory.hs
    │   │   │   │   │   │   │       ├── IssueChat.hs
    │   │   │   │   │   │   │       ├── IssueConfig.hs
    │   │   │   │   │   │   │       ├── IssueMessage.hs
    │   │   │   │   │   │   │       ├── IssueOption.hs
    │   │   │   │   │   │   │       ├── IssueReport.hs
    │   │   │   │   │   │   │       └── IssueTranslation.hs
    │   │   │   │   │   │   ├── CachedQueries/
    │   │   │   │   │   │   │   ├── MediaFile.hs
    │   │   │   │   │   │   │   └── Issue/
    │   │   │   │   │   │   │       ├── IssueCategory.hs
    │   │   │   │   │   │   │       ├── IssueConfig.hs
    │   │   │   │   │   │   │       ├── IssueMessage.hs
    │   │   │   │   │   │   │       └── IssueOption.hs
    │   │   │   │   │   │   └── Queries/
    │   │   │   │   │   │       ├── MediaFile.hs
    │   │   │   │   │   │       └── Issue/
    │   │   │   │   │   │           ├── Comment.hs
    │   │   │   │   │   │           ├── IGMConfig.hs
    │   │   │   │   │   │           ├── IGMIssue.hs
    │   │   │   │   │   │           ├── IssueCategory.hs
    │   │   │   │   │   │           ├── IssueChat.hs
    │   │   │   │   │   │           ├── IssueConfig.hs
    │   │   │   │   │   │           ├── IssueMessage.hs
    │   │   │   │   │   │           ├── IssueOption.hs
    │   │   │   │   │   │           ├── IssueReport.hs
    │   │   │   │   │   │           └── IssueTranslation.hs
    │   │   │   │   │   └── Tools/
    │   │   │   │   │       ├── Error.hs
    │   │   │   │   │       └── UtilsTH.hs
    │   │   │   │   ├── Registry/
    │   │   │   │   │   └── Beckn/
    │   │   │   │   │       ├── Interface.hs
    │   │   │   │   │       ├── Interface/
    │   │   │   │   │       │   ├── Nammayatri.hs
    │   │   │   │   │       │   └── Types.hs
    │   │   │   │   │       └── Nammayatri/
    │   │   │   │   │           ├── Flow.hs
    │   │   │   │   │           └── Types.hs
    │   │   │   │   └── UrlShortner/
    │   │   │   │       ├── Common.hs
    │   │   │   │       └── Types.hs
    │   │   │   └── src-read-only/
    │   │   │       └── API/
    │   │   │           └── Types/
    │   │   │               ├── ProviderPlatform/
    │   │   │               │   ├── IssueManagement.hs
    │   │   │               │   └── IssueManagement/
    │   │   │               │       ├── Issue.hs
    │   │   │               │       └── Endpoints/
    │   │   │               │           └── Issue.hs
    │   │   │               └── RiderPlatform/
    │   │   │                   ├── IssueManagement.hs
    │   │   │                   └── IssueManagement/
    │   │   │                       ├── Issue.hs
    │   │   │                       ├── IssueList.hs
    │   │   │                       └── Endpoints/
    │   │   │                           ├── Issue.hs
    │   │   │                           └── IssueList.hs
    │   │   ├── special-zone/
    │   │   │   ├── package.yaml
    │   │   │   ├── special-zone-a.cabal
    │   │   │   └── src/
    │   │   │       └── Lib/
    │   │   │           ├── Queries/
    │   │   │           │   ├── GateInfo.hs
    │   │   │           │   ├── GateInfoGeom.hs
    │   │   │           │   ├── SpecialLocation.hs
    │   │   │           │   ├── SpecialLocationGeom.hs
    │   │   │           │   └── SpecialLocationPriority.hs
    │   │   │           ├── Tabular/
    │   │   │           │   ├── GateInfo.hs
    │   │   │           │   ├── GateInfoGeom.hs
    │   │   │           │   ├── SpecialLocation.hs
    │   │   │           │   ├── SpecialLocationGeom.hs
    │   │   │           │   └── SpecialLocationPriority.hs
    │   │   │           └── Types/
    │   │   │               ├── GateInfo.hs
    │   │   │               ├── SpecialLocation.hs
    │   │   │               └── SpecialLocationPriority.hs
    │   │   ├── utils/
    │   │   │   ├── package.yaml
    │   │   │   ├── utils.cabal
    │   │   │   └── src/
    │   │   │       ├── Tools/
    │   │   │       │   ├── DynamicLogic.hs
    │   │   │       │   └── SharedRedisKeys.hs
    │   │   │       └── Utils/
    │   │   │           ├── Common/
    │   │   │           │   ├── CacUtils.hs
    │   │   │           │   ├── Events.hs
    │   │   │           │   ├── Cac/
    │   │   │           │   │   ├── ContextConstants.hs
    │   │   │           │   │   ├── KeyNameConstants.hs
    │   │   │           │   │   ├── PrefixConstants.hs
    │   │   │           │   │   ├── UtilsConstants.hs
    │   │   │           │   │   └── UtilsTH.hs
    │   │   │           │   └── JWT/
    │   │   │           │       ├── Config.hs
    │   │   │           │       └── TransitClaim.hs
    │   │   │           └── QRCode/
    │   │   │               └── Scanner.hs
    │   │   ├── webhook/
    │   │   │   ├── package.yaml
    │   │   │   ├── webhook.cabal
    │   │   │   ├── spec/
    │   │   │   │   ├── dsl-config.dhall
    │   │   │   │   └── Storage/
    │   │   │   │       └── webhook.yaml
    │   │   │   ├── src/
    │   │   │   │   ├── Domain/
    │   │   │   │   │   ├── Action/
    │   │   │   │   │   │   ├── Flow.hs
    │   │   │   │   │   │   └── WebhookHandler.hs
    │   │   │   │   │   └── Types/
    │   │   │   │   │       └── WebhookExtra.hs
    │   │   │   │   └── Lib/
    │   │   │   │       └── Webhook/
    │   │   │   │           └── Storage/
    │   │   │   │               ├── Beam/
    │   │   │   │               │   └── BeamFlow.hs
    │   │   │   │               └── Queries/
    │   │   │   │                   └── WebhookExtra.hs
    │   │   │   └── src-read-only/
    │   │   │       └── Lib/
    │   │   │           └── Webhook/
    │   │   │               ├── Storage/
    │   │   │               │   ├── Beam/
    │   │   │               │   │   └── Webhook.hs
    │   │   │               │   └── Queries/
    │   │   │               │       ├── Webhook.hs
    │   │   │               │       └── OrphanInstances/
    │   │   │               │           └── Webhook.hs
    │   │   │               └── Types/
    │   │   │                   └── Webhook.hs
    │   │   └── yudhishthira/
    │   │       ├── package.yaml
    │   │       ├── yudhishthira.cabal
    │   │       ├── spec/
    │   │       │   ├── dsl-config.dhall
    │   │       │   └── Storage/
    │   │       │       ├── AppDynamicLogic.yaml
    │   │       │       ├── ChakraQueries.yaml
    │   │       │       ├── NammaTag.yaml
    │   │       │       ├── TagActionNotificationConfig.yaml
    │   │       │       ├── TimeBoundConfig.yaml
    │   │       │       └── UserData.yaml
    │   │       ├── src/
    │   │       │   ├── Lib/
    │   │       │   │   └── Yudhishthira/
    │   │       │   │       ├── Event.hs
    │   │       │   │       ├── Types.hs
    │   │       │   │       ├── TypesTH.hs
    │   │       │   │       ├── Event/
    │   │       │   │       │   ├── KaalChakra.hs
    │   │       │   │       │   └── KaalChakra/
    │   │       │   │       │       ├── Internal.hs
    │   │       │   │       │       ├── Jobs.hs
    │   │       │   │       │       ├── Parse.hs
    │   │       │   │       │       └── Template.hs
    │   │       │   │       ├── Flow/
    │   │       │   │       │   └── Dashboard.hs
    │   │       │   │       ├── Storage/
    │   │       │   │       │   ├── Beam/
    │   │       │   │       │   │   └── BeamFlow.hs
    │   │       │   │       │   ├── CachedQueries/
    │   │       │   │       │   │   ├── AppDynamicLogicElement.hs
    │   │       │   │       │   │   ├── AppDynamicLogicRollout.hs
    │   │       │   │       │   │   └── TimeBoundConfig.hs
    │   │       │   │       │   └── Queries/
    │   │       │   │       │       ├── AppDynamicLogicElementExtra.hs
    │   │       │   │       │       ├── AppDynamicLogicRolloutExtra.hs
    │   │       │   │       │       ├── NammaTagExtra.hs
    │   │       │   │       │       ├── TimeBoundConfigExtra.hs
    │   │       │   │       │       ├── UserDataExtra.hs
    │   │       │   │       │       └── Transformers/
    │   │       │   │       │           └── NammaTag.hs
    │   │       │   │       ├── Tools/
    │   │       │   │       │   ├── Error.hs
    │   │       │   │       │   └── Utils.hs
    │   │       │   │       └── Types/
    │   │       │   │           ├── Application.hs
    │   │       │   │           ├── Common.hs
    │   │       │   │           ├── ConfigPilot.hs
    │   │       │   │           ├── KaalChakra.hs
    │   │       │   │           ├── Manual.hs
    │   │       │   │           └── Tag.hs
    │   │       │   └── Tools/
    │   │       │       └── Beam/
    │   │       │           └── UtilsTH.hs
    │   │       ├── src-read-only/
    │   │       │   └── Lib/
    │   │       │       └── Yudhishthira/
    │   │       │           ├── Storage/
    │   │       │           │   ├── Beam/
    │   │       │           │   │   ├── AppDynamicLogicElement.hs
    │   │       │           │   │   ├── AppDynamicLogicRollout.hs
    │   │       │           │   │   ├── ChakraQueries.hs
    │   │       │           │   │   ├── NammaTag.hs
    │   │       │           │   │   ├── NammaTagTrigger.hs
    │   │       │           │   │   ├── TagActionNotificationConfig.hs
    │   │       │           │   │   ├── TimeBoundConfig.hs
    │   │       │           │   │   └── UserData.hs
    │   │       │           │   └── Queries/
    │   │       │           │       ├── AppDynamicLogicElement.hs
    │   │       │           │       ├── AppDynamicLogicRollout.hs
    │   │       │           │       ├── ChakraQueries.hs
    │   │       │           │       ├── NammaTag.hs
    │   │       │           │       ├── NammaTagTrigger.hs
    │   │       │           │       ├── TagActionNotificationConfig.hs
    │   │       │           │       ├── TimeBoundConfig.hs
    │   │       │           │       ├── UserData.hs
    │   │       │           │       └── OrphanInstances/
    │   │       │           │           ├── AppDynamicLogicElement.hs
    │   │       │           │           ├── AppDynamicLogicRollout.hs
    │   │       │           │           ├── NammaTag.hs
    │   │       │           │           ├── TimeBoundConfig.hs
    │   │       │           │           └── UserData.hs
    │   │       │           └── Types/
    │   │       │               ├── AppDynamicLogicElement.hs
    │   │       │               ├── AppDynamicLogicRollout.hs
    │   │       │               ├── ChakraQueries.hs
    │   │       │               ├── NammaTag.hs
    │   │       │               ├── NammaTagTrigger.hs
    │   │       │               ├── TagActionNotificationConfig.hs
    │   │       │               ├── TimeBoundConfig.hs
    │   │       │               └── UserData.hs
    │   │       └── test/
    │   │           ├── app/
    │   │           │   └── Main.hs
    │   │           └── src/
    │   │               └── KaalChakraJobs.hs
    │   ├── load-test/
    │   │   ├── README.md
    │   │   ├── createDrivers.py
    │   │   ├── default.nix
    │   │   ├── .env
    │   │   ├── scripts/
    │   │   │   ├── driverOffer.py
    │   │   │   └── riderApp.py
    │   │   ├── services/
    │   │   │   └── shareOTP.py
    │   │   └── setup/
    │   │       ├── auth.py
    │   │       └── locationUpdateService.py
    │   ├── newman-tests/
    │   │   ├── README.md
    │   │   ├── Dev.postman_environment.json
    │   │   ├── run-tests.sh
    │   │   ├── tests/
    │   │   │   ├── 001-happy-case-flow/
    │   │   │   │   ├── happy-case-flow.postman_collection.json
    │   │   │   │   ├── post-test.sh
    │   │   │   │   └── pre-test.sh
    │   │   │   ├── 002-customer-cancellation/
    │   │   │   │   ├── customer-cancellation.postman_collection.json
    │   │   │   │   ├── post-test.sh
    │   │   │   │   └── pre-test.sh
    │   │   │   ├── 003-driver-cancellation/
    │   │   │   │   ├── driver-cancellation.postman_collection.json
    │   │   │   │   ├── post-test.sh
    │   │   │   │   └── pre-test.sh
    │   │   │   └── 004-registry-apis/
    │   │   │       ├── ny-registry-crud-apis.postman_collection.json
    │   │   │       ├── post-test.sh
    │   │   │       └── pre-test.sh
    │   │   └── utils/
    │   │       ├── checkStuckEntities.sh
    │   │       ├── clearBlockedDriverListForRiders.sh
    │   │       ├── clearStuckEntities.sh
    │   │       └── flushRedis.sh
    │   ├── nix/
    │   │   ├── arion-configuration.nix
    │   │   ├── debug.nix
    │   │   ├── docker.nix
    │   │   ├── osrm.nix
    │   │   ├── pre-commit.nix
    │   │   ├── run-mobility-stack.nix
    │   │   ├── scripts.nix
    │   │   └── services/
    │   │       ├── nammayatri.nix
    │   │       ├── ports.nix
    │   │       └── postgres-with-replica.nix
    │   ├── swagger/
    │   │   ├── index.css
    │   │   ├── index.html
    │   │   ├── oauth2-redirect.html
    │   │   ├── swagger-initializer.js
    │   │   ├── swagger-ui-bundle.js
    │   │   ├── swagger-ui-es-bundle-core.js
    │   │   ├── swagger-ui-es-bundle.js
    │   │   ├── swagger-ui-standalone-preset.js
    │   │   ├── swagger-ui.css
    │   │   └── swagger-ui.js
    │   └── test/
    │       ├── beckn-test.cabal
    │       ├── package.yaml
    │       ├── app/
    │       │   └── Main.hs
    │       └── src/
    │           ├── Common.hs
    │           ├── HSpec.hs
    │           ├── Resources.hs
    │           ├── Runner.hs
    │           ├── TestMain.hs
    │           ├── TestSilentIOLogger.hs
    │           ├── Utils.hs
    │           ├── Mobility/
    │           │   ├── AppBackend/
    │           │   │   ├── APICalls.hs
    │           │   │   ├── Fixtures.hs
    │           │   │   ├── Queries.hs
    │           │   │   └── Utils.hs
    │           │   ├── ARDU/
    │           │   │   ├── APICalls.hs
    │           │   │   ├── CancelFlow.hs
    │           │   │   ├── DriverAcceptsNonrelevantQuote.hs
    │           │   │   ├── DriverOffersTwice.hs
    │           │   │   ├── Fixtures.hs
    │           │   │   ├── HealthCheck.hs
    │           │   │   ├── MapsConfig.hs
    │           │   │   ├── NearestDrivers.hs
    │           │   │   ├── Queries.hs
    │           │   │   ├── Spec.hs
    │           │   │   ├── SuccessFlow.hs
    │           │   │   ├── SyncRide.hs
    │           │   │   └── Utils.hs
    │           │   └── Fixtures/
    │           │       └── Routes.hs
    │           └── PublicTransport/
    │               ├── API.hs
    │               ├── Common.hs
    │               ├── HealthCheck.hs
    │               ├── Search.hs
    │               └── Spec.hs
    ├── docs/
    │   ├── CONTRIBUTING.md
    │   ├── .DS_Store
    │   └── technical/
    │       └── spec/
    │           └── Delivery/
    │               ├── backend.yaml
    │               └── frontend.yaml
    ├── Frontend/
    │   ├── README.md
    │   ├── asset-store-script.sh
    │   ├── default.nix
    │   ├── driverJuspayAssets.sh
    │   ├── format.sh
    │   ├── keyGenerator.py
    │   ├── packages.dhall
    │   ├── translations.json
    │   ├── translator.py
    │   ├── userJuspayAssets.sh
    │   ├── .eslintrc.json
    │   ├── android-native/
    │   │   ├── bundleFetch.py
    │   │   ├── bundling.sh
    │   │   ├── ci.json
    │   │   ├── Gemfile
    │   │   ├── gradle.properties
    │   │   ├── gradlew
    │   │   ├── gradlew.bat
    │   │   ├── local-temp.properties
    │   │   ├── releaseApp.sh
    │   │   ├── spagobundling.sh
    │   │   ├── updateLocalVersion.py
    │   │   ├── updateVersion.py
    │   │   ├── versions.txt
    │   │   ├── .gitignore
    │   │   ├── app/
    │   │   │   ├── google-services-temp.json
    │   │   │   ├── gradlew
    │   │   │   ├── gradlew.bat
    │   │   │   ├── proguard-rules.pro
    │   │   │   ├── .gitignore
    │   │   │   ├── gradle/
    │   │   │   │   └── wrapper/
    │   │   │   │       └── gradle-wrapper.properties
    │   │   │   ├── libs/
    │   │   │   │   └── android-jsc-r250231.aar
    │   │   │   └── src/
    │   │   │       ├── androidTest/
    │   │   │       │   └── java/
    │   │   │       │       └── in/
    │   │   │       │           └── juspay/
    │   │   │       │               └── mobility/
    │   │   │       │                   └── ExampleInstrumentedTest.java
    │   │   │       ├── dev/
    │   │   │       │   ├── assets/
    │   │   │       │   │   └── juspay/
    │   │   │       │   │       └── becknbase.html
    │   │   │       │   └── res/
    │   │   │       │       └── values/
    │   │   │       │           └── bools.xml
    │   │   │       ├── driver/
    │   │   │       │   ├── aapleYatriPartner/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── boot_loader.js
    │   │   │       │   │   │       └── index_bundle.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   ├── ic_launcher_debug.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_round.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── mipmap-hdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-mdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri.json
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri_new.json
    │   │   │       │   │       │   ├── end_ride_qr_anim.json
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   ├── ny_bundle_splash_lottie.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_failure.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_success.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_telugu_03.json
    │   │   │       │   │       │   ├── rippling_online_effect.json
    │   │   │       │   │       │   └── splash_lottie.json
    │   │   │       │   │       └── values/
    │   │   │       │   │           ├── colors.xml
    │   │   │       │   │           ├── constants.xml
    │   │   │       │   │           ├── ic_launcher_background.xml
    │   │   │       │   │           ├── ic_launcher_debug_background.xml
    │   │   │       │   │           ├── strings.xml
    │   │   │       │   │           └── styles.xml
    │   │   │       │   ├── assets/
    │   │   │       │   │   └── juspay/
    │   │   │       │   │       └── juspay_assets.json
    │   │   │       │   ├── bridgeDriver/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── index_bundle.js
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── boot_loader.js
    │   │   │       │   │   │       └── index_bundle.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── mipmap-hdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-mdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_debug.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_foreground.webp
    │   │   │       │   │       │   ├── ic_launcher_debug_round.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie.json
    │   │   │       │   │       │   ├── end_ride_qr_anim.json
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_failure.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_success.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_03_2.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_bengali_03_2.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_03_2.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_03_2.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_malayalam_03_2.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_03_2.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_telugu_03_2.json
    │   │   │       │   │       │   ├── rippling_online_effect.json
    │   │   │       │   │       │   └── splash_lottie.json
    │   │   │       │   │       └── values/
    │   │   │       │   │           ├── colors.xml
    │   │   │       │   │           ├── constants.xml
    │   │   │       │   │           ├── ic_launcher_background.xml
    │   │   │       │   │           ├── ic_launcher_debug_background.xml
    │   │   │       │   │           ├── strings.xml
    │   │   │       │   │           └── styles.xml
    │   │   │       │   ├── common/
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   └── zone_label_layout.xml
    │   │   │       │   │       └── raw/
    │   │   │       │   │           ├── blue_pulse_animation.json
    │   │   │       │   │           ├── end_ride_qr_anim_cab.json
    │   │   │       │   │           ├── ny_coin_conversion_v2.json
    │   │   │       │   │           ├── ny_ic_coin_earned_previous_day.json
    │   │   │       │   │           ├── splash_lottie_blr.json
    │   │   │       │   │           ├── splash_lottie_chn.json
    │   │   │       │   │           ├── splash_lottie_del.json
    │   │   │       │   │           ├── splash_lottie_hyd.json
    │   │   │       │   │           ├── splash_lottie_mys.json
    │   │   │       │   │           └── ys_coin_conversion.json
    │   │   │       │   ├── jatriSaathiDriver/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── index_bundle.js
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       └── boot_loader.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_intercity_variant_tag.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   ├── ic_launcher_debug.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_round.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie.json
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_new.json
    │   │   │       │   │       │   ├── end_ride_qr_anim.json
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   ├── ny_bundle_splash_lottie.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_failure.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_success.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_bengali_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_bengali_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_02.json
    │   │   │       │   │       │   └── rippling_online_effect.json
    │   │   │       │   │       └── values/
    │   │   │       │   │           ├── colors.xml
    │   │   │       │   │           ├── constants.xml
    │   │   │       │   │           ├── ic_launcher_background.xml
    │   │   │       │   │           ├── ic_launcher_debug_background.xml
    │   │   │       │   │           ├── strings.xml
    │   │   │       │   │           └── styles.xml
    │   │   │       │   ├── keralaSavaariPartner/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── index_bundle.js
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── boot_loader.js
    │   │   │       │   │   │       └── index_bundle.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   ├── ic_launcher_debug.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_round.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── mipmap-hdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-mdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri.json
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri_new.json
    │   │   │       │   │       │   ├── end_ride_qr_anim.json
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   ├── ny_bundle_splash_lottie.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_failure.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_success.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_telugu_03.json
    │   │   │       │   │       │   ├── rippling_online_effect.json
    │   │   │       │   │       │   └── splash_lottie.json
    │   │   │       │   │       └── values/
    │   │   │       │   │           ├── colors.xml
    │   │   │       │   │           ├── constants.xml
    │   │   │       │   │           ├── ic_launcher_background.xml
    │   │   │       │   │           ├── ic_launcher_debug_background.xml
    │   │   │       │   │           ├── strings.xml
    │   │   │       │   │           └── styles.xml
    │   │   │       │   ├── manaYatriPartner/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── index_bundle.js
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── boot_loader.js
    │   │   │       │   │   │       └── index_bundle.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   ├── ic_launcher_debug.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_round.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri.json
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri_new.json
    │   │   │       │   │       │   ├── end_ride_qr_anim.json
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   ├── ny_bundle_splash_lottie.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_failure.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_success.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_telugu_03.json
    │   │   │       │   │       │   ├── rippling_online_effect.json
    │   │   │       │   │       │   └── splash_lottie.json
    │   │   │       │   │       └── values/
    │   │   │       │   │           ├── colors.xml
    │   │   │       │   │           ├── constants.xml
    │   │   │       │   │           ├── ic_launcher_background.xml
    │   │   │       │   │           ├── ic_launcher_debug_background.xml
    │   │   │       │   │           ├── strings.xml
    │   │   │       │   │           └── styles.xml
    │   │   │       │   ├── nammaYatriPartner/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── index_bundle.js
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── boot_loader.js
    │   │   │       │   │   │       └── index_bundle.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   ├── ic_launcher_debug.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_round.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie.json
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_new.json
    │   │   │       │   │       │   ├── end_ride_qr_anim.json
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   ├── ny_bundle_splash_lottie.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_failure.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_success.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_bengali_03_2.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_02.json
    │   │   │       │   │       │   ├── ny_sub_intro_english_jan1.json
    │   │   │       │   │       │   ├── ny_sub_intro_hindi_jan1.json
    │   │   │       │   │       │   ├── ny_sub_intro_kannada_jan1.json
    │   │   │       │   │       │   ├── ny_sub_intro_malayalam_jan1.json
    │   │   │       │   │       │   ├── ny_sub_intro_tamil_jan1.json
    │   │   │       │   │       │   ├── ny_sub_intro_telugu_jan1.json
    │   │   │       │   │       │   ├── rippling_online_effect.json
    │   │   │       │   │       │   └── splash_lottie.json
    │   │   │       │   │       └── values/
    │   │   │       │   │           ├── colors.xml
    │   │   │       │   │           ├── constants.xml
    │   │   │       │   │           ├── ic_launcher_background.xml
    │   │   │       │   │           ├── ic_launcher_debug_background.xml
    │   │   │       │   │           ├── strings.xml
    │   │   │       │   │           └── styles.xml
    │   │   │       │   ├── odishaYatriPartner/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── index_bundle.js
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       ├── payments.zip
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── boot_loader.js
    │   │   │       │   │   │       └── index_bundle.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   ├── ic_launcher_debug.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_round.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── mipmap-hdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-mdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── mipmap-xxxhdpi/
    │   │   │       │   │       │   ├── ic_launcher.webp
    │   │   │       │   │       │   ├── ic_launcher_foreground.webp
    │   │   │       │   │       │   └── ic_launcher_round.webp
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri.json
    │   │   │       │   │       │   ├── enable_locatio_permission_lottie_manayatri_new.json
    │   │   │       │   │       │   ├── end_ride_qr_anim.json
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   ├── ny_bundle_splash_lottie.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_failure.json
    │   │   │       │   │       │   ├── ny_ic_coins_redeemed_success.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_hindi_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_kannada_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_01.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_02.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_tamil_03.json
    │   │   │       │   │       │   ├── ny_ic_subscription_info_telugu_03.json
    │   │   │       │   │       │   ├── rippling_online_effect.json
    │   │   │       │   │       │   └── splash_lottie.json
    │   │   │       │   │       └── values/
    │   │   │       │   │           ├── colors.xml
    │   │   │       │   │           ├── constants.xml
    │   │   │       │   │           ├── ic_launcher_background.xml
    │   │   │       │   │           ├── ic_launcher_debug_background.xml
    │   │   │       │   │           ├── strings.xml
    │   │   │       │   │           └── styles.xml
    │   │   │       │   ├── passCulturePartner/
    │   │   │       │   │   ├── AndroidManifest.xml
    │   │   │       │   │   ├── assets/
    │   │   │       │   │   │   ├── fonts/
    │   │   │       │   │   │   │   ├── Montserrat-Black.ttf
    │   │   │       │   │   │   │   ├── Montserrat-BlackItalic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-Bold.ttf
    │   │   │       │   │   │   │   ├── Montserrat-BoldItalic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-ExtraBold.ttf
    │   │   │       │   │   │   │   ├── Montserrat-ExtraBoldItalic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-ExtraLight.ttf
    │   │   │       │   │   │   │   ├── Montserrat-ExtraLightItalic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-Italic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-Light.ttf
    │   │   │       │   │   │   │   ├── Montserrat-LightItalic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-Medium.ttf
    │   │   │       │   │   │   │   ├── Montserrat-MediumItalic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-Regular.ttf
    │   │   │       │   │   │   │   ├── Montserrat-SemiBold.ttf
    │   │   │       │   │   │   │   ├── Montserrat-SemiBoldItalic.ttf
    │   │   │       │   │   │   │   ├── Montserrat-Thin.ttf
    │   │   │       │   │   │   │   └── Montserrat-ThinItalic.ttf
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       ├── config.json
    │   │   │       │   │   │       ├── index_bundle.js
    │   │   │       │   │   │       ├── juspay_assets.json
    │   │   │       │   │   │       └── v1-configuration.js
    │   │   │       │   │   ├── js/
    │   │   │       │   │   │   └── juspay/
    │   │   │       │   │   │       └── boot_loader.js
    │   │   │       │   │   └── res/
    │   │   │       │   │       ├── drawable/
    │   │   │       │   │       │   ├── dashed_line.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │   │       │   ├── rotate_drawable.xml
    │   │   │       │   │       │   └── shape_profile_drawable.xml
    │   │   │       │   │       ├── layout/
    │   │   │       │   │       │   ├── loading_screen_overlay.xml
    │   │   │       │   │       │   ├── sheet_view.xml
    │   │   │       │   │       │   ├── splash.xml
    │   │   │       │   │       │   └── splash_fallback.xml
    │   │   │       │   │       ├── mipmap-anydpi-v26/
    │   │   │       │   │       │   ├── ic_launcher.xml
    │   │   │       │   │       │   ├── ic_launcher_debug.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_round.xml
    │   │   │       │   │       │   └── ic_launcher_round.xml
    │   │   │       │   │       ├── raw/
    │   │   │       │   │       │   ├── ic_vehicle_processing.json
    │   │   │       │   │       │   └── rippling_online_effect.json
    │   │   │       │   │       ├── values/
    │   │   │       │   │       │   ├── attrs.xml
    │   │   │       │   │       │   ├── bools.xml
    │   │   │       │   │       │   ├── colors.xml
    │   │   │       │   │       │   ├── constants.xml
    │   │   │       │   │       │   ├── dimens.xml
    │   │   │       │   │       │   ├── ic_launcher_background.xml
    │   │   │       │   │       │   ├── ic_launcher_debug_background.xml
    │   │   │       │   │       │   ├── strings.xml
    │   │   │       │   │       │   ├── styles.xml
    │   │   │       │   │       │   └── values.xml
    │   │   │       │   │       ├── values-bn/
    │   │   │       │   │       │   └── strings.xml
    │   │   │       │   │       ├── values-fr/
    │   │   │       │   │       │   └── strings.xml
    │   │   │       │   │       ├── values-hi/
    │   │   │       │   │       │   └── strings.xml
    │   │   │       │   │       ├── values-kn/
    │   │   │       │   │       │   └── strings.xml
    │   │   │       │   │       ├── values-ml/
    │   │   │       │   │       │   └── strings.xml
    │   │   │       │   │       └── values-ta/
    │   │   │       │   │           └── strings.xml
    │   │   │       │   └── yatriPartner/
    │   │   │       │       ├── AndroidManifest.xml
    │   │   │       │       ├── assets/
    │   │   │       │       │   └── juspay/
    │   │   │       │       │       ├── config.json
    │   │   │       │       │       ├── index_bundle.js
    │   │   │       │       │       ├── juspay_assets.json
    │   │   │       │       │       └── v1-configuration.js
    │   │   │       │       ├── js/
    │   │   │       │       │   └── juspay/
    │   │   │       │       │       └── boot_loader.js
    │   │   │       │       └── res/
    │   │   │       │           ├── drawable/
    │   │   │       │           │   ├── dashed_line.xml
    │   │   │       │           │   ├── ic_launcher_background.xml
    │   │   │       │           │   ├── ic_roundcorner_overlay.xml
    │   │   │       │           │   ├── rotate_drawable.xml
    │   │   │       │           │   └── shape_profile_drawable.xml
    │   │   │       │           ├── layout/
    │   │   │       │           │   ├── loading_screen_overlay.xml
    │   │   │       │           │   ├── splash.xml
    │   │   │       │           │   └── splash_fallback.xml
    │   │   │       │           ├── mipmap-anydpi-v26/
    │   │   │       │           │   ├── ic_launcher.xml
    │   │   │       │           │   ├── ic_launcher_debug.xml
    │   │   │       │           │   ├── ic_launcher_debug_round.xml
    │   │   │       │           │   └── ic_launcher_round.xml
    │   │   │       │           ├── raw/
    │   │   │       │           │   ├── enable_locatio_permission_lottie.json
    │   │   │       │           │   ├── enable_locatio_permission_lottie_new.json
    │   │   │       │           │   ├── ic_vehicle_processing.json
    │   │   │       │           │   ├── ny_bundle_splash_lottie.json
    │   │   │       │           │   └── rippling_online_effect.json
    │   │   │       │           └── values/
    │   │   │       │               ├── colors.xml
    │   │   │       │               ├── constants.xml
    │   │   │       │               ├── ic_launcher_background.xml
    │   │   │       │               ├── ic_launcher_debug_background.xml
    │   │   │       │               ├── strings.xml
    │   │   │       │               └── styles.xml
    │   │   │       ├── generateHashes/
    │   │   │       │   └── java/
    │   │   │       │       └── in/
    │   │   │       │           └── juspay/
    │   │   │       │               └── mobility/
    │   │   │       │                   └── Test.java
    │   │   │       ├── main/
    │   │   │       │   ├── AndroidManifest.xml
    │   │   │       │   ├── assets/
    │   │   │       │   │   ├── Payments-Loader.json
    │   │   │       │   │   ├── fonts/
    │   │   │       │   │   │   ├── DigitalNumbers-Regular.ttf
    │   │   │       │   │   │   ├── FEFont.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-Black.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-Bold.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-ExtraBold.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-ExtraLight.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-Light.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-Medium.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-Regular.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-SemiBold.ttf
    │   │   │       │   │   │   ├── NotoSansKannada-Thin.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-Black.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-Bold.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-ExtraBold.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-ExtraLight.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-Light.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-Medium.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-Regular.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-SemiBold.ttf
    │   │   │       │   │   │   ├── NotoSansTelugu-Thin.ttf
    │   │   │       │   │   │   ├── Plus Jakarta Sans.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-Bold.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-BoldItalic.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-ExtraBold.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-ExtraBoldItalic.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-ExtraLight.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-ExtraLightItalic.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-Italic.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-Light.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-LightItalic.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-Medium.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-MediumItalic.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-Regular.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-SemiBold.ttf
    │   │   │       │   │   │   ├── PlusJakartaSans-SemiBoldItalic.ttf
    │   │   │       │   │   │   └── Seven-Segment.ttf
    │   │   │       │   │   └── juspay/
    │   │   │       │   │       ├── becknbase.html
    │   │   │       │   │       ├── splash_config.json
    │   │   │       │   │       ├── v1-assets_downloader.jsa
    │   │   │       │   │       └── zone_config.json
    │   │   │       │   ├── java/
    │   │   │       │   │   └── in/
    │   │   │       │   │       └── juspay/
    │   │   │       │   │           └── mobility/
    │   │   │       │   │               ├── FirebaseMessaging.java
    │   │   │       │   │               ├── MainActivity.java
    │   │   │       │   │               ├── MainApplicationConsumer.java
    │   │   │       │   │               ├── MainApplicationProvider.java
    │   │   │       │   │               ├── MobilityServiceHolder.java
    │   │   │       │   │               ├── MobilityServices.java
    │   │   │       │   │               ├── ResourceHandler.java
    │   │   │       │   │               └── Utils.java
    │   │   │       │   ├── js/
    │   │   │       │   │   └── tracker.js
    │   │   │       │   └── res/
    │   │   │       │       ├── anim/
    │   │   │       │       │   ├── bottom_to_top.xml
    │   │   │       │       │   ├── fade_out.xml
    │   │   │       │       │   └── top_to_bottom.xml
    │   │   │       │       ├── drawable/
    │   │   │       │       │   ├── app_notification_background.xml
    │   │   │       │       │   ├── bubble_message_shape_left.xml
    │   │   │       │       │   ├── bubble_message_shape_right.xml
    │   │   │       │       │   ├── circle_red.xml
    │   │   │       │       │   ├── dashed_line.xml
    │   │   │       │       │   ├── delivery_banner_rectangle.xml
    │   │   │       │       │   ├── ic_accessibility_tag.xml
    │   │   │       │       │   ├── ic_agency_cancelled.xml
    │   │   │       │       │   ├── ic_agency_offline.xml
    │   │   │       │       │   ├── ic_app_update.xml
    │   │   │       │       │   ├── ic_audio_loader.xml
    │   │   │       │       │   ├── ic_blue600_bg.xml
    │   │   │       │       │   ├── ic_blue_bg.xml
    │   │   │       │       │   ├── ic_favourite_rider.xml
    │   │   │       │       │   ├── ic_goto_bg.xml
    │   │   │       │       │   ├── ic_grey_border.xml
    │   │   │       │       │   ├── ic_invoice_border.xml
    │   │   │       │       │   ├── ic_line.xml
    │   │   │       │       │   ├── ic_loader.xml
    │   │   │       │       │   ├── ic_pause.xml
    │   │   │       │       │   ├── ic_play.xml
    │   │   │       │       │   ├── ic_ride_not_found.xml
    │   │   │       │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │       │   ├── ic_share_location.xml
    │   │   │       │       │   ├── ic_sp_zone_tag.xml
    │   │   │       │       │   ├── ic_tip_tag.xml
    │   │   │       │       │   ├── intercity_banner_rectangle.xml
    │   │   │       │       │   ├── label_shadow.xml
    │   │   │       │       │   ├── loader_overlay.xml
    │   │   │       │       │   ├── message_bg.xml
    │   │   │       │       │   ├── message_sheet_bg.xml
    │   │   │       │       │   ├── notification_count_background.xml
    │   │   │       │       │   ├── popup_background.xml
    │   │   │       │       │   ├── progress_circle.xml
    │   │   │       │       │   ├── progress_loader.xml
    │   │   │       │       │   ├── progressbar_clip.xml
    │   │   │       │       │   ├── rental_banner_rectangle.xml
    │   │   │       │       │   ├── rotate_drawable.xml
    │   │   │       │       │   ├── round_circle.xml
    │   │   │       │       │   ├── round_corner.xml
    │   │   │       │       │   ├── round_corner_button.xml
    │   │   │       │       │   ├── round_corner_layout.xml
    │   │   │       │       │   ├── rounded_corner_background.xml
    │   │   │       │       │   ├── rounded_yellow_background.xml
    │   │   │       │       │   ├── silent_popup_background.xml
    │   │   │       │       │   ├── tip_banner_rectangle.xml
    │   │   │       │       │   ├── widget_close_gradient.xml
    │   │   │       │       │   └── zone_curve.xml
    │   │   │       │       ├── font/
    │   │   │       │       │   ├── dmsans_bold.ttf
    │   │   │       │       │   ├── plus_jakarta_sans_bold.ttf
    │   │   │       │       │   ├── plus_jakartasans_medium.ttf
    │   │   │       │       │   ├── plus_jakartasans_medium_italic.ttf
    │   │   │       │       │   ├── plus_jakartasans_regular.ttf
    │   │   │       │       │   ├── plus_jakartasans_semibold.ttf
    │   │   │       │       │   └── saira_semi_condensed_semi_bold.ttf
    │   │   │       │       ├── layout/
    │   │   │       │       │   ├── activity_main.xml
    │   │   │       │       │   ├── activity_main_without_bg.xml
    │   │   │       │       │   ├── api_loader.xml
    │   │   │       │       │   ├── app_notification.xml
    │   │   │       │       │   ├── dynamic_update_loader.xml
    │   │   │       │       │   ├── floating_widget_layout.xml
    │   │   │       │       │   ├── invoice_template.xml
    │   │   │       │       │   ├── loader.xml
    │   │   │       │       │   ├── loading_screen_overlay.xml
    │   │   │       │       │   ├── marker_code_layout.xml
    │   │   │       │       │   ├── marker_label_layout.xml
    │   │   │       │       │   ├── message_sheet.xml
    │   │   │       │       │   └── permission_steps_layout.xml
    │   │   │       │       ├── raw/
    │   │   │       │       │   ├── accepted_by_another_driver_lottie.json
    │   │   │       │       │   ├── audio_upload_animation.json
    │   │   │       │       │   ├── auto_rickshaw_processing.json
    │   │   │       │       │   ├── finding_rides_loader_auto_kochi.json
    │   │   │       │       │   ├── finding_rides_loader_auto_yellow_black.json
    │   │   │       │       │   ├── finding_rides_loader_bike.json
    │   │   │       │       │   ├── finding_rides_loader_no_text.json
    │   │   │       │       │   ├── finding_rides_loader_with_text_v2.json
    │   │   │       │       │   ├── finding_rides_loader_without_text_cab.json
    │   │   │       │       │   ├── ic_cab_vehicle_processing.json
    │   │   │       │       │   ├── map_style_aubergine.json
    │   │   │       │       │   ├── map_style_dark.json
    │   │   │       │       │   ├── map_style_night.json
    │   │   │       │       │   ├── map_style_retro.json
    │   │   │       │       │   ├── map_style_silver.json
    │   │   │       │       │   ├── notification_bell.json
    │   │   │       │       │   ├── ny_ic_generic_loader.json
    │   │   │       │       │   ├── ny_ic_loader.json
    │   │   │       │       │   ├── primary_button_loader.json
    │   │   │       │       │   ├── primary_button_loader_white.json
    │   │   │       │       │   ├── progress_loader_line.json
    │   │   │       │       │   ├── record_audio_animation.json
    │   │   │       │       │   ├── ride_accepted_lottie.json
    │   │   │       │       │   ├── right_arrow.json
    │   │   │       │       │   ├── sample_data.json
    │   │   │       │       │   ├── search_loader.json
    │   │   │       │       │   ├── splash_lottie.json
    │   │   │       │       │   ├── success_lottie.json
    │   │   │       │       │   └── waiting_for_customer_lottie.json
    │   │   │       │       ├── values/
    │   │   │       │       │   ├── attrs.xml
    │   │   │       │       │   ├── bools.xml
    │   │   │       │       │   ├── colors.xml
    │   │   │       │       │   ├── dimens.xml
    │   │   │       │       │   ├── ic_launcher_background.xml
    │   │   │       │       │   ├── strings.xml
    │   │   │       │       │   ├── styles.xml
    │   │   │       │       │   └── values.xml
    │   │   │       │       ├── values-bn/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-fr/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-hi/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-kn/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-ml/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-ta/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-te/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       └── xml/
    │   │   │       │           ├── file_paths.xml
    │   │   │       │           ├── file_provider_paths.xml
    │   │   │       │           ├── provider_paths.xml
    │   │   │       │           └── share_download_paths.xml
    │   │   │       ├── test/
    │   │   │       │   └── java/
    │   │   │       │       └── in/
    │   │   │       │           └── juspay/
    │   │   │       │               └── mobility/
    │   │   │       │                   └── ExampleUnitTest.java
    │   │   │       └── user/
    │   │   │           ├── assets/
    │   │   │           │   └── juspay/
    │   │   │           │       └── juspay_assets.json
    │   │   │           ├── bridge/
    │   │   │           │   ├── AndroidManifest.xml
    │   │   │           │   ├── assets/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── config.json
    │   │   │           │   │       ├── index_bundle.js
    │   │   │           │   │       ├── juspay_assets.json
    │   │   │           │   │       └── v1-configuration.js
    │   │   │           │   ├── js/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── boot_loader.js
    │   │   │           │   │       └── index_bundle.js
    │   │   │           │   └── res/
    │   │   │           │       ├── drawable/
    │   │   │           │       │   ├── button.xml
    │   │   │           │       │   └── ic_launcher_background.xml
    │   │   │           │       ├── layout/
    │   │   │           │       │   ├── invoice_template.xml
    │   │   │           │       │   ├── splash.xml
    │   │   │           │       │   └── splash_fallback.xml
    │   │   │           │       ├── mipmap-anydpi-v26/
    │   │   │           │       │   ├── ic_launcher.xml
    │   │   │           │       │   ├── ic_launcher_debug.xml
    │   │   │           │       │   ├── ic_launcher_debug_round.xml
    │   │   │           │       │   └── ic_launcher_round.xml
    │   │   │           │       ├── raw/
    │   │   │           │       │   ├── finding_rides_loader_auto_kochi.json
    │   │   │           │       │   ├── finding_rides_loader_auto_yellow_black.json
    │   │   │           │       │   ├── finding_rides_loader_with_text_v2.json
    │   │   │           │       │   ├── finding_rides_loader_without_text_cab.json
    │   │   │           │       │   └── ic_vehicle_processing.json
    │   │   │           │       └── values/
    │   │   │           │           ├── colors.xml
    │   │   │           │           ├── constants.xml
    │   │   │           │           ├── ic_launcher_background.xml
    │   │   │           │           ├── ic_launcher_debug_background.xml
    │   │   │           │           ├── strings.xml
    │   │   │           │           └── styles.xml
    │   │   │           ├── common/
    │   │   │           │   └── res/
    │   │   │           │       ├── layout/
    │   │   │           │       │   └── zone_label_layout.xml
    │   │   │           │       └── raw/
    │   │   │           │           ├── ny_ic_sos_active.json
    │   │   │           │           └── walk_anim.json
    │   │   │           ├── jatriSaathi/
    │   │   │           │   ├── AndroidManifest.xml
    │   │   │           │   ├── assets/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── config.json
    │   │   │           │   │       ├── juspay_assets.json
    │   │   │           │   │       └── v1-configuration.js
    │   │   │           │   ├── js/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       └── boot_loader.js
    │   │   │           │   └── res/
    │   │   │           │       ├── drawable/
    │   │   │           │       │   ├── button.xml
    │   │   │           │       │   └── ic_intercity_variant_tag.xml
    │   │   │           │       ├── layout/
    │   │   │           │       │   ├── invoice_template.xml
    │   │   │           │       │   ├── loading_screen_overlay.xml
    │   │   │           │       │   ├── splash.xml
    │   │   │           │       │   └── splash_fallback.xml
    │   │   │           │       ├── mipmap-anydpi-v26/
    │   │   │           │       │   ├── ic_launcher.xml
    │   │   │           │       │   ├── ic_launcher_debug.xml
    │   │   │           │       │   ├── ic_launcher_debug_round.xml
    │   │   │           │       │   └── ic_launcher_round.xml
    │   │   │           │       ├── raw/
    │   │   │           │       │   ├── finding_rides_loader_ambulance.json
    │   │   │           │       │   ├── finding_rides_loader_with_text_v2.json
    │   │   │           │       │   ├── finding_rides_loader_without_text_cab.json
    │   │   │           │       │   ├── ic_vehicle_processing.json
    │   │   │           │       │   ├── ny_bundle_splash_lottie.json
    │   │   │           │       │   └── splash_lottie.json
    │   │   │           │       └── values/
    │   │   │           │           ├── colors.xml
    │   │   │           │           ├── constants.xml
    │   │   │           │           ├── ic_launcher_background.xml
    │   │   │           │           ├── ic_launcher_debug_background.xml
    │   │   │           │           ├── strings.xml
    │   │   │           │           └── styles.xml
    │   │   │           ├── manaYatri/
    │   │   │           │   ├── AndroidManifest.xml
    │   │   │           │   ├── assets/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── config.json
    │   │   │           │   │       ├── index_bundle.js
    │   │   │           │   │       ├── juspay_assets.json
    │   │   │           │   │       └── v1-configuration.js
    │   │   │           │   ├── js/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── boot_loader.js
    │   │   │           │   │       └── index_bundle.js
    │   │   │           │   └── res/
    │   │   │           │       ├── drawable/
    │   │   │           │       │   ├── button.xml
    │   │   │           │       │   └── ic_launcher_background.xml
    │   │   │           │       ├── layout/
    │   │   │           │       │   ├── invoice_template.xml
    │   │   │           │       │   ├── splash.xml
    │   │   │           │       │   └── splash_fallback.xml
    │   │   │           │       ├── mipmap-anydpi-v26/
    │   │   │           │       │   ├── ic_launcher.xml
    │   │   │           │       │   ├── ic_launcher_debug.xml
    │   │   │           │       │   ├── ic_launcher_debug_round.xml
    │   │   │           │       │   └── ic_launcher_round.xml
    │   │   │           │       ├── raw/
    │   │   │           │       │   ├── finding_rides_loader_auto_kochi.json
    │   │   │           │       │   ├── finding_rides_loader_auto_yellow_black.json
    │   │   │           │       │   ├── finding_rides_loader_with_text_v2.json
    │   │   │           │       │   ├── finding_rides_loader_without_text_cab.json
    │   │   │           │       │   ├── ic_vehicle_processing.json
    │   │   │           │       │   └── ny_bundle_splash_lottie.json
    │   │   │           │       └── values/
    │   │   │           │           ├── colors.xml
    │   │   │           │           ├── constants.xml
    │   │   │           │           ├── ic_launcher_background.xml
    │   │   │           │           ├── ic_launcher_debug_background.xml
    │   │   │           │           ├── strings.xml
    │   │   │           │           └── styles.xml
    │   │   │           ├── nammaYatri/
    │   │   │           │   ├── AndroidManifest.xml
    │   │   │           │   ├── assets/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── config.json
    │   │   │           │   │       ├── index_bundle.js
    │   │   │           │   │       ├── juspay_assets.json
    │   │   │           │   │       └── v1-configuration.js
    │   │   │           │   ├── js/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── boot_loader.js
    │   │   │           │   │       └── index_bundle.js
    │   │   │           │   └── res/
    │   │   │           │       ├── drawable/
    │   │   │           │       │   ├── button.xml
    │   │   │           │       │   └── ic_launcher_background.xml
    │   │   │           │       ├── layout/
    │   │   │           │       │   ├── invoice_template.xml
    │   │   │           │       │   ├── splash.xml
    │   │   │           │       │   └── splash_fallback.xml
    │   │   │           │       ├── mipmap-anydpi-v26/
    │   │   │           │       │   ├── ic_launcher.xml
    │   │   │           │       │   ├── ic_launcher_debug.xml
    │   │   │           │       │   ├── ic_launcher_debug_round.xml
    │   │   │           │       │   └── ic_launcher_round.xml
    │   │   │           │       ├── raw/
    │   │   │           │       │   ├── finding_rides_loader_auto_kochi.json
    │   │   │           │       │   ├── finding_rides_loader_auto_yellow_black.json
    │   │   │           │       │   ├── finding_rides_loader_with_text_v2.json
    │   │   │           │       │   ├── finding_rides_loader_without_text_cab.json
    │   │   │           │       │   ├── ic_vehicle_processing.json
    │   │   │           │       │   └── ny_bundle_splash_lottie.json
    │   │   │           │       └── values/
    │   │   │           │           ├── colors.xml
    │   │   │           │           ├── constants.xml
    │   │   │           │           ├── ic_launcher_background.xml
    │   │   │           │           ├── ic_launcher_debug_background.xml
    │   │   │           │           ├── strings.xml
    │   │   │           │           └── styles.xml
    │   │   │           ├── odishaYatri/
    │   │   │           │   ├── AndroidManifest.xml
    │   │   │           │   ├── assets/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── config.json
    │   │   │           │   │       ├── index_bundle.js
    │   │   │           │   │       ├── juspay_assets.json
    │   │   │           │   │       └── v1-configuration.js
    │   │   │           │   ├── js/
    │   │   │           │   │   └── juspay/
    │   │   │           │   │       ├── boot_loader.js
    │   │   │           │   │       └── index_bundle.js
    │   │   │           │   └── res/
    │   │   │           │       ├── drawable/
    │   │   │           │       │   ├── button.xml
    │   │   │           │       │   └── ic_launcher_background.xml
    │   │   │           │       ├── layout/
    │   │   │           │       │   ├── invoice_template.xml
    │   │   │           │       │   ├── splash.xml
    │   │   │           │       │   └── splash_fallback.xml
    │   │   │           │       ├── mipmap-anydpi-v26/
    │   │   │           │       │   ├── ic_launcher.xml
    │   │   │           │       │   ├── ic_launcher_debug.xml
    │   │   │           │       │   ├── ic_launcher_debug_round.xml
    │   │   │           │       │   └── ic_launcher_round.xml
    │   │   │           │       ├── mipmap-hdpi/
    │   │   │           │       │   ├── ic_launcher.webp
    │   │   │           │       │   ├── ic_launcher_foreground.webp
    │   │   │           │       │   └── ic_launcher_round.webp
    │   │   │           │       ├── mipmap-mdpi/
    │   │   │           │       │   ├── ic_launcher.webp
    │   │   │           │       │   ├── ic_launcher_foreground.webp
    │   │   │           │       │   └── ic_launcher_round.webp
    │   │   │           │       ├── mipmap-xhdpi/
    │   │   │           │       │   ├── ic_launcher.webp
    │   │   │           │       │   ├── ic_launcher_foreground.webp
    │   │   │           │       │   └── ic_launcher_round.webp
    │   │   │           │       ├── mipmap-xxhdpi/
    │   │   │           │       │   ├── ic_launcher.webp
    │   │   │           │       │   ├── ic_launcher_foreground.webp
    │   │   │           │       │   └── ic_launcher_round.webp
    │   │   │           │       ├── mipmap-xxxhdpi/
    │   │   │           │       │   ├── ic_launcher.webp
    │   │   │           │       │   ├── ic_launcher_foreground.webp
    │   │   │           │       │   └── ic_launcher_round.webp
    │   │   │           │       ├── raw/
    │   │   │           │       │   ├── finding_rides_loader_auto_kochi.json
    │   │   │           │       │   ├── finding_rides_loader_auto_yellow_black.json
    │   │   │           │       │   ├── finding_rides_loader_with_text_v2.json
    │   │   │           │       │   ├── finding_rides_loader_without_text_cab.json
    │   │   │           │       │   ├── ic_vehicle_processing.json
    │   │   │           │       │   └── ny_bundle_splash_lottie.json
    │   │   │           │       └── values/
    │   │   │           │           ├── colors.xml
    │   │   │           │           ├── constants.xml
    │   │   │           │           ├── ic_launcher_background.xml
    │   │   │           │           ├── ic_launcher_debug_background.xml
    │   │   │           │           ├── strings.xml
    │   │   │           │           └── styles.xml
    │   │   │           └── yatri/
    │   │   │               ├── AndroidManifest.xml
    │   │   │               ├── assets/
    │   │   │               │   └── juspay/
    │   │   │               │       ├── config.json
    │   │   │               │       ├── index_bundle.js
    │   │   │               │       ├── juspay_assets.json
    │   │   │               │       └── v1-configuration.js
    │   │   │               ├── js/
    │   │   │               │   └── juspay/
    │   │   │               │       └── boot_loader.js
    │   │   │               └── res/
    │   │   │                   ├── drawable/
    │   │   │                   │   ├── button.xml
    │   │   │                   │   └── ic_launcher_background.xml
    │   │   │                   ├── layout/
    │   │   │                   │   ├── invoice_template.xml
    │   │   │                   │   ├── loading_screen_overlay.xml
    │   │   │                   │   ├── splash.xml
    │   │   │                   │   └── splash_fallback.xml
    │   │   │                   ├── mipmap-anydpi-v26/
    │   │   │                   │   ├── ic_launcher.xml
    │   │   │                   │   ├── ic_launcher_debug.xml
    │   │   │                   │   ├── ic_launcher_debug_round.xml
    │   │   │                   │   └── ic_launcher_round.xml
    │   │   │                   ├── raw/
    │   │   │                   │   ├── finding_rides_loader_auto_kochi.json
    │   │   │                   │   ├── finding_rides_loader_auto_yellow_black.json
    │   │   │                   │   ├── finding_rides_loader_with_text_v2.json
    │   │   │                   │   ├── finding_rides_loader_without_text_cab.json
    │   │   │                   │   ├── ic_vehicle_processing.json
    │   │   │                   │   └── ny_bundle_splash_lottie.json
    │   │   │                   └── values/
    │   │   │                       ├── colors.xml
    │   │   │                       ├── constants.xml
    │   │   │                       ├── ic_launcher_background.xml
    │   │   │                       ├── ic_launcher_debug_background.xml
    │   │   │                       ├── strings.xml
    │   │   │                       └── styles.xml
    │   │   ├── fastlane/
    │   │   │   ├── Appfile
    │   │   │   ├── Fastfile
    │   │   │   └── report.xml
    │   │   ├── gradle/
    │   │   │   ├── libs.versions.toml
    │   │   │   └── wrapper/
    │   │   │       └── gradle-wrapper.properties
    │   │   ├── mobility-app/
    │   │   │   ├── consumer-rules.pro
    │   │   │   ├── proguard-rules.pro
    │   │   │   ├── .gitignore
    │   │   │   └── src/
    │   │   │       ├── androidTest/
    │   │   │       │   └── java/
    │   │   │       │       └── in/
    │   │   │       │           └── juspay/
    │   │   │       │               └── mobility/
    │   │   │       │                   └── app/
    │   │   │       │                       └── ExampleInstrumentedTest.java
    │   │   │       ├── main/
    │   │   │       │   ├── AndroidManifest.xml
    │   │   │       │   ├── assets/
    │   │   │       │   │   └── juspay/
    │   │   │       │   │       └── ride_request_theme.json
    │   │   │       │   ├── java/
    │   │   │       │   │   └── in/
    │   │   │       │   │       └── juspay/
    │   │   │       │   │           └── mobility/
    │   │   │       │   │               └── app/
    │   │   │       │   │                   ├── BootUpReceiver.java
    │   │   │       │   │                   ├── CameraUtils.java
    │   │   │       │   │                   ├── ChatBroadCastReceiver.java
    │   │   │       │   │                   ├── ChatService.java
    │   │   │       │   │                   ├── CheckPermissionOverlay.java
    │   │   │       │   │                   ├── DrawableUtil.java
    │   │   │       │   │                   ├── FCMBundleUpdateBroadcastReceiver.java
    │   │   │       │   │                   ├── GeoHash.java
    │   │   │       │   │                   ├── GPSBroadcastReceiver.java
    │   │   │       │   │                   ├── GpsListeningService.java
    │   │   │       │   │                   ├── GRPCNotificationService.java
    │   │   │       │   │                   ├── InAppNotification.java
    │   │   │       │   │                   ├── LocationUpdateService.java
    │   │   │       │   │                   ├── LocationUpdateServiceV2.java
    │   │   │       │   │                   ├── LocationUpdateWorker.java
    │   │   │       │   │                   ├── Log.java
    │   │   │       │   │                   ├── MessageOverlayService.java
    │   │   │       │   │                   ├── MobilityAppBridge.java
    │   │   │       │   │                   ├── MyFirebaseMessagingService.java
    │   │   │       │   │                   ├── NotificationUtils.java
    │   │   │       │   │                   ├── OtpUtils.java
    │   │   │       │   │                   ├── OverlayMessagingService.java
    │   │   │       │   │                   ├── OverlaySheetService.java
    │   │   │       │   │                   ├── PermissionUtils.java
    │   │   │       │   │                   ├── ReelsPlayerView.java
    │   │   │       │   │                   ├── RemoteAssetsDownloader.java
    │   │   │       │   │                   ├── RideRequestActivity.java
    │   │   │       │   │                   ├── RideRequestUtils.java
    │   │   │       │   │                   ├── SheetAdapter.java
    │   │   │       │   │                   ├── SheetModel.java
    │   │   │       │   │                   ├── SheetTheme.java
    │   │   │       │   │                   ├── SliderComponent.java
    │   │   │       │   │                   ├── TranslatorMLKit.java
    │   │   │       │   │                   ├── Utils.java
    │   │   │       │   │                   ├── WidgetService.java
    │   │   │       │   │                   ├── YoutubeVideoView.java
    │   │   │       │   │                   ├── callbacks/
    │   │   │       │   │                   │   ├── CallBack.java
    │   │   │       │   │                   │   └── ShowNotificationCallBack.java
    │   │   │       │   │                   ├── carousel/
    │   │   │       │   │                   │   ├── Transformations.java
    │   │   │       │   │                   │   ├── ViewPagerItem.java
    │   │   │       │   │                   │   ├── VPAdapter.java
    │   │   │       │   │                   │   └── VPPageTransformer.java
    │   │   │       │   │                   ├── dataModel/
    │   │   │       │   │                   │   └── VariantConfig.java
    │   │   │       │   │                   ├── reels/
    │   │   │       │   │                   │   ├── ExoplayerItem.java
    │   │   │       │   │                   │   ├── ReelController.java
    │   │   │       │   │                   │   ├── ReelViewAdapter.java
    │   │   │       │   │                   │   └── ReelViewPagerItem.java
    │   │   │       │   │                   ├── RemoteConfigs/
    │   │   │       │   │                   │   └── MobilityRemoteConfigs.java
    │   │   │       │   │                   ├── services/
    │   │   │       │   │                   │   └── MobilityAppUpdate.java
    │   │   │       │   │                   └── workers/
    │   │   │       │   │                       ├── MyLocationWorker.java
    │   │   │       │   │                       └── WorkScheduler.java
    │   │   │       │   ├── proto/
    │   │   │       │   │   └── notificationService.proto
    │   │   │       │   └── res/
    │   │   │       │       ├── anim/
    │   │   │       │       │   ├── bottom_to_top.xml
    │   │   │       │       │   ├── fade_out.xml
    │   │   │       │       │   ├── fadein.xml
    │   │   │       │       │   └── top_to_bottom.xml
    │   │   │       │       ├── drawable/
    │   │   │       │       │   ├── app_notification_background.xml
    │   │   │       │       │   ├── black_background.xml
    │   │   │       │       │   ├── black_gradient.xml
    │   │   │       │       │   ├── blue_curve.xml
    │   │   │       │       │   ├── bubble_message_shape_left.xml
    │   │   │       │       │   ├── bubble_message_shape_right.xml
    │   │   │       │       │   ├── button_layout.xml
    │   │   │       │       │   ├── dashed_line.xml
    │   │   │       │       │   ├── delivery_banner_rectangle.xml
    │   │   │       │       │   ├── ic_ac_variant_tag.xml
    │   │   │       │       │   ├── ic_accessibility_tag.xml
    │   │   │       │       │   ├── ic_app_update.xml
    │   │   │       │       │   ├── ic_audio_loader.xml
    │   │   │       │       │   ├── ic_congestion_tag.xml
    │   │   │       │       │   ├── ic_favourite_rider.xml
    │   │   │       │       │   ├── ic_gold_tier_ride_tag.xml
    │   │   │       │       │   ├── ic_goto_bg.xml
    │   │   │       │       │   ├── ic_grey_border.xml
    │   │   │       │       │   ├── ic_intercity_variant_tag.xml
    │   │   │       │       │   ├── ic_invoice_border.xml
    │   │   │       │       │   ├── ic_line.xml
    │   │   │       │       │   ├── ic_loader.xml
    │   │   │       │       │   ├── ic_orange_tag.xml
    │   │   │       │       │   ├── ic_pause.xml
    │   │   │       │       │   ├── ic_pet_tag.xml
    │   │   │       │       │   ├── ic_play.xml
    │   │   │       │       │   ├── ic_rental_variant_tag.xml
    │   │   │       │       │   ├── ic_ride_not_found.xml
    │   │   │       │       │   ├── ic_roundcorner_overlay.xml
    │   │   │       │       │   ├── ic_share_location.xml
    │   │   │       │       │   ├── ic_sp_zone_tag.xml
    │   │   │       │       │   ├── ic_stops_bg.xml
    │   │   │       │       │   ├── ic_tip_tag.xml
    │   │   │       │       │   ├── intercity_banner_rectangle.xml
    │   │   │       │       │   ├── label_shadow.xml
    │   │   │       │       │   ├── loader_overlay.xml
    │   │   │       │       │   ├── message_bg.xml
    │   │   │       │       │   ├── message_sheet_bg.xml
    │   │   │       │       │   ├── message_sheet_chat_background.xml
    │   │   │       │       │   ├── notification_count_background.xml
    │   │   │       │       │   ├── popup_background.xml
    │   │   │       │       │   ├── progress_circle.xml
    │   │   │       │       │   ├── progress_loader.xml
    │   │   │       │       │   ├── progressbar_clip.xml
    │   │   │       │       │   ├── rectangle_9506.xml
    │   │   │       │       │   ├── reel_thumb.xml
    │   │   │       │       │   ├── rental_banner_rectangle.xml
    │   │   │       │       │   ├── rotate_drawable.xml
    │   │   │       │       │   ├── round_circle.xml
    │   │   │       │       │   ├── round_corner.xml
    │   │   │       │       │   ├── round_corner_button.xml
    │   │   │       │       │   ├── round_corner_layout.xml
    │   │   │       │       │   ├── round_corner_transparent.xml
    │   │   │       │       │   ├── rounded_corner_background.xml
    │   │   │       │       │   ├── silent_popup_background.xml
    │   │   │       │       │   ├── tip_banner_rectangle.xml
    │   │   │       │       │   ├── widget_close_gradient.xml
    │   │   │       │       │   └── zone_curve.xml
    │   │   │       │       ├── font/
    │   │   │       │       │   ├── dmsans_bold.ttf
    │   │   │       │       │   ├── plus_jakarta_sans_bold.ttf
    │   │   │       │       │   ├── plus_jakartasans_medium.ttf
    │   │   │       │       │   ├── plus_jakartasans_regular.ttf
    │   │   │       │       │   ├── plus_jakartasans_semibold.ttf
    │   │   │       │       │   └── saira_semi_condensed_semi_bold.ttf
    │   │   │       │       ├── layout/
    │   │   │       │       │   ├── activity_main.xml
    │   │   │       │       │   ├── activity_ride_request.xml
    │   │   │       │       │   ├── api_loader.xml
    │   │   │       │       │   ├── app_notification.xml
    │   │   │       │       │   ├── bottom_sheet_permission.xml
    │   │   │       │       │   ├── bottom_sheet_permission_callmiss_driver.xml
    │   │   │       │       │   ├── dynamic_update_loader.xml
    │   │   │       │       │   ├── floating_widget_layout.xml
    │   │   │       │       │   ├── loader.xml
    │   │   │       │       │   ├── loader_mini.xml
    │   │   │       │       │   ├── loading_screen_overlay.xml
    │   │   │       │       │   ├── message_overlay.xml
    │   │   │       │       │   ├── message_sheet.xml
    │   │   │       │       │   ├── microphone_permission_dialog_2.xml
    │   │   │       │       │   ├── permission_steps_layout.xml
    │   │   │       │       │   ├── reel_view_pager_item.xml
    │   │   │       │       │   ├── reels_player_view.xml
    │   │   │       │       │   ├── sheet_view.xml
    │   │   │       │       │   ├── splash.xml
    │   │   │       │       │   ├── stop_detected_message_sheet.xml
    │   │   │       │       │   ├── viewpager_item.xml
    │   │   │       │       │   ├── viewpager_layout_view.xml
    │   │   │       │       │   └── youtube_video_view.xml
    │   │   │       │       ├── mipmap-anydpi-v26/
    │   │   │       │       │   ├── ic_launcher.xml
    │   │   │       │       │   └── ic_launcher_round.xml
    │   │   │       │       ├── raw/
    │   │   │       │       │   ├── accepted_by_another_driver_lottie.json
    │   │   │       │       │   ├── auto_rickshaw_processing.json
    │   │   │       │       │   ├── finding_rides_loader_auto_kochi.json
    │   │   │       │       │   ├── finding_rides_loader_auto_yellow_black.json
    │   │   │       │       │   ├── finding_rides_loader_bike.json
    │   │   │       │       │   ├── finding_rides_loader_no_text.json
    │   │   │       │       │   ├── finding_rides_loader_with_text_v2.json
    │   │   │       │       │   ├── finding_rides_loader_without_text_cab.json
    │   │   │       │       │   ├── ic_cab_vehicle_processing.json
    │   │   │       │       │   ├── map_style_aubergine.json
    │   │   │       │       │   ├── map_style_dark.json
    │   │   │       │       │   ├── map_style_night.json
    │   │   │       │       │   ├── map_style_retro.json
    │   │   │       │       │   ├── map_style_silver.json
    │   │   │       │       │   ├── mic_and_notification_permission_android.json
    │   │   │       │       │   ├── mic_permission_driver_lottie.json
    │   │   │       │       │   ├── payment_lottie_loader.json
    │   │   │       │       │   ├── primary_button_loader.json
    │   │   │       │       │   ├── progress_loader_line.json
    │   │   │       │       │   ├── ride_accepted_lottie.json
    │   │   │       │       │   ├── sample_data.json
    │   │   │       │       │   ├── search_loader.json
    │   │   │       │       │   ├── splash_lottie.json
    │   │   │       │       │   ├── success_lottie.json
    │   │   │       │       │   ├── voip_permissions_lottie.json
    │   │   │       │       │   └── waiting_for_customer_lottie.json
    │   │   │       │       ├── values/
    │   │   │       │       │   ├── bools.xml
    │   │   │       │       │   ├── colors.xml
    │   │   │       │       │   ├── constants.xml
    │   │   │       │       │   ├── dimens.xml
    │   │   │       │       │   ├── ic_launcher_background.xml
    │   │   │       │       │   ├── strings.xml
    │   │   │       │       │   ├── styles.xml
    │   │   │       │       │   └── values.xml
    │   │   │       │       ├── values-bn/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-hi/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-kn/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-ml/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-ta/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       ├── values-te/
    │   │   │       │       │   └── strings.xml
    │   │   │       │       └── xml/
    │   │   │       │           ├── file_paths.xml
    │   │   │       │           └── remote_config_defaults.xml
    │   │   │       └── test/
    │   │   │           └── java/
    │   │   │               └── in/
    │   │   │                   └── juspay/
    │   │   │                       └── mobility/
    │   │   │                           └── app/
    │   │   │                               └── ExampleUnitTest.java
    │   │   ├── mobility-common/
    │   │   │   ├── consumer-rules.pro
    │   │   │   ├── juspayAssets.sh
    │   │   │   ├── keep.sh
    │   │   │   ├── proguard-rules.pro
    │   │   │   ├── .gitignore
    │   │   │   └── src/
    │   │   │       └── main/
    │   │   │           ├── AndroidManifest.xml
    │   │   │           ├── assets/
    │   │   │           │   └── juspay/
    │   │   │           │       └── v1-assets_downloader.jsa
    │   │   │           ├── java/
    │   │   │           │   └── in/
    │   │   │           │       └── juspay/
    │   │   │           │           └── mobility/
    │   │   │           │               └── common/
    │   │   │           │                   ├── AudioRecorder.java
    │   │   │           │                   ├── CircleRippleEffect.java
    │   │   │           │                   ├── CircleRippleEffectOptions.java
    │   │   │           │                   ├── CustomTextView.java
    │   │   │           │                   ├── DeviceIdentifier.java
    │   │   │           │                   ├── FileUtils.java
    │   │   │           │                   ├── GeoCoderHelper.java
    │   │   │           │                   ├── MapRemoteConfig.java
    │   │   │           │                   ├── MapUpdate.java
    │   │   │           │                   ├── MediaPlayer.java
    │   │   │           │                   ├── MediaPlayerView.java
    │   │   │           │                   ├── MobilityCommonBridge.java
    │   │   │           │                   ├── OnTaskCompleteListener.java
    │   │   │           │                   ├── PaymentPage.java
    │   │   │           │                   ├── ShakeDetector.java
    │   │   │           │                   ├── SoundVisualizerBarView.java
    │   │   │           │                   ├── SpeechRecognition.java
    │   │   │           │                   ├── SpeechRecognitionView.java
    │   │   │           │                   ├── UICallBacks.java
    │   │   │           │                   ├── cropImage/
    │   │   │           │                   │   ├── BitmapCroppingWorkerTask.java
    │   │   │           │                   │   ├── BitmapLoadingWorkerTask.java
    │   │   │           │                   │   ├── BitmapUtils.java
    │   │   │           │                   │   ├── CropImage.java
    │   │   │           │                   │   ├── CropImageActivity.java
    │   │   │           │                   │   ├── CropImageAnimation.java
    │   │   │           │                   │   ├── CropImageOptions.java
    │   │   │           │                   │   ├── CropImageView.java
    │   │   │           │                   │   ├── CropOverlayView.java
    │   │   │           │                   │   ├── CropWindowHandler.java
    │   │   │           │                   │   └── CropWindowMoveHandler.java
    │   │   │           │                   ├── mediaPlayer/
    │   │   │           │                   │   ├── DefaultMediaPlayerControl.java
    │   │   │           │                   │   ├── MediaPlayerControl.java
    │   │   │           │                   │   ├── MediaPlayerOnCompleteListener.java
    │   │   │           │                   │   ├── MediaPlayerOnDurationListener.java
    │   │   │           │                   │   ├── MediaPlayerOnPauseListener.java
    │   │   │           │                   │   ├── MediaPlayerOnPlayListener.java
    │   │   │           │                   │   ├── MediaPlayerOnPreparedListener.java
    │   │   │           │                   │   ├── MediaPlayerOnStopListener.java
    │   │   │           │                   │   └── MediaPlayerWithId.java
    │   │   │           │                   ├── services/
    │   │   │           │                   │   ├── MobilityAPIResponse.java
    │   │   │           │                   │   ├── MobilityCallAPI.java
    │   │   │           │                   │   └── TLSSocketFactory.java
    │   │   │           │                   └── utils/
    │   │   │           │                       ├── CipherUtil.java
    │   │   │           │                       └── Utils.java
    │   │   │           └── res/
    │   │   │               ├── drawable/
    │   │   │               │   ├── circular_button_background.xml
    │   │   │               │   ├── halo_bg.xml
    │   │   │               │   ├── ic_audio_loader.xml
    │   │   │               │   ├── ic_grey_border.xml
    │   │   │               │   ├── ic_play.xml
    │   │   │               │   ├── progress_loader.xml
    │   │   │               │   ├── referral_code_background.xml
    │   │   │               │   ├── round_corner_layout.xml
    │   │   │               │   └── rounded_yellow_background.xml
    │   │   │               ├── layout/
    │   │   │               │   ├── loader.xml
    │   │   │               │   ├── marker_code_layout.xml
    │   │   │               │   ├── marker_label_layout.xml
    │   │   │               │   ├── ny_crop_image_activity.xml
    │   │   │               │   ├── ny_crop_image_view.xml
    │   │   │               │   ├── referral_code.xml
    │   │   │               │   ├── sounwave_view.xml
    │   │   │               │   └── speech_recognition_view.xml
    │   │   │               ├── menu/
    │   │   │               │   └── ny_crop_image_menu.xml
    │   │   │               ├── raw/
    │   │   │               │   ├── juspay_mobility_common_keep.xml
    │   │   │               │   ├── juspay_mobilitycommon_keep.xml
    │   │   │               │   └── map_style_retro.json
    │   │   │               ├── values/
    │   │   │               │   ├── attrs.xml
    │   │   │               │   ├── colors.xml
    │   │   │               │   ├── constants.xml
    │   │   │               │   ├── dimens.xml
    │   │   │               │   └── strings.xml
    │   │   │               ├── values-hi/
    │   │   │               │   └── strings.xml
    │   │   │               ├── values-kn/
    │   │   │               │   └── strings.xml
    │   │   │               └── values-ta/
    │   │   │                   └── strings.xml
    │   │   ├── mobility-customer/
    │   │   │   ├── consumer-rules.pro
    │   │   │   ├── juspayAssets.sh
    │   │   │   ├── keep.sh
    │   │   │   ├── proguard-rules.pro
    │   │   │   ├── .gitignore
    │   │   │   └── src/
    │   │   │       ├── debug/
    │   │   │       │   ├── assets/
    │   │   │       │   │   └── juspay/
    │   │   │       │   │       └── becknbase.html
    │   │   │       │   └── res/
    │   │   │       │       └── values/
    │   │   │       │           ├── bools.xml
    │   │   │       │           └── values.xml
    │   │   │       └── main/
    │   │   │           ├── AndroidManifest.xml
    │   │   │           ├── assets/
    │   │   │           │   ├── fonts/
    │   │   │           │   │   ├── dummy.ttf
    │   │   │           │   │   └── .gitkeep
    │   │   │           │   └── juspay/
    │   │   │           │       ├── becknbase.html
    │   │   │           │       ├── config.json
    │   │   │           │       ├── juspay_assets.json
    │   │   │           │       └── v1-configuration.js
    │   │   │           ├── java/
    │   │   │           │   └── in/
    │   │   │           │       └── juspay/
    │   │   │           │           └── mobility/
    │   │   │           │               └── customer/
    │   │   │           │                   └── MobilityCustomerBridge.java
    │   │   │           ├── js/
    │   │   │           │   └── juspay/
    │   │   │           │       ├── index_bundle.js
    │   │   │           │       └── tracker.js
    │   │   │           ├── res/
    │   │   │           │   ├── drawable/
    │   │   │           │   │   ├── ic_invoice_border.xml
    │   │   │           │   │   ├── ic_line.xml
    │   │   │           │   │   └── ic_line_white.xml
    │   │   │           │   ├── layout/
    │   │   │           │   │   └── invoice_template.xml
    │   │   │           │   ├── raw/
    │   │   │           │   │   └── keep.xml
    │   │   │           │   ├── values/
    │   │   │           │   │   ├── attrs.xml
    │   │   │           │   │   ├── bools.xml
    │   │   │           │   │   ├── colors.xml
    │   │   │           │   │   ├── ic_launcher_background.xml
    │   │   │           │   │   └── strings.xml
    │   │   │           │   ├── values-hi/
    │   │   │           │   │   └── strings.xml
    │   │   │           │   ├── values-kn/
    │   │   │           │   │   └── strings.xml
    │   │   │           │   ├── values-ta/
    │   │   │           │   │   └── strings.xml
    │   │   │           │   └── xml/
    │   │   │           │       └── file_paths.xml
    │   │   │           └── user/
    │   │   │               ├── mobility_pm/
    │   │   │               │   ├── assets/
    │   │   │               │   │   └── juspay/
    │   │   │               │   │       ├── becknbase.html
    │   │   │               │   │       ├── config.json
    │   │   │               │   │       ├── juspay_assets.json
    │   │   │               │   │       └── v1-configuration.js
    │   │   │               │   └── res/
    │   │   │               │       ├── drawable/
    │   │   │               │       │   ├── ic_invoice_border.xml
    │   │   │               │       │   ├── ic_line.xml
    │   │   │               │       │   └── ic_line_white.xml
    │   │   │               │       ├── layout/
    │   │   │               │       │   └── invoice_template.xml
    │   │   │               │       ├── raw/
    │   │   │               │       │   └── keep.xml
    │   │   │               │       ├── values/
    │   │   │               │       │   ├── attrs.xml
    │   │   │               │       │   ├── bools.xml
    │   │   │               │       │   ├── colors.xml
    │   │   │               │       │   ├── dimens.xml
    │   │   │               │       │   ├── ic_launcher_background.xml
    │   │   │               │       │   └── strings.xml
    │   │   │               │       ├── values-hi/
    │   │   │               │       │   └── strings.xml
    │   │   │               │       ├── values-kn/
    │   │   │               │       │   └── strings.xml
    │   │   │               │       ├── values-ta/
    │   │   │               │       │   └── strings.xml
    │   │   │               │       └── xml/
    │   │   │               │           └── file_paths.xml
    │   │   │               ├── mobility_rs/
    │   │   │               │   ├── assets/
    │   │   │               │   │   ├── fonts/
    │   │   │               │   │   │   ├── dummy.ttf
    │   │   │               │   │   │   └── .gitkeep
    │   │   │               │   │   └── juspay/
    │   │   │               │   │       ├── becknbase.html
    │   │   │               │   │       ├── config.json
    │   │   │               │   │       ├── juspay_assets.json
    │   │   │               │   │       └── v1-configuration.js
    │   │   │               │   └── res/
    │   │   │               │       ├── drawable/
    │   │   │               │       │   ├── ic_invoice_border.xml
    │   │   │               │       │   ├── ic_line.xml
    │   │   │               │       │   └── ic_line_white.xml
    │   │   │               │       ├── layout/
    │   │   │               │       │   └── invoice_template.xml
    │   │   │               │       ├── raw/
    │   │   │               │       │   └── keep.xml
    │   │   │               │       ├── values/
    │   │   │               │       │   ├── attrs.xml
    │   │   │               │       │   ├── bools.xml
    │   │   │               │       │   ├── colors.xml
    │   │   │               │       │   ├── dimens.xml
    │   │   │               │       │   ├── ic_launcher_background.xml
    │   │   │               │       │   └── strings.xml
    │   │   │               │       ├── values-hi/
    │   │   │               │       │   └── strings.xml
    │   │   │               │       ├── values-kn/
    │   │   │               │       │   └── strings.xml
    │   │   │               │       ├── values-ta/
    │   │   │               │       │   └── strings.xml
    │   │   │               │       └── xml/
    │   │   │               │           └── file_paths.xml
    │   │   │               └── passculture/
    │   │   │                   ├── assets/
    │   │   │                   │   ├── fonts/
    │   │   │                   │   │   ├── Montserrat-Black.ttf
    │   │   │                   │   │   ├── Montserrat-BlackItalic.ttf
    │   │   │                   │   │   ├── Montserrat-Bold.ttf
    │   │   │                   │   │   ├── Montserrat-BoldItalic.ttf
    │   │   │                   │   │   ├── Montserrat-ExtraBold.ttf
    │   │   │                   │   │   ├── Montserrat-ExtraBoldItalic.ttf
    │   │   │                   │   │   ├── Montserrat-ExtraLight.ttf
    │   │   │                   │   │   ├── Montserrat-ExtraLightItalic.ttf
    │   │   │                   │   │   ├── Montserrat-Italic.ttf
    │   │   │                   │   │   ├── Montserrat-Light.ttf
    │   │   │                   │   │   ├── Montserrat-LightItalic.ttf
    │   │   │                   │   │   ├── Montserrat-Medium.ttf
    │   │   │                   │   │   ├── Montserrat-MediumItalic.ttf
    │   │   │                   │   │   ├── Montserrat-Regular.ttf
    │   │   │                   │   │   ├── Montserrat-SemiBold.ttf
    │   │   │                   │   │   ├── Montserrat-SemiBoldItalic.ttf
    │   │   │                   │   │   ├── Montserrat-Thin.ttf
    │   │   │                   │   │   └── Montserrat-ThinItalic.ttf
    │   │   │                   │   └── juspay/
    │   │   │                   │       ├── becknbase.html
    │   │   │                   │       ├── config.json
    │   │   │                   │       ├── juspay_assets.json
    │   │   │                   │       └── v1-configuration.js
    │   │   │                   └── res/
    │   │   │                       ├── drawable/
    │   │   │                       │   ├── ic_invoice_border.xml
    │   │   │                       │   ├── ic_line.xml
    │   │   │                       │   └── ic_line_white.xml
    │   │   │                       ├── font/
    │   │   │                       │   ├── medium.ttf
    │   │   │                       │   ├── regular.ttf
    │   │   │                       │   └── semibold.ttf
    │   │   │                       ├── layout/
    │   │   │                       │   └── invoice_template.xml
    │   │   │                       ├── raw/
    │   │   │                       │   └── keep.xml
    │   │   │                       ├── values/
    │   │   │                       │   ├── attrs.xml
    │   │   │                       │   ├── bools.xml
    │   │   │                       │   ├── colors.xml
    │   │   │                       │   ├── dimens.xml
    │   │   │                       │   ├── ic_launcher_background.xml
    │   │   │                       │   └── strings.xml
    │   │   │                       └── xml/
    │   │   │                           └── file_paths.xml
    │   │   └── mobility-driver/
    │   │       ├── consumer-rules.pro
    │   │       ├── proguard-rules.pro
    │   │       ├── .gitignore
    │   │       └── src/
    │   │           ├── debug/
    │   │           │   ├── assets/
    │   │           │   │   └── juspay/
    │   │           │   │       └── becknbase.html
    │   │           │   └── res/
    │   │           │       └── values/
    │   │           │           ├── bools.xml
    │   │           │           └── values.xml
    │   │           └── main/
    │   │               ├── AndroidManifest.xml
    │   │               ├── assets/
    │   │               │   └── juspay/
    │   │               │       ├── becknbase.html
    │   │               │       ├── config.json
    │   │               │       └── juspay_assets.json
    │   │               ├── java/
    │   │               │   └── in/
    │   │               │       └── juspay/
    │   │               │           └── mobility/
    │   │               │               └── driver/
    │   │               │                   ├── CheckPermissionAutoStart.java
    │   │               │                   └── MobilityDriverBridge.java
    │   │               └── res/
    │   │                   ├── font/
    │   │                   │   └── plus_jakartasans_regular.ttf
    │   │                   ├── layout/
    │   │                   │   └── validate_documents_preview.xml
    │   │                   └── values/
    │   │                       ├── bools.xml
    │   │                       └── strings.xml
    │   ├── nix/
    │   │   └── pre-commit.nix
    │   ├── ui-common/
    │   │   ├── README.md
    │   │   ├── spago.dhall
    │   │   ├── .gitignore
    │   │   └── src/
    │   │       ├── Animation/
    │   │       │   ├── Animation.purs
    │   │       │   └── Config.purs
    │   │       ├── CarouselHolder/
    │   │       │   ├── CarouselHolder.purs
    │   │       │   ├── Controller.purs
    │   │       │   └── View.purs
    │   │       ├── Common/
    │   │       │   ├── Styles/
    │   │       │   │   └── Colors.purs
    │   │       │   └── Types/
    │   │       │       └── App.purs
    │   │       ├── Components/
    │   │       │   ├── AddAudioModel/
    │   │       │   │   ├── AddAudioModel.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── AddImagesModel/
    │   │       │   │   ├── AddImageModel.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── Banner/
    │   │       │   │   ├── Banner.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── BannerCarousel/
    │   │       │   │   ├── BannerCarousel.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── BoxContainer/
    │   │       │   │   ├── BoxContainer.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── Calendar/
    │   │       │   │   ├── Calendar.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── ChatView/
    │   │       │   │   ├── ChatView.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── CheckListView/
    │   │       │   │   ├── CheckListView.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── ChooseVehicle/
    │   │       │   │   ├── ChooseVehicle.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── ComplaintsModel/
    │   │       │   │   ├── ComplaintsModel.purs
    │   │       │   │   ├── Controllers.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── Controllers/
    │   │       │   │   └── PrimarySelectItem.purs
    │   │       │   ├── DateTimeSelector/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── DateTimeSelector.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── DropDownCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── DropDownCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── ErrorModal/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── ErrorModal.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── GenericHeader/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── GenericHeader.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── GenericMessageModal/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── GenericMessageModal.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── InfoBox/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── InfoBox.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── IssueList/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── IssueList.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── IssueView/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── IssueView.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── LargeBannerCarousel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── LargeBannerCarousel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── MenuButton/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── MenuButton.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── MobileNumberEditor/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── MobileNumberEditor.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── OptionsMenuView/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── OptionsMenuView.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── PopUpModal/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── PopUpModal.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── PrimaryButton/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── PrimaryButton.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── PrimaryEditText/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── PrimaryEditText.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── RateCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── RateCard.purs
    │   │       │   │   ├── Utils.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── RatingCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── RatingCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── RecordAudioModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── RecordAudioModel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── RequestInfoCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── RequestInfoCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── RideCompletedCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── RideCompletedCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── RideSummaryCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── RideSummaryCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── Safety/
    │   │       │   │   ├── SafetyActionTileView.purs
    │   │       │   │   ├── SafetyAudioRecording.purs
    │   │       │   │   ├── Utils.purs
    │   │       │   │   └── SosButtonAndDescription/
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── SosButtonAndDescription.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── SelectListModal/
    │   │       │   │   ├── CancelRide.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── Selector/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── Selector.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── SeparatorView/
    │   │       │   │   └── View.purs
    │   │       │   ├── SourceToDestination/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── SourceToDestination.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── SwitchButtonView/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── SwitchButton.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── TipsView/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── TipsView.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── UpdateErrorModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── UpdateErrorModel.purs
    │   │       │   │   └── View.purs
    │   │       │   └── ViewImageModel/
    │   │       │       ├── Controller.purs
    │   │       │       ├── View.purs
    │   │       │       └── ViewImageModel.purs
    │   │       ├── DecodeUtil/
    │   │       │   ├── DecodeUtil.js
    │   │       │   └── DecodeUtil.purs
    │   │       ├── Engineering/
    │   │       │   ├── Constants.purs
    │   │       │   ├── Constants/
    │   │       │   │   └── Configs.purs
    │   │       │   ├── Error/
    │   │       │   │   ├── Types.purs
    │   │       │   │   ├── Utils.purs
    │   │       │   │   ├── Types/
    │   │       │   │   │   ├── ApiError.purs
    │   │       │   │   │   └── ApiResponseError.purs
    │   │       │   │   └── Utils/
    │   │       │   │       ├── ApiErrorHandler.purs
    │   │       │   │       └── ApiResponseConverter.purs
    │   │       │   ├── Helpers/
    │   │       │   │   ├── Accessor.purs
    │   │       │   │   ├── API.purs
    │   │       │   │   ├── BackTrack.purs
    │   │       │   │   ├── Commons.js
    │   │       │   │   ├── Commons.purs
    │   │       │   │   ├── Events.js
    │   │       │   │   ├── Events.purs
    │   │       │   │   ├── Firebase.purs
    │   │       │   │   ├── GeoHash.js
    │   │       │   │   ├── GeoHash.purs
    │   │       │   │   ├── JBridge.js
    │   │       │   │   ├── JBridge.purs
    │   │       │   │   ├── Log.js
    │   │       │   │   ├── Log.purs
    │   │       │   │   ├── LogEvent.js
    │   │       │   │   ├── LogEvent.purs
    │   │       │   │   ├── MobilityPrelude.js
    │   │       │   │   ├── MobilityPrelude.purs
    │   │       │   │   ├── RippleCircles.js
    │   │       │   │   ├── RippleCircles.purs
    │   │       │   │   ├── Suggestions.purs
    │   │       │   │   ├── Timers.js
    │   │       │   │   ├── Timers.purs
    │   │       │   │   ├── Utils.js
    │   │       │   │   ├── Utils.purs
    │   │       │   │   ├── Pooling/
    │   │       │   │   │   ├── Pooling.js
    │   │       │   │   │   └── Pooling.purs
    │   │       │   │   └── SessionCache/
    │   │       │   │       ├── SessionCache.purs
    │   │       │   │       └── Types.purs
    │   │       │   └── OS/
    │   │       │       ├── Permission.js
    │   │       │       └── Permission.purs
    │   │       ├── FileProviders/
    │   │       │   ├── AssetsProvider/
    │   │       │   │   ├── AssetsProvider.js
    │   │       │   │   └── AssetsProvider.purs
    │   │       │   └── ConfigProvider/
    │   │       │       ├── ConfigProvider.js
    │   │       │       └── ConfigProvider.purs
    │   │       ├── Font/
    │   │       │   ├── Size.purs
    │   │       │   ├── Style.js
    │   │       │   └── Style.purs
    │   │       ├── LoaderOverlay/
    │   │       │   ├── Controller.purs
    │   │       │   ├── Handler.purs
    │   │       │   ├── ScreenData.purs
    │   │       │   └── View.purs
    │   │       ├── Locale/
    │   │       │   └── Utils.purs
    │   │       ├── LocalStorage/
    │   │       │   ├── Cache.js
    │   │       │   └── Cache.purs
    │   │       ├── MerchantConfig/
    │   │       │   ├── DefaultConfig.purs
    │   │       │   ├── Types.purs
    │   │       │   └── Utils/
    │   │       │       ├── Utils.js
    │   │       │       └── Utils.purs
    │   │       ├── Payment/
    │   │       │   ├── PaymentPage.js
    │   │       │   ├── PaymentPage.purs
    │   │       │   └── Domain/
    │   │       │       └── Payments.purs
    │   │       ├── RemoteConfig/
    │   │       │   ├── RemoteConfig.purs
    │   │       │   ├── Types.purs
    │   │       │   ├── Utils.js
    │   │       │   └── Utils.purs
    │   │       ├── Resource/
    │   │       │   └── Constants.purs
    │   │       ├── Screens/
    │   │       │   └── DriverProfileScreen/
    │   │       │       ├── Controller.purs
    │   │       │       ├── ScreenData.purs
    │   │       │       └── View.purs
    │   │       ├── Services/
    │   │       │   ├── Api.purs
    │   │       │   ├── Backend.purs
    │   │       │   ├── CallAPI.js
    │   │       │   └── CallAPI.purs
    │   │       ├── Styles/
    │   │       │   └── Types.purs
    │   │       ├── Toast/
    │   │       │   ├── Controller.purs
    │   │       │   ├── Handler.purs
    │   │       │   ├── ScreenData.purs
    │   │       │   └── View.purs
    │   │       └── Types/
    │   │           └── Sdk.purs
    │   ├── ui-customer/
    │   │   ├── cleanBower.sh
    │   │   ├── cleanSpago.sh
    │   │   ├── Components.md
    │   │   ├── index.js
    │   │   ├── JBridge.md
    │   │   ├── package.json
    │   │   ├── spago.dhall
    │   │   ├── stringGen.py
    │   │   ├── webpack.android.js
    │   │   ├── webpack.config.js
    │   │   ├── webpack.ios.js
    │   │   ├── .gitignore
    │   │   ├── dist/
    │   │   │   ├── index.html
    │   │   │   └── socket.js
    │   │   ├── scripts/
    │   │   │   ├── Multilingual.xlsx
    │   │   │   ├── notify.js
    │   │   │   └── scripts_generator.py
    │   │   └── src/
    │   │       ├── App.purs
    │   │       ├── Flow.purs
    │   │       ├── Main.purs
    │   │       ├── ModifyScreenTypes.purs
    │   │       ├── Components/
    │   │       │   ├── CommonComponentConfig.purs
    │   │       │   ├── ChooseYourRide/
    │   │       │   │   ├── ChooseYourRide.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── DeliveryParcelImageAndOtp/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── DeliveryParcelImageAndOtp.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── DriverInfoCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── DriverInfoCard.purs
    │   │       │   │   ├── View.purs
    │   │       │   │   └── Common/
    │   │       │   │       ├── Types.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── DropDownWithHeader/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── DropDownWithHeader.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── EmergencyHelp/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── EmergencyHelp.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── FavouriteDriverInfoCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── FavouriteDriverInfoCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── FavouriteLocationModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── FavouriteLocationModel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── GenericRadioButton/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── GenericRadioButton.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── IncrementDecrementModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── IncrementDecrementModel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── IndividualRideCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── IndividualRideCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── InputView/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── InputView.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── Loader/
    │   │       │   │   ├── DotLoader.purs
    │   │       │   │   └── Types.purs
    │   │       │   ├── LocationListItem/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── LocationListItem.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── LocationTagBar/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── LocationTagBar.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── LocationTagBarV2/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── LocationTagBarV2.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── MessagingView/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── MessagingView.purs
    │   │       │   │   ├── View.purs
    │   │       │   │   └── Common/
    │   │       │   │       ├── Types.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── NewContact/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── NewContact.purs
    │   │       │   │   └── view.purs
    │   │       │   ├── PopupWithCheckbox/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── PopupWithCheckbox.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── PricingTutorialModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── PricingTutorialModel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── ProviderModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── ProviderModel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── QuoteListItem/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── QuoteListItem.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── QuoteListModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── QuoteListModel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── Referral/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── Referral.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── SavedLocationCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── SavedLocationCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── SaveFavouriteCard/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── SaveFavouriteCard.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── SearchLocationModel/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── SearchLocationModel.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── ServiceTierCard/
    │   │       │   │   └── View.purs
    │   │       │   ├── SettingSideBar/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── SettingSideBar.purs
    │   │       │   │   └── View.purs
    │   │       │   └── StepsHeaderModel/
    │   │       │       ├── Controller.purs
    │   │       │       ├── StepsHeaderModel.purs
    │   │       │       └── View.purs
    │   │       ├── generated/
    │   │       │   └── source/
    │   │       │       └── main_locale.json
    │   │       ├── Helpers/
    │   │       │   ├── API.purs
    │   │       │   ├── Auth.purs
    │   │       │   ├── CommonView.purs
    │   │       │   ├── FrfsUtils.purs
    │   │       │   ├── Log.purs
    │   │       │   ├── MapUtils.purs
    │   │       │   ├── PrestoUtils.purs
    │   │       │   ├── Referral.purs
    │   │       │   ├── Ride.purs
    │   │       │   ├── SpecialZoneAndHotSpots.purs
    │   │       │   ├── SuggestionUtils.js
    │   │       │   ├── SuggestionUtils.purs
    │   │       │   ├── TipConfig.purs
    │   │       │   ├── Utils.js
    │   │       │   ├── Utils.purs
    │   │       │   ├── Version.purs
    │   │       │   └── Storage/
    │   │       │       ├── Storage.purs
    │   │       │       └── Flow/
    │   │       │           ├── BaseApp.purs
    │   │       │           └── SearchStatus.purs
    │   │       ├── MerchantConfigs/
    │   │       │   ├── DefaultConfig.purs
    │   │       │   └── Types.purs
    │   │       ├── RemoteConfigs/
    │   │       │   ├── RemoteConfig.purs
    │   │       │   ├── Types.purs
    │   │       │   └── Utils.purs
    │   │       ├── Resources/
    │   │       │   ├── Constants.purs
    │   │       │   ├── Animation/
    │   │       │   │   └── Config.purs
    │   │       │   ├── Localizable/
    │   │       │   │   ├── Strings.purs
    │   │       │   │   └── Types.purs
    │   │       │   ├── LocalizableV2/
    │   │       │   │   ├── BN.purs
    │   │       │   │   ├── EN.purs
    │   │       │   │   ├── HI.purs
    │   │       │   │   ├── KN.purs
    │   │       │   │   ├── ML.purs
    │   │       │   │   ├── OD.purs
    │   │       │   │   ├── Strings.purs
    │   │       │   │   ├── TA.purs
    │   │       │   │   ├── TE.purs
    │   │       │   │   └── Types.purs
    │   │       │   └── Styles/
    │   │       │       └── Colors.purs
    │   │       ├── Screens/
    │   │       │   ├── Handlers.purs
    │   │       │   ├── ScreenNames.purs
    │   │       │   ├── Types.purs
    │   │       │   ├── CustomerUtils/
    │   │       │   │   ├── AadhaarVerificationScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── AboutUsScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── AddNewAddressScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── AppUpdatePopUp/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── ContactUsScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── EmergencyContactsScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── FaqScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── FavouriteDriverTrips/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── HelpAndSupportScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── InvoiceScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── MyProfileScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── MyRidesScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── ReferralPayoutScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── View.purs
    │   │       │   │   │   └── UpiVerificationScreen/
    │   │       │   │   │       └── UpiVerificationScreen.purs
    │   │       │   │   ├── ReferralScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── ReportIssueChatScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── RideSelectionScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── RideSummaryScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SavedLocationScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SelectFaqScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SelectLanguageScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   └── TripDetailsScreen/
    │   │       │   │       ├── ComponentConfig.purs
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── Handler.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── NammaSafetyFlow/
    │   │       │   │   ├── ComponentConfig.purs
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── Handler.purs
    │   │       │   │   ├── ScreenData.purs
    │   │       │   │   ├── View.purs
    │   │       │   │   ├── ActivateSafetyScreen/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── Components/
    │   │       │   │   │   ├── ContactCircle.purs
    │   │       │   │   │   ├── ContactsList.purs
    │   │       │   │   │   ├── HeaderView.purs
    │   │       │   │   │   ├── HelperViews.purs
    │   │       │   │   │   └── SafetyUtils.purs
    │   │       │   │   ├── DataExplainWithFetch/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SafetyEducationScreen/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SafetySettingsScreen/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SetupSafetySettingsScreen/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   └── SosActiveScreen/
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── Handler.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── OnBoardingFlow/
    │   │       │   │   ├── AccountSetUpScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── ChooseLanguageScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── EnterMobileNumberScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── PermissionScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SuccessScreen/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   └── WelcomeScreen/
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── Handler.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── ParcelDeliveryFlow/
    │   │       │   │   └── ParcelDeliveryScreen/
    │   │       │   │       ├── ComponentConfig.purs
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── RentalBookingFlow/
    │   │       │   │   ├── RentalScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   └── RideScheduledScreen/
    │   │       │   │       ├── ComponentConfig.purs
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── RideBookingFlow/
    │   │       │   │   ├── DriverProfileScreen/
    │   │       │   │   │   └── Handler.purs
    │   │       │   │   ├── HomeScreen/
    │   │       │   │   │   ├── BannerConfig.purs
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── PopUpConfigs.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   ├── View.purs
    │   │       │   │   │   └── Controllers/
    │   │       │   │   │       ├── CarouselBannerController.purs
    │   │       │   │   │       ├── PopUpModelControllers.purs
    │   │       │   │   │       └── Types.purs
    │   │       │   │   ├── PickupInstructionsScreen/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   └── RiderRideCompletedCard/
    │   │       │   │       ├── ComponentConfig.purs
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── Handler.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── SearchLocationFlow/
    │   │       │   │   └── SearchLocationScreen/
    │   │       │   │       ├── ComponentConfig.purs
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── SelectContactsFlow/
    │   │       │   │   └── SelectContactsScreen/
    │   │       │   │       ├── ComponentConfig.purs
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── Handler.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       └── View.purs
    │   │       │   ├── SplashScreen/
    │   │       │   │   ├── Controller.purs
    │   │       │   │   ├── Handler.purs
    │   │       │   │   └── View.purs
    │   │       │   ├── TicketBookingFlow/
    │   │       │   │   ├── BusTicketBooking/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── BusTrackingScreen/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── MetroMyTickets/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── MetroTicketBooking/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── MetroTicketDetails/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── MetroTicketStatus/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── PlaceDetails/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── PlaceList/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── SelectBusRoute/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── TicketBooking/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── TicketInfo/
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   ├── TicketList/
    │   │       │   │   │   ├── ComponentConfig.purs
    │   │       │   │   │   ├── Controller.purs
    │   │       │   │   │   ├── Handler.purs
    │   │       │   │   │   ├── ScreenData.purs
    │   │       │   │   │   ├── Transformer.purs
    │   │       │   │   │   └── View.purs
    │   │       │   │   └── TicketStatus/
    │   │       │   │       ├── ComponentConfig.purs
    │   │       │   │       ├── Controller.purs
    │   │       │   │       ├── Handler.purs
    │   │       │   │       ├── ScreenData.purs
    │   │       │   │       ├── Transformer.purs
    │   │       │   │       └── View.purs
    │   │       │   └── TrackRide/
    │   │       │       └── FollowRideScreen/
    │   │       │           ├── ComponentConfig.purs
    │   │       │           ├── Controller.purs
    │   │       │           ├── Handler.purs
    │   │       │           ├── ScreenData.purs
    │   │       │           └── View.purs
    │   │       ├── Services/
    │   │       │   ├── Accessor.purs
    │   │       │   ├── API.purs
    │   │       │   ├── Backend.purs
    │   │       │   ├── Config.js
    │   │       │   ├── Config.purs
    │   │       │   ├── ConfigJBridge.js
    │   │       │   ├── ConfigJBridge.purs
    │   │       │   ├── EndPoint.purs
    │   │       │   └── FlowCache.purs
    │   │       └── Shimmer/
    │   │           └── Shimmer.purs
    │   └── ui-driver/
    │       ├── cleanBower.sh
    │       ├── cleanSpago.sh
    │       ├── Components.md
    │       ├── index.js
    │       ├── JBridge.md
    │       ├── package.json
    │       ├── ps-lazy-loader.js
    │       ├── spago.dhall
    │       ├── webpack.android.js
    │       ├── webpack.config.js
    │       ├── .gitignore
    │       ├── dist/
    │       │   ├── index.html
    │       │   └── socket.js
    │       ├── scripts/
    │       │   └── notify.js
    │       └── src/
    │           ├── App.purs
    │           ├── Flow.purs
    │           ├── Main.purs
    │           ├── ModifyScreenTypes.purs
    │           ├── Components/
    │           │   ├── DriverProfileScoreCard.purs
    │           │   ├── ExtraChargeCard.purs
    │           │   ├── AppOnboardingNavBar/
    │           │   │   ├── AppOnboardingNavBar.purs
    │           │   │   ├── Controller.purs
    │           │   │   └── View.purs
    │           │   ├── BottomDrawerList/
    │           │   │   ├── BottomDrawerList.purs
    │           │   │   ├── Controller.purs
    │           │   │   └── View.purs
    │           │   ├── BottomNavBar/
    │           │   │   ├── BottomNavBar.purs
    │           │   │   ├── Controller.purs
    │           │   │   └── View.purs
    │           │   ├── DatePickerModel/
    │           │   │   ├── Controller.purs
    │           │   │   ├── DatePickerModal.purs
    │           │   │   └── View.purs
    │           │   ├── DownloadStatementPopUp/
    │           │   │   ├── Controller.purs
    │           │   │   └── View.purs
    │           │   ├── DueDetailsList/
    │           │   │   ├── Controller.purs
    │           │   │   ├── DueDetailsList.purs
    │           │   │   └── View.purs
    │           │   ├── GoToLocationModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── GoToLocationModal.purs
    │           │   │   └── View.purs
    │           │   ├── InAppKeyboardModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── InAppKeyboardModal.purs
    │           │   │   └── View.purs
    │           │   ├── IndividualRideCard/
    │           │   │   ├── Controller.purs
    │           │   │   ├── IndividualRideCard.purs
    │           │   │   └── View.purs
    │           │   ├── InputTextView/
    │           │   │   ├── Controller.purs
    │           │   │   ├── InputTextView.purs
    │           │   │   └── View.purs
    │           │   ├── LocationListItem/
    │           │   │   ├── Controller.purs
    │           │   │   ├── LocationListItem.purs
    │           │   │   └── View.purs
    │           │   ├── MakePaymentModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── MakePaymentModal.purs
    │           │   │   └── View.purs
    │           │   ├── MenuButton/
    │           │   │   ├── Controller.purs
    │           │   │   ├── SelectMenuButton.purs
    │           │   │   └── View.purs
    │           │   ├── NotificationCard/
    │           │   │   ├── Controller.purs
    │           │   │   ├── NotificationCard.purs
    │           │   │   └── View.purs
    │           │   ├── NotificationDetailModel/
    │           │   │   ├── Controller.purs
    │           │   │   ├── NotificationDetailModel.purs
    │           │   │   └── View.purs
    │           │   ├── OnboardingHeader/
    │           │   │   ├── Controller.purs
    │           │   │   ├── OnboardingHeader.purs
    │           │   │   └── View.purs
    │           │   ├── PaymentHistoryListItem/
    │           │   │   ├── Controller.purs
    │           │   │   ├── PaymentHistoryListItem.purs
    │           │   │   └── View.purs
    │           │   ├── PaymentHistoryModel/
    │           │   │   ├── Controller.purs
    │           │   │   ├── PaymentHistoryModel.purs
    │           │   │   └── View.purs
    │           │   ├── PlanCard/
    │           │   │   ├── Controller.purs
    │           │   │   ├── PlanCard.purs
    │           │   │   └── View.purs
    │           │   ├── PrimaryEditText/
    │           │   │   ├── Controller.purs
    │           │   │   └── View.purs
    │           │   ├── PrimarySelectItem/
    │           │   │   ├── PrimarySelectItem.purs
    │           │   │   └── View.purs
    │           │   ├── ReferralMobileNumber/
    │           │   │   ├── Controller.purs
    │           │   │   ├── ReferralMobileNumber.purs
    │           │   │   └── View.purs
    │           │   ├── ReferralStepsView/
    │           │   │   ├── Controller.purs
    │           │   │   ├── ReferralStepsView.purs
    │           │   │   └── View.purs
    │           │   ├── RegistrationModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── RegistrationModal.purs
    │           │   │   └── View.purs
    │           │   ├── RideActionModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── RideActionModal.purs
    │           │   │   └── View.purs
    │           │   ├── RideAllocationModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── RideAllocationModal.purs
    │           │   │   └── View.purs
    │           │   ├── RideRequestCard/
    │           │   │   ├── Controller.purs
    │           │   │   └── View.purs
    │           │   ├── SelectPlansModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── SelectPlansModal.purs
    │           │   │   └── View.purs
    │           │   ├── SelectVehicleTypeModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── SelectVehicleTypeModal.purs
    │           │   │   └── View.purs
    │           │   ├── StatsModel/
    │           │   │   ├── Controller.purs
    │           │   │   ├── StatsModel.purs
    │           │   │   └── View.purs
    │           │   ├── TripStageTopBar/
    │           │   │   ├── Controller.purs
    │           │   │   ├── TripStageTopBar.purs
    │           │   │   └── View.purs
    │           │   ├── TutorialModal/
    │           │   │   ├── Controller.purs
    │           │   │   ├── TutorialModal.purs
    │           │   │   └── View.purs
    │           │   └── ValidateDocumentModal/
    │           │       ├── Controller.purs
    │           │       ├── ValidateDocumentModal.purs
    │           │       └── View.purs
    │           ├── generated/
    │           │   └── source/
    │           │       └── main_locale.json
    │           ├── Helpers/
    │           │   ├── API.purs
    │           │   ├── SplashUtils.purs
    │           │   ├── Storage.purs
    │           │   ├── Utils.js
    │           │   └── Utils.purs
    │           ├── MerchantConfigs/
    │           │   ├── DefaultConfig.purs
    │           │   └── Types.purs
    │           ├── RemoteConfigs/
    │           │   ├── RemoteConfig.purs
    │           │   ├── Types.purs
    │           │   ├── Utils.js
    │           │   └── Utils.purs
    │           ├── Resource/
    │           │   ├── Constants.js
    │           │   ├── Constants.purs
    │           │   ├── Animation/
    │           │   │   └── Config.purs
    │           │   ├── Localizable/
    │           │   │   ├── Strings.purs
    │           │   │   └── Types.purs
    │           │   ├── LocalizableV2/
    │           │   │   ├── BN.purs
    │           │   │   ├── EN.purs
    │           │   │   ├── HI.purs
    │           │   │   ├── KN.purs
    │           │   │   ├── ML.purs
    │           │   │   ├── OD.purs
    │           │   │   ├── StringsV2.purs
    │           │   │   ├── StringsV2Lazy.js
    │           │   │   ├── StringsV2Lazy.purs
    │           │   │   ├── StringsV2OG.purs
    │           │   │   ├── TA.purs
    │           │   │   ├── TE.purs
    │           │   │   └── TypesV2.purs
    │           │   └── Styles/
    │           │       └── Color.purs
    │           ├── Screens/
    │           │   ├── Handlers.purs
    │           │   ├── ScreenNames.purs
    │           │   ├── Types.purs
    │           │   ├── AadhaarVerificationScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── AboutUsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── AcknowledgementScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── AddVehicleDetailsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── ApplicationStatusScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── AppUpdatePopUpScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── BankDetailScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── Benefits/
    │           │   │   ├── BenefitsScreen/
    │           │   │   │   ├── ComponentConfig.purs
    │           │   │   │   ├── Controller.purs
    │           │   │   │   ├── Handler.purs
    │           │   │   │   ├── ScreenData.purs
    │           │   │   │   ├── Transformer.purs
    │           │   │   │   └── View.purs
    │           │   │   ├── LmsQuizScreen/
    │           │   │   │   ├── ComponentConfig.purs
    │           │   │   │   ├── Controller.purs
    │           │   │   │   ├── Handler.purs
    │           │   │   │   ├── ScreenData.purs
    │           │   │   │   ├── Transformer.purs
    │           │   │   │   └── View.purs
    │           │   │   └── LmsVideoScreen/
    │           │   │       ├── Controller.purs
    │           │   │       ├── Handler.purs
    │           │   │       ├── ScreenData.purs
    │           │   │       ├── Transformer.purs
    │           │   │       └── View.purs
    │           │   ├── BookingOptionsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── CancellationRateScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── ChooseCityScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── ChooseLanguageScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── CustomerReferralTrackerScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   ├── Types.purs
    │           │   │   └── View.purs
    │           │   ├── DocumentCapture/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── DocumentDetailsScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── DriverCompleteProfileScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── DriverDetailsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── DriverEarningsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── DriverProfileScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── DriverRideRatingScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── DriverSavedLocationScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── EditAadhaarDetailsScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── EditBankDetailsScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── EnterMobileNumberScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── EnterOTPScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── ExtraChargeInfoScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── HelpAndSupportScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── HomeScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── PopUpConfig.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── HotspotScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── MeterRideScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── MeterScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── MetroWarriorsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── NoInternetScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   └── View.purs
    │           │   ├── NotificationsScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── OnBoardingSubscriptionScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── PaymentHistoryScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── PermissionsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── PopUpScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── RateCardScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── ReferralScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── RegistrationScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── ReportIssueChatScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── RideHistoryScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── RideRequestScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── RideSelectionScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── RideSummaryScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── ScheduledRideAcceptedScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── SelectLanguageScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── SplashScreen/
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   └── View.purs
    │           │   ├── SubscriptionScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   ├── Transformer.purs
    │           │   │   └── View.purs
    │           │   ├── TripDetailsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── UploadAdhaarScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── UploadDrivingLicenseScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── UploadParcelImageScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── VehicleDetailsScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   ├── WelcomeScreen/
    │           │   │   ├── ComponentConfig.purs
    │           │   │   ├── Controller.purs
    │           │   │   ├── Handler.purs
    │           │   │   ├── ScreenData.purs
    │           │   │   └── View.purs
    │           │   └── WriteToUsScreen/
    │           │       ├── ComponentConfig.purs
    │           │       ├── Controller.purs
    │           │       ├── Handler.purs
    │           │       ├── ScreenData.purs
    │           │       └── View.purs
    │           └── Services/
    │               ├── Accessor.purs
    │               ├── API.purs
    │               ├── Backend.purs
    │               ├── Config.js
    │               ├── Config.purs
    │               ├── ConfigJBridge.js
    │               ├── ConfigJBridge.purs
    │               └── Endpoint.purs
    └── .github/
        ├── labeler.yml
        ├── PULL_REQUEST_TEMPLATE.md
        ├── .DS_Store
        ├── ISSUE_TEMPLATE/
        │   ├── bug_report.md
        │   └── feature_request.md
        └── workflows/
            ├── db-check.yaml
            ├── fe-check-android.yaml
            ├── fe-check-customer.yaml
            ├── fe-check-driver.yaml
            ├── generate_python_code.yml
            ├── hlint.yaml
            ├── ImageCompress.yml
            ├── manual-image-compression.yml
            ├── nix.yaml
            ├── purs-lint.yaml
            ├── raise-pr-to-asset-store.yaml
            ├── release-apk-driver.yaml
            ├── release-apk.yaml
            ├── stale.yaml
            └── update-build-version.yaml
