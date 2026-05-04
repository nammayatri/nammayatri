# We decouple ports information on this file, so that
# the `kill-svc-ports` script can use it.
{
  # Databases
  db-primary = 5434;
  db-primary-replica = 5435;
  passetto-db = 5422;
  clickhouse = 8123;

  # Redis
  redis = 6379;
  redis-cluster-n1 = 30001;
  redis-cluster-n2 = 30002;
  redis-cluster-n3 = 30003;
  redis-cluster-n4 = 30004;
  redis-cluster-n5 = 30005;
  redis-cluster-n6 = 30006;

  # Kafka
  zookeeper = 2181;
  kafka = 29092;

  # Infrastructure
  nginx = 8085;
  passetto-service = 8021;
  mock-server = 8080;
  beckn-gateway = 8015;
  mock-registry = 8020;
  # osrm-server port is not configurable atm
  # osrm-server = 5001;

  # Mock services (Backend/app/mocks/*) — ports come from each mock's
  # App.hs / dhall config. Used by `kill-svc-ports` and any tooling
  # that talks to them directly during local dev.
  mock-fcm = 4545;
  mock-sms = 4343;
  mock-idfy = 6235;
  mock-google = 8019;
  mock-rider-platform = 8027;
  # mock-payment and mock-public-transport-provider-platform share 8091
  # in their dev dhall configs. They aren't run together in the local
  # stack today, but if both get enabled at once one of them needs a bump.
  mock-payment = 8091;
  mock-public-transport-provider-platform = 8091;

  # Application services
  rider-app = 8013;
  # External port (driver-proxy listens here and forwards to 8116, except
  # /ui/driver/location which goes to location-tracking-service on 8081).
  dynamic-offer-driver-app = 8016;
  # Internal port the driver-app actually binds to.
  dynamic-offer-driver-app-internal = 8116;
  rider-dashboard = 8017;
  provider-dashboard = 8018;
  rider-app-scheduler = 8058;
  driver-offer-allocator = 8055;
  location-tracking-service = 8081;
  image-api-helper = 8099;

  # Notification service
  notification-service-grpc = 50051;
  notification-service-http = 9091;

  # Application metrics
  rider-app-metrics = 9999;
  driver-app-metrics = 9997;
  beckn-gateway-metrics = 9998;
  rider-dashboard-metrics = 9991;
  provider-dashboard-metrics = 9992;
  rider-producer-metrics = 9990;

  # Dev tools
  context-api = 7082;
  test-dashboard = 3000;
  metabase = 3001;
  redis-commander = 8431;
}
