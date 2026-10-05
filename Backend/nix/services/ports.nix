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

  # Legacy mock-service ports (the retired Haskell mocks under
  # Backend/app/mocks/*). The python mock-server
  # (dev/mock-servers/server.py) binds each of these as a legacy
  # listener so seeded URLs keep working. Used by `kill-svc-ports`
  # and any tooling that talks to them directly during local dev.
  mock-fcm = 4545;
  mock-sms = 4343;
  mock-idfy = 6235;
  mock-google = 8019;
  mock-payment = 8091;

  # Application services
  rider-app = 8013;
  # External port (driver-proxy listens here and forwards to 8116, except
  # /ui/driver/location which goes to location-tracking-service on 8081).
  dynamic-offer-driver-app = 8016;
  # Internal port the driver-app actually binds to.
  dynamic-offer-driver-app-internal = 8116;
  rider-app-scheduler = 8058;
  driver-offer-allocator = 8055;
  location-tracking-service = 8081;

  # Notification service
  notification-service-grpc = 50051;
  notification-service = 9091;

  # Application metrics
  rider-app-metrics = 9999;
  driver-app-metrics = 9997;
  beckn-gateway-metrics = 9998;
  rider-producer-metrics = 9990;
  producer-metrics = 9993;
  rider-producer-healthcheck = 8114;
  producer-healthcheck = 8115;
  driver-offer-allocator-metrics = 8056;
  rider-app-scheduler-metrics = 8057;
  kafka-ride-events-consumer-metrics = 9995;

  # Dev tools
  caddy-reverse-proxy = 9090;
  test-context-api = 7082;
  config-sync-server = 8090;
  metabase = 3001;
  victoria-metrics = 8428;
  db-manager-backend = 3010;
  db-manager-frontend = 5183;
}
