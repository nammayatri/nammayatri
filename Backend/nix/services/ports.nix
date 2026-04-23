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

  # Application services
  rider-app = 8013;
  dynamic-offer-driver-app = 8016;
  rider-dashboard = 8017;
  provider-dashboard = 8018;
  rider-app-scheduler = 8058;
  driver-offer-allocator = 8055;

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
  pgweb = 8432;
  redis-commander = 8431;
}
