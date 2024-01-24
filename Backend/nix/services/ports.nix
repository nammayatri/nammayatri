# We decouple ports information on this file, so that
# the `kill-svc-ports` script can use it.
{
  db-primary = 5434;
  db-primary-replica = 5435;
  redis = 6379;
  redis-cluster-n1 = 30001;
  redis-cluster-n2 = 30002;
  redis-cluster-n3 = 30003;
  redis-cluster-n4 = 30004;
  redis-cluster-n5 = 30005;
  redis-cluster-n6 = 30006;
  zookeeper = 2181;
  kafka = 29092;
  nginx = 8080;
  passetto-db = 5422;
  passetto-service = 8021;
  # osrm-server port is not configurable atm
  # osrm-server = 5000;
}
