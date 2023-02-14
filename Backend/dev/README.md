# Monitoring stack

## Prometheus
Prometheus config file `./prometheus/config.yml` is configured to scrape beckn applications running on the host machine.

Since we are running docker containers in bridge mode, we cannot access host applications with `localhost`, instead we need to use host ip.

Replace the ip `192.168.29.32` with your host ip in the config file before starting up prometheus, also make sure the beckn apps are running on the host.


Prometheus dashboad - http://localhost:9090

Check whether Prometheus is able to scrape all the applications - http://localhost:9090/targets

## Grafana
Grafana dashboards are saved in `./grafana/dashboards` directory.

Grafana dashboard - http://localhost:3000/

Username: `admin`
Password: `beckn`

Ref:
- https://ops.tips/blog/initialize-grafana-with-preconfigured-dashboards/