IMAGE_NAME ?= atlas

SOURCE_COMMIT := $(shell git rev-parse HEAD)
VERSION := $(shell git rev-parse --short HEAD)

DEP_IMAGE ?= atlas-dep

.PHONY: build-dep build run-svc run-monitoring stop-all-containers run-mobility-stack format-all-files

run-svc: ./dev/docker-compose.yml
	# Setup and run DB, redis and passetto instances in docker containers
	docker-compose -f ./dev/docker-compose.yml up -d --remove-orphans

run-monitoring: ./dev/docker-compose.yml
	# Run monitoring stack - Prometheus and grafana in docker containers
	docker-compose -f ./dev/docker-compose.yml --profile monitoring up -d

run-pgadmin: ./dev/docker-compose.yml
	# Run pgadmin stack - Pgadmin in a docker container
	docker-compose -f ./dev/docker-compose.yml --profile pgadmin up -d

stop-all-containers: ./dev/docker-compose.yml
	# Stop all docker containers
	docker-compose -f ./dev/docker-compose.yml down --remove-orphans

run-mobility-stack: ./dev/run.sh
	# Run all binaries needed for mobility network
	./dev/run.sh

new-service: ./dev/new-service.sh
	# Create new service skeleton
	./dev/new-service.sh

format-all-files: ./dev/format-all-files.sh
	#Checking and formatting .hs and .dhall files
	./dev/format-all-files.sh

build-dep: Dockerfile.dep
	$(info Building $(DEP_IMAGE):latest / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(DEP_IMAGE):latest -f Dockerfile.dep .
	rm -rf .ssh

build: Dockerfile
	$(info Building $(IMAGE_NAME):$(VERSION) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(IMAGE_NAME):$(VERSION) -f Dockerfile . --build-arg "DEP_IMAGE_PATH=$(DEP_IMAGE)"
	rm -rf .ssh