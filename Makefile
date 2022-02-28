PROD_AWS_REGION := ap-south-1
PROD_AWS_ACCOUNT_ID := 147728078333

SANDBOX_AWS_REGION := ap-southeast-1
SANDBOX_AWS_ACCOUNT_ID := 701342709052

IMAGE_NAME ?= beckn-uat

SOURCE_COMMIT := $(shell git rev-parse HEAD)
VERSION := $(shell git rev-parse --short HEAD)

DEP_IMAGE ?= beckn

# If this is a PR build, use --fast and ignore optimisations
CHANGE_ID ?= undefined
ifeq ($(CHANGE_ID), undefined)
  BUILD_ARGS :=
else
  BUILD_ARGS := --fast
endif

# For PR builds, take the branch target as the dep image, for branches, it's
# the branch name itself
ifeq ($(CHANGE_ID), undefined)
  DEP_LABEL := $(BRANCH_NAME)
else
  DEP_LABEL := $(CHANGE_TARGET)
endif

IMAGE_REPO ?= SANDBOX

ifeq ($(IMAGE_REPO), PRODUCTION)
	AWS_REGION := $(PROD_AWS_REGION)
	AWS_ACCOUNT_ID := $(PROD_AWS_ACCOUNT_ID)
else
	AWS_REGION := $(SANDBOX_AWS_REGION)
	AWS_ACCOUNT_ID := $(SANDBOX_AWS_ACCOUNT_ID)
endif

NS := $(AWS_ACCOUNT_ID).dkr.ecr.$(AWS_REGION).amazonaws.com

.PHONY: build-dep push-dep build push run-svc run-monitoring stop-all-containers run-mobility-stack

build-dep: Dockerfile.dep
	$(info Building $(DEP_IMAGE):$(DEP_LABEL) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(DEP_IMAGE):$(DEP_LABEL) -f Dockerfile.dep .

push-dep: Dockerfile.dep
	aws ecr get-login-password --region $(AWS_REGION) | docker login --username AWS --password-stdin $(AWS_ACCOUNT_ID).dkr.ecr.$(AWS_REGION).amazonaws.com
	docker tag $(DEP_IMAGE):$(DEP_LABEL) $(NS)/$(DEP_IMAGE):$(DEP_LABEL)
	docker push $(NS)/$(DEP_IMAGE):$(DEP_LABEL)

build: Dockerfile
	$(info Building $(IMAGE_NAME):$(VERSION) / git-head: $(SOURCE_COMMIT))
	# Login with aws ecr to pull dependency base image
	aws ecr get-login-password --region $(AWS_REGION) | docker login --username AWS --password-stdin $(AWS_ACCOUNT_ID).dkr.ecr.$(AWS_REGION).amazonaws.com
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(IMAGE_NAME):$(VERSION) -f Dockerfile --build-arg "DEP_LABEL=$(DEP_LABEL)" --build-arg "DEP_IMAGE_PATH=$(NS)/$(DEP_IMAGE)" --build-arg "BUILD_ARGS=$(BUILD_ARGS)" .

push:
	# Login with aws ecr to push build image
	aws ecr get-login-password --region $(AWS_REGION) | docker login --username AWS --password-stdin $(AWS_ACCOUNT_ID).dkr.ecr.$(AWS_REGION).amazonaws.com
	docker tag $(IMAGE_NAME):$(VERSION) $(NS)/$(IMAGE_NAME):$(VERSION)
	docker push $(NS)/$(IMAGE_NAME):$(VERSION)

run-svc: ./dev/docker-compose.yml
	# Setup and run DB, redis and passetto instances in docker containers
	docker-compose -f ./dev/docker-compose.yml up -d --remove-orphans

run-monitoring: ./dev/docker-compose.yml
	# Run monitoring stack - Prometheus and grafana in docker containers
	docker-compose -f ./dev/docker-compose.yml --profile monitoring up -d

stop-all-containers: ./dev/docker-compose.yml
	# Stop all docker containers
	docker-compose -f ./dev/docker-compose.yml down --remove-orphans

run-mobility-stack: ./dev/run.sh
	# Run all binaries needed for mobility network
	./dev/run.sh

new-service: ./dev/new-service.sh
	# Create new service skeleton
	./dev/new-service.sh

build-dep-local: Dockerfile.dep
	$(info Building $(DEP_IMAGE):latest / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(DEP_IMAGE):latest -f Dockerfile.dep .
	rm -rf .ssh

build-local: Dockerfile
	$(info Building $(IMAGE_NAME):$(VERSION) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(IMAGE_NAME):$(VERSION) -f Dockerfile . --build-arg "DEP_IMAGE_PATH=$(DEP_IMAGE)"
	rm -rf .ssh