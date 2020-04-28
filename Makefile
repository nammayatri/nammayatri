# Override envars using -e
# make release -e NS=docker.io/juspay/test -e VERSION=1.2.3 -e IMAGE_NAME=docker-test

NS ?= asia.gcr.io/jp-k8s-internal
VERSION ?= latest

IMAGE_NAME ?= beckn-epass-uat

SOURCE_COMMIT := $(shell git rev-parse HEAD)

DEP_IMAGE ?= beckn-epass

DEP_LABEL ?= master

.PHONY: build push shell run start stop rm release

build-dep: Dockerfile.dep
	$(info Building $(NS)/$(DEP_IMAGE):$(DEP_LABEL) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(NS)/$(DEP_IMAGE):$(DEP_LABEL) -f Dockerfile.dep .

push-dep: Dockerfile.dep
	docker push $(NS)/$(DEP_IMAGE):$(DEP_LABEL)

build: Dockerfile
	$(info Building $(NS)/$(IMAGE_NAME):$(VERSION) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(NS)/$(IMAGE_NAME):$(VERSION) -f Dockerfile --build-arg "DEP_LABEL=$(DEP_LABEL)" --build-arg "DEP_IMAGE=$(DEP_IMAGE)" .

push:
	docker push $(NS)/$(IMAGE_NAME):$(VERSION)

default: build
