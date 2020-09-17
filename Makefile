NS ?= asia.gcr.io/jp-k8s-internal
VERSION ?= latest

IMAGE_NAME ?= beckn-uat

SOURCE_COMMIT := $(shell git rev-parse HEAD)

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

.PHONY: build-dep push-dep build push

build-dep: Dockerfile.dep
	$(info Building $(NS)/$(DEP_IMAGE):$(DEP_LABEL) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(NS)/$(DEP_IMAGE):$(DEP_LABEL) -f Dockerfile.dep --build-arg .

push-dep: Dockerfile.dep
	docker push $(NS)/$(DEP_IMAGE):$(DEP_LABEL)

build: Dockerfile
	$(info Building $(NS)/$(IMAGE_NAME):$(VERSION) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(NS)/$(IMAGE_NAME):$(VERSION) -f Dockerfile --build-arg "DEP_LABEL=$(DEP_LABEL)" --build-arg "DEP_IMAGE=$(DEP_IMAGE)" --build-arg "BUILD_ARGS=$(BUILD_ARGS)" .

push:
	docker push $(NS)/$(IMAGE_NAME):$(VERSION)

default: build
