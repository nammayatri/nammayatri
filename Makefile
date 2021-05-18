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

# For production builds, use "beckn" as image name
ifeq ($(BRANCH_NAME), production)
  IMAGE_NAME := beckn
endif

.PHONY: build-dep push-dep build push aws-auth

build-dep: Dockerfile.dep
	$(info Building $(DEP_IMAGE):$(DEP_LABEL) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(DEP_IMAGE):$(DEP_LABEL) -f Dockerfile.dep .

push-dep: Dockerfile.dep
	docker tag $(DEP_IMAGE):$(DEP_LABEL) $(NS)/$(DEP_IMAGE):$(DEP_LABEL)
	docker push $(NS)/$(DEP_IMAGE):$(DEP_LABEL)

build: Dockerfile
	$(info Building $(IMAGE_NAME):$(VERSION) / git-head: $(SOURCE_COMMIT))
	rm -rf .ssh && cp -R ~/.ssh .
	docker build -t $(IMAGE_NAME):$(VERSION) -f Dockerfile --build-arg "NS=$(NS)" --build-arg "DEP_LABEL=$(DEP_LABEL)" --build-arg "DEP_IMAGE=$(DEP_IMAGE)" --build-arg "BUILD_ARGS=$(BUILD_ARGS)" .

push:
	docker tag $(IMAGE_NAME):$(VERSION) $(NS)/$(IMAGE_NAME):$(VERSION)
	docker push $(NS)/$(IMAGE_NAME):$(VERSION)

default: build

AWS_REGION := ap-south-1
AWS_ACCOUNT_ID := 147728078333

aws-auth:
	aws ecr get-login-password --region $(AWS_REGION) | docker login --username AWS --password-stdin $(AWS_ACCOUNT_ID).dkr.ecr.$(AWS_REGION).amazonaws.com 