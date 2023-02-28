ARG DEP_IMAGE_PATH=nammayatri-dep
ARG DEP_LABEL=latest

FROM ${DEP_IMAGE_PATH}:${DEP_LABEL} as build
COPY . /opt/build/

WORKDIR /opt/build/Backend

# Make sure we haven't added any hlint warnings
RUN hlint_count=$(hlint -g -j --json | jq '.|length') && \
  echo "Found ${hlint_count} warnings" && \
  test ${hlint_count} -le 0

# And that we're ormolu-clean
RUN ormolu_files=`for i in $(git ls-files | grep '\.hs$'); do ormolu -m check -o '-XTypeApplications' -o '-fplugin=RecordDotPreprocessor' $i || echo $i; done` && \
  echo "Unformatted files: ${ormolu_files}" && \
  test -z ${ormolu_files}

ARG BUILD_ARGS

RUN stack build --system-ghc

RUN stack test mobility-core

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/Backend/bin

# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:18.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=build /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

RUN apt-get update && apt-get install -y \
  ca-certificates \
  build-essential \
  libssl-dev \
  libpq-dev \
  binutils \
  libmysqlclient-dev \
  librdkafka-dev


RUN DEBIAN_FRONTEND=noninteractive apt-get install -y awscli

COPY --from=build /opt/build/Backend/swagger .
COPY --from=build /opt/build/Backend/bin .
COPY --from=build /opt/build/Backend/swagger ./swagger
CMD ["/opt/app/rider-platform/rider-app-exe"]
