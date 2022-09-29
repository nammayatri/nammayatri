ARG DEP_IMAGE_PATH=beckn
ARG DEP_LABEL=latest

FROM ${DEP_IMAGE_PATH}:${DEP_LABEL} as build
COPY . /opt/build/

WORKDIR /opt/build

COPY ./.ssh/id_rsa /root/.ssh/id_rsa
RUN chmod 400 /root/.ssh/id_rsa
RUN ssh-keyscan -H "ssh.bitbucket.juspay.net" >> ~/.ssh/known_hosts

# Make sure we haven't added any hlint warnings
RUN hlint_count=$(hlint -g -j --json | jq '.|length') && \
  echo "Found ${hlint_count} warnings" && \
  test ${hlint_count} -le 0
# And that we're ormolu-clean
RUN ormolu_files=`for i in $(git ls-files | grep '\.hs$'); do ormolu -m check -o '-XTypeApplications' -o '-fplugin=RecordDotPreprocessor' $i || echo $i; done` && \
  echo "Unformatted files: ${ormolu_files}" && \
  test -z ${ormolu_files}

ARG BUILD_ARGS

RUN stack build --system-ghc ${BUILD_ARGS}

RUN stack test ${BUILD_ARGS} beckn-core

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

RUN rm -f /root/.ssh/id_rsa


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
  librdkafka-dev \
  awscli

COPY --from=build /opt/build/bin .
CMD ["/opt/app/app-backend-exe"]
