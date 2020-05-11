ARG DEP_IMAGE=beckn
ARG DEP_LABEL=master

FROM asia.gcr.io/jp-k8s-internal/${DEP_IMAGE}:${DEP_LABEL} as build
COPY . /opt/build/

WORKDIR /opt/build

COPY ./.ssh/id_rsa /root/.ssh/id_rsa
RUN chmod 400 /root/.ssh/id_rsa
RUN ssh-keyscan -H "bitbucket.org" >> ~/.ssh/known_hosts

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

RUN rm -f /root/.ssh/id_rsa


# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:16.04 as app
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
  libmysqlclient-dev

COPY --from=build /opt/build/bin .
CMD ["/opt/app/app-backend-exe"]
