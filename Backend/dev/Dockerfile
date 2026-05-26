FROM node:20 AS builder

WORKDIR /app
COPY test-tool/dashboard/package.json ./
RUN npm install

COPY test-tool/dashboard/ ./
RUN npm run build

FROM python:3.11-slim

WORKDIR /app
COPY config-sync/requirements.txt /app/config-sync/requirements.txt
RUN python3 -m pip install --no-cache-dir -r /app/config-sync/requirements.txt

RUN apt-get update && apt-get install -y --no-install-recommends \
    nodejs \
    npm \
    redis-tools \
    postgresql-client \
    curl \
    ca-certificates \
    gettext-base \
    patchelf \
    libgmp10 \
    libffi8 \
    libsodium23 \
    libncursesw6 \
    && npm install -g newman@6 \
    && rm -rf /var/lib/apt/lists/*

COPY test-tool/context-api/ /app/test-tool/context-api/
COPY integration-tests/     /app/integration-tests/
COPY config-sync/           /app/config-sync/

# passetto-server is shipped as per-arch prebuilts under config-sync/bin/.
# Stage the right one for $TARGETARCH at both lookup paths used by
# config_transfer.py (SCRIPT_DIR/bin/passetto-server-<arch> AND
# SCRIPT_DIR/passetto-server). The binaries are built under Nix so their
# ELF interpreter points at /nix/store/...-glibc-*/lib/ld-linux-*.so.* —
# patchelf'd to Debian's loader so they run in this image. GLIBC symbol
# requirements top out at 2.34, which Bookworm (glibc 2.36) satisfies.
ARG TARGETARCH
RUN case "$TARGETARCH" in \
      amd64) PS_ARCH=x86_64;  INTERP=/lib64/ld-linux-x86-64.so.2 ;; \
      arm64) PS_ARCH=aarch64; INTERP=/lib/ld-linux-aarch64.so.1 ;; \
      *)     echo "unsupported TARGETARCH=$TARGETARCH" && exit 1 ;; \
    esac \
 && SRC="/app/config-sync/bin/passetto-server-${PS_ARCH}" \
 && test -f "$SRC" || (echo "missing $SRC" && exit 1) \
 && chmod +x "$SRC" \
 && patchelf --set-interpreter "$INTERP" "$SRC" \
 && "$SRC" --help >/dev/null 2>&1 || "$SRC" --version >/dev/null 2>&1 || true \
 && ln -sf "$SRC" /app/config-sync/passetto-server \
 && ln -sf "$SRC" /usr/local/bin/passetto-server

RUN mkdir -p /app/test-tool/context-api/coverage-data \
    && mkdir -p /app/config-sync/assets/data

COPY --from=builder /app/build /usr/share/spa

EXPOSE 7082 8090

ENV PYTHONUNBUFFERED=1 \
    RUN_LOCAL_TESTING_DATA_ON_STARTUP=false

CMD ["python3", "/app/test-tool/context-api/server.py"]