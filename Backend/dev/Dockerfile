FROM node:20 AS builder

WORKDIR /app
COPY test-tool/dashboard/package.json ./
RUN npm install

COPY test-tool/dashboard/ ./

ENV REACT_APP_PROXY_BASE=/api \
    REACT_APP_LOCAL_API_BASE="" \
    REACT_APP_RIDER_URL="https://api.sandbox.moving.tech" \
    REACT_APP_DRIVER_URL="https://api.sandbox.moving.tech" \
    REACT_APP_PROVIDER_DASHBOARD_URL="https://dashboard.integ.moving.tech" \
    REACT_APP_MOCK_SERVER_URL=""

RUN npm run build

FROM python:3.12-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    nginx \
    nodejs \
    npm \
    supervisor \
    python3-psycopg2 \
    redis-tools \
    postgresql-client \
    curl \
    ca-certificates \
    && npm install -g newman@6 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY test-tool/context-api/ /app/test-tool/context-api/
COPY integration-tests/      /app/integration-tests/
COPY config-sync/            /app/config-sync/

RUN pip install --no-cache-dir -r /app/config-sync/requirements.txt

RUN mkdir -p /app/test-tool/context-api/coverage-data \
    && mkdir -p /app/config-sync/assets/data

COPY --from=builder /app/build /usr/share/nginx/html

COPY test-tool/k8s/nginx.conf /etc/nginx/conf.d/default.conf
RUN rm -f /etc/nginx/sites-enabled/default

COPY test-tool/k8s/supervisord.conf /etc/supervisor/conf.d/ny-test.conf

EXPOSE 80

ENV PYTHONUNBUFFERED=1 \
    RUN_LOCAL_TESTING_DATA_ON_STARTUP=false

CMD ["supervisord", "-n", "-c", "/etc/supervisor/supervisord.conf"]
