#!/usr/bin/env python3
"""
Test Context API + CORS Proxy

1. Serves test context from local DB (merchants, riders, drivers, tokens)
2. Proxies API calls to rider-app/driver-app with CORS headers

Endpoints:
  GET  /api/context              → All test context data
  GET  /api/riders               → Available riders
  GET  /api/drivers              → Available drivers
  GET  /api/merchants            → Available merchants
  GET  /api/variants             → Vehicle variants
  ANY  /proxy/rider/*            → Proxy to rider-app (localhost:8013)
  ANY  /proxy/driver/*           → Proxy to driver-app (localhost:8016)

Port: 7082
"""

import json
import sys
import os
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse
import urllib.request
import urllib.error

PORT = 7082

RIDER_URL = os.environ.get("RIDER_URL", "http://localhost:8013")
DRIVER_URL = os.environ.get("DRIVER_URL", "http://localhost:8016")

DB_CONFIG = {
    "host": os.environ.get("DB_HOST", "localhost"),
    "port": int(os.environ.get("DB_PORT", "5434")),
    "dbname": os.environ.get("DB_NAME", "atlas_dev"),
    "user": os.environ.get("DB_USER", os.environ.get("USER", "atlas")),
    "password": os.environ.get("DB_PASS", ""),
}


def get_conn():
    import psycopg2
    return psycopg2.connect(**DB_CONFIG)


def query(sql, params=()):
    try:
        conn = get_conn()
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute(sql, params)
        cols = [d[0] for d in cur.description] if cur.description else []
        rows = [dict(zip(cols, r)) for r in cur.fetchall()] if cols else []
        conn.close()
        return rows
    except Exception as e:
        return {"error": str(e)}


def get_merchants():
    riders = query("""
        SELECT m.id, m.short_id, m.name, m.online_payment,
               moc.id as city_id, moc.city, moc.country, moc.state
        FROM atlas_app.merchant m
        LEFT JOIN atlas_app.merchant_operating_city moc ON moc.merchant_id = m.id
        ORDER BY m.short_id, moc.city
    """)
    drivers = query("""
        SELECT m.id, m.short_id, m.name,
               moc.id as city_id, moc.city, moc.country, moc.currency
        FROM atlas_driver_offer_bpp.merchant m
        LEFT JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.merchant_id = m.id
        ORDER BY m.short_id, moc.city
    """)
    return {"rider_merchants": riders, "driver_merchants": drivers}


def get_riders():
    return query("""
        SELECT p.id as person_id, p.first_name, p.role,
               m.short_id as merchant, moc.city,
               rt.token, rt.verified
        FROM atlas_app.person p
        JOIN atlas_app.merchant m ON m.id = p.merchant_id
        LEFT JOIN atlas_app.merchant_operating_city moc ON moc.id = p.merchant_operating_city_id
        LEFT JOIN atlas_app.registration_token rt ON rt.entity_id = p.id AND rt.verified = true
        WHERE p.role = 'USER' AND rt.token IS NOT NULL
        ORDER BY m.short_id, p.first_name
    """)


def get_drivers():
    return query("""
        SELECT p.id as person_id, p.first_name, p.role,
               m.short_id as merchant, m.id as merchant_id, moc.city, moc.currency,
               rt.token, rt.verified, v.variant as vehicle_variant
        FROM atlas_driver_offer_bpp.person p
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = p.merchant_id
        LEFT JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = p.merchant_operating_city_id
        LEFT JOIN atlas_driver_offer_bpp.registration_token rt ON rt.entity_id = p.id AND rt.verified = true
        LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
        WHERE p.role = 'DRIVER' AND rt.token IS NOT NULL
        ORDER BY m.short_id, p.first_name
    """)


def get_variants(city_id=None):
    sql = """
        SELECT vst.id, vst.service_tier_type, vst.name, vst.seating_capacity,
               vst.is_air_conditioned, vst.is_enabled, vst.priority,
               vst.allowed_vehicle_variant,
               m.short_id as merchant, moc.city, moc.currency
        FROM atlas_driver_offer_bpp.vehicle_service_tier vst
        JOIN atlas_driver_offer_bpp.merchant m ON m.id = vst.merchant_id
        JOIN atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = vst.merchant_operating_city_id
        WHERE vst.is_enabled = true
    """
    params = ()
    if city_id:
        sql += " AND vst.merchant_operating_city_id = %s"
        params = (city_id,)
    sql += " ORDER BY m.short_id, vst.priority"
    return query(sql, params)


def get_full_context():
    return {
        "merchants": get_merchants(),
        "riders": get_riders(),
        "drivers": get_drivers(),
        "variants": get_variants(),
    }


class ContextHandler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        print(f"  \033[93m[Context API]\033[0m {args[0]}")

    def _cors_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "*")

    def _send_json(self, data, status=200):
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self._cors_headers()
        self.end_headers()
        self.wfile.write(json.dumps(data, default=str).encode())

    def do_OPTIONS(self):
        self.send_response(200)
        self._cors_headers()
        self.end_headers()

    def _proxy(self, method):
        parsed = urlparse(self.path)
        path = parsed.path

        # Determine target — rider uses /v2 prefix, driver uses /ui prefix, lts direct
        LTS_URL = os.environ.get("LTS_URL", "http://localhost:8081")
        if path.startswith("/proxy/rider/"):
            target_base = RIDER_URL
            target_path = "/v2" + path[len("/proxy/rider"):]
        elif path.startswith("/proxy/lts/"):
            target_base = LTS_URL
            target_path = "/ui" + path[len("/proxy/lts"):]
        elif path.startswith("/proxy/driver/"):
            target_base = DRIVER_URL
            target_path = "/ui" + path[len("/proxy/driver"):]
        else:
            return False

        target_url = f"{target_base}{target_path}"
        if parsed.query:
            target_url += f"?{parsed.query}"

        # Read request body
        content_len = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(content_len) if content_len > 0 else None

        # Forward headers (except Host)
        fwd_headers = {}
        for key in self.headers:
            if key.lower() not in ("host", "origin", "referer"):
                fwd_headers[key] = self.headers[key]

        try:
            req = urllib.request.Request(target_url, data=body, headers=fwd_headers, method=method)
            with urllib.request.urlopen(req, timeout=30) as resp:
                resp_body = resp.read()
                self.send_response(resp.status)
                self.send_header("Content-Type", resp.headers.get("Content-Type", "application/json"))
                self._cors_headers()
                self.end_headers()
                self.wfile.write(resp_body)
        except urllib.error.HTTPError as e:
            resp_body = e.read()
            self.send_response(e.code)
            self.send_header("Content-Type", "application/json")
            self._cors_headers()
            self.end_headers()
            self.wfile.write(resp_body)
        except Exception as e:
            self._send_json({"error": str(e)}, 502)

        return True

    def _read_json_body(self):
        content_len = int(self.headers.get("Content-Length", 0))
        if content_len > 0:
            return json.loads(self.rfile.read(content_len))
        return {}

    def _handle(self, method):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")

        # Proxy requests
        if path.startswith("/proxy/"):
            return self._proxy(method)

        # POST API endpoints
        if method == "POST" and path == "/api/inflate-distance":
            body = self._read_json_body()
            ride_id = body.get("rideId")
            multiplier = body.get("multiplier", 3)
            if not ride_id:
                self._send_json({"error": "rideId required"}, 400)
                return True
            try:
                conn = get_conn()
                conn.autocommit = True
                cur = conn.cursor()
                cur.execute("""
                    UPDATE atlas_driver_offer_bpp.ride r
                    SET traveled_distance = COALESCE(
                        (SELECT estimated_distance FROM atlas_driver_offer_bpp.booking WHERE id = r.booking_id), 5000
                    ) * %s
                    WHERE r.id = %s
                """, (multiplier, ride_id))
                rows = cur.rowcount
                conn.close()
                if rows > 0:
                    self._send_json({"result": "Success", "rowsUpdated": rows, "multiplier": multiplier})
                else:
                    self._send_json({"error": f"No ride found with id {ride_id}"}, 404)
            except Exception as e:
                self._send_json({"error": str(e)}, 500)
            return True

        # GET API endpoints
        if method != "GET" and not path.startswith("/proxy/"):
            self._send_json({"error": "method not allowed"}, 405)
            return True

        if path == "/api/context":
            self._send_json(get_full_context())
        elif path == "/api/riders":
            self._send_json(get_riders())
        elif path == "/api/drivers":
            self._send_json(get_drivers())
        elif path == "/api/merchants":
            self._send_json(get_merchants())
        elif path.startswith("/api/variants"):
            parts = path.split("/")
            city_id = parts[3] if len(parts) > 3 else None
            self._send_json(get_variants(city_id))
        elif path == "/api/health":
            self._send_json({"status": "ok"})
        else:
            self._send_json({"error": "not found"}, 404)
        return True

    def do_GET(self):
        self._handle("GET")

    def do_POST(self):
        self._handle("POST")

    def do_PUT(self):
        self._handle("PUT")

    def do_DELETE(self):
        self._handle("DELETE")


def main():
    port = PORT
    for i, arg in enumerate(sys.argv):
        if arg == "--port" and i + 1 < len(sys.argv):
            port = int(sys.argv[i + 1])

    server = HTTPServer(("0.0.0.0", port), ContextHandler)
    print(f"\n  \033[93m📋 Context API + CORS Proxy on http://localhost:{port}\033[0m")
    print(f"  DB: {DB_CONFIG['user']}@{DB_CONFIG['host']}:{DB_CONFIG['port']}/{DB_CONFIG['dbname']}")
    print(f"  Proxy: /proxy/rider/* → {RIDER_URL}")
    print(f"  Proxy: /proxy/driver/* → {DRIVER_URL}\n")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutdown.")
        server.server_close()


if __name__ == "__main__":
    main()
