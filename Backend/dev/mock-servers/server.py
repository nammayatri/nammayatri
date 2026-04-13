#!/usr/bin/env python3
"""
Unified mock server for NammaYatri merchant service providers.

Each service handler lives in services/<name>.py with a handle(handler, path, body) function.

Two override mechanisms for test automation:

1. POST /mock/status  — keyed by explicit ID (e.g., payment order ID)
   {"service": "juspay", "id": ["order-123"], "status": "CHARGED", "data": {"amount": 150}}

2. POST /mock/override — keyed by request field extraction (middleware)
   {"service": "cmrl", "extract": "body.mob", "value": "9876543210",
    "response": {"returnCode": "500", "returnMessage": "Technical Failure"}}

   Extract syntax:
     body.<json_path>  — from JSON body (nested: body.data.customer.phone)
     path.<index>      — from URL path segment (0-based)
     query.<param>     — from query parameter
     header.<name>     — from request header

   Every handler receives matched overrides via handler._request_override dict.
"""

import argparse
import json
import logging
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse

import status_store
from services import juspay, stripe, paytm, exotel, acko, sos, kapture, whatsapp, mmi, nextbillion, gridline, transit, hyperverge, gullak, openai, cmrl, cris, ebix, mlpricing, ondc

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")
log = logging.getLogger("mock-server")

# Route table: (path substring, module, service name for overrides)
ROUTES = [
    ("/juspay",      juspay,      "juspay"),
    ("/stripe",      stripe,      "stripe"),
    ("/paytm",       paytm,       "paytm"),
    ("/exotel",      exotel,      "exotel"),
    ("/acko",        acko,        "acko"),
    ("/erss",        sos,         "sos"),
    ("/sos",         sos,         "sos"),
    ("/kapture",     kapture,     "kapture"),
    ("/gupshup",     whatsapp,    "whatsapp"),
    ("/karix",       whatsapp,    "whatsapp"),
    ("/mmi",         mmi,         "mmi"),
    ("/nextbillion", nextbillion, "nextbillion"),
    ("/gridline",    gridline,    "gridline"),
    ("/nandi",       transit,     "transit"),
    ("/gtfs",        transit,     "transit"),
    ("/hyperverge",  hyperverge,  "hyperverge"),
    ("/gullak",      gullak,      "gullak"),
    ("/openai",      openai,      "openai"),
    ("/cmrl",        cmrl,        "cmrl"),
    ("/cris",        cris,        "cris"),
    ("/ebix",        ebix,        "ebix"),
    ("/mlpricing",   mlpricing,   "mlpricing"),
    ("/ondc",        ondc,        "ondc"),
]


class MockHandler(BaseHTTPRequestHandler):

    def do_GET(self):
        self._handle()

    def do_POST(self):
        self._handle()

    def do_PUT(self):
        self._handle()

    def do_DELETE(self):
        self._handle()

    # ── Override access for handlers ──

    def _get_override(self, service, *candidate_ids):
        """Check status store for an override matching any candidate ID.
        Also checks extract-based rules and merges request-level overrides."""
        status_val = None
        data = {}
        # Check explicit ID-based status store
        for cid in candidate_ids:
            if cid:
                entry = status_store.get_status(service, cid)
                if entry:
                    status_val = entry["status"]
                    data = entry.get("data", {})
                    break
        # If no explicit match, try extract-based rules
        if status_val is None:
            parsed = urlparse(self.path)
            extract_status, extract_data = status_store.get_extract_status(
                service, parsed.path, None, None, None)
            if extract_status:
                status_val = extract_status
                data = extract_data
        # Merge request-level overrides from middleware (set by _apply_overrides)
        req_override = getattr(self, "_request_override", {})
        if req_override:
            data = status_store.deep_merge(data, req_override)
        return status_val, data

    # ── Main handler ──

    def _handle(self):
        parsed = urlparse(self.path)
        path = parsed.path
        query = parsed.query
        body = self._read_body()

        log.info(f"{self.command} {self.path}")

        # ── Mock APIs ──
        if path == "/mock/status":
            return self._mock_status(body)
        if path == "/mock/override":
            return self._mock_override(body)

        if path == "/" or path == "/health":
            return self._json({"status": "UP", "service": "mock-server"})

        # ── Flush Redis (for test orchestration) ──
        if path == "/flush-redis" and self.command == "POST":
            import subprocess
            subprocess.run(["redis-cli", "-p", "6379", "FLUSHALL"], capture_output=True)
            for p in [30001, 30002, 30003, 30004, 30005, 30006]:
                subprocess.run(["redis-cli", "-p", str(p), "-c", "FLUSHALL"], capture_output=True)
            return self._json({"flushed": True})

        # ── Route to service handler with middleware override check ──
        for prefix, module, svc_name in ROUTES:
            if prefix in path:
                # Middleware: check override rules and attach to handler
                self._apply_overrides(svc_name, path, query, body)
                module.handle(self, path, body)
                return

        self._json({"status": "ok", "mock": True, "path": path})

    def _apply_overrides(self, service, path, query, body):
        """Middleware: check all override rules for this service, extract IDs from request,
        and attach matched response overrides to self._request_override.
        For services with encrypted bodies (CRIS, CMRL), registered body decoders
        are used to decrypt and parse the request before matching."""
        body_parsed = None
        if body:
            try:
                text = body.decode("utf-8") if isinstance(body, bytes) else body
                body_parsed = json.loads(text)
            except (json.JSONDecodeError, ValueError):
                pass

        headers = {k.lower(): v for k, v in self.headers.items()}
        override = status_store.check_overrides(service, path, query, headers, body_parsed, body_raw=body)
        self._request_override = override

    # ── Mock status API (existing) ──

    def _mock_status(self, body):
        if self.command == "GET":
            result = {"statuses": status_store.list_statuses(), "overrides": status_store.list_overrides()}
            return self._json(result)

        if self.command == "DELETE":
            status_store.clear_statuses()
            status_store.clear_overrides()
            return self._json({"cleared": True})

        if self.command == "POST":
            try:
                data = json.loads(body) if body else {}
            except json.JSONDecodeError:
                return self._json({"error": "invalid JSON"}, status=400)

            service = data.get("service")
            identifiers = data.get("id")
            status = data.get("status")
            extract = data.get("extract")  # e.g. "path.2" to extract ID from URL path index 2

            if not service or not status:
                return self._json({"error": "service and status are required"}, status=400)
            if not identifiers and not extract:
                return self._json({"error": "either id (string or array) or extract (e.g. 'path.2') is required"}, status=400)

            if identifiers:
                count = status_store.set_status(service, identifiers, status, data.get("data"))
                return self._json({"ok": True, "service": service, "id": identifiers, "status": status, "ttl": status_store.STATUS_TTL, "updated": count})
            else:
                # Store as an extract-based rule: when a request to this service
                # has a matching path/body/query value, return this status
                match = data.get("match")  # e.g. "/orders" — only trigger on paths containing this
                status_store.set_extract_status(service, extract, status, match=match, data=data.get("data"))
                return self._json({"ok": True, "service": service, "extract": extract, "match": match, "status": status, "ttl": status_store.STATUS_TTL})

        return self._json({"error": "method not allowed"}, status=405)

    # ── Mock override API (new) ──

    def _mock_override(self, body):
        """
        POST /mock/override — add a request-matching override rule
          {"service": "cmrl", "extract": "body.mob", "value": "9876543210",
           "response": {"returnCode": "500", "returnMessage": "Technical Failure"}}

        GET /mock/override — list all active overrides

        DELETE /mock/override — clear all overrides
        """
        if self.command == "GET":
            return self._json(status_store.list_overrides())

        if self.command == "DELETE":
            try:
                data = json.loads(body) if body else {}
            except json.JSONDecodeError:
                return self._json({"error": "invalid JSON"}, status=400)

            service = data.get("service")
            extract = data.get("extract")
            value = data.get("value")

            if not service or not extract or value is None:
                return self._json({"error": "service, extract, and value are required"}, status=400)

            removed = status_store.delete_override(service, extract, str(value))
            return self._json({"ok": True, "removed": removed, "service": service, "extract": extract, "value": str(value)})

        if self.command == "POST":
            try:
                data = json.loads(body) if body else {}
            except json.JSONDecodeError:
                return self._json({"error": "invalid JSON"}, status=400)

            service = data.get("service")
            extract = data.get("extract")
            value = data.get("value")
            response = data.get("response")

            if not service or not extract or value is None or not response:
                return self._json({"error": "service, extract, value, and response are required"}, status=400)

            status_store.add_override(service, extract, value, response)
            return self._json({"ok": True, "service": service, "extract": extract, "value": str(value), "ttl": status_store.STATUS_TTL})

        return self._json({"error": "method not allowed"}, status=405)

    # ── Helpers ──

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length > 0:
            return self.rfile.read(length)
        return b""

    def _json(self, data, status=200):
        body = json.dumps(data).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):
        pass


def main():
    parser = argparse.ArgumentParser(description="Unified mock server for NammaYatri")
    parser.add_argument("--port", type=int, default=8080, help="Port (default: 8080)")
    args = parser.parse_args()

    server = HTTPServer(("0.0.0.0", args.port), MockHandler)
    log.info(f"Mock server running on :{args.port}")
    log.info(f"APIs: POST/GET/DELETE /mock/status, POST/GET/DELETE /mock/override")
    log.info(f"Services: {', '.join(r[0].strip('/') for r in ROUTES)}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        log.info("Shutting down")
        server.server_close()


if __name__ == "__main__":
    main()
