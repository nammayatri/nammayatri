#!/usr/bin/env python3
import json
import sys
from queue import Empty
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

import runner

PORT = 7083


class _Handler(BaseHTTPRequestHandler):
    def log_message(self, *_):
        pass

    def _cors(self):
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type, Authorization')

    def do_OPTIONS(self):
        self.send_response(204)
        self._cors()
        self.end_headers()

    def do_GET(self):
        p = self.path.rstrip('/')

        if p == '/health':
            self._json({'ok': True})
            return

        if p.startswith('/events/'):
            run_id = p[len('/events/'):]
            self._sse(run_id)
            return

        self._json({'error': 'Not found'}, 404)

    def do_POST(self):
        p = self.path.rstrip('/')
        length = int(self.headers.get('Content-Length', 0))
        raw = self.rfile.read(length) if length else b'{}'

        if p == '/start':
            try:
                config = json.loads(raw)
            except Exception as e:
                self._json({'error': f'Bad JSON: {e}'}, 400)
                return
            run_id = runner.start_run(config)
            self._json({'runId': run_id})
            return

        if p.startswith('/stop/'):
            run_id = p[len('/stop/'):]
            ok = runner.stop_run(run_id)
            self._json({'ok': ok})
            return

        self._json({'error': 'Not found'}, 404)

    def _json(self, data, status=200):
        body = json.dumps(data).encode()
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(body)))
        self._cors()
        self.end_headers()
        self.wfile.write(body)

    def _sse(self, run_id: str):
        eq = runner.get_queue(run_id)
        if eq is None:
            self._json({'error': 'Run not found or already finished'}, 404)
            return

        self.send_response(200)
        self.send_header('Content-Type', 'text/event-stream')
        self.send_header('Cache-Control', 'no-cache')
        self.send_header('Connection', 'keep-alive')
        self._cors()
        self.end_headers()

        try:
            while True:
                try:
                    event = eq.get(timeout=15)
                    self.wfile.write(f'data: {json.dumps(event)}\n\n'.encode())
                    self.wfile.flush()
                    if event.get('type') in ('run_complete', 'error'):
                        break
                except Empty:
                    # Heartbeat to keep the connection alive
                    self.wfile.write(b': ping\n\n')
                    self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError):
            pass


if __name__ == '__main__':
    print(
        '\n'
        '  ⚠️  Do not start this file directly.\n'
        '\n'
        '  Load test endpoints are embedded in test-local-api (port 7083).\n'
        '  Use Overmind to start that service instead:\n'
        '\n'
        '      overmind start   (or restart test-local-api in the Overmind UI)\n'
        '\n'
        '  Starting this standalone server would steal port 7083 and crash test-local-api.\n',
        file=sys.stderr
    )
    sys.exit(1)
