# Shared by every proxied location, so a header can never be set on one route
# and forgotten on another.

proxy_http_version 1.1;
proxy_set_header Host              $host;
proxy_set_header X-Real-IP         $remote_addr;
proxy_set_header X-Forwarded-For   $proxy_add_x_forwarded_for;
proxy_set_header X-Forwarded-Proto $scheme;
proxy_set_header Connection        "";

# rider-app can be slow on a cold cache; a rider on a bad connection is slower
# still. Long enough to be honest, short enough that a hung upstream does not
# hold connections open indefinitely.
proxy_connect_timeout 5s;
proxy_send_timeout    30s;
proxy_read_timeout    30s;

# The upstream sets its own; passing ours through as well would duplicate them.
proxy_hide_header X-Powered-By;
