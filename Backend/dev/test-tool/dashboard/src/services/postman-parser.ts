/**
 * Postman Collection Parser
 *
 * Converts Postman v2.1.0 collection JSON into dashboard Step[]/TreeNode[]
 * so integration-tests collections are the single source of truth.
 */

// ── Types ──

export interface PostmanCollection {
  info: { name: string; description?: string };
  event?: PostmanEvent[];
  item: PostmanItem[];
  variable?: PostmanVariable[];
}

export interface PostmanItem {
  name: string;
  /**
   * Optional custom grouping label. Not part of the Postman spec — real
   * Postman/Newman ignore unknown fields, so this is harmless to them. When set,
   * the dashboard groups consecutive steps sharing the same value into one box.
   */
  _group?: string;
  event?: PostmanEvent[];
  request: {
    method: string;
    header?: Array<{ key: string; value: string }>;
    body?: { mode: string; raw?: string };
    url: PostmanUrl | string;
  };
  response?: any[];
}

interface PostmanUrl {
  raw: string;
  host?: string[];
  path?: string[];
  query?: Array<{ key: string; value: string }>;
  variable?: Array<{ key: string; value: string }>;
}

interface PostmanEvent {
  listen: 'test' | 'prerequest';
  script: { exec: string[]; type: string };
}

interface PostmanVariable {
  key: string;
  value: string;
  type?: string;
}

export interface ParsedStep {
  id: string;
  index: number;
  name: string;
  method: 'GET' | 'POST' | 'PUT' | 'DELETE';
  service: 'rider' | 'driver' | 'lts' | 'provider-dashboard' | 'rider-dashboard' | 'mock-idfy' | 'mock-server' | 'juspay-payment' | 'internal';
  /** URL path with {{var}} placeholders, relative to proxy prefix */
  pathTemplate: string;
  /** Raw URL template before service resolution */
  rawUrl: string;
  headers: Record<string, string>;
  auth: boolean;
  bodyTemplate: string | null;
  testScript: string | null;
  prereqScript: string | null;
  /** Tag for grouping: driver, rider, or system */
  tag: 'driver' | 'rider' | 'system';
  /** Explicit group label from the item's `_group` field, or null */
  group: string | null;
  /** Detected wait/delay in ms from prerequest script */
  delayMs: number;
}

export interface ParsedTreeNode {
  id: string;
  title: string;
  tag: 'driver' | 'rider' | 'system';
  stepIds: string[];
  /**
   * True when this node was formed by an explicit `_group` field (e.g. "Setup Driver A").
   * Such a group can mix actors, so the UI shows a per-step actor tag instead of a
   * single (potentially misleading) tag on the group header.
   */
  prefixGroup?: boolean;
}

// ── Base URL → Service mapping ──

const URL_VAR_TO_SERVICE: Record<string, { service: ParsedStep['service']; stripPrefix?: string }> = {
  'baseUrl_app': { service: 'rider' },
  'baseURL_namma_P': { service: 'driver' },
  'baseUrl_lts': { service: 'lts' },
  'baseURL_BPP_Dashboard': { service: 'provider-dashboard' },
  'baseURL_BPP_Dashboard_Internal': { service: 'provider-dashboard' },
  'dashboard_base_url': { service: 'provider-dashboard' },
  'bap_dashboard_url': { service: 'rider-dashboard' },
  // baseUrl_dashboard points at the rider-app itself with a /dashboard path prefix
  // (http://localhost:8013/dashboard), NOT the separate rider-dashboard service on :8017.
  // Routing it through the rider proxy preserves the /dashboard prefix extracted from the env URL.
  'baseUrl_dashboard': { service: 'rider' },
  'mockServerUrl': { service: 'mock-server' },
  'mock_server_url': { service: 'juspay-payment' },
  'dashboard_url': { service: 'provider-dashboard' },
};

// ── Parser ──

export function parseCollection(
  collection: PostmanCollection,
  envVars: Record<string, string>
): { steps: ParsedStep[]; nodes: ParsedTreeNode[]; collectionVars: Record<string, string> } {
  // Extract collection-level variables
  const collectionVars: Record<string, string> = {};
  for (const v of collection.variable ?? []) {
    if (v.key && v.value) collectionVars[v.key] = v.value;
  }

  const steps: ParsedStep[] = [];

  for (let i = 0; i < collection.item.length; i++) {
    const item = collection.item[i];
    const step = parseItem(item, i, envVars);
    steps.push(step);
  }

  const nodes = autoGroup(steps);
  return { steps, nodes, collectionVars };
}

function resolvePathVariables(url: PostmanUrl | string): string {
  if (typeof url === 'string') return url;
  let raw = url.raw ?? '';
  // Replace :varName with {{varName}} using the url.variable array
  if (url.variable) {
    for (const v of url.variable) {
      if (v.key && v.value) {
        raw = raw.replace(`:${v.key}`, v.value);
      }
    }
  }
  // Also handle any remaining :varName not in variable array → treat as {{varName}}
  raw = raw.replace(/:(\w+)/g, (match, key) => `{{${key}}}`);

  // If the raw URL doesn't have query params but the query array does, append them
  if (url.query && url.query.length > 0 && !raw.includes('?')) {
    const enabledParams = url.query.filter((q: any) => !q.disabled);
    if (enabledParams.length > 0) {
      const qs = enabledParams.map((q: any) => `${q.key}=${q.value}`).join('&');
      raw += `?${qs}`;
    }
  }
  return raw;
}

function parseItem(item: PostmanItem, index: number, envVars: Record<string, string>): ParsedStep {
  const rawUrl = resolvePathVariables(item.request.url);

  const { service, pathTemplate } = resolveService(rawUrl, envVars);

  // Headers
  const headers: Record<string, string> = {};
  let auth = false;
  for (const h of item.request.header ?? []) {
    headers[h.key] = h.value;
    if (h.key.toLowerCase() === 'token') auth = true;
  }

  // Body
  const bodyTemplate = item.request.body?.raw ?? null;

  // Scripts
  const testScript = extractScript(item.event, 'test');
  const prereqScript = extractScript(item.event, 'prerequest');

  // Detect delay from prerequest (setTimeout or busy-wait pattern)
  const delayMs = detectDelay(prereqScript);

  // Tag based on name
  const tag = inferTag(item.name);

  // Explicit grouping label (custom `_group` field), if present
  const group = item._group?.trim() || null;

  const id = `postman-${String(index).padStart(2, '0')}-${sanitize(item.name)}`;

  return {
    id,
    index,
    name: item.name,
    method: (item.request.method?.toUpperCase() || 'GET') as ParsedStep['method'],
    service,
    pathTemplate,
    rawUrl,
    headers,
    auth,
    bodyTemplate,
    testScript,
    prereqScript,
    tag,
    group,
    delayMs,
  };
}

function resolveService(
  rawUrl: string,
  envVars: Record<string, string>
): { service: ParsedStep['service']; pathTemplate: string } {
  // Find the base URL variable like {{baseUrl_app}}/path/...
  const match = rawUrl.match(/^\{\{(\w+)\}\}(.*)$/);
  if (match) {
    const varName = match[1];
    const pathPart = match[2]; // e.g. /rideSearch or /auth/{{driver_authId}}/verify
    const mapping = URL_VAR_TO_SERVICE[varName];
    if (mapping) {
      // Extract the path portion from the env variable value (e.g. /bpp/driver-offer from http://localhost:8018/bpp/driver-offer)
      const envVal = envVars[varName] ?? '';
      let basePath = '';
      try {
        const url = new URL(envVal);
        basePath = url.pathname === '/' ? '' : url.pathname.replace(/\/$/, '');
      } catch { /* not a valid URL, no base path */ }
      return { service: mapping.service, pathTemplate: basePath + (pathPart || '/') };
    }
    // Unknown base URL variable — treat as internal
    return { service: 'internal', pathTemplate: rawUrl };
  }

  // If URL has no variable prefix, check if it's a resolved URL
  for (const [varName, mapping] of Object.entries(URL_VAR_TO_SERVICE)) {
    const baseUrl = envVars[varName];
    if (baseUrl && rawUrl.startsWith(baseUrl)) {
      return { service: mapping.service, pathTemplate: rawUrl.slice(baseUrl.length) || '/' };
    }
  }

  return { service: 'internal', pathTemplate: rawUrl };
}

function extractScript(events: PostmanEvent[] | undefined, listen: 'test' | 'prerequest'): string | null {
  if (!events) return null;
  const parts = events
    .filter(e => e.listen === listen && e.script?.exec)
    .map(e => e.script!.exec!.join('\n').trim())
    .filter(s => s.length > 0);
  if (parts.length === 0) return null;
  return parts.join('\n');
}

function detectDelay(script: string | null): number {
  if (!script) return 0;
  // Match: const start = Date.now(); while (Date.now() - start < 5000) {}
  const busyWait = script.match(/Date\.now\(\)\s*-\s*start\s*<\s*(\d+)/);
  if (busyWait) return parseInt(busyWait[1], 10);
  // Match: setTimeout(..., 5000)
  const timeout = script.match(/setTimeout\s*\([^,]+,\s*(\d+)/);
  if (timeout) return parseInt(timeout[1], 10);
  return 0;
}

function inferTag(name: string): 'driver' | 'rider' | 'system' {
  const lower = name.toLowerCase();
  if (lower.includes('(dashboard)') || lower.includes('dashboard')) return 'system';
  if (lower.includes('(driver)') || lower.startsWith('driver') || lower.includes('go online') || lower.includes('bank account')) return 'driver';
  return 'rider';
}

function sanitize(name: string): string {
  return name.replace(/[^a-zA-Z0-9_-]/g, '_').replace(/^_+|_+$/g, '').substring(0, 40);
}

// ── Auto-grouping into TreeNodes ──

// A step may declare an explicit group via the custom `_group` field on its Postman
// item (carried through to ParsedStep.group). When set, consecutive steps sharing the
// same group value form one box titled by that group — regardless of actor tag — so
// e.g. all "Setup Driver A" steps stay together even though they mix driver/system
// calls. Steps without `_group` fall back to grouping by actor tag (unchanged
// behaviour), so collections that don't use the field are completely unaffected.
function autoGroup(steps: ParsedStep[]): ParsedTreeNode[] {
  const nodes: ParsedTreeNode[] = [];
  let currentNode: ParsedTreeNode | null = null;
  let currentKey: string | null = null; // 'grp:<label>' or 'tag:<tag>'

  for (const step of steps) {
    const key = step.group ? `grp:${step.group}` : `tag:${step.tag}`;
    // Start a new group when the grouping key changes (explicit group label or,
    // lacking one, the actor tag) or it's the first step.
    if (!currentNode || currentKey !== key) {
      currentNode = {
        id: `node-${nodes.length}`,
        title: step.group ?? inferNodeTitle(step, nodes.length),
        tag: step.tag,
        stepIds: [],
        prefixGroup: step.group !== null,
      };
      nodes.push(currentNode);
      currentKey = key;
    }
    currentNode.stepIds.push(step.id);
  }

  // Refine titles of single-step tag-grouped nodes to the step name.
  // Explicitly-grouped nodes keep their group title even when they hold one step.
  for (const node of nodes) {
    if (node.stepIds.length === 1) {
      const step = steps.find(s => s.id === node.stepIds[0]);
      if (step && step.group === null) node.title = step.name;
    }
  }

  return nodes;
}

function inferNodeTitle(firstStep: ParsedStep, index: number): string {
  const name = firstStep.name.toLowerCase();
  if (name.includes('auth') || name.includes('otp')) return `${firstStep.tag === 'driver' ? 'Driver' : 'Rider'} Authentication`;
  if (name.includes('search')) return 'Discovery';
  if (name.includes('accept') || name.includes('nearby')) return 'Driver Accept';
  if (name.includes('booking') || name.includes('select')) return 'Booking';
  if (name.includes('start') || name.includes('end')) return 'Fulfillment';
  if (name.includes('dashboard') || name.includes('switch') || name.includes('enable') || name.includes('vehicle')) return 'Setup (Dashboard)';
  if (name.includes('stripe') || name.includes('payment') || name.includes('bank')) return 'Payment Setup';
  if (name.includes('station')) return 'Station Lookup';
  return `${firstStep.tag.charAt(0).toUpperCase() + firstStep.tag.slice(1)} Steps (${index + 1})`;
}
