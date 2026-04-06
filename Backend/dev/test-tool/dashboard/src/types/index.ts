export type StepStatus = 'pending' | 'running' | 'pass' | 'fail' | 'skip';

export interface Step {
  id: string;
  name: string;
  method: 'GET' | 'POST' | 'PUT' | 'DELETE';
  service: 'rider' | 'driver' | 'lts' | 'internal' | 'provider-dashboard' | 'mock-idfy';
  path: string | ((ctx: Record<string, any>) => string);
  auth?: boolean;
  body?: (ctx: Record<string, any>) => any;
  assert?: (data: any) => string | null;
  save?: (data: any, ctx: Record<string, any>) => void;
  note?: string;
  extraHeaders?: Record<string, string> | ((ctx: Record<string, any>) => Record<string, string>);
  skip?: boolean | ((ctx: Record<string, any>) => boolean);
  useCatalog?: string;
  // Polling: retry the step every intervalMs until assert passes or timeoutMs reached
  poll?: { intervalMs: number; timeoutMs: number };
  // Summary: extract key info from response to show in the log line
  summary?: (data: any, ctx: Record<string, any>) => string;
}

export interface StepResult {
  stepId: string;
  status: StepStatus;
  durationMs: number;
  response?: any;
  error?: string;
  statusCode?: number;
  summary?: string;
  /** Per-service log deltas captured during this step's execution */
  serviceLogs?: Record<string, string>;
  /** Assertion results from Postman test script */
  assertions?: Array<{ name: string; passed: boolean; error?: string }>;
}

export interface Scenario {
  id: string;
  name: string;
  description: string;
  steps: Step[];
}

export interface FlowGroup {
  id: string;
  name: string;
  scenarios: Scenario[];
}

export interface Config {
  riderUrl: string;
  driverUrl: string;
  token: string;
  stripeUrl: string;
}

export interface LogEntry {
  time: string;
  level: 'info' | 'success' | 'error' | 'warn' | 'req';
  message: string;
  request?: { method: string; url: string; body?: any };
  response?: { status: number; body?: any };
  /** Per-service log deltas for this request */
  serviceLogs?: Record<string, string>;
}
