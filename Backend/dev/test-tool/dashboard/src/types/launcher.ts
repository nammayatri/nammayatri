export type LauncherCategory = 'backend' | 'tool' | 'mobile' | 'infra' | 'other';

export interface SpecPort {
  name: string;
  port: number;
  protocol?: 'http' | 'tcp' | 'udp';
  allowOverride?: boolean;
  openInBrowser?: boolean;
}

export interface SpecDomain {
  name: string;
  url: string;
}

export type InputType = 'file' | 'select' | 'text' | 'textarea' | 'secret-text' | 'number' | 'boolean';

export interface SpecInput {
  key: string;
  label: string;
  type: InputType;
  accept?: string[];
  options?: string[];
  default?: unknown;
  storeAs?: 'path' | 'env-inline' | 'value';
  required?: boolean;
}

export type Lifecycle = 'one-shot' | 'long-running';

export interface SpecStage {
  id: string;
  builtin?: 'source';
  needs?: string[];
  cwd?: string;
  run?: string;
  lifecycle?: Lifecycle;
  invalidatedBy?: string[];
  readyProbe?: { kind: 'log' | 'http' | 'tcp'; pattern?: string; url?: string; port?: string };
}

export type LogKind = 'stage' | 'file' | 'cmd' | 'http';

export interface SpecLog {
  name: string;
  kind: LogKind;
  stage?: string;
  path?: string;
  follow?: boolean;
  cmd?: string;
  url?: string;
}

export interface LauncherSpec {
  name: string;
  title: string;
  icon?: string;
  category?: LauncherCategory;
  tags?: string[];
  source?: { url?: string; ref?: string; destDir?: string };
  ports?: SpecPort[];
  adbReverse?: string[];
  tools?: string[];
  domains?: SpecDomain[];
  inputs?: SpecInput[];
  env?: Record<string, string>;
  stages: SpecStage[];
  workflows?: Record<string, string[]>;
  workflowDescriptions?: Record<string, string>;
  logs?: SpecLog[];
  hooks?: { preStart?: string[]; postStop?: string[] };
}

export interface LauncherSummary {
  name: string;
  title: string;
  icon?: string;
  category?: LauncherCategory;
  tags?: string[];
  ports?: SpecPort[];
  domains?: SpecDomain[];
}

export type StageState = 'idle' | 'running' | 'ready' | 'stale' | 'failed';

export interface StageStatus {
  id: string;
  state: StageState;
  lifecycle: Lifecycle;
  stale: boolean;
  lastExit: number | null;
  startedAt: number | null;
  finishedAt: number | null;
  sessionId: string | null;
  command?: string | null;
  outputTail?: string | null;
  runs?: StageRun[];
}

export interface StageRun {
  command: string;
  exit: number | null;
  started_at: number | null;
  finished_at: number | null;
  output_tail: string;
}

export interface InputView {
  // Files and secrets come back as descriptors, never raw values.
  [k: string]: any;
}

export interface LauncherDetailPayload {
  spec: LauncherSpec;
  inputs: InputView;
  stages: StageStatus[];
  missingRequired: string[];
}
