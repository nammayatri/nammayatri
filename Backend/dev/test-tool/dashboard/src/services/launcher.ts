import { LOCAL_API_BASE } from '../config';
import { LauncherDetailPayload, LauncherSummary } from '../types/launcher';

const base = `${LOCAL_API_BASE}/api/launcher`;

const j = async <T,>(path: string, init?: RequestInit): Promise<T> => {
  const res = await fetch(path, init);
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    throw new Error(`${res.status}: ${text || res.statusText}`);
  }
  return res.json();
};

export const listLaunchers = (): Promise<LauncherSummary[]> => j(`${base}`);

export const getLauncher = (slug: string): Promise<LauncherDetailPayload> =>
  j(`${base}/${encodeURIComponent(slug)}`);

export type InputPayload =
  | { value: string | number | boolean }
  | { filename: string; content_base64: string };

export const setInputs = (
  slug: string,
  values: Record<string, InputPayload>,
): Promise<{ inputs: Record<string, any> }> =>
  j(`${base}/${encodeURIComponent(slug)}/inputs`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(values),
  });

export const setSource = (
  slug: string,
  body: { ref?: string; localPath?: string | null },
): Promise<any> =>
  j(`${base}/${encodeURIComponent(slug)}/source`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });

export const browseFolder = (
  initial?: string,
): Promise<{ path: string | null; cancelled?: boolean; error?: string }> =>
  j(`${LOCAL_API_BASE}/api/browse-folder`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ initial: initial || '' }),
  });

export const runWorkflow = (slug: string, name: string): Promise<any> =>
  j(`${base}/${encodeURIComponent(slug)}/workflow/${encodeURIComponent(name)}`, {
    method: 'POST',
  });

export const runStage = (slug: string, stageId: string, force = false): Promise<any> =>
  j(`${base}/${encodeURIComponent(slug)}/stage/${encodeURIComponent(stageId)}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ force }),
  });

export const stopStage = (slug: string, stageId: string): Promise<any> =>
  j(`${base}/${encodeURIComponent(slug)}/stage/${encodeURIComponent(stageId)}/stop`, {
    method: 'POST',
  });

export const getStatus = (slug: string): Promise<{ stages: any[] }> =>
  j(`${base}/${encodeURIComponent(slug)}/status`);

export const logStreamUrl = (slug: string, logName: string): string =>
  `${base}/${encodeURIComponent(slug)}/logs/${encodeURIComponent(logName)}/stream`;

export const stageStreamUrl = (slug: string, stageId: string): string =>
  `${base}/${encodeURIComponent(slug)}/stage/${encodeURIComponent(stageId)}/stream`;

export const fileToBase64 = (file: File): Promise<string> =>
  new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onerror = () => reject(reader.error);
    reader.onload = () => {
      const r = reader.result as string;
      const idx = r.indexOf(',');
      resolve(idx >= 0 ? r.slice(idx + 1) : r);
    };
    reader.readAsDataURL(file);
  });
