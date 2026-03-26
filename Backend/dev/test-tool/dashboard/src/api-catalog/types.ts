/**
 * API Catalog Types
 *
 * Each API definition includes its endpoint, method, request body builder,
 * and links to available mock data presets.
 */

export interface ApiDef {
  id: string;
  name: string;
  description: string;
  method: 'GET' | 'POST' | 'PUT' | 'DELETE';
  service: 'rider' | 'driver';
  path: string | ((ctx: Record<string, any>) => string); // can be dynamic
  auth: boolean;
  queryParams?: (ctx: Record<string, any>) => Record<string, string>;
  bodyBuilder?: (preset: any, ctx: Record<string, any>) => any;
  extractFromResponse?: (data: any, ctx: Record<string, any>) => void;
  assert?: (data: any) => string | null;
  mockDataPresets: MockDataPreset[];
}

export interface MockDataPreset {
  id: string;
  name: string;
  description: string;
  data: any; // the preset payload or fields
}

export interface LocationPreset {
  name: string;
  city: string;
  gps: { lat: number; lon: number };
  address: {
    street?: string;
    city?: string;
    state?: string;
    country?: string;
    area?: string;
    areaCode?: string;
    building?: string;
    title?: string;
  };
}
