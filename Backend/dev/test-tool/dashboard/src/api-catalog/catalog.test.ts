/**
 * API Catalog Tests
 * Tests for API catalog types and definitions
 */

import { ApiDef, MockDataPreset, LocationPreset } from './types';
import { buildRideSearchApi } from './rideSearch';
import { buildGetEstimatesApi } from './getEstimates';
import { buildEstimateSelectApi } from './estimateSelect';
import { kochiLocations, bangaloreLocations } from '../mock-data/locations';

describe('API Catalog Types', () => {
  describe('ApiDef Interface', () => {
    it('should create a valid ApiDef', () => {
      const apiDef: ApiDef = {
        id: 'test-api',
        name: 'Test API',
        description: 'A test API',
        method: 'GET',
        service: 'rider',
        path: '/test',
        auth: true,
        mockDataPresets: [],
      };

      expect(apiDef.id).toBe('test-api');
      expect(apiDef.name).toBe('Test API');
      expect(apiDef.method).toBe('GET');
      expect(apiDef.service).toBe('rider');
    });

    it('should support dynamic path function', () => {
      const apiDef: ApiDef = {
        id: 'dynamic-api',
        name: 'Dynamic API',
        description: 'API with dynamic path',
        method: 'GET',
        service: 'rider',
        path: (ctx) => `/items/${ctx.itemId}`,
        auth: true,
        mockDataPresets: [],
      };

      const ctx = { itemId: '123' };
      const resolvedPath = typeof apiDef.path === 'function' ? apiDef.path(ctx) : apiDef.path;
      expect(resolvedPath).toBe('/items/123');
    });

    it('should support query params builder', () => {
      const apiDef: ApiDef = {
        id: 'query-api',
        name: 'Query API',
        description: 'API with query params',
        method: 'GET',
        service: 'rider',
        path: '/search',
        auth: true,
        queryParams: (ctx) => ({ q: ctx.query, limit: '10' }),
        mockDataPresets: [],
      };

      const ctx = { query: 'test' };
      const params = apiDef.queryParams?.(ctx);
      expect(params).toEqual({ q: 'test', limit: '10' });
    });

    it('should support body builder', () => {
      const apiDef: ApiDef = {
        id: 'body-api',
        name: 'Body API',
        description: 'API with body builder',
        method: 'POST',
        service: 'rider',
        path: '/create',
        auth: true,
        bodyBuilder: (preset, ctx) => ({
          name: preset.data.name,
          userId: ctx.userId,
        }),
        mockDataPresets: [
          {
            id: 'preset-1',
            name: 'Preset 1',
            description: 'Test preset',
            data: { name: 'Test Name' },
          },
        ],
      };

      const preset = apiDef.mockDataPresets[0];
      const ctx = { userId: 'user-123' };
      const body = apiDef.bodyBuilder?.(preset, ctx);
      expect(body).toEqual({ name: 'Test Name', userId: 'user-123' });
    });

    it('should support extractFromResponse', () => {
      const extractFn = jest.fn();
      const apiDef: ApiDef = {
        id: 'extract-api',
        name: 'Extract API',
        description: 'API with extract',
        method: 'GET',
        service: 'rider',
        path: '/data',
        auth: true,
        extractFromResponse: extractFn,
        mockDataPresets: [],
      };

      const data = { id: '123' };
      const ctx = {};
      apiDef.extractFromResponse?.(data, ctx);
      expect(extractFn).toHaveBeenCalledWith(data, ctx);
    });

    it('should support assert function', () => {
      const apiDef: ApiDef = {
        id: 'assert-api',
        name: 'Assert API',
        description: 'API with assert',
        method: 'GET',
        service: 'rider',
        path: '/check',
        auth: true,
        assert: (data) => data.valid ? null : 'Invalid data',
        mockDataPresets: [],
      };

      expect(apiDef.assert?.({ valid: true })).toBeNull();
      expect(apiDef.assert?.({ valid: false })).toBe('Invalid data');
    });
  });

  describe('MockDataPreset Interface', () => {
    it('should create a valid MockDataPreset', () => {
      const preset: MockDataPreset = {
        id: 'preset-1',
        name: 'Test Preset',
        description: 'A test preset',
        data: { key: 'value', number: 123 },
      };

      expect(preset.id).toBe('preset-1');
      expect(preset.name).toBe('Test Preset');
      expect(preset.data).toEqual({ key: 'value', number: 123 });
    });
  });

  describe('LocationPreset Interface', () => {
    it('should create a valid LocationPreset', () => {
      const location: LocationPreset = {
        name: 'Test Location',
        city: 'Test City',
        gps: { lat: 10.0, lon: 76.0 },
        address: {
          street: 'Test Street',
          city: 'Test City',
          state: 'Test State',
          country: 'India',
          area: 'Test Area',
          areaCode: '123456',
          building: 'Test Building',
          title: 'Test Title',
        },
      };

      expect(location.name).toBe('Test Location');
      expect(location.gps.lat).toBe(10.0);
      expect(location.gps.lon).toBe(76.0);
      expect(location.address?.street).toBe('Test Street');
    });

    it('should support minimal LocationPreset', () => {
      const location: LocationPreset = {
        name: 'Minimal Location',
        city: 'City',
        gps: { lat: 0, lon: 0 },
        address: {},
      };

      expect(location.address).toEqual({});
    });
  });

  describe('Ride Search API', () => {
    it('should build ride search API definition', () => {
      const api = buildRideSearchApi('Kochi');

      expect(api.id).toBe('ride-search');
      expect(api.name).toBe('Ride Search');
      expect(api.method).toBe('POST');
      expect(api.service).toBe('rider');
      expect(api.path).toBe('/rideSearch');
      expect(api.auth).toBe(true);
    });

    it('should have mock data presets', () => {
      const api = buildRideSearchApi('Kochi');

      expect(api.mockDataPresets.length).toBeGreaterThan(0);
      expect(api.mockDataPresets[0]).toHaveProperty('id');
      expect(api.mockDataPresets[0]).toHaveProperty('name');
      expect(api.mockDataPresets[0]).toHaveProperty('data');
    });

    it('should build request body from preset', () => {
      const api = buildRideSearchApi('Kochi');
      const preset = api.mockDataPresets[0];
      const ctx: Record<string, any> = {};

      const body = api.bodyBuilder?.(preset, ctx);

      expect(body).toHaveProperty('fareProductType');
      expect(body).toHaveProperty('contents');
      expect(body.contents).toHaveProperty('origin');
    });

    it('should extract searchId from response', () => {
      const api = buildRideSearchApi('Kochi');
      const ctx: Record<string, any> = {};
      const response = { searchId: 'search-456' };

      api.extractFromResponse?.(response, ctx);

      expect(ctx.searchId).toBe('search-456');
    });
  });

  describe('Get Estimates API', () => {
    it('should build get estimates API definition', () => {
      const api = buildGetEstimatesApi();

      expect(api.id).toBe('get-estimates');
      expect(api.name).toBe('Get Estimates / Quotes');
      expect(api.method).toBe('GET');
      expect(api.service).toBe('rider');
    });

    it('should have dynamic path based on searchId', () => {
      const api = buildGetEstimatesApi();
      const ctx = { searchId: 'search-789' };

      const path = typeof api.path === 'function' ? api.path(ctx) : api.path;
      expect(path).toBe('/rideSearch/search-789/results');
    });

    it('should extract estimates from response', () => {
      const api = buildGetEstimatesApi();
      const ctx: Record<string, any> = {};
      const response = {
        estimates: [{ id: 'est-1' }, { id: 'est-2' }],
      };

      api.extractFromResponse?.(response, ctx);

      expect(ctx.estimates).toHaveLength(2);
      expect(ctx.selectedEstimateId).toBe('est-1');
    });

    it('should handle empty estimates', () => {
      const api = buildGetEstimatesApi();
      const ctx: Record<string, any> = {};
      const response = { estimates: [] };

      api.extractFromResponse?.(response, ctx);

      expect(ctx.estimates).toHaveLength(0);
      expect(ctx.selectedEstimateId).toBeUndefined();
    });
  });

  describe('Estimate Select API', () => {
    it('should build estimate select API definition', () => {
      const api = buildEstimateSelectApi();

      expect(api.id).toBe('estimate-select');
      expect(api.name).toBe('Select Estimate');
      expect(api.method).toBe('POST');
      expect(api.service).toBe('rider');
    });

    it('should have dynamic path based on selectedEstimateId', () => {
      const api = buildEstimateSelectApi();
      const ctx = { selectedEstimateId: 'estimate-123' };

      const path = typeof api.path === 'function' ? api.path(ctx) : api.path;
      expect(path).toBe('/estimate/estimate-123/select2');
    });

    it('should have payment instrument mock data presets', () => {
      const api = buildEstimateSelectApi();

      expect(api.mockDataPresets.length).toBeGreaterThan(0);
      expect(api.mockDataPresets.some(p => p.id === 'with-card-payment')).toBe(true);
      expect(api.mockDataPresets.some(p => p.id === 'cash-payment')).toBe(true);
    });

    it('should build request body with card payment instrument', () => {
      const api = buildEstimateSelectApi();
      const cardPreset = api.mockDataPresets.find(p => p.id === 'with-card-payment');
      const ctx: Record<string, any> = {};

      const body = api.bodyBuilder?.(cardPreset!, ctx);

      expect(body).toHaveProperty('paymentInstrument');
      expect(body.paymentInstrument).toEqual({ instrumentType: 'Card', instrumentName: 'DefaultCardType' });
    });

    it('should extract selectTtl from response', () => {
      const api = buildEstimateSelectApi();
      const ctx: Record<string, any> = {};
      const response = { selectTtl: 30 };

      api.extractFromResponse?.(response, ctx);

      expect(ctx.selectTtl).toBe(30);
    });

    it('should extract result from response when selectTtl not present', () => {
      const api = buildEstimateSelectApi();
      const ctx: Record<string, any> = {};
      const response = { result: 'success' };

      api.extractFromResponse?.(response, ctx);

      expect(ctx.selectTtl).toBe('success');
    });
  });

  describe('Mock Location Data', () => {
    it('should have Kochi locations', () => {
      expect(kochiLocations.length).toBeGreaterThan(0);
      expect(kochiLocations[0].city).toBe('Kochi');
      expect(kochiLocations[0]).toHaveProperty('gps');
      expect(kochiLocations[0]).toHaveProperty('address');
    });

    it('should have Bangalore locations', () => {
      expect(bangaloreLocations.length).toBeGreaterThan(0);
      expect(bangaloreLocations[0].city).toBe('Bangalore');
    });

    it('should have valid GPS coordinates', () => {
      const allLocations = [...kochiLocations, ...bangaloreLocations];

      allLocations.forEach(location => {
        expect(location.gps.lat).toBeGreaterThanOrEqual(-90);
        expect(location.gps.lat).toBeLessThanOrEqual(90);
        expect(location.gps.lon).toBeGreaterThanOrEqual(-180);
        expect(location.gps.lon).toBeLessThanOrEqual(180);
      });
    });

    it('should have unique location names within each city', () => {
      const kochiNames = kochiLocations.map(l => l.name);
      const bangaloreNames = bangaloreLocations.map(l => l.name);

      expect(new Set(kochiNames).size).toBe(kochiNames.length);
      expect(new Set(bangaloreNames).size).toBe(bangaloreNames.length);
    });
  });
});
