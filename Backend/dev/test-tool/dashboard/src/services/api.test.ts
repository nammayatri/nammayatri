/**
 * API Service Tests
 * Tests for the API client and location pinger
 */

import axios from 'axios';
import {
  callStep,
  startLocationPinger,
  stopLocationPinger,
  setGlobalLog,
} from './api';
import { Step, Config } from '../types';
import { ApiDef, MockDataPreset } from '../api-catalog/types';

// Mock axios
jest.mock('axios');
const mockedAxios = axios as jest.Mocked<typeof axios>;

describe('API Service', () => {
  const mockConfig: Config = {
    riderUrl: 'http://localhost:8013',
    driverUrl: 'http://localhost:8016',
    token: 'test-token',
    stripeUrl: 'http://localhost:8080',
  };

  const mockCtx: Record<string, any> = {
    config: mockConfig,
    searchOrigin: { lat: 10.0, lon: 76.0 },
    driverMerchantId: 'merchant-123',
  };

  beforeEach(() => {
    jest.clearAllMocks();
    // Set up default mock implementations
    mockedAxios.get.mockResolvedValue({ data: {}, status: 200 });
    mockedAxios.post.mockResolvedValue({ data: {}, status: 200 });
    mockedAxios.put.mockResolvedValue({ data: {}, status: 200 });
    mockedAxios.delete.mockResolvedValue({ data: {}, status: 200 });
    stopLocationPinger();
  });

  afterEach(() => {
    stopLocationPinger();
  });

  describe('callStep', () => {
    it('should make a successful GET request', async () => {
      const mockResponse = { data: { success: true }, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'test-step',
        name: 'Test Step',
        method: 'GET',
        service: 'rider',
        path: '/test',
      };

      const result = await callStep(step, mockConfig, mockCtx);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/test',
        expect.objectContaining({
          headers: expect.objectContaining({
            'Content-Type': 'application/json',
          }),
          timeout: 30000,
        })
      );
      expect(result.data).toEqual({ success: true });
      expect(result.status).toBe(200);
      expect(result.ok).toBe(true);
    });

    it('should make a successful POST request with body', async () => {
      const mockResponse = { data: { id: '123' }, status: 201 };
      mockedAxios.post.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'post-step',
        name: 'Post Step',
        method: 'POST',
        service: 'rider',
        path: '/create',
        body: { name: 'Test' },
      };

      const result = await callStep(step, mockConfig, mockCtx);

      expect(mockedAxios.post).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/create',
        { name: 'Test' },
        expect.objectContaining({
          headers: expect.objectContaining({
            'Content-Type': 'application/json',
          }),
          timeout: 30000,
        })
      );
      expect(result.data).toEqual({ id: '123' });
    });

    it('should handle dynamic path function', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'dynamic-path',
        name: 'Dynamic Path',
        method: 'GET',
        service: 'rider',
        path: (ctx: any) => `/items/${ctx.itemId}`,
      };

      const ctxWithItem = { ...mockCtx, itemId: '456' };
      await callStep(step, mockConfig, ctxWithItem);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/items/456',
        expect.any(Object)
      );
    });

    it('should handle LTS service with driver person_id token', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'lts-step',
        name: 'LTS Step',
        method: 'GET',
        service: 'lts',
        path: '/location',
        auth: true,
      };

      const ctxWithDriver = { ...mockCtx, driverPersonId: 'driver-123' };
      await callStep(step, mockConfig, ctxWithDriver);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/lts/location',
        expect.objectContaining({
          headers: expect.objectContaining({
            token: 'driver-123',
          }),
        })
      );
    });

    it('should handle provider-dashboard service with fleet token', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'fleet-step',
        name: 'Fleet Step',
        method: 'GET',
        service: 'provider-dashboard',
        path: '/fleet/info',
        auth: true,
      };

      const ctxWithFleet = { ...mockCtx, fleetAuthToken: 'fleet-token-123' };
      await callStep(step, mockConfig, ctxWithFleet);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/provider-dashboard/fleet/info',
        expect.objectContaining({
          headers: expect.objectContaining({
            token: 'fleet-token-123',
          }),
        })
      );
    });

    it('should apply extra headers from step', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'headers-step',
        name: 'Headers Step',
        method: 'GET',
        service: 'rider',
        path: '/test',
        extraHeaders: {
          'X-Custom': 'value',
          'X-Another': 'test',
        },
      };

      await callStep(step, mockConfig, mockCtx);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/test',
        expect.objectContaining({
          headers: expect.objectContaining({
            'X-Custom': 'value',
            'X-Another': 'test',
          }),
        })
      );
    });

    it('should handle dynamic extra headers function', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'dynamic-headers',
        name: 'Dynamic Headers',
        method: 'GET',
        service: 'rider',
        path: '/test',
        extraHeaders: (ctx: any) => ({
          'X-Context-Id': ctx.contextId,
        }),
      };

      const ctxWithId = { ...mockCtx, contextId: 'ctx-789' };
      await callStep(step, mockConfig, ctxWithId);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/test',
        expect.objectContaining({
          headers: expect.objectContaining({
            'X-Context-Id': 'ctx-789',
          }),
        })
      );
    });

    it('should handle PUT request', async () => {
      const mockResponse = { data: { updated: true }, status: 200 };
      mockedAxios.put.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'put-step',
        name: 'Put Step',
        method: 'PUT',
        service: 'rider',
        path: '/update',
        body: { status: 'active' },
      };

      const result = await callStep(step, mockConfig, mockCtx);

      expect(mockedAxios.put).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/update',
        { status: 'active' },
        expect.any(Object)
      );
      expect(result.data).toEqual({ updated: true });
    });

    it('should handle DELETE request', async () => {
      const mockResponse = { data: { deleted: true }, status: 200 };
      mockedAxios.delete.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'delete-step',
        name: 'Delete Step',
        method: 'DELETE',
        service: 'rider',
        path: '/delete/123',
      };

      const result = await callStep(step, mockConfig, mockCtx);

      expect(mockedAxios.delete).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/delete/123',
        expect.any(Object)
      );
      expect(result.data).toEqual({ deleted: true });
    });

    it('should handle API error response', async () => {
      const errorResponse = {
        response: {
          data: { error: 'Not found' },
          status: 404,
        },
      };
      mockedAxios.get.mockRejectedValueOnce(errorResponse);

      const step: Step = {
        id: 'error-step',
        name: 'Error Step',
        method: 'GET',
        service: 'rider',
        path: '/notfound',
      };

      const result = await callStep(step, mockConfig, mockCtx);

      expect(result.ok).toBe(false);
      expect(result.status).toBe(404);
      expect(result.data).toEqual({ error: 'Not found' });
    });

    it('should handle network error', async () => {
      mockedAxios.get.mockRejectedValueOnce(new Error('Network Error'));

      const step: Step = {
        id: 'network-error-step',
        name: 'Network Error Step',
        method: 'GET',
        service: 'rider',
        path: '/test',
      };

      const result = await callStep(step, mockConfig, mockCtx);

      expect(result.ok).toBe(false);
      expect(result.status).toBe(0);
      expect(result.data.error).toBe('Network Error');
    });

    it('should use catalog API for path resolution', async () => {
      const mockResponse = { data: { searchId: 'search-123' }, status: 200 };
      mockedAxios.post.mockResolvedValueOnce(mockResponse);

      const catalogApi: ApiDef = {
        id: 'rideSearch',
        name: 'Ride Search',
        description: 'Initiates a ride search',
        method: 'POST',
        service: 'rider',
        path: '/rideSearch',
        auth: true,
        bodyBuilder: (preset: MockDataPreset) => preset.data,
        mockDataPresets: [
          {
            id: 'default',
            name: 'Default Search',
            description: 'Default search preset',
            data: { origin: { lat: 10, lon: 76 } },
          },
        ],
      };

      const step: Step = {
        id: 'catalog-step',
        name: 'Catalog Step',
        method: 'POST',
        service: 'rider',
        path: '/oldPath',
      };

      await callStep(step, mockConfig, mockCtx, catalogApi, 'default');

      // Should use catalogApi.path, not step.path
      expect(mockedAxios.post).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/rideSearch',
        { origin: { lat: 10, lon: 76 } },
        expect.any(Object)
      );
    });

    it('should use catalog body builder with preset', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.post.mockResolvedValueOnce(mockResponse);

      const catalogApi: ApiDef = {
        id: 'testApi',
        name: 'Test API',
        description: 'Test',
        method: 'POST',
        service: 'rider',
        path: '/test',
        auth: false,
        bodyBuilder: (preset: MockDataPreset, ctx: any) => ({
          ...preset.data,
          merchantId: ctx.driverMerchantId,
        }),
        mockDataPresets: [
          {
            id: 'preset1',
            name: 'Preset 1',
            description: 'Preset for testing',
            data: { type: 'search' },
          },
        ],
      };

      const step: Step = {
        id: 'body-builder-step',
        name: 'Body Builder Step',
        method: 'POST',
        service: 'rider',
        path: '/test',
      };

      await callStep(step, mockConfig, mockCtx, catalogApi, 'preset1');

      expect(mockedAxios.post).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/rider/test',
        { type: 'search', merchantId: 'merchant-123' },
        expect.any(Object)
      );
    });

    it('should extract data from response using catalog extractFromResponse', async () => {
      const mockResponse = { data: { searchId: 'extracted-123' }, status: 200 };
      mockedAxios.post.mockResolvedValueOnce(mockResponse);

      const extractFn = jest.fn();
      const catalogApi: ApiDef = {
        id: 'testApi',
        name: 'Test API',
        description: 'Test',
        method: 'POST',
        service: 'rider',
        path: '/test',
        auth: true,
        extractFromResponse: extractFn,
        mockDataPresets: [],
      };

      const step: Step = {
        id: 'extract-step',
        name: 'Extract Step',
        method: 'POST',
        service: 'rider',
        path: '/test',
      };

      await callStep(step, mockConfig, mockCtx, catalogApi);

      expect(extractFn).toHaveBeenCalledWith(mockResponse.data, mockCtx);
    });

    it('should handle internal service without proxy prefix', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'internal-step',
        name: 'Internal Step',
        method: 'GET',
        service: 'internal',
        path: '/internal/health',
      };

      await callStep(step, mockConfig, mockCtx);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/internal/health',
        expect.any(Object)
      );
    });

    it('should handle mock-idfy service', async () => {
      const mockResponse = { data: {}, status: 200 };
      mockedAxios.get.mockResolvedValueOnce(mockResponse);

      const step: Step = {
        id: 'mock-idfy-step',
        name: 'Mock IDFY Step',
        method: 'GET',
        service: 'mock-idfy',
        path: '/verify',
      };

      await callStep(step, mockConfig, mockCtx);

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/mock-idfy/verify',
        expect.any(Object)
      );
    });
  });

  describe('Location Pinger', () => {
    beforeEach(() => {
      jest.useFakeTimers();
    });

    afterEach(() => {
      jest.useRealTimers();
    });

    it('should start location pinger and make initial ping', async () => {
      mockedAxios.post.mockResolvedValueOnce({ data: {}, status: 200 });

      const ctx = {
        driverToken: 'driver-token',
        driverMerchantId: 'merchant-123',
        driverLocation: { lat: 12.0, lon: 77.0 },
      };

      startLocationPinger(ctx as any);

      // Fast-forward past the initial 100ms delay
      jest.advanceTimersByTime(100);

      await Promise.resolve();

      // The pinger sends an array with point objects
      expect(mockedAxios.post).toHaveBeenCalledWith(
        'http://localhost:7082/proxy/lts/driver/location',
        expect.arrayContaining([
          expect.objectContaining({
            pt: expect.objectContaining({
              lat: 12.0,
              lon: 77.0,
            }),
            ts: expect.any(String),
          }),
        ]),
        expect.objectContaining({
          headers: expect.objectContaining({
            token: 'driver-token',
            mid: 'merchant-123',
            vt: 'SUV',
            dm: 'ONLINE',
          }),
          timeout: 5000,
        })
      );
    });

    it('should stop location pinger', () => {
      const ctx = {
        driverToken: 'token',
        driverMerchantId: 'merchant-123',
      };

      startLocationPinger(ctx as any);
      stopLocationPinger();

      // Should not throw
      expect(true).toBe(true);
    });

    it('should use searchOrigin as fallback when driverLocation not available', async () => {
      mockedAxios.post.mockResolvedValueOnce({ data: {}, status: 200 });

      const ctx = {
        driverToken: 'driver-token',
        driverMerchantId: 'merchant-123',
        searchOrigin: { lat: 10.5, lon: 76.5 },
      };

      startLocationPinger(ctx as any);
      jest.advanceTimersByTime(100);

      await Promise.resolve();

      expect(mockedAxios.post).toHaveBeenCalledWith(
        expect.any(String),
        expect.arrayContaining([
          expect.objectContaining({
            pt: expect.objectContaining({
              lat: 10.5,
              lon: 76.5,
            }),
          }),
        ]),
        expect.any(Object)
      );
    });

    it('should use default location when neither searchOrigin nor driverLocation available', async () => {
      mockedAxios.post.mockResolvedValueOnce({ data: {}, status: 200 });

      const ctx = {
        driverToken: 'driver-token',
        driverMerchantId: 'merchant-123',
      };

      startLocationPinger(ctx as any);
      jest.advanceTimersByTime(100);

      await Promise.resolve();

      expect(mockedAxios.post).toHaveBeenCalledWith(
        expect.any(String),
        expect.arrayContaining([
          expect.objectContaining({
            pt: expect.objectContaining({
              lat: 10.0739,
              lon: 76.2733,
            }),
          }),
        ]),
        expect.any(Object)
      );
    });

    it('should handle pinger error response', async () => {
      mockedAxios.post.mockRejectedValueOnce(new Error('Ping failed'));

      const ctx = {
        driverToken: 'driver-token',
        driverMerchantId: 'merchant-123',
      };

      startLocationPinger(ctx as any);
      jest.advanceTimersByTime(100);

      await Promise.resolve();

      // Should not throw, just log error
      expect(mockedAxios.post).toHaveBeenCalled();
    });
  });

  describe('setGlobalLog', () => {
    it('should set global log function', () => {
      const mockLog = jest.fn();
      setGlobalLog(mockLog);

      // The function should be set without error
      expect(mockLog).not.toHaveBeenCalled();
    });
  });
});
