/**
 * Context Service Tests
 * Tests for the context API client
 */

import axios from 'axios';
import {
  fetchTestContext,
  fetchRiders,
  fetchDrivers,
  RiderInfo,
  DriverInfo,
  MerchantInfo,
  VariantInfo,
  TestContext,
} from './context';

// Mock axios
jest.mock('axios');
const mockedAxios = axios as jest.Mocked<typeof axios>;

describe('Context Service', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('fetchTestContext', () => {
    it('should fetch and return test context successfully', async () => {
      const mockContext: TestContext = {
        merchants: {
          rider_merchants: [
            {
              id: 'merchant-1',
              short_id: 'rider_merch',
              name: 'Rider Merchant',
              city_id: 'city-1',
              city: 'Kochi',
              country: 'India',
              online_payment: true,
              currency: 'INR',
            },
          ],
          driver_merchants: [
            {
              id: 'merchant-2',
              short_id: 'driver_merch',
              name: 'Driver Merchant',
              city_id: 'city-1',
              city: 'Kochi',
              country: 'India',
              currency: 'INR',
            },
          ],
        },
        riders: [
          {
            person_id: 'rider-1',
            first_name: 'John',
            merchant: 'rider_merch',
            city: 'Kochi',
            token: 'token-123',
            verified: true,
          },
        ],
        drivers: [
          {
            person_id: 'driver-1',
            first_name: 'Jane',
            merchant: 'driver_merch',
            merchant_id: 'merchant-2',
            city: 'Kochi',
            currency: 'INR',
            token: 'token-456',
            verified: true,
            vehicle_variant: 'AUTO_RICKSHAW',
          },
        ],
        variants: [
          {
            id: 'variant-1',
            service_tier_type: 'AUTO_RICKSHAW',
            name: 'Auto',
            seating_capacity: 3,
            is_enabled: true,
            merchant: 'driver_merch',
            city: 'Kochi',
            currency: 'INR',
          },
        ],
        admin_credentials: {
          rider_merch: { email: 'admin@test.com', password: 'password123' },
        },
      };

      mockedAxios.get.mockResolvedValueOnce({ data: mockContext });

      const result = await fetchTestContext();

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/api/context',
        { timeout: 5000 }
      );
      expect(result).toEqual(mockContext);
    });

    it('should handle empty arrays in response', async () => {
      const mockContext = {
        merchants: {
          rider_merchants: [],
          driver_merchants: [],
        },
        riders: [],
        drivers: [],
        variants: [],
        admin_credentials: {},
      };

      mockedAxios.get.mockResolvedValueOnce({ data: mockContext });

      const result = await fetchTestContext();

      expect(result).toEqual(mockContext);
    });

    it('should handle null/undefined values gracefully', async () => {
      mockedAxios.get.mockResolvedValueOnce({
        data: {
          merchants: null,
          riders: undefined,
          drivers: 'not-an-array',
          variants: null,
          admin_credentials: null,
        },
      });

      const result = await fetchTestContext();

      expect(result).toEqual({
        merchants: {
          rider_merchants: [],
          driver_merchants: [],
        },
        riders: [],
        drivers: [],
        variants: [],
        admin_credentials: {},
      });
    });

    it('should return null on API error', async () => {
      mockedAxios.get.mockRejectedValueOnce(new Error('Network error'));

      const result = await fetchTestContext();

      expect(result).toBeNull();
    });

    it('should return null on timeout', async () => {
      mockedAxios.get.mockRejectedValueOnce(new Error('timeout'));

      const result = await fetchTestContext();

      expect(result).toBeNull();
    });

    it('should handle partial data response', async () => {
      mockedAxios.get.mockResolvedValueOnce({
        data: {
          merchants: {
            rider_merchants: [{ id: 'r1', short_id: 'rider', name: 'Rider', city_id: 'c1', city: 'Kochi', country: 'IN' }],
          },
          riders: [{ person_id: 'p1', first_name: 'Test', merchant: 'rider', city: 'Kochi', token: 't1', verified: true }],
        },
      });

      const result = await fetchTestContext();

      expect(result).toEqual({
        merchants: {
          rider_merchants: [{ id: 'r1', short_id: 'rider', name: 'Rider', city_id: 'c1', city: 'Kochi', country: 'IN' }],
          driver_merchants: [],
        },
        riders: [{ person_id: 'p1', first_name: 'Test', merchant: 'rider', city: 'Kochi', token: 't1', verified: true }],
        drivers: [],
        variants: [],
        admin_credentials: {},
      });
    });
  });

  describe('fetchRiders', () => {
    it('should fetch and return riders successfully', async () => {
      const mockRiders: RiderInfo[] = [
        {
          person_id: 'rider-1',
          first_name: 'John',
          merchant: 'rider_merch',
          city: 'Kochi',
          token: 'token-123',
          verified: true,
        },
        {
          person_id: 'rider-2',
          first_name: 'Alice',
          merchant: 'rider_merch',
          city: 'Kochi',
          token: 'token-789',
          verified: false,
        },
      ];

      mockedAxios.get.mockResolvedValueOnce({ data: mockRiders });

      const result = await fetchRiders();

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/api/riders',
        { timeout: 5000 }
      );
      expect(result).toEqual(mockRiders);
    });

    it('should return empty array on API error', async () => {
      mockedAxios.get.mockRejectedValueOnce(new Error('Network error'));

      const result = await fetchRiders();

      expect(result).toEqual([]);
    });

    it('should return empty array for non-array response', async () => {
      mockedAxios.get.mockResolvedValueOnce({ data: null });

      const result = await fetchRiders();

      expect(result).toEqual([]);
    });

    it('should handle empty riders array', async () => {
      mockedAxios.get.mockResolvedValueOnce({ data: [] });

      const result = await fetchRiders();

      expect(result).toEqual([]);
    });
  });

  describe('fetchDrivers', () => {
    it('should fetch and return drivers successfully', async () => {
      const mockDrivers: DriverInfo[] = [
        {
          person_id: 'driver-1',
          first_name: 'Jane',
          merchant: 'driver_merch',
          merchant_id: 'merchant-2',
          city: 'Kochi',
          currency: 'INR',
          token: 'token-456',
          verified: true,
          vehicle_variant: 'AUTO_RICKSHAW',
        },
        {
          person_id: 'driver-2',
          first_name: 'Bob',
          merchant: 'driver_merch',
          merchant_id: 'merchant-2',
          city: 'Kochi',
          currency: 'INR',
          token: 'token-000',
          verified: true,
        },
      ];

      mockedAxios.get.mockResolvedValueOnce({ data: mockDrivers });

      const result = await fetchDrivers();

      expect(mockedAxios.get).toHaveBeenCalledWith(
        'http://localhost:7082/api/drivers',
        { timeout: 5000 }
      );
      expect(result).toEqual(mockDrivers);
    });

    it('should return empty array on API error', async () => {
      mockedAxios.get.mockRejectedValueOnce(new Error('Network error'));

      const result = await fetchDrivers();

      expect(result).toEqual([]);
    });

    it('should return empty array for non-array response', async () => {
      mockedAxios.get.mockResolvedValueOnce({ data: undefined });

      const result = await fetchDrivers();

      expect(result).toEqual([]);
    });

    it('should handle empty drivers array', async () => {
      mockedAxios.get.mockResolvedValueOnce({ data: [] });

      const result = await fetchDrivers();

      expect(result).toEqual([]);
    });
  });

  describe('Type interfaces', () => {
    it('should have correct RiderInfo structure', () => {
      const rider: RiderInfo = {
        person_id: 'test-id',
        first_name: 'Test',
        merchant: 'test-merchant',
        city: 'TestCity',
        token: 'test-token',
        verified: true,
      };

      expect(rider.person_id).toBe('test-id');
      expect(rider.verified).toBe(true);
    });

    it('should have correct DriverInfo structure with optional vehicle_variant', () => {
      const driverWithVariant: DriverInfo = {
        person_id: 'driver-id',
        first_name: 'Driver',
        merchant: 'merchant',
        merchant_id: 'merchant-id',
        city: 'City',
        currency: 'INR',
        token: 'token',
        verified: true,
        vehicle_variant: 'SEDAN',
      };

      const driverWithoutVariant: DriverInfo = {
        person_id: 'driver-id-2',
        first_name: 'Driver2',
        merchant: 'merchant',
        merchant_id: 'merchant-id',
        city: 'City',
        currency: 'INR',
        token: 'token2',
        verified: false,
      };

      expect(driverWithVariant.vehicle_variant).toBe('SEDAN');
      expect(driverWithoutVariant.vehicle_variant).toBeUndefined();
    });

    it('should have correct MerchantInfo structure with optional fields', () => {
      const fullMerchant: MerchantInfo = {
        id: 'm1',
        short_id: 'short',
        name: 'Merchant',
        city_id: 'c1',
        city: 'City',
        country: 'Country',
        online_payment: true,
        currency: 'INR',
      };

      const minimalMerchant: MerchantInfo = {
        id: 'm2',
        short_id: 'short2',
        name: 'Merchant2',
        city_id: 'c2',
        city: 'City2',
        country: 'Country2',
      };

      expect(fullMerchant.online_payment).toBe(true);
      expect(fullMerchant.currency).toBe('INR');
      expect(minimalMerchant.online_payment).toBeUndefined();
      expect(minimalMerchant.currency).toBeUndefined();
    });

    it('should have correct VariantInfo structure', () => {
      const variant: VariantInfo = {
        id: 'v1',
        service_tier_type: 'AUTO_RICKSHAW',
        name: 'Auto',
        seating_capacity: 3,
        is_enabled: true,
        merchant: 'merchant',
        city: 'City',
        currency: 'INR',
      };

      expect(variant.seating_capacity).toBe(3);
      expect(variant.is_enabled).toBe(true);
    });
  });
});
