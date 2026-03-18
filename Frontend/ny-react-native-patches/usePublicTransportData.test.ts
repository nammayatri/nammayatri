/**
 * Tests for usePublicTransportData hook singleton pattern fix
 * 
 * These tests verify that:
 * 1. Only one API call is made when multiple components use the hook simultaneously
 * 2. Cache is properly read on subsequent component mounts
 * 3. Version mismatch detection still works correctly
 * 4. Manual refresh still works
 * 5. Premium bus refetch logic still works
 */

import { renderHook, waitFor, act } from '@testing-library/react-native';
import { usePublicTransportData, PUBLIC_TRANSPORT_DATA_KEY, PUBLIC_TRANSPORT_CONFIG_VERSION_KEY } from '../consumer/src-v2/multimodal/hooks/usePublicTransportData';
import { globalCache } from '../consumer/src-v2/systems/cache/cache';
import { useLazyPublicTransportDataGetQuery } from '../consumer/src/api/integrations/rtk/PublicTransportDataGet';
import { useSelector, useDispatch } from 'react-redux';
import { useNetInfo } from '@react-native-community/netinfo';
import { createMMKV } from '../consumer/src/utils/mmkvUtils';

// Mock dependencies
jest.mock('../consumer/src/api/integrations/rtk/PublicTransportDataGet');
jest.mock('react-redux');
jest.mock('@react-native-community/netinfo');
jest.mock('../consumer/src/utils/mmkvUtils');
jest.mock('../consumer/src/src-v2/systems/logger');

// Mock MMKV storage
const mockStorage = {
    getString: jest.fn(),
    set: jest.fn(),
    remove: jest.fn(),
};

// Mock RTK Query trigger
const mockTrigger = jest.fn();
let mockQueryState: {
    data: any;
    error: any;
    isLoading: boolean;
    isSuccess: boolean;
} = {
    data: null,
    error: null,
    isLoading: false,
    isSuccess: false,
};

// Track API call count
let apiCallCount = 0;

// Mock implementations
const setupMocks = () => {
    // Reset API call count
    apiCallCount = 0;

    // Reset global cache
    globalCache.delete(PUBLIC_TRANSPORT_DATA_KEY);

    // Mock MMKV
    (createMMKV as jest.Mock).mockReturnValue(mockStorage);

    // Mock RTK Query hook
    mockTrigger.mockImplementation(() => {
        apiCallCount++;
        return Promise.resolve({ data: mockQueryState.data });
    });

    (useLazyPublicTransportDataGetQuery as jest.Mock).mockReturnValue([
        mockTrigger,
        mockQueryState,
    ]);

    // Mock Redux
    (useDispatch as jest.Mock).mockReturnValue(jest.fn());
    (useSelector as jest.Mock).mockImplementation((selector: any) => {
        // Return default user profile
        return {
            id: 'test-user',
            publicTransportVersion: 'v1.0.0',
        };
    });

    // Mock NetInfo - default to connected
    (useNetInfo as jest.Mock).mockReturnValue({ isConnected: true });
};

describe('usePublicTransportData - Singleton Pattern Fix', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        setupMocks();
        mockStorage.getString.mockReturnValue(null); // No cached data by default
    });

    afterEach(() => {
        // Clean up global cache after each test
        globalCache.delete(PUBLIC_TRANSPORT_DATA_KEY);
    });

    describe('Single API Call Per Session', () => {
        it('should make only one API call when multiple components mount simultaneously', async () => {
            // Setup: No cached data
            mockStorage.getString.mockReturnValue(null);

            // Mount multiple components simultaneously
            const { result: result1 } = renderHook(() => usePublicTransportData(true));
            const { result: result2 } = renderHook(() => usePublicTransportData(true));
            const { result: result3 } = renderHook(() => usePublicTransportData(true));

            // Wait for effects to run
            await waitFor(() => {
                expect(result1.current.cacheInitialized).toBe(true);
                expect(result2.current.cacheInitialized).toBe(true);
                expect(result3.current.cacheInitialized).toBe(true);
            });

            // Verify only one API call was made
            expect(apiCallCount).toBe(1);
        });

        it('should make only one API call when components mount sequentially', async () => {
            // Setup: No cached data
            mockStorage.getString.mockReturnValue(null);

            // Mount first component
            const { result: result1, unmount: unmount1 } = renderHook(() => usePublicTransportData(true));
            
            await waitFor(() => {
                expect(result1.current.cacheInitialized).toBe(true);
            });

            // Mount second component
            const { result: result2 } = renderHook(() => usePublicTransportData(true));
            
            await waitFor(() => {
                expect(result2.current.cacheInitialized).toBe(true);
            });

            // Mount third component
            const { result: result3 } = renderHook(() => usePublicTransportData(true));
            
            await waitFor(() => {
                expect(result3.current.cacheInitialized).toBe(true);
            });

            // Verify only one API call was made
            expect(apiCallCount).toBe(1);
        });

        it('should not make API call when hook is disabled', async () => {
            // Setup: No cached data
            mockStorage.getString.mockReturnValue(null);

            // Mount component with enabled=false
            const { result } = renderHook(() => usePublicTransportData(false));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Verify no API call was made
            expect(apiCallCount).toBe(0);
        });

        it('should not make API call when offline', async () => {
            // Setup: No cached data, offline
            mockStorage.getString.mockReturnValue(null);
            (useNetInfo as jest.Mock).mockReturnValue({ isConnected: false });

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Verify no API call was made
            expect(apiCallCount).toBe(0);
        });
    });

    describe('Cache Hit on Subsequent Mounts', () => {
        it('should use cached data on subsequent component mounts without API call', async () => {
            // Setup: Simulate cached data in storage
            const cachedData = {
                routes: [],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            // Pre-populate global cache
            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            // Mount multiple components
            const { result: result1 } = renderHook(() => usePublicTransportData(true));
            const { result: result2 } = renderHook(() => usePublicTransportData(true));
            const { result: result3 } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result1.current.isDataAvailable).toBe(true);
                expect(result2.current.isDataAvailable).toBe(true);
                expect(result3.current.isDataAvailable).toBe(true);
            });

            // Verify no API calls were made (data from cache)
            expect(apiCallCount).toBe(0);
            expect(result1.current.isDataLoaded).toBe(true);
            expect(result2.current.isDataLoaded).toBe(true);
            expect(result3.current.isDataLoaded).toBe(true);
        });

        it('should correctly report data availability from global cache', async () => {
            // Setup: Pre-populate global cache
            const cachedData = {
                routes: [{ code: 'R1', longName: 'Route 1' }],
                stations: [{ code: 'S1', name: 'Station 1' }],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.isDataAvailable).toBe(true);
            });

            expect(result.current.cachedVersion).toBe('v1.0.0');
        });
    });

    describe('Version Mismatch Detection', () => {
        it('should trigger refetch when cached version differs from profile version', async () => {
            // Setup: Cached version differs from profile version
            const cachedData = {
                routes: [],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            // Profile has newer version
            (useSelector as jest.Mock).mockImplementation(() => ({
                id: 'test-user',
                publicTransportVersion: 'v2.0.0', // Different from cached
            }));

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Wait for the effect to trigger
            await waitFor(() => {
                expect(apiCallCount).toBe(1);
            });

            expect(result.current.hasVersionMismatch).toBe(true);
        });

        it('should not trigger refetch when versions match', async () => {
            // Setup: Cached version matches profile version
            const cachedData = {
                routes: [],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            // Profile has same version
            (useSelector as jest.Mock).mockImplementation(() => ({
                id: 'test-user',
                publicTransportVersion: 'v1.0.0', // Same as cached
            }));

            // Pre-populate global cache
            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Verify no API call was made
            expect(apiCallCount).toBe(0);
            expect(result.current.hasVersionMismatch).toBe(false);
        });

        it('should handle version mismatch across multiple components', async () => {
            // Setup: Version mismatch scenario
            const cachedData = {
                routes: [],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            (useSelector as jest.Mock).mockImplementation(() => ({
                id: 'test-user',
                publicTransportVersion: 'v2.0.0',
            }));

            // Mount multiple components
            const { result: result1 } = renderHook(() => usePublicTransportData(true));
            const { result: result2 } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result1.current.cacheInitialized).toBe(true);
                expect(result2.current.cacheInitialized).toBe(true);
            });

            // Wait for API call
            await waitFor(() => {
                expect(apiCallCount).toBe(1);
            });

            // Only one API call should be made despite version mismatch
            expect(apiCallCount).toBe(1);
        });
    });

    describe('Manual Refresh', () => {
        it('should trigger new API call on manual refresh', async () => {
            // Setup: Initial cached data
            const cachedData = {
                routes: [],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Reset API call count after initial mount
            apiCallCount = 0;

            // Trigger manual refresh
            await act(async () => {
                await result.current.refreshData();
            });

            // Verify API call was made
            expect(apiCallCount).toBe(1);
        });

        it('should clear cache on manual refresh', async () => {
            // Setup: Initial cached data
            const cachedData = {
                routes: [],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.isDataAvailable).toBe(true);
            });

            // Trigger manual refresh
            await act(async () => {
                await result.current.refreshData();
            });

            // Verify storage was cleared
            expect(mockStorage.remove).toHaveBeenCalledWith(PUBLIC_TRANSPORT_DATA_KEY);
            expect(mockStorage.remove).toHaveBeenCalledWith(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY);
        });
    });

    describe('Premium Bus Refetch Logic', () => {
        it('should trigger refetch when premium bus data is missing', async () => {
            // Setup: Data without premium bus
            const cachedData = {
                routes: [
                    { code: 'R1', longName: 'Route 1', serviceType: 'REGULAR' },
                ],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            // Enable premium bus refetch
            (useSelector as jest.Mock).mockImplementation((selector: any, context: any) => {
                if (context?.appConfig) {
                    return {
                        flowConfig: {
                            enablePremiumBusDataRefetch: true,
                        },
                    };
                }
                return {
                    id: 'test-user',
                    publicTransportVersion: 'v1.0.0',
                };
            });

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Should trigger refetch for premium bus
            await waitFor(() => {
                expect(apiCallCount).toBeGreaterThanOrEqual(1);
            });
        });

        it('should not trigger premium bus refetch when premium bus data exists', async () => {
            // Setup: Data with premium bus
            const cachedData = {
                routes: [
                    { code: 'R1', longName: 'Route 1', serviceType: 'PREMIUM' },
                ],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Should not trigger refetch
            expect(apiCallCount).toBe(0);
        });
    });

    describe('Loading States', () => {
        it('should report loading state during initial fetch', async () => {
            // Setup: No cached data
            mockStorage.getString.mockReturnValue(null);

            // Set loading state
            mockQueryState.isLoading = true;

            const { result } = renderHook(() => usePublicTransportData(true));

            // Should be loading initially
            expect(result.current.isLoading).toBe(true);

            // Update to not loading
            mockQueryState.isLoading = false;
            mockQueryState.isSuccess = true;

            await waitFor(() => {
                expect(result.current.isLoading).toBe(false);
            });
        });

        it('should report correct data loaded state', async () => {
            // Setup: Cached data
            const cachedData = {
                routes: [],
                stations: [],
                routeStopMappings: [],
                publicTransportConfigVersion: 'v1.0.0',
            };
            
            mockStorage.getString.mockImplementation((key: string) => {
                if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
                if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(cachedData);
                return null;
            });

            globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

            (useSelector as jest.Mock).mockImplementation(() => ({
                id: 'test-user',
                publicTransportVersion: 'v1.0.0',
            }));

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.isDataLoaded).toBe(true);
            });
        });
    });

    describe('Error Handling', () => {
        it('should handle API errors gracefully', async () => {
            // Setup: No cached data, API will fail
            mockStorage.getString.mockReturnValue(null);
            mockQueryState.error = new Error('Network error');

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            expect(result.current.error).toBeDefined();
        });

        it('should handle storage read errors gracefully', async () => {
            // Setup: Storage throws error
            mockStorage.getString.mockImplementation(() => {
                throw new Error('Storage error');
            });

            const { result } = renderHook(() => usePublicTransportData(true));

            await waitFor(() => {
                expect(result.current.cacheInitialized).toBe(true);
            });

            // Should still be functional despite storage error
            expect(result.current.cachedVersion).toBeNull();
        });
    });
});

describe('usePublicTransportData - Integration Scenarios', () => {
    beforeEach(() => {
        jest.clearAllMocks();
        setupMocks();
        globalCache.delete(PUBLIC_TRANSPORT_DATA_KEY);
    });

    it('should handle the complete lifecycle: initial fetch -> cache -> version mismatch -> refetch', async () => {
        // Phase 1: Initial mount with no cache
        mockStorage.getString.mockReturnValue(null);

        const { result, rerender } = renderHook(() => usePublicTransportData(true));

        await waitFor(() => {
            expect(result.current.cacheInitialized).toBe(true);
        });

        // Should trigger initial fetch
        expect(apiCallCount).toBe(1);

        // Phase 2: Simulate successful API response
        const apiResponse = {
            routes: [{ code: 'R1', longName: 'Route 1' }],
            stations: [{ code: 'S1', name: 'Station 1' }],
            routeStopMappings: [],
            publicTransportConfigVersion: 'v1.0.0',
        };

        mockQueryState.data = apiResponse;
        mockQueryState.isSuccess = true;

        // Update storage mock to return cached data
        mockStorage.getString.mockImplementation((key: string) => {
            if (key === PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) return 'v1.0.0';
            if (key === PUBLIC_TRANSPORT_DATA_KEY) return JSON.stringify(apiResponse);
            return null;
        });

        // Pre-populate global cache
        globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, apiResponse, 1000 * 60 * 60 * 24);

        // Phase 3: Mount new component - should use cache
        const { result: result2 } = renderHook(() => usePublicTransportData(true));

        await waitFor(() => {
            expect(result2.current.isDataAvailable).toBe(true);
        });

        // No new API call
        expect(apiCallCount).toBe(1);

        // Phase 4: Simulate version mismatch
        (useSelector as jest.Mock).mockImplementation(() => ({
            id: 'test-user',
            publicTransportVersion: 'v2.0.0', // New version
        }));

        // Clear cache to simulate version mismatch scenario
        globalCache.delete(PUBLIC_TRANSPORT_DATA_KEY);

        const { result: result3 } = renderHook(() => usePublicTransportData(true));

        await waitFor(() => {
            expect(result3.current.cacheInitialized).toBe(true);
        });

        // Should trigger refetch due to version mismatch
        await waitFor(() => {
            expect(apiCallCount).toBe(2);
        });
    });

    it('should handle rapid mount/unmount cycles without duplicate API calls', async () => {
        mockStorage.getString.mockReturnValue(null);

        // Rapid mount/unmount cycles
        for (let i = 0; i < 5; i++) {
            const { unmount } = renderHook(() => usePublicTransportData(true));
            unmount();
        }

        // Final mount
        const { result } = renderHook(() => usePublicTransportData(true));

        await waitFor(() => {
            expect(result.current.cacheInitialized).toBe(true);
        });

        // Should only make one API call despite multiple mounts
        expect(apiCallCount).toBe(1);
    });
});
