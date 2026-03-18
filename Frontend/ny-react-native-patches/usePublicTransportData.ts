/* eslint-disable functional/immutable-data */
/* eslint-disable functional/no-let */
import { useEffect, useState, useMemo, useCallback, useRef } from 'react';
import { useLazyPublicTransportDataGetQuery } from '@/api/integrations/rtk/PublicTransportDataGet';
import { createMMKV } from '@/utils/mmkvUtils';
import { useNetInfo } from '@react-native-community/netinfo';
import { useDispatch, useSelector } from 'react-redux';
import { selectUserProfile, setProfile } from '@/typescript/state/client/user';
import { RootState } from '@/typescript/state/store';
import { publicTransportData, transportStation, transportRoute } from '@/readOnly/api/types/PublicTransportData.gen';
import { FRFSServiceTierType_fRFSServiceTierType } from '@/readOnly/api/types/Enums.gen';
import { globalCache } from '@/src-v2/systems/cache/cache';
import { api } from '@/typescript/state/api';
import { logger } from '@/src-v2/systems/logger';
import { safeJsonParse } from '@/src-v2/components/SafeJsonParser';
import { isNull } from 'lodash';
import { profileRes } from '@/readOnly/api/types/ProfileRes.gen';
import { useAppSelector } from '@/typescript/state/hooks';
import { selectAppConfig } from '@/typescript/state/client/session';

// Storage key for the public transport data
export const PUBLIC_TRANSPORT_DATA_KEY = 'public_transport_data_v25';
export const PUBLIC_TRANSPORT_CONFIG_VERSION_KEY = 'public_transport_config_version';

// Storage instance for caching the data locally
const storage = createMMKV();

// ============================================================================
// SINGLETON PATTERN - Module-level state to coordinate fetches across all hook instances
// ============================================================================

/** Tracks if a fetch is currently in progress across all component instances */
let isFetchInProgress = false;

/** Stores the promise of the ongoing fetch so multiple callers can await the same request */
let ongoingFetchPromise: Promise<publicTransportData | null> | null = null;

/** Callbacks to notify all hook instances when fetch completes */
const fetchCompletionCallbacks: Set<(success: boolean) => void> = new Set();

/** Notify all registered callbacks that fetch completed */
const notifyFetchCompletion = (success: boolean): void => {
    fetchCompletionCallbacks.forEach(callback => {
        try {
            callback(success);
        } catch (e) {
            console.error('[PublicTransportData] Error in fetch completion callback:', e);
        }
    });
    fetchCompletionCallbacks.clear();
};

/** Register a callback to be called when the current fetch completes */
const registerFetchCompletionCallback = (callback: (success: boolean) => void): (() => void) => {
    fetchCompletionCallbacks.add(callback);
    return () => {
        fetchCompletionCallbacks.delete(callback);
    };
};

/** Reset the fetch state (useful for testing or error recovery) */
const resetFetchState = (): void => {
    isFetchInProgress = false;
    ongoingFetchPromise = null;
    fetchCompletionCallbacks.clear();
};

// ============================================================================

/**
 * Hook to manage public transport data fetching and caching
 * Uses a singleton pattern to ensure only one API call is made across all component instances
 * 
 * @param enabled - Whether the hook should fetch data (default: true)
 * @returns Object containing data availability status, loading state, error, and refresh function
 */
export const usePublicTransportData = (enabled: boolean = true) => {
    const [isDataAvailable, setIsDataAvailable] = useState(false);
    const [isLoading, setIsLoading] = useState(false);
    const [error, setError] = useState<Error | null>(null);
    const [isDataLoaded, setIsDataLoaded] = useState(false);
    const hasAttemptedFetchRef = useRef(false);
    const dispatch = useDispatch();
    const userProfile = useSelector(selectUserProfile);
    const netInfo = useNetInfo();
    const appConfig = useAppSelector(selectAppConfig);
    const [triggerPublicTransportDataGet] = useLazyPublicTransportDataGetQuery();

    // Check if we should fetch new data based on version mismatch
    const shouldFetchData = useCallback((cachedData: publicTransportData | null): boolean => {
        if (!cachedData) return true;
        
        const cachedVersion = globalCache.get<string>(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY);
        const currentVersion = cachedData.configVersion;
        
        // If versions don't match, we need to fetch new data
        if (cachedVersion && currentVersion && cachedVersion !== currentVersion) {
            console.log('[PublicTransportData] Version mismatch detected, fetching new data');
            return true;
        }
        
        return false;
    }, []);

    // Get cached data from storage
    const getCachedData = useCallback((): publicTransportData | null => {
        try {
            const cachedDataString = storage.getString(PUBLIC_TRANSPORT_DATA_KEY);
            if (cachedDataString) {
                const parsedData = safeJsonParse<publicTransportData>(cachedDataString);
                if (parsedData) {
                    // Also update global cache
                    globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, parsedData);
                    if (parsedData.configVersion) {
                        globalCache.set(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, parsedData.configVersion);
                    }
                    return parsedData;
                }
            }
        } catch (e) {
            console.error('[PublicTransportData] Error reading cached data:', e);
        }
        return null;
    }, []);

    // Internal function that performs the actual fetch
    const performFetch = useCallback(async (): Promise<publicTransportData | null> => {
        // Don't fetch if offline
        if (!netInfo.isConnected) {
            console.log('[PublicTransportData] Device is offline, skipping fetch');
            // Use cached data if available
            const cachedData = getCachedData();
            if (cachedData) {
                return cachedData;
            }
            return null;
        }

        // Check if we have valid cached data
        const cachedData = getCachedData();
        
        if (cachedData) {
            // If we have cached data and no version mismatch, use it
            if (!shouldFetchData(cachedData)) {
                console.log('[PublicTransportData] Using cached data, no fetch needed');
                return cachedData;
            }
        }

        try {
            console.log('[PublicTransportData] Fetching fresh data from API');
            const response = await triggerPublicTransportDataGet({});
            
            if (response.data) {
                // Store in both MMKV and global cache
                storage.set(PUBLIC_TRANSPORT_DATA_KEY, JSON.stringify(response.data));
                globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, response.data);
                
                // Update version in global cache
                if (response.data.configVersion) {
                    storage.set(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, response.data.configVersion);
                    globalCache.set(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, response.data.configVersion);
                }
                
                console.log('[PublicTransportData] Data fetched and cached successfully');
                return response.data;
            } else if (response.error) {
                throw new Error(`API Error: ${JSON.stringify(response.error)}`);
            }
        } catch (err) {
            console.error('[PublicTransportData] Error fetching data:', err);
            // Fall back to cached data if available
            const cachedData = getCachedData();
            if (cachedData) {
                return cachedData;
            }
            throw err;
        }
        return null;
    }, [netInfo.isConnected, triggerPublicTransportDataGet, getCachedData, shouldFetchData]);

    // Public fetch function that coordinates with other hook instances
    const fetchData = useCallback(async (): Promise<void> => {
        // If a fetch is already in progress, wait for it to complete
        if (isFetchInProgress && ongoingFetchPromise) {
            console.log('[PublicTransportData] Fetch already in progress, waiting for completion');
            setIsLoading(true);
            try {
                await ongoingFetchPromise;
                setIsDataAvailable(true);
                setIsDataLoaded(true);
            } catch (err) {
                setError(err instanceof Error ? err : new Error(String(err)));
            } finally {
                setIsLoading(false);
            }
            return;
        }

        // Start a new fetch
        isFetchInProgress = true;
        setIsLoading(true);
        setError(null);

        ongoingFetchPromise = performFetch();

        try {
            const data = await ongoingFetchPromise;
            if (data) {
                setIsDataAvailable(true);
                setIsDataLoaded(true);
            } else {
                setIsDataAvailable(false);
            }
            notifyFetchCompletion(true);
        } catch (err) {
            console.error('[PublicTransportData] Error in fetchData:', err);
            setError(err instanceof Error ? err : new Error(String(err)));
            setIsDataAvailable(false);
            notifyFetchCompletion(false);
        } finally {
            setIsLoading(false);
            isFetchInProgress = false;
            ongoingFetchPromise = null;
        }
    }, [performFetch]);

    // Initial data load - only fetch if enabled and we haven't attempted yet
    useEffect(() => {
        if (!enabled) {
            return;
        }

        // Only fetch if we haven't loaded data and haven't attempted a fetch
        if (!isDataLoaded && !hasAttemptedFetchRef.current) {
            hasAttemptedFetchRef.current = true;
            fetchData();
        }
    }, [enabled, fetchData]);

    // Listen for network reconnection to refresh data
    useEffect(() => {
        if (netInfo.isConnected && isDataLoaded && enabled) {
            // Optionally refresh data when coming back online
            // This is debounced to prevent multiple rapid calls
            const timeoutId = setTimeout(() => {
                fetchData();
            }, 1000);
            return () => clearTimeout(timeoutId);
        }
    }, [netInfo.isConnected, isDataLoaded, enabled, fetchData]);

    return {
        isDataAvailable,
        isDataLoaded,
        error,
        refreshData: fetchData
    };
};

/**
 * Helper function to get cached public transport data synchronously
 * @returns The cached public transport data or null if not available
 */
export const getPublicTransportData = (): publicTransportData | null => {
    try {
        // First check global cache
        const globalData = globalCache.get<publicTransportData>(PUBLIC_TRANSPORT_DATA_KEY);
        if (globalData) {
            return globalData;
        }

        // Fall back to storage
        const cachedDataString = storage.getString(PUBLIC_TRANSPORT_DATA_KEY);
        if (cachedDataString) {
            const parsedData = safeJsonParse<publicTransportData>(cachedDataString);
            if (parsedData) {
                // Update global cache for future calls
                globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, parsedData);
                return parsedData;
            }
        }
    } catch (e) {
        console.error('[PublicTransportData] Error getting cached data:', e);
    }
    return null;
};

/**
 * Helper function to get stations from cached data
 * @returns Array of transport stations or empty array if not available
 */
export const getTransportStations = (): transportStation[] => {
    const data = getPublicTransportData();
    return data?.stations ?? [];
};

/**
 * Helper function to get routes from cached data
 * @returns Array of transport routes or empty array if not available
 */
export const getTransportRoutes = (): transportRoute[] => {
    const data = getPublicTransportData();
    return data?.routes ?? [];
};

/**
 * Helper function to get service tiers from cached data
 * @returns Array of service tiers or empty array if not available
 */
export const getServiceTiers = (): FRFSServiceTierType_fRFSServiceTierType[] => {
    const data = getPublicTransportData();
    return data?.serviceTiers ?? [];
};

/**
 * Helper function to check if public transport data is available
 * @returns boolean indicating if data is available
 */
export const hasPublicTransportData = (): boolean => {
    return getPublicTransportData() !== null;
};

/**
 * Helper function to clear cached public transport data
 */
export const clearPublicTransportData = (): void => {
    try {
        storage.delete(PUBLIC_TRANSPORT_DATA_KEY);
        storage.delete(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY);
        globalCache.delete(PUBLIC_TRANSPORT_DATA_KEY);
        globalCache.delete(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY);
        console.log('[PublicTransportData] Cached data cleared');
    } catch (e) {
        console.error('[PublicTransportData] Error clearing cached data:', e);
    }
};

// Export reset function for testing purposes
export { resetFetchState };
