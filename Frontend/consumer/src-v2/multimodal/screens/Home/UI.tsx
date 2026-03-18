/**
 * Multimodal Home UI Component
 * 
 * This component displays the multimodal home screen with public transport data.
 * Uses usePublicTransportData hook with proper singleton behavior.
 */

import React, { useEffect, useCallback } from 'react';
import {
  View,
  Text,
  StyleSheet,
  TouchableOpacity,
  ScrollView,
  ActivityIndicator,
} from 'react-native';
import { usePublicTransportData } from '../../hooks/usePublicTransportData';

// ============================================================================
// Types
// ============================================================================

interface HomeUIProps {
  city?: string;
  onRouteSelect?: (route: Route) => void;
  onStationSelect?: (station: Station) => void;
}

interface Route {
  id: string;
  code: string;
  name: string;
  type: string;
  vehicleServiceType?: string;
}

interface Station {
  id: string;
  code: string;
  name: string;
  lat: number;
  lon: number;
}

// ============================================================================
// Component
// ============================================================================

export const MultimodalHomeUI: React.FC<HomeUIProps> = ({
  city = 'bangalore',
  onRouteSelect,
  onStationSelect,
}) => {
  // ============================================================================
  // Hooks
  // ============================================================================

  // Use the public transport data hook
  // This hook has singleton behavior - multiple instances share the same state
  const {
    data: publicTransportData,
    isLoading,
    error,
    refreshData,
    isPremiumMissing,
  } = usePublicTransportData({
    enablePremiumBusDataRefetch: true,
    city,
    profileVersion: 'v1.0.0',
  });

  // ============================================================================
  // Handlers
  // ============================================================================

  const handleRouteSelect = useCallback((route: Route) => {
    console.log('[MultimodalHomeUI] Route selected:', route.name);
    onRouteSelect?.(route);
  }, [onRouteSelect]);

  const handleStationSelect = useCallback((station: Station) => {
    console.log('[MultimodalHomeUI] Station selected:', station.name);
    onStationSelect?.(station);
  }, [onStationSelect]);

  const handleRefresh = useCallback(() => {
    console.log('[MultimodalHomeUI] Manual refresh requested');
    // Only call refreshData on explicit user action
    refreshData();
  }, [refreshData]);

  // ============================================================================
  // Effects
  // ============================================================================

  // Log data changes for debugging
  useEffect(() => {
    if (publicTransportData) {
      console.log('[MultimodalHomeUI] Data received:', {
        version: publicTransportData.version,
        routes: publicTransportData.routes.length,
        stations: publicTransportData.stations.length,
        hasPremiumBus: publicTransportData.hasPremiumBus,
      });
    }
  }, [publicTransportData]);

  // ============================================================================
  // IMPORTANT: No redundant refreshData() call here
  // ============================================================================
  
  /*
   * The hook handles data fetching automatically:
   * - On mount: Fetches if no cached data
   * - On city change: Fetches if city changes
   * - On version mismatch: Fetches to get updated data
   * 
   * Do NOT add: useEffect(() => { refreshData(); }, [city, refreshData]);
   * This would cause duplicate API calls!
   */

  // ============================================================================
  // Render Helpers
  // ============================================================================

  const renderHeader = () => (
    <View style={styles.header}>
      <Text style={styles.headerTitle}>Multimodal Transport</Text>
      <TouchableOpacity style={styles.refreshButton} onPress={handleRefresh}>
        <Text style={styles.refreshButtonText}>↻ Refresh</Text>
      </TouchableOpacity>
    </View>
  );

  const renderLoading = () => (
    <View style={styles.loadingContainer}>
      <ActivityIndicator size="large" color="#3498db" />
      <Text style={styles.loadingText}>Loading transport data...</Text>
    </View>
  );

  const renderError = () => (
    <View style={styles.errorContainer}>
      <Text style={styles.errorText}>
        {error?.message || 'Failed to load transport data'}
      </Text>
      <TouchableOpacity style={styles.retryButton} onPress={handleRefresh}>
        <Text style={styles.retryButtonText}>Retry</Text>
      </TouchableOpacity>
    </View>
  );

  const renderRoutes = () => (
    <View style={styles.section}>
      <Text style={styles.sectionTitle}>
        Routes ({publicTransportData?.routes.length || 0})
      </Text>
      <ScrollView horizontal showsHorizontalScrollIndicator={false}>
        {publicTransportData?.routes.map((route) => (
          <TouchableOpacity
            key={route.id}
            style={styles.routeCard}
            onPress={() => handleRouteSelect(route)}
          >
            <Text style={styles.routeName}>{route.name}</Text>
            <Text style={styles.routeType}>{route.type}</Text>
            {route.vehicleServiceType && (
              <Text style={styles.routeServiceType}>
                {route.vehicleServiceType}
              </Text>
            )}
          </TouchableOpacity>
        ))}
      </ScrollView>
    </View>
  );

  const renderStations = () => (
    <View style={styles.section}>
      <Text style={styles.sectionTitle}>
        Stations ({publicTransportData?.stations.length || 0})
      </Text>
      <ScrollView style={styles.stationsList}>
        {publicTransportData?.stations.map((station) => (
          <TouchableOpacity
            key={station.id}
            style={styles.stationItem}
            onPress={() => handleStationSelect(station)}
          >
            <Text style={styles.stationName}>{station.name}</Text>
            <Text style={styles.stationCode}>{station.code}</Text>
          </TouchableOpacity>
        ))}
      </ScrollView>
    </View>
  );

  const renderPremiumBusWarning = () => {
    if (!isPremiumMissing) return null;
    
    return (
      <View style={styles.warningBanner}>
        <Text style={styles.warningText}>
          ⚠️ Premium bus routes are being updated. Some routes may not be available.
        </Text>
      </View>
    );
  };

  const renderContent = () => {
    if (isLoading && !publicTransportData) {
      return renderLoading();
    }

    if (error && !publicTransportData) {
      return renderError();
    }

    return (
      <ScrollView style={styles.content}>
        {renderPremiumBusWarning()}
        
        <View style={styles.infoCard}>
          <Text style={styles.infoLabel}>Data Version</Text>
          <Text style={styles.infoValue}>
            {publicTransportData?.version || 'N/A'}
          </Text>
        </View>

        {renderRoutes()}
        {renderStations()}
      </ScrollView>
    );
  };

  // ============================================================================
  // Render
  // ============================================================================

  return (
    <View style={styles.screen}>
      {renderHeader()}
      {renderContent()}
    </View>
  );
};

// ============================================================================
// Styles
// ============================================================================

const styles = StyleSheet.create({
  screen: {
    flex: 1,
    backgroundColor: '#f5f5f5',
  },
  header: {
    backgroundColor: '#3498db',
    padding: 16,
    paddingTop: 48,
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
  },
  headerTitle: {
    fontSize: 20,
    fontWeight: 'bold',
    color: '#fff',
  },
  refreshButton: {
    backgroundColor: 'rgba(255,255,255,0.2)',
    paddingHorizontal: 12,
    paddingVertical: 6,
    borderRadius: 16,
  },
  refreshButtonText: {
    color: '#fff',
    fontSize: 14,
    fontWeight: '600',
  },
  content: {
    flex: 1,
    padding: 16,
  },
  loadingContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  loadingText: {
    marginTop: 16,
    fontSize: 16,
    color: '#666',
  },
  errorContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    padding: 32,
  },
  errorText: {
    fontSize: 16,
    color: '#e74c3c',
    marginBottom: 16,
    textAlign: 'center',
  },
  retryButton: {
    backgroundColor: '#3498db',
    paddingHorizontal: 24,
    paddingVertical: 12,
    borderRadius: 8,
  },
  retryButtonText: {
    color: '#fff',
    fontSize: 16,
    fontWeight: '600',
  },
  warningBanner: {
    backgroundColor: '#fff3cd',
    padding: 12,
    borderRadius: 8,
    marginBottom: 16,
    borderLeftWidth: 4,
    borderLeftColor: '#ffc107',
  },
  warningText: {
    fontSize: 14,
    color: '#856404',
  },
  infoCard: {
    backgroundColor: '#fff',
    padding: 16,
    borderRadius: 8,
    marginBottom: 16,
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
  },
  infoLabel: {
    fontSize: 14,
    color: '#666',
  },
  infoValue: {
    fontSize: 14,
    fontWeight: '600',
    color: '#333',
  },
  section: {
    marginBottom: 24,
  },
  sectionTitle: {
    fontSize: 18,
    fontWeight: '600',
    color: '#333',
    marginBottom: 12,
  },
  routeCard: {
    backgroundColor: '#fff',
    padding: 16,
    marginRight: 12,
    borderRadius: 12,
    width: 160,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.1,
    shadowRadius: 4,
    elevation: 3,
  },
  routeName: {
    fontSize: 16,
    fontWeight: '600',
    color: '#333',
    marginBottom: 4,
  },
  routeType: {
    fontSize: 14,
    color: '#666',
  },
  routeServiceType: {
    fontSize: 12,
    color: '#3498db',
    marginTop: 4,
  },
  stationsList: {
    maxHeight: 300,
  },
  stationItem: {
    backgroundColor: '#fff',
    padding: 16,
    marginBottom: 8,
    borderRadius: 8,
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
  },
  stationName: {
    fontSize: 16,
    fontWeight: '600',
    color: '#333',
  },
  stationCode: {
    fontSize: 14,
    color: '#666',
  },
});

// ============================================================================
// Export
// ============================================================================

export default MultimodalHomeUI;
