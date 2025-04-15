import React, { createContext, useState, useEffect, useCallback } from 'react';
import { v4 as uuidv4 } from 'uuid';

// Default ticket place data
const createDefaultTicketPlace = (name, description, placeType) => {
  const id = uuidv4();

  // Create business hours with fixed IDs for consistency
  const businessHour1Id = uuidv4();
  const businessHour2Id = uuidv4();
  const businessHour3Id = uuidv4();

  // Create service categories with fixed IDs for consistency
  const category1Id = uuidv4();
  const category2Id = uuidv4();
  const category3Id = uuidv4();

  // Create people categories with fixed IDs for consistency
  const people1Id = uuidv4();
  const people2Id = uuidv4();
  const people3Id = uuidv4();

  // Create service IDs for consistency
  const service1Id = uuidv4();
  const service2Id = uuidv4();

  // Create vendor IDs for consistency
  const vendor1Id = uuidv4();
  // const vendor2Id = uuidv4();

  // Create business hours
  const businessHour1 = {
    id: businessHour1Id,
    name: "Morning Hours",
    btype: {
      tag: "Slot",
      contents: "09:00:00"
    },
    categoryId: [category1Id],
    bookingClosingTime: "08:00:00"
  };

  const businessHour2 = {
    id: businessHour2Id,
    name: "Afternoon Hours",
    btype: {
      tag: "Slot",
      contents: "14:00:00"
    },
    categoryId: [category2Id],
    bookingClosingTime: "13:00:00"
  };

  const businessHour3 = {
    id: businessHour3Id,
    name: "Full Day",
    btype: {
      tag: "Duration",
      contents: ["09:00:00", "17:00:00"]
    },
    categoryId: [category3Id],
    bookingClosingTime: "08:30:00"
  };

  // Create people categories
  const people1 = {
    id: people1Id,
    name: "Adult",
    description: "Adult (18+ years)",
    pricingType: "AllDays",
    priceAmount: 50.0,
    priceCurrency: "INR",
    timeBounds: {
      tag: "Unbounded"
    },
    vendorSplitDetails: [
      {
        vendorId: vendor1Id,
        splitAmount: 50.0,
        splitType: "FIXED"
      }
    ]
  };

  const people2 = {
    id: people2Id,
    name: "Child",
    description: "Child (5-17 years)",
    pricingType: "AllDays",
    priceAmount: 25.0,
    priceCurrency: "INR",
    timeBounds: {
      tag: "Unbounded"
    },
    vendorSplitDetails: [
      {
        vendorId: vendor1Id,
        splitAmount: 25.0,
        splitType: "FIXED"
      }
    ]
  };

  const people3 = {
    id: people3Id,
    name: "Senior",
    description: "Senior (65+ years)",
    pricingType: "AllDays",
    priceAmount: 35.0,
    priceCurrency: "INR",
    timeBounds: {
      tag: "Unbounded"
    },
    vendorSplitDetails: [
      {
        vendorId: vendor1Id,
        splitAmount: 35.0,
        splitType: "FIXED"
      }
    ]
  };

  // Create service categories
  const category1 = {
    id: category1Id,
    name: "Regular Entry",
    description: "Standard entry ticket",
    allowedSeats: 100,
    availableSeats: 100,
    peopleCategory: [people1Id, people2Id]
  };

  const category2 = {
    id: category2Id,
    name: "VIP Entry",
    description: "Premium entry with special access",
    allowedSeats: 50,
    availableSeats: 50,
    peopleCategory: [people1Id, people3Id]
  };

  const category3 = {
    id: category3Id,
    name: "Guided Tour",
    description: "Tour with a professional guide",
    allowedSeats: 20,
    availableSeats: 20,
    peopleCategory: [people1Id]
  };

  // Create special occasions
  const specialOccasion1Id = uuidv4();
  const specialOccasion2Id = uuidv4();

  const specialOccasion1 = {
    id: specialOccasion1Id,
    name: "Holiday Special",
    specialDayType: "Open",
    date: new Date(new Date().getFullYear(), 11, 25).toISOString().split('T')[0], // Christmas
    entityId: category1Id, // Applies to a specific category
    businessHours: [businessHour1Id, businessHour2Id],
    description: "Special holiday hours and pricing",
    placeId: id
  };

  const specialOccasion2 = {
    id: specialOccasion2Id,
    name: "Maintenance Day",
    specialDayType: "Closed",
    date: new Date(new Date().getFullYear(), new Date().getMonth() + 1, 15).toISOString().split('T')[0], // 15th of next month
    entityId: category2Id, // Applies to a specific category
    businessHours: [],
    description: "Closed for maintenance",
    placeId: id
  };

  // Create services
  const service1 = {
    id: service1Id,
    service: "General Admission",
    shortDesc: "General admission to the venue",
    operationalDays: ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
    operationalDate: null,
    maxVerification: 1,
    allowFutureBooking: true,
    allowCancellation: false,
    expiry: { tag: "VisitDate", contents: "12:00:00" },
    businessHours: [businessHour1Id, businessHour2Id]
  };

  const service2 = {
    id: service2Id,
    service: "Guided Tour",
    shortDesc: "Guided tour with an expert",
    operationalDays: ["Monday", "Wednesday", "Friday", "Saturday", "Sunday"],
    operationalDate: null,
    maxVerification: 1,
    allowFutureBooking: true,
    allowCancellation: true,
    expiry: { tag: "InstantExpiry", contents: 120 },
    businessHours: [businessHour3Id]
  };

  return {
    id,
    name,
    description,
    shortDesc: description.substring(0, 50) + (description.length > 50 ? '...' : ''),
    address: "123 Museum Street, Downtown",
    latitude: 12.9716,
    longitude: 77.5946,
    status: "Active",
    priority: 0,
    placeType,
    allowSameDayBooking: true,
    gallery: [],
    iconUrl: "",
    mapImageUrl: "",
    termsAndConditions: [],
    termsAndConditionsUrl: "",
    openTimings: "09:00:00",
    closeTimings: "17:00:00",
    services: [service1, service2],
    businessHours: [businessHour1, businessHour2, businessHour3],
    serviceCategories: [category1, category2, category3],
    servicePeopleCategories: [people1, people2, people3],
    specialOccasions: [specialOccasion1, specialOccasion2]
  };
};

// Create default ticket places
const cityMuseum = createDefaultTicketPlace(
  "City Museum",
  "A beautiful museum in the heart of the city featuring historical artifacts and modern art.",
  "Museum"
);

const adventurePark = createDefaultTicketPlace(
  "Adventure Park",
  "Exciting adventure park with thrilling rides and attractions for all ages.",
  "ThemePark"
);

const wildlifeSafari = createDefaultTicketPlace(
  "Wildlife Safari",
  "Experience the thrill of seeing exotic animals in their natural habitat.",
  "Zoo"
);

// Default ticket place IDs
const DEFAULT_TICKET_PLACE_IDS = [cityMuseum.id, adventurePark.id, wildlifeSafari.id];

// Default ticket place map
const DEFAULT_TICKET_PLACE_MAP = {
  [cityMuseum.id]: cityMuseum,
  [adventurePark.id]: adventurePark,
  [wildlifeSafari.id]: wildlifeSafari
};

// Default active ticket place ID
const DEFAULT_ACTIVE_TICKET_PLACE_ID = cityMuseum.id;

// Create the context
export const TicketContext = createContext();

// Create the provider component
export const TicketProvider = ({ children }) => {
  const [ticketPlaceIds, setTicketPlaceIds] = useState([]);
  const [ticketPlaceMap, setTicketPlaceMap] = useState({});
  const [activeTicketPlaceId, setActiveTicketPlaceId] = useState(null);
  const [jsonPreview, setJsonPreview] = useState('');
  const [apiConfig, setApiConfig] = useState(() => {
    return {
      baseUrl: localStorage.getItem('apiBaseUrl') || 'http://localhost:8017/bap/YATRI/Kochi',
      token: localStorage.getItem('apiToken') || '0466f4fb-6af8-49f5-8d0c-9196101afdc4'
    };
  });
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);
  const [statusFilter, setStatusFilter] = useState(() => localStorage.getItem('statusFilter') || 'Active');

  // Track API calls to prevent duplicates
  const apiCallInProgress = React.useRef(false);

  // Function to clear all ticket-related localStorage data
  const clearAllTicketData = useCallback(() => {
    try {
      // Get all localStorage keys
      const allKeys = Object.keys(localStorage);

      // Find all ticket-related keys
      const ticketKeys = allKeys.filter(key =>
        key === 'ticketPlaceIds' ||
        key === 'activeTicketPlaceId' ||
        key.startsWith('ticketPlace_')
      );

      // Remove all ticket-related keys
      ticketKeys.forEach(key => localStorage.removeItem(key));

      console.log('All ticket data cleared from localStorage');

      // Reset state to defaults
      setTicketPlaceIds(DEFAULT_TICKET_PLACE_IDS);
      setTicketPlaceMap(DEFAULT_TICKET_PLACE_MAP);
      setActiveTicketPlaceId(DEFAULT_ACTIVE_TICKET_PLACE_ID);
      setJsonPreview(JSON.stringify(DEFAULT_TICKET_PLACE_MAP[DEFAULT_ACTIVE_TICKET_PLACE_ID], null, 2));

      // Save defaults to localStorage
      localStorage.setItem('ticketPlaceIds', JSON.stringify(DEFAULT_TICKET_PLACE_IDS));
      localStorage.setItem('activeTicketPlaceId', DEFAULT_ACTIVE_TICKET_PLACE_ID);

      for (const id of DEFAULT_TICKET_PLACE_IDS) {
        localStorage.setItem(`ticketPlace_${id}`, JSON.stringify(DEFAULT_TICKET_PLACE_MAP[id]));
      }

      return true;
    } catch (error) {
      console.error('Error clearing ticket data from localStorage:', error);
      return false;
    }
  }, []);

  // NOTE: We've removed the automatic clearing of localStorage data on initial render
  // This allows users to add new ticket places that persist between sessions

  // Load data from localStorage on initial render
  useEffect(() => {
    try {
      // Get all localStorage keys
      const allKeys = Object.keys(localStorage);

      // Find all ticket place keys
      const ticketPlaceKeys = allKeys.filter(key => key.startsWith('ticketPlace_'));

      // Extract IDs from keys
      const extractedIds = ticketPlaceKeys.map(key => key.replace('ticketPlace_', ''));

      // Try to load ticket place IDs from localStorage or use extracted IDs
      const savedIds = localStorage.getItem('ticketPlaceIds');
      let ids = [];

      if (savedIds) {
        // Use saved IDs but ensure they all have corresponding data
        const parsedIds = JSON.parse(savedIds);
        ids = parsedIds.filter(id => {
          return localStorage.getItem(`ticketPlace_${id}`) !== null;
        });
      } else {
        // Use extracted IDs
        ids = extractedIds;
      }

      // If no valid IDs were found, use defaults
      if (ids.length === 0) {
        ids = DEFAULT_TICKET_PLACE_IDS;
      }

      // Update ticketPlaceIds
      setTicketPlaceIds(ids);

      // Save the updated IDs to localStorage
      localStorage.setItem('ticketPlaceIds', JSON.stringify(ids));

      // Try to load ticket place map
      const map = {};
      let hasData = false;

      for (const id of ids) {
        const savedPlace = localStorage.getItem(`ticketPlace_${id}`);
        if (savedPlace) {
          map[id] = JSON.parse(savedPlace);
          hasData = true;
        }
      }

      // If no data was found in localStorage, use defaults
      if (!hasData) {
        setTicketPlaceMap(DEFAULT_TICKET_PLACE_MAP);

        // Save defaults to localStorage
        for (const id of DEFAULT_TICKET_PLACE_IDS) {
          localStorage.setItem(`ticketPlace_${id}`, JSON.stringify(DEFAULT_TICKET_PLACE_MAP[id]));
        }
      } else {
        setTicketPlaceMap(map);
      }

      // Try to load active ticket place ID
      const savedActiveId = localStorage.getItem('activeTicketPlaceId');
      const activeId = savedActiveId || (ids.length > 0 ? ids[0] : null);
      setActiveTicketPlaceId(activeId);

      // Set initial JSON preview
      if (activeId && (map[activeId] || DEFAULT_TICKET_PLACE_MAP[activeId])) {
        const activePlace = map[activeId] || DEFAULT_TICKET_PLACE_MAP[activeId];
        setJsonPreview(JSON.stringify(activePlace, null, 2));
      }
    } catch (error) {
      console.error('Error loading data from localStorage:', error);

      // Use defaults if there's an error
      setTicketPlaceIds(DEFAULT_TICKET_PLACE_IDS);
      setTicketPlaceMap(DEFAULT_TICKET_PLACE_MAP);
      setActiveTicketPlaceId(DEFAULT_ACTIVE_TICKET_PLACE_ID);
      setJsonPreview(JSON.stringify(DEFAULT_TICKET_PLACE_MAP[DEFAULT_ACTIVE_TICKET_PLACE_ID], null, 2));
    }
  }, []);

  // Update API configuration
  const updateApiConfig = async (config) => {
    setApiConfig(config);
    setIsLoading(true);
    setError(null);
    
    // Clear existing data when config changes
    setTicketPlaceIds([]);
    setTicketPlaceMap({});
    setActiveTicketPlaceId(null);
    
    try {
      // Immediately fetch ticket places with the new config
      await fetchTicketPlaces(statusFilter);
    } catch (error) {
      console.error('Error fetching ticket places after config update:', error);
      setError(error.message);
    }
  };

  // Fetch all ticket places from API
  const fetchTicketPlaces = useCallback(async (status = statusFilter) => {
    if (apiCallInProgress.current) {
      console.log('API call already in progress, skipping duplicate call');
      return [];
    }

    apiCallInProgress.current = true;
    setIsLoading(true);
    setError(null);

    try {
      localStorage.setItem('statusFilter', status);
      setStatusFilter(status);

      if (!apiConfig || !apiConfig.baseUrl || !apiConfig.token) {
        throw new Error('API configuration is missing. Please check your settings.');
      }

      const response = await fetch(`${apiConfig.baseUrl}/ticketdashboard/ticketplaces?status=${status}`, {
        method: 'GET',
        headers: {
          'token': apiConfig.token,
          'Content-Type': 'application/json',
          'Accept': 'application/json'
        }
      });

      if (!response.ok) {
        const errorText = await response.text();
        throw new Error(`Failed to fetch ticket places: ${response.status} - ${errorText}`);
      }

      const data = await response.json();

      if (!data || !Array.isArray(data)) {
        throw new Error('Invalid response format from server');
      }

      const ids = data.map(place => place.id);
      const map = {};
      data.forEach(place => {
        map[place.id] = place;
      });

      setTicketPlaceIds(ids);
      setTicketPlaceMap(map);

      if (ids.length > 0 && (!activeTicketPlaceId || !ids.includes(activeTicketPlaceId))) {
        const newActiveId = ids[0];
        setActiveTicketPlaceId(newActiveId);
        localStorage.setItem('activeTicketPlaceId', newActiveId);
      }

      return ids;
    } catch (error) {
      console.error('Error fetching ticket places:', error);
      setError(error.message);
      throw error;
    } finally {
      setIsLoading(false);
      setTimeout(() => {
        apiCallInProgress.current = false;
      }, 500);
    }
  }, [apiConfig, activeTicketPlaceId]);

  // Fetch ticket place details by ID
  const fetchTicketPlaceDetails = useCallback(async (placeId) => {
    if (!placeId) {
      console.log('No placeId provided to fetchTicketPlaceDetails');
      return null;
    }

    console.log(`Starting fetchTicketPlaceDetails for ID: ${placeId}`);
    setIsLoading(true);
    setError(null);
    try {
      console.log(`Fetching ticket place details for ID: ${placeId}`);
      console.log(`API Config: ${apiConfig.baseUrl}, token: ${apiConfig.token ? 'present' : 'missing'}`);

      const response = await fetch(`${apiConfig.baseUrl}/ticketdashboard/ticketplace/${placeId}/info`, {
        method: 'GET',
        headers: {
          'token': apiConfig.token,
          'Content-Type': 'application/json',
          'Accept': 'application/json'
        }
      });

      console.log(`Response status: ${response.status}`);

      if (!response.ok) {
        throw new Error(`Failed to fetch ticket place details: ${response.status}`);
      }

      // Try to get the response text for debugging
      const responseText = await response.text();
      console.log('Response received, length:', responseText.length);

      // Parse the response text as JSON
      let data;
      try {
        data = JSON.parse(responseText);
        console.log('Successfully parsed JSON response');
      } catch (e) {
        console.error('Error parsing response as JSON:', e);
        throw new Error('Invalid JSON response from server');
      }

      if (data) {
        console.log('Successfully received ticket place data');

        // Update the ticket place map
        setTicketPlaceMap(prevMap => {
          const newMap = {
            ...prevMap,
            [placeId]: data
          };
          console.log(`Updated ticket place map, now has ${Object.keys(newMap).length} entries`);
          return newMap;
        });

        // Update JSON preview if this is the active place
        if (placeId === activeTicketPlaceId) {
          // Update JSON preview
          setJsonPreview(JSON.stringify(data, null, 2));
        }

        // Add a small delay to ensure state updates are processed
        await new Promise(resolve => setTimeout(resolve, 100));

        console.log('fetchTicketPlaceDetails completed successfully');
        return data;
      } else {
        throw new Error('Invalid response format');
      }
    } catch (error) {
      console.error(`Error fetching ticket place details for ${placeId}:`, error);
      setError(error.message);

      // Even if there's an error, try to get the place from the local map
      const localPlace = ticketPlaceMap[placeId];
      if (localPlace) {
        console.log('Returning locally cached place data');
        return localPlace;
      }

      return null;
    } finally {
      // Ensure loading state is cleared with a delay
      setTimeout(() => {
        setIsLoading(false);
        console.log('Loading state cleared');
      }, 500); // Small delay to ensure UI updates
    }
  }, [apiConfig, ticketPlaceMap]);

  // Get all ticket place IDs
  const getTicketPlaceIds = useCallback(() => {
    return ticketPlaceIds;
  }, [ticketPlaceIds]);

  // Get ticket place info by ID
  const getTicketPlaceInfo = useCallback((placeId) => {
    return ticketPlaceMap[placeId] || null;
  }, [ticketPlaceMap]);

  // Function to get the active place directly
  const getActiveTicketPlace = useCallback(() => {
    if (!activeTicketPlaceId || !ticketPlaceMap[activeTicketPlaceId]) {
      return null;
    }
    return ticketPlaceMap[activeTicketPlaceId];
  }, [activeTicketPlaceId, ticketPlaceMap]);

  // Set active ticket place
  const setActiveTicketPlace = useCallback((place) => {
    if (!place || !place.id) return;

    setActiveTicketPlaceId(place.id);
    localStorage.setItem('activeTicketPlaceId', place.id);

    // Update JSON preview
    setJsonPreview(JSON.stringify(place, null, 2));
  }, []);

  // Add new ticket place
  const addTicketPlace = useCallback((place) => {
    if (!place) return null;

    const id = place.id || uuidv4();
    const newPlace = {
      ...place,
      id,
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString()
    };

    // Update state
    setTicketPlaceMap(prevMap => ({
      ...prevMap,
      [id]: newPlace
    }));

    setTicketPlaceIds(prevIds => {
      const newIds = [...prevIds, id];
      localStorage.setItem('ticketPlaceIds', JSON.stringify(newIds));
      return newIds;
    });

    // Save to localStorage
    localStorage.setItem(`ticketPlace_${id}`, JSON.stringify(newPlace));

    // Set as active if it's the first place
    if (ticketPlaceIds.length === 0) {
      setActiveTicketPlaceId(id);
      localStorage.setItem('activeTicketPlaceId', id);
    }

    // Update JSON preview if this is the active place
    if (activeTicketPlaceId === id) {
      setJsonPreview(JSON.stringify(newPlace, null, 2));
    }

    return id;
  }, [ticketPlaceIds, activeTicketPlaceId]);

  // Update ticket place using API
  const updateTicketPlaceAPI = useCallback(async (updatedPlace) => {
    if (!updatedPlace || !updatedPlace.id) return;

    // Ensure all required fields are present
    const completePlace = {
      ...updatedPlace,
      // Ensure these fields exist
      description: updatedPlace.description || '',
      shortDesc: updatedPlace.shortDesc || '',
      address: updatedPlace.address || '',
      latitude: updatedPlace.latitude || 0,
      longitude: updatedPlace.longitude || 0,
      status: updatedPlace.status || 'Active',
      priority: updatedPlace.priority !== undefined ? parseInt(updatedPlace.priority) : 0,
      placeType: updatedPlace.placeType || 'Museum',
      allowSameDayBooking: updatedPlace.allowSameDayBooking !== undefined ? updatedPlace.allowSameDayBooking : true,
      gallery: updatedPlace.gallery || [],
      iconUrl: updatedPlace.iconUrl || '',
      mapImageUrl: updatedPlace.mapImageUrl || '',
      termsAndConditions: updatedPlace.termsAndConditions || [],
      termsAndConditionsUrl: updatedPlace.termsAndConditionsUrl || '',
      openTimings: updatedPlace.openTimings || '09:00:00',
      closeTimings: updatedPlace.closeTimings || '17:00:00',
      services: updatedPlace.services || [],
      businessHours: updatedPlace.businessHours || [],
      serviceCategories: updatedPlace.serviceCategories || [],
      servicePeopleCategories: updatedPlace.servicePeopleCategories || [],
      specialOccasions: updatedPlace.specialOccasions || []
    };

    setIsLoading(true);
    setError(null);
    try {
      // Log the data we're sending to the API
      console.log('Updating ticket place with data:', JSON.stringify(completePlace, null, 2));

      const response = await fetch(`${apiConfig.baseUrl}/ticketdashboard/ticketplace/update`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          'token': apiConfig.token
        },
        body: JSON.stringify(completePlace)
      });

      // Log the response status
      console.log('Update response status:', response.status);

      // Try to get the response text for debugging
      const responseText = await response.text();
      console.log('Response text:', responseText);

      if (!response.ok) {
        throw new Error(`Failed to update ticket place: ${response.status} - ${responseText}`);
      }

      // Parse the response text as JSON if possible
      let data;
      try {
        data = JSON.parse(responseText);
      } catch (e) {
        console.error('Error parsing response as JSON:', e);
        data = { success: false, message: 'Invalid JSON response' };
      }

      if (data && (data.success || data.result === "Success")) {
        // Fetch the updated ticket place details
        const updatedDetails = await fetchTicketPlaceDetails(updatedPlace.id);

        // Update JSON preview if this is the active place
        if (activeTicketPlaceId === updatedPlace.id) {
          setJsonPreview(JSON.stringify(updatedDetails, null, 2));
        }

        // Force a refresh of the ticket places list
        await fetchTicketPlaces(statusFilter);

        return true;
      } else {
        throw new Error(data.message || 'Failed to update ticket place');
      }
    } catch (error) {
      console.error('Error updating ticket place:', error);
      setError(error.message);
      return false;
    } finally {
      setIsLoading(false);
    }
  }, [apiConfig, activeTicketPlaceId, fetchTicketPlaceDetails, fetchTicketPlaces, statusFilter]);

  // Update ticket place in local state only (no API call)
  const updateTicketPlace = useCallback((updatedPlace) => {
    if (!updatedPlace || !updatedPlace.id) return;

    setTicketPlaceMap(prevMap => {
      const place = prevMap[updatedPlace.id];
      if (!place) return prevMap;

      const updatedPlaceWithTimestamp = {
        ...place,
        ...updatedPlace,
        priority: updatedPlace.priority !== undefined ? parseInt(updatedPlace.priority) : place.priority,
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${updatedPlace.id}`, JSON.stringify(updatedPlaceWithTimestamp));

      return {
        ...prevMap,
        [updatedPlace.id]: updatedPlaceWithTimestamp
      };
    });
  }, []);

  // Delete ticket place
  const deleteTicketPlace = useCallback((id) => {
    if (!id) return;

    // Remove from state
    setTicketPlaceMap(prevMap => {
      const newMap = { ...prevMap };
      delete newMap[id];
      return newMap;
    });

    setTicketPlaceIds(prevIds => {
      const newIds = prevIds.filter(placeId => placeId !== id);
      localStorage.setItem('ticketPlaceIds', JSON.stringify(newIds));

      // Update active ID if needed
      if (activeTicketPlaceId === id) {
        const newActiveId = newIds.length > 0 ? newIds[0] : null;
        setActiveTicketPlaceId(newActiveId);
        localStorage.setItem('activeTicketPlaceId', newActiveId || '');

        // Update JSON preview
        if (newActiveId && ticketPlaceMap[newActiveId]) {
          setJsonPreview(JSON.stringify(ticketPlaceMap[newActiveId], null, 2));
        } else {
          setJsonPreview('');
        }
      }

      return newIds;
    });

    // Remove from localStorage
    localStorage.removeItem(`ticketPlace_${id}`);
  }, [activeTicketPlaceId, ticketPlaceMap]);

  // Generate JSON for API
  const generateApiJson = useCallback(() => {
    const activePlace = getActiveTicketPlace();
    if (!activePlace) return null;

    // Update the JSON preview
    const jsonStr = JSON.stringify(activePlace, null, 2);
    setJsonPreview(jsonStr);

    return activePlace;
  }, [getActiveTicketPlace]);

  // Download JSON file
  const downloadJsonFile = useCallback((filename = 'ticket-place.json') => {
    const activePlace = getActiveTicketPlace();
    if (!activePlace) return;

    const jsonStr = JSON.stringify(activePlace, null, 2);
    const blob = new Blob([jsonStr], { type: 'application/json' });
    const url = URL.createObjectURL(blob);

    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();

    setTimeout(() => {
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }, 0);
  }, [getActiveTicketPlace]);

  // Load JSON from file
  const loadJsonFromFile = useCallback((jsonData) => {
    if (!jsonData) return false;

    try {
      // Check if it's a single ticket place
      if (jsonData.id) {
        // Add or update the ticket place
        const id = jsonData.id;

        // Update state
        setTicketPlaceMap(prevMap => {
          const newMap = {
            ...prevMap,
            [id]: jsonData
          };

          // Save to localStorage
          localStorage.setItem(`ticketPlace_${id}`, JSON.stringify(jsonData));

          return newMap;
        });

        // Add to IDs if not already there
        setTicketPlaceIds(prevIds => {
          if (prevIds.includes(id)) return prevIds;

          const newIds = [...prevIds, id];
          localStorage.setItem('ticketPlaceIds', JSON.stringify(newIds));
          return newIds;
        });

        // Set as active
        setActiveTicketPlaceId(id);
        localStorage.setItem('activeTicketPlaceId', id);

        // Update JSON preview
        setJsonPreview(JSON.stringify(jsonData, null, 2));

        return true;
      }

      return false;
    } catch (error) {
      console.error('Error loading JSON data:', error);
      return false;
    }
  }, []);

  // Add service category
  const addServiceCategory = useCallback((categoryData) => {
    if (!activeTicketPlaceId) return;

    const id = uuidv4();
    const newCategory = {
      ...categoryData,
      id
    };

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedPlace = {
        ...place,
        serviceCategories: [...(place.serviceCategories || []), newCategory],
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));

    return id;
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Update service category
  const updateServiceCategory = useCallback((categoryId, categoryData) => {
    if (!activeTicketPlaceId) return;

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedCategories = place.serviceCategories.map(category => {
        if (category.id === categoryId) {
          return {
            ...category,
            ...categoryData
          };
        }
        return category;
      });

      const updatedPlace = {
        ...place,
        serviceCategories: updatedCategories,
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Add service people category
  const addServicePeopleCategory = useCallback((categoryData) => {
    if (!activeTicketPlaceId) return;

    console.log('Adding service people category:', categoryData);

    // Parse price amount safely
    let priceAmount = 0;
    if (categoryData.priceAmount) {
      const parsed = parseFloat(categoryData.priceAmount);
      if (!isNaN(parsed)) {
        priceAmount = parsed;
      }
    }

    console.log('Parsed price amount:', priceAmount);

    const id = uuidv4();
    const newCategory = {
      name: categoryData.name,
      description: categoryData.description,
      pricingType: categoryData.pricingType,
      id,
      priceAmount: priceAmount,
      priceCurrency: categoryData.priceCurrency || 'INR',
      timeBounds: categoryData.timeBounds || { tag: 'Unbounded' },
      vendorSplitDetails: categoryData.vendorSplitDetails || []
    };

    console.log('New category:', newCategory);

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      // Ensure servicePeopleCategories exists
      if (!place.servicePeopleCategories) {
        console.log('Initializing servicePeopleCategories array for place:', place.id);
        place.servicePeopleCategories = [];
      }

      const updatedPlace = {
        ...place,
        servicePeopleCategories: [...place.servicePeopleCategories, newCategory],
        updatedAt: new Date().toISOString()
      };

      console.log('Updated place with new category:', updatedPlace);

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));

    return id;
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Update service people category
  const updateServicePeopleCategory = useCallback((categoryId, categoryData) => {
    if (!activeTicketPlaceId) return;

    console.log('Updating service people category:', categoryId, categoryData);

    // Parse price amount safely
    let priceAmount = 0;
    if (categoryData.priceAmount) {
      const parsed = parseFloat(categoryData.priceAmount);
      if (!isNaN(parsed)) {
        priceAmount = parsed;
      }
    }

    console.log('Parsed price amount:', priceAmount);

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      // Ensure servicePeopleCategories exists
      if (!place.servicePeopleCategories) {
        console.error('servicePeopleCategories is undefined for place:', place);
        place.servicePeopleCategories = [];
      }

      const updatedCategories = place.servicePeopleCategories.map(category => {
        if (category.id === categoryId) {
          const updatedCategory = {
            ...category,
            name: categoryData.name,
            description: categoryData.description,
            pricingType: categoryData.pricingType,
            priceAmount: priceAmount,
            priceCurrency: categoryData.priceCurrency || 'INR',
            timeBounds: categoryData.timeBounds || { tag: 'Unbounded' },
            vendorSplitDetails: categoryData.vendorSplitDetails || []
          };

          // We're now using priceAmount and priceCurrency directly

          console.log('Updated category:', updatedCategory);
          return updatedCategory;
        }
        return category;
      });

      const updatedPlace = {
        ...place,
        servicePeopleCategories: updatedCategories,
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Add service
  const addService = useCallback((serviceData) => {
    if (!activeTicketPlaceId) return;

    const id = uuidv4();
    const newService = {
      ...serviceData,
      id
    };

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedPlace = {
        ...place,
        services: [...(place.services || []), newService],
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));

    return id;
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Update service
  const updateService = useCallback((serviceId, serviceData) => {
    if (!activeTicketPlaceId) return;

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedServices = place.services.map(service => {
        if (service.id === serviceId) {
          return {
            ...service,
            ...serviceData
          };
        }
        return service;
      });

      const updatedPlace = {
        ...place,
        services: updatedServices,
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Add business hour
  const addBusinessHour = useCallback((businessHourData) => {
    if (!activeTicketPlaceId) return;

    const id = uuidv4();
    const newBusinessHour = {
      ...businessHourData,
      id
    };

    // Format btype based on type
    if (newBusinessHour.btypeType === 'Slot') {
      newBusinessHour.btype = {
        tag: 'Slot',
        contents: newBusinessHour.slotTime
      };
    } else {
      newBusinessHour.btype = {
        tag: 'Duration',
        contents: [newBusinessHour.durationStartTime, newBusinessHour.durationEndTime]
      };
    }

    // Remove temporary fields
    delete newBusinessHour.btypeType;
    delete newBusinessHour.slotTime;
    delete newBusinessHour.durationStartTime;
    delete newBusinessHour.durationEndTime;

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedPlace = {
        ...place,
        businessHours: [...(place.businessHours || []), newBusinessHour],
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));

    return id;
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Update business hour
  const updateBusinessHour = useCallback((businessHourId, businessHourData) => {
    if (!activeTicketPlaceId) return;

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedBusinessHours = place.businessHours.map(businessHour => {
        if (businessHour.id === businessHourId) {
          const updatedBusinessHour = {
            ...businessHour,
            ...businessHourData
          };

          // Format btype based on type
          if (businessHourData.btypeType === 'Slot') {
            updatedBusinessHour.btype = {
              tag: 'Slot',
              contents: businessHourData.slotTime
            };
          } else if (businessHourData.btypeType === 'Duration') {
            updatedBusinessHour.btype = {
              tag: 'Duration',
              contents: [businessHourData.durationStartTime, businessHourData.durationEndTime]
            };
          }

          // Remove temporary fields
          delete updatedBusinessHour.btypeType;
          delete updatedBusinessHour.slotTime;
          delete updatedBusinessHour.durationStartTime;
          delete updatedBusinessHour.durationEndTime;

          return updatedBusinessHour;
        }
        return businessHour;
      });

      const updatedPlace = {
        ...place,
        businessHours: updatedBusinessHours,
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Add special occasion
  const addSpecialOccasion = useCallback((occasionData) => {
    if (!activeTicketPlaceId) return;

    console.log('Adding special occasion:', occasionData);

    const id = uuidv4();
    const newOccasion = {
      ...occasionData,
      id
    };

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedPlace = {
        ...place,
        specialOccasions: [...(place.specialOccasions || []), newOccasion],
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));

    return id;
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Update special occasion
  const updateSpecialOccasion = useCallback((occasionId, occasionData) => {
    if (!activeTicketPlaceId) return;

    console.log('Updating special occasion:', occasionId, occasionData);

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedOccasions = place.specialOccasions.map(occasion => {
        if (occasion.id === occasionId) {
          return {
            ...occasion,
            ...occasionData
          };
        }
        return occasion;
      });

      const updatedPlace = {
        ...place,
        specialOccasions: updatedOccasions,
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Delete special occasion
  const deleteSpecialOccasion = useCallback((occasionId) => {
    if (!activeTicketPlaceId) return;

    console.log('Deleting special occasion:', occasionId);

    setTicketPlaceMap(prevMap => {
      const place = prevMap[activeTicketPlaceId];
      if (!place) return prevMap;

      const updatedOccasions = place.specialOccasions.filter(occasion => occasion.id !== occasionId);

      const updatedPlace = {
        ...place,
        specialOccasions: updatedOccasions,
        updatedAt: new Date().toISOString()
      };

      // Save to localStorage
      localStorage.setItem(`ticketPlace_${activeTicketPlaceId}`, JSON.stringify(updatedPlace));

      return {
        ...prevMap,
        [activeTicketPlaceId]: updatedPlace
      };
    });

    // Update JSON preview
    setJsonPreview(JSON.stringify(getActiveTicketPlace(), null, 2));
  }, [activeTicketPlaceId, getActiveTicketPlace]);

  // Save current ticket place to API
  const saveCurrentTicketPlace = useCallback(async () => {
    const activePlace = getActiveTicketPlace();
    if (!activePlace) {
      console.error('No active ticket place to save');
      return false;
    }

    console.log('Saving current ticket place to API:', activePlace);
    setIsLoading(true);

    try {
      // Make a deep copy of the active place to avoid reference issues
      const placeCopy = JSON.parse(JSON.stringify(activePlace));

      // Call updateTicketPlaceAPI with the copy
      const result = await updateTicketPlaceAPI(placeCopy);

      if (result) {
        console.log('Successfully saved ticket place to API');

        // Refresh the data after saving
        if (activePlace.id) {
          try {
            await fetchTicketPlaceDetails(activePlace.id);
          } catch (refreshError) {
            console.error('Error refreshing data after save:', refreshError);
            // Continue even if refresh fails
          }
        }

        return true;
      } else {
        console.error('Failed to save ticket place to API');
        return false;
      }
    } catch (error) {
      console.error('Error saving ticket place to API:', error);
      setError(error.message);
      return false;
    } finally {
      setIsLoading(false);
    }
  }, [getActiveTicketPlace, updateTicketPlaceAPI, fetchTicketPlaceDetails, setIsLoading, setError]);

  // Context value
  const contextValue = {
    ticketPlaceIds,
    ticketPlaceMap,
    activeTicketPlaceId,
    jsonPreview,
    setJsonPreview,
    apiConfig,
    updateApiConfig,
    isLoading,
    setIsLoading,
    error,
    statusFilter,
    setStatusFilter,
    fetchTicketPlaces,
    fetchTicketPlaceDetails,
    getTicketPlaceIds,
    getTicketPlaceInfo,
    getActiveTicketPlace,
    setActiveTicketPlace,
    addTicketPlace,
    updateTicketPlace,
    updateTicketPlaceAPI,
    saveCurrentTicketPlace,
    deleteTicketPlace,
    generateApiJson,
    downloadJsonFile,
    loadJsonFromFile,
    clearAllTicketData,
    addServiceCategory,
    updateServiceCategory,
    addServicePeopleCategory,
    updateServicePeopleCategory,
    addService,
    updateService,
    addBusinessHour,
    updateBusinessHour,
    addSpecialOccasion,
    updateSpecialOccasion,
    deleteSpecialOccasion
  };

  return (
    <TicketContext.Provider value={contextValue}>
      {children}
    </TicketContext.Provider>
  );
};

// Custom hook to use the ticket context
export const useTicket = () => {
  const context = React.useContext(TicketContext);
  if (context === undefined) {
    throw new Error('useTicket must be used within a TicketProvider');
  }
  return context;
};
