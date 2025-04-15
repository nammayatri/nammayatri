/**
 * Format time string (HH:MM) for display
 * @param {string} timeString Time string in HH:MM format
 * @returns {string} Formatted time string
 */
export const formatTime = (timeString) => {
  if (!timeString) return '';

  try {
    const [hours, minutes] = timeString.split(':');
    const date = new Date();
    date.setHours(parseInt(hours, 10));
    date.setMinutes(parseInt(minutes, 10));

    return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
  } catch (error) {
    console.error('Error formatting time:', error);
    return timeString;
  }
};

/**
 * Format date string (YYYY-MM-DD) for display
 * @param {string} dateString Date string in YYYY-MM-DD format
 * @returns {string} Formatted date string
 */
export const formatDate = (dateString) => {
  if (!dateString) return '';

  try {
    const date = new Date(dateString);
    return date.toLocaleDateString(undefined, {
      year: 'numeric',
      month: 'long',
      day: 'numeric'
    });
  } catch (error) {
    console.error('Error formatting date:', error);
    return dateString;
  }
};

/**
 * Get days of the week
 * @returns {string[]} Array of days of the week
 */
export const getDaysOfWeek = () => [
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday',
  'Sunday'
];

/**
 * Get business hour types
 * @returns {Object[]} Array of business hour types
 */
export const getBusinessHourTypes = () => [
  { value: 'slot', label: 'Time Slots' },
  { value: 'duration', label: 'Duration' }
];

/**
 * Get special occasion types
 * @returns {Object[]} Array of special occasion types
 */
export const getSpecialOccasionTypes = () => [
  { value: 'Open', label: 'Open' },
  { value: 'Closed', label: 'Closed' }
];

/**
 * Get special occasion date types
 * @returns {Object[]} Array of special occasion date types
 */
export const getSpecialOccasionDateTypes = () => [
  { value: 'specific', label: 'Specific Date' },
  { value: 'weekly', label: 'Weekly' }
];

/**
 * Get special occasion apply to types
 * @returns {Object[]} Array of special occasion apply to types
 */
export const getSpecialOccasionApplyToTypes = () => [
  { value: 'all', label: 'All Services' },
  { value: 'service', label: 'Specific Service' },
  { value: 'category', label: 'Specific Category' }
];

/**
 * Get ticket place status options
 * @returns {Object[]} Array of ticket place status options
 */
export const getTicketPlaceStatusOptions = () => [
  { value: 'Active', label: 'Active' },
  { value: 'Inactive', label: 'Inactive' },
  { value: 'ComingSoon', label: 'Coming Soon' },
  { value: 'Ended', label: 'Ended' }
];

/**
 * Get place type options
 * @returns {Object[]} Array of place type options
 */
export const getPlaceTypeOptions = () => [
  { value: 'Museum', label: 'Museum' },
  { value: 'ThemePark', label: 'Theme Park' },
  { value: 'AmusementPark', label: 'Amusement Park' },
  { value: 'WaterPark', label: 'Water Park' },
  { value: 'WildLifeSanctuary', label: 'Wildlife Sanctuary' },
  { value: 'ArtGallery', label: 'Art Gallery' },
  { value: 'HeritageSite', label: 'Heritage Site' },
  { value: 'ReligiousSite', label: 'Religious Site' },
  { value: 'Other', label: 'Other' }
];

/**
 * Find an entity by ID, handling both prefixed and non-prefixed UUIDs
 * @param {Array} array Array of entities to search
 * @param {string} id ID to find
 * @returns {Object|undefined} Found entity or undefined
 */
export const findEntityById = (array, id) => {
  if (!array || !Array.isArray(array) || !id) return undefined;

  // First try direct match
  let entity = array.find(item => item.id === id);

  // If not found and id has a prefix (contains a hyphen), try without prefix
  if (!entity && id.includes('-')) {
    const idWithoutPrefix = id.split('-').slice(1).join('-');
    entity = array.find(item => item.id === idWithoutPrefix);
  }

  // If not found and id doesn't have a prefix, try with common prefixes
  if (!entity && !id.includes('-')) {
    const commonPrefixes = ['bh-', 'service-', 'category-', 'people-', 'so-'];
    for (const prefix of commonPrefixes) {
      entity = array.find(item => item.id === `${prefix}${id}`);
      if (entity) break;
    }
  }

  return entity;
};
