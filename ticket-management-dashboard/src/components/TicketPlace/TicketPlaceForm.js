import React, { useState, useEffect } from 'react';
import { Card, Form, Row, Col } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import { getTicketPlaceStatusOptions, getPlaceTypeOptions } from '../../utils/helpers';

const TicketPlaceForm = () => {
  const { updateTicketPlace, getActiveTicketPlace } = useTicket();
  const [formData, setFormData] = useState({
    id: '',
    name: '',
    description: '',
    shortDesc: '',
    address: '',
    latitude: '',
    longitude: '',
    status: 'Active',
    priority: 0,
    placeType: 'Museum',
    allowSameDayBooking: true,
    gallery: [],
    iconUrl: '',
    mapImageUrl: '',
    termsAndConditions: [],
    termsAndConditionsUrl: '',
    openTimings: '',
    closeTimings: ''
  });
  const [isDirty, setIsDirty] = useState(false);

  // Get the active place directly from context
  const activePlace = getActiveTicketPlace();

  // Initialize form data when active ticket place changes
  useEffect(() => {
    if (activePlace) {
      setFormData({
        id: activePlace.id,
        name: activePlace.name || '',
        description: activePlace.description || '',
        shortDesc: activePlace.shortDesc || '',
        address: activePlace.address || '',
        latitude: activePlace.latitude || '',
        longitude: activePlace.longitude || '',
        status: activePlace.status || 'Active',
        priority: activePlace.priority ?? 0,
        placeType: activePlace.placeType || 'Museum',
        allowSameDayBooking: activePlace.allowSameDayBooking !== undefined ? activePlace.allowSameDayBooking : true,
        gallery: activePlace.gallery || [],
        iconUrl: activePlace.iconUrl || '',
        mapImageUrl: activePlace.mapImageUrl || '',
        termsAndConditions: activePlace.termsAndConditions || [],
        termsAndConditionsUrl: activePlace.termsAndConditionsUrl || '',
        openTimings: activePlace.openTimings || '',
        closeTimings: activePlace.closeTimings || ''
      });
      setIsDirty(false);
    }
  }, [activePlace]);



  // Handle general form input changes
  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;

    // Handle different input types
    let processedValue;
    if (type === 'checkbox') {
      processedValue = checked;
    } else if (name === 'priority') {
      processedValue = value === '' ? 0 : parseInt(value, 10);
      if (isNaN(processedValue)) processedValue = 0;
    } else {
      processedValue = value;
    }

    setFormData(prev => ({
      ...prev,
      [name]: processedValue
    }));
    setIsDirty(true);
  };

  // Function to normalize latitude and longitude values before saving
  const normalizeCoordinates = (data) => {
    const normalized = { ...data };

    // Convert latitude to number if it's a valid number
    if (typeof normalized.latitude === 'string') {
      if (normalized.latitude && !isNaN(parseFloat(normalized.latitude))) {
        normalized.latitude = parseFloat(normalized.latitude);
      } else if (normalized.latitude === '' || normalized.latitude === '.' || normalized.latitude === '-' || normalized.latitude === '-.') {
        normalized.latitude = 0;
      }
    }

    // Convert longitude to number if it's a valid number
    if (typeof normalized.longitude === 'string') {
      if (normalized.longitude && !isNaN(parseFloat(normalized.longitude))) {
        normalized.longitude = parseFloat(normalized.longitude);
      } else if (normalized.longitude === '' || normalized.longitude === '.' || normalized.longitude === '-' || normalized.longitude === '-.') {
        normalized.longitude = 0;
      }
    }

    return normalized;
  };

  // Update local state when form is modified
  useEffect(() => {
    if (isDirty && formData.id) {
      // Normalize coordinates before updating
      const normalizedData = normalizeCoordinates(formData);

      // Update the form data in the local state only
      updateTicketPlace(normalizedData);

      // Reset dirty state
      setIsDirty(false);
    }
  }, [isDirty, formData, updateTicketPlace]);

  return (
    <Card className="mb-4">
      <Card.Header className="d-flex justify-content-between align-items-center">
        <h5 className="mb-0">Ticket Place Details</h5>
      </Card.Header>
      <Card.Body>
        <Row className="mb-3">
          <Col md={6}>
            <Form.Group>
              <Form.Label>Name <span className="text-danger">*</span></Form.Label>
              <Form.Control
                type="text"
                name="name"
                value={formData.name}
                onChange={handleChange}
                placeholder="e.g., City Museum"
                required
              />
            </Form.Group>
          </Col>
          <Col md={3}>
            <Form.Group>
              <Form.Label>Status</Form.Label>
              <Form.Select
                name="status"
                value={formData.status}
                onChange={handleChange}
              >
                {getTicketPlaceStatusOptions().map(option => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Form.Select>
            </Form.Group>
          </Col>
          <Col md={3}>
            <Form.Group>
              <Form.Label>Priority</Form.Label>
              <Form.Control
                type="text"
                name="priority"
                value={formData.priority}
                onChange={handleChange}
                pattern="[0-9]*"
                inputMode="numeric"
                style={{ WebkitAppearance: 'none', MozAppearance: 'textfield' }}
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={6}>
            <Form.Group>
              <Form.Label>Place Type</Form.Label>
              <Form.Select
                name="placeType"
                value={formData.placeType}
                onChange={handleChange}
              >
                {getPlaceTypeOptions().map(option => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Form.Select>
            </Form.Group>
          </Col>
          <Col md={6}>
            <Form.Group className="mt-4">
              <Form.Check
                type="checkbox"
                name="allowSameDayBooking"
                checked={formData.allowSameDayBooking}
                onChange={handleChange}
                label="Allow Same Day Booking"
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={6}>
            <Form.Group>
              <Form.Label>Short Description</Form.Label>
              <Form.Control
                type="text"
                name="shortDesc"
                value={formData.shortDesc}
                onChange={handleChange}
                placeholder="Brief description"
              />
            </Form.Group>
          </Col>
          <Col md={3}>
            <Form.Group>
              <Form.Label>Open Time</Form.Label>
              <Form.Control
                type="time"
                name="openTimings"
                value={formData.openTimings}
                onChange={handleChange}
              />
            </Form.Group>
          </Col>
          <Col md={3}>
            <Form.Group>
              <Form.Label>Close Time</Form.Label>
              <Form.Control
                type="time"
                name="closeTimings"
                value={formData.closeTimings}
                onChange={handleChange}
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={12}>
            <Form.Group>
              <Form.Label>Full Description</Form.Label>
              <Form.Control
                as="textarea"
                name="description"
                value={formData.description}
                onChange={handleChange}
                rows={3}
                placeholder="Enter a detailed description of the place"
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={12}>
            <Form.Group>
              <Form.Label>Address</Form.Label>
              <Form.Control
                type="text"
                name="address"
                value={formData.address}
                onChange={handleChange}
                placeholder="Full address"
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={6}>
            <Form.Group>
              <Form.Label>Latitude</Form.Label>
              <input
                type="number"
                step="any"
                className="form-control"
                name="latitude"
                value={formData.latitude}
                onChange={handleChange}
                placeholder="e.g., 12.9716"
              />
            </Form.Group>
          </Col>
          <Col md={6}>
            <Form.Group>
              <Form.Label>Longitude</Form.Label>
              <input
                type="number"
                step="any"
                className="form-control"
                name="longitude"
                value={formData.longitude}
                onChange={handleChange}
                placeholder="e.g., 77.5946"
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={6}>
            <Form.Group>
              <Form.Label>Icon URL</Form.Label>
              <Form.Control
                type="text"
                name="iconUrl"
                value={formData.iconUrl}
                onChange={handleChange}
                placeholder="URL to icon image"
              />
            </Form.Group>
          </Col>
          <Col md={6}>
            <Form.Group>
              <Form.Label>Map Image URL</Form.Label>
              <Form.Control
                type="text"
                name="mapImageUrl"
                value={formData.mapImageUrl}
                onChange={handleChange}
                placeholder="URL to map image"
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={12}>
            <Form.Group>
              <Form.Label>Terms & Conditions URL</Form.Label>
              <Form.Control
                type="text"
                name="termsAndConditionsUrl"
                value={formData.termsAndConditionsUrl}
                onChange={handleChange}
                placeholder="URL to terms and conditions document"
              />
            </Form.Group>
          </Col>
        </Row>

        <Row className="mb-3">
          <Col md={12}>
            <Form.Group>
              <Form.Label>Terms & Conditions</Form.Label>
              <Form.Control
                as="textarea"
                name="termsAndConditionsText"
                value={formData.termsAndConditions.join('\n')}
                onChange={(e) => {
                  const lines = e.target.value.split('\n');
                  setFormData(prev => ({
                    ...prev,
                    termsAndConditions: lines
                  }));
                  setIsDirty(true);
                }}
                rows={5}
                placeholder="Enter terms and conditions (each line will be a separate item)"
              />
              <Form.Text className="text-muted">
                Enter each term or condition on a new line. Each line will be treated as a separate item.
              </Form.Text>
            </Form.Group>
          </Col>
        </Row>
      </Card.Body>
    </Card>
  );
};

export default TicketPlaceForm;
