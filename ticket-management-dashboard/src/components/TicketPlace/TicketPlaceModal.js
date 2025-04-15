import React, { useState, useEffect } from 'react';
import { Modal, Button, Form, Row, Col } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import { getTicketPlaceStatusOptions } from '../../utils/helpers';
import { v4 as uuidv4 } from 'uuid';

const TicketPlaceModal = ({ show, onHide, place, onSave }) => {
  const { updateTicketPlaceAPI, generateApiJson, fetchTicketPlaces } = useTicket();
  const [formData, setFormData] = useState({
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
    openTimings: '09:00:00',
    closeTimings: '17:00:00'
  });

  // Initialize form data when place prop changes
  useEffect(() => {
    if (place) {
      setFormData({
        name: place.name || '',
        description: place.description || '',
        shortDesc: place.shortDesc || '',
        address: place.address || '',
        latitude: place.latitude || '',
        longitude: place.longitude || '',
        status: place.status || 'Active',
        priority: place.priority ?? 0,
        placeType: place.placeType || 'Museum',
        allowSameDayBooking: place.allowSameDayBooking !== undefined ? place.allowSameDayBooking : true,
        gallery: place.gallery || [],
        iconUrl: place.iconUrl || '',
        mapImageUrl: place.mapImageUrl || '',
        termsAndConditions: place.termsAndConditions || [],
        termsAndConditionsUrl: place.termsAndConditionsUrl || '',
        openTimings: place.openTimings || '09:00:00',
        closeTimings: place.closeTimings || '17:00:00'
      });
    } else {
      // Reset form when creating new
      setFormData({
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
        openTimings: '09:00:00',
        closeTimings: '17:00:00'
      });
    }
  }, [place, show]);

  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;

    // Handle different input types
    let processedValue;
    if (type === 'checkbox') {
      processedValue = checked;
    } else if (name === 'priority') {
      processedValue = value === '' ? 0 : parseInt(value, 10);
      if (isNaN(processedValue)) processedValue = 0;
    } else if (name === 'latitude' || name === 'longitude') {
      // Allow decimal numbers for lat/lon
      if (value === '' || /^-?\d*\.?\d*$/.test(value)) {
        processedValue = value;
      } else {
        return; // Invalid input, don't update
      }
    } else {
      processedValue = value;
    }

    setFormData(prev => ({
      ...prev,
      [name]: processedValue
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();

    // Validate required fields
    if (!formData.name) {
      alert('Name is required');
      return;
    }

    try {
      // Generate a new ID for new ticket places
      const ticketPlaceId = place ? place.id : uuidv4();

      // Convert lat/lon to numbers before saving
      const dataToSave = {
        ...formData,
        id: ticketPlaceId,
        shortDesc: formData.description ? formData.description.substring(0, 50) + (formData.description.length > 50 ? '...' : '') : '',
        latitude: formData.latitude ? parseFloat(formData.latitude) : 0,
        longitude: formData.longitude ? parseFloat(formData.longitude) : 0,
        // Initialize empty arrays for new ticket places
        services: [],
        businessHours: [],
        serviceCategories: [],
        servicePeopleCategories: [],
        specialOccasions: [],
        // Add timestamps
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString()
      };

      console.log('Saving ticket place:', dataToSave);

      // Update or create the ticket place using API
      const success = await updateTicketPlaceAPI(dataToSave);

      if (!success) {
        throw new Error('Failed to save ticket place');
      }

      // Generate and update the JSON
      await generateApiJson();

      // Refresh the ticket places list
      await fetchTicketPlaces();

      // Call onSave callback if provided
      if (onSave) {
        onSave(dataToSave);
      }

      onHide();
    } catch (error) {
      console.error('Error saving ticket place:', error);
      alert('Failed to save ticket place. Please try again.');
    }
  };

  return (
    <Modal show={show} onHide={onHide} size="lg">
      <Modal.Header closeButton>
        <Modal.Title>{place ? 'Edit Ticket Place' : 'Create Ticket Place'}</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form onSubmit={handleSubmit}>
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
                  <option value="Museum">Museum</option>
                  <option value="ThemePark">Theme Park</option>
                  <option value="AmusementPark">Amusement Park</option>
                  <option value="WaterPark">Water Park</option>
                  <option value="WildLifeSanctuary">Wildlife Sanctuary</option>
                  <option value="ArtGallery">Art Gallery</option>
                  <option value="HeritageSite">Heritage Site</option>
                  <option value="ReligiousSite">Religious Site</option>
                  <option value="Other">Other</option>
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
            <Col md={12}>
              <Form.Group>
                <Form.Label>Description</Form.Label>
                <Form.Control
                  as="textarea"
                  name="description"
                  value={formData.description}
                  onChange={handleChange}
                  rows={3}
                  placeholder="Enter a description of the place"
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

          <Row>
            <Col md={6}>
              <Form.Group>
                <Form.Label>Latitude</Form.Label>
                <Form.Control
                  type="text"
                  name="latitude"
                  value={formData.latitude}
                  onChange={handleChange}
                  placeholder="e.g., 12.9716"
                  pattern="-?\d*\.?\d*"
                  inputMode="decimal"
                />
              </Form.Group>
            </Col>
            <Col md={6}>
              <Form.Group>
                <Form.Label>Longitude</Form.Label>
                <Form.Control
                  type="text"
                  name="longitude"
                  value={formData.longitude}
                  onChange={handleChange}
                  placeholder="e.g., 77.5946"
                  pattern="-?\d*\.?\d*"
                  inputMode="decimal"
                />
              </Form.Group>
            </Col>
          </Row>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Cancel
        </Button>
        <Button variant="primary" onClick={handleSubmit}>
          {place ? 'Save Changes' : 'Create Place'}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default TicketPlaceModal;
