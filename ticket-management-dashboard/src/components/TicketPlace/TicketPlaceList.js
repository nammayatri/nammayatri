import React, { useState, useEffect } from 'react';
import { Card, Button, Table, Badge, Modal, Form, Row, Col, Alert, Spinner } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faEdit, faTrash, faPlus, faTicketAlt, faSync, faSave } from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import { getTicketPlaceStatusOptions, getPlaceTypeOptions } from '../../utils/helpers';
import { v4 as uuidv4 } from 'uuid';
import ApiConfig from '../Common/ApiConfig';
import './TicketPlaceList.css';

const TicketPlaceList = ({ onSelectPlace }) => {
  const {
    ticketPlaceIds,
    ticketPlaceMap,
    updateTicketPlace,
    saveCurrentTicketPlace,
    setActiveTicketPlace,
    addTicketPlace,
    deleteTicketPlace,
    generateApiJson,
    fetchTicketPlaces,
    fetchTicketPlaceDetails,
    apiConfig,
    updateApiConfig,
    isLoading,
    error,
    statusFilter,
    setStatusFilter
  } = useTicket();
  const [showModal, setShowModal] = useState(false);
  const [currentPlace, setCurrentPlace] = useState(null);
  const [formData, setFormData] = useState({
    id: '',
    name: '',
    description: '',
    shortDesc: '',
    address: '',
    latitude: '',
    longitude: '',
    status: 'Active',
    priority: 1,
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

  // Fetch ticket places when component mounts or API config changes
  useEffect(() => {
    if (apiConfig.baseUrl && apiConfig.token) {
      console.log('TicketPlaceList: API config is valid, fetching ticket places...');
      fetchTicketPlaces();
    } else {
      console.log('TicketPlaceList: API config is not valid, skipping fetch');
    }
  }, [apiConfig, fetchTicketPlaces]);

  const handleAddClick = () => {
    setCurrentPlace(null);
    setFormData({
      id: uuidv4(),
      name: '',
      description: '',
      shortDesc: 'Short description',
      address: '',
      latitude: '',
      longitude: '',
      status: 'Active',
      priority: 1,
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
    setShowModal(true);
  };

  const handleEditClick = (place) => {
    setCurrentPlace(place);
    setFormData({
      id: place.id,
      name: place.name || '',
      description: place.description || '',
      shortDesc: place.shortDesc || 'Short description',
      address: place.address || '',
      latitude: place.latitude || '',
      longitude: place.longitude || '',
      status: place.status || 'Active',
      priority: place.priority || 1,
      placeType: place.placeType || 'Museum',
      allowSameDayBooking: place.allowSameDayBooking !== undefined ? place.allowSameDayBooking : true,
      gallery: place.gallery || [],
      iconUrl: place.iconUrl || '',
      mapImageUrl: place.mapImageUrl || '',
      termsAndConditions: place.termsAndConditions || [],
      termsAndConditionsUrl: place.termsAndConditionsUrl || '',
      openTimings: place.openTimings || '',
      closeTimings: place.closeTimings || ''
    });
    setShowModal(true);
  };

  const handleDeleteClick = (id) => {
    if (window.confirm('Are you sure you want to delete this ticket place?')) {
      // Delete the ticket place
      deleteTicketPlace(id);
      console.log(`Deleted ticket place with ID: ${id}`);
    }
  };

  const handleCloseModal = () => {
    setShowModal(false);
  };

  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;

    // Handle different input types
    let processedValue;
    if (type === 'checkbox') {
      processedValue = checked;
    } else if (name === 'priority') {
      processedValue = value === '' ? '' : parseInt(value, 10);
      if (isNaN(processedValue) || processedValue < 0) {
        processedValue = 0;
      }
    } else if (name === 'latitude' || name === 'longitude') {
      processedValue = value === '' ? '' : parseFloat(value) || 0;
    } else if (name === 'termsAndConditions') {
      // Split by newline and filter out empty lines
      processedValue = value.split('\n').filter(line => line.trim() !== '');
      if (value === '') {
        processedValue = [];
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

    if (!formData.shortDesc) {
      alert('Short description is required');
      return;
    }

    // Ensure all required fields are included
    const ticketPlaceData = {
      ...formData,
      // Convert string values to appropriate types
      latitude: formData.latitude ? parseFloat(formData.latitude) : 0,
      longitude: formData.longitude ? parseFloat(formData.longitude) : 0,
      priority: formData.priority !== undefined && formData.priority !== '' ? parseInt(formData.priority, 10) : 0,
      // Ensure arrays are initialized
      gallery: formData.gallery || [],
      termsAndConditions: formData.termsAndConditions || [],
      services: [],
      businessHours: [],
      serviceCategories: [],
      servicePeopleCategories: [],
      specialOccasions: []
    };

    // Create or update the ticket place
    if (currentPlace) {
      // Update existing place
      await updateTicketPlace(ticketPlaceData);
    } else {
      // Create new place with a UUID
      const newPlace = {
        ...ticketPlaceData,
        id: uuidv4()
      };
      await updateTicketPlace(newPlace);
    }

    // Refresh the ticket places list
    await fetchTicketPlaces();

    setShowModal(false);
  };

  const handleSelectPlace = async (place) => {
    // Fetch the full details of the ticket place
    const placeDetails = await fetchTicketPlaceDetails(place.id);

    if (placeDetails) {
      setActiveTicketPlace(placeDetails);
      if (onSelectPlace) {
        onSelectPlace(placeDetails);
      }
    }
  };

  // Get status badge color
  const getStatusBadgeVariant = (status) => {
    switch (status) {
      case 'Active':
        return 'success';
      case 'Inactive':
        return 'secondary';
      case 'ComingSoon':
        return 'info';
      case 'Ended':
        return 'danger';
      default:
        return 'primary';
    }
  };

  // Format place type for display
  const formatPlaceType = (type) => {
    if (!type) return '';
    return type.replace(/([A-Z])/g, ' $1').trim();
  };

  // Render the list of ticket places
  const renderTicketPlaces = () => {
    if (!ticketPlaceIds || ticketPlaceIds.length === 0) {
      return (
        <tr>
          <td colSpan="6" className="text-center">No ticket places found. Click "Add Ticket Place" to create one.</td>
        </tr>
      );
    }

    return ticketPlaceIds.map((placeId) => {
      const place = ticketPlaceMap[placeId];
      if (!place) {
        console.log(`No place found for ID: ${placeId}`);
        return null;
      }
      return (
      <tr key={place.id}>
        <td>
          <Button
            variant="link"
            className="place-name-link"
            onClick={() => handleSelectPlace(place)}
          >
            {place.name}
          </Button>
        </td>
        <td>{place.shortDesc}</td>
        <td>{formatPlaceType(place.placeType)}</td>
        <td>
          <Badge bg={getStatusBadgeVariant(place.status)}>
            {place.status}
          </Badge>
        </td>
        <td className="text-center">{place.priority}</td>
        <td>
          <Button
            variant="outline-primary"
            size="sm"
            className="me-2"
            onClick={() => handleEditClick(place)}
          >
            <FontAwesomeIcon icon={faEdit} />
          </Button>
          <Button
            variant="outline-danger"
            size="sm"
            onClick={() => handleDeleteClick(place.id)}
          >
            <FontAwesomeIcon icon={faTrash} />
          </Button>
        </td>
      </tr>
    )});
  };

  return (
    <>
      {/* API Configuration */}
      <ApiConfig onSave={updateApiConfig} />

      {/* Error Alert */}
      {error && (
        <Alert variant="danger" className="mb-4">
          <strong>Error:</strong> {error}
        </Alert>
      )}

      {/* Status Filter */}
      <Card className="mb-4">
        <Card.Header>
          <h5 className="mb-0">Filter Ticket Places</h5>
        </Card.Header>
        <Card.Body>
          <Row>
            <Col md={6}>
              <Form.Group>
                <Form.Label>Status</Form.Label>
                <Form.Select
                  value={statusFilter}
                  onChange={(e) => {
                    const newStatus = e.target.value;
                    fetchTicketPlaces(newStatus);
                  }}
                >
                  {getTicketPlaceStatusOptions().map(option => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </Form.Select>
              </Form.Group>
            </Col>
          </Row>
        </Card.Body>
      </Card>

      {/* Ticket Places List */}
      <Card className="mb-4">
        <Card.Header className="d-flex justify-content-between align-items-center">
          <h5 className="mb-0">Ticket Places</h5>
          <div>
            <Button
              variant="outline-primary"
              size="sm"
              onClick={async () => {
                if (isLoading) return;

                const result = await saveCurrentTicketPlace();
                if (result) {
                  alert('Changes saved successfully!');
                } else {
                  alert('Failed to save changes. Please try again.');
                }
              }}
              className="me-2"
              disabled={isLoading}
            >
              <FontAwesomeIcon icon={faSave} className="me-1" /> Save Changes
            </Button>
            <Button
              variant="outline-secondary"
              size="sm"
              onClick={() => fetchTicketPlaces()}
              className="me-2"
              disabled={isLoading}
            >
              <FontAwesomeIcon icon={faSync} className={isLoading ? "fa-spin me-1" : "me-1"} /> Refresh
            </Button>
            <Button
              variant="primary"
              size="sm"
              onClick={handleAddClick}
              disabled={isLoading}
            >
              <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Ticket Place
            </Button>
          </div>
        </Card.Header>
        <Card.Body>
          {isLoading ? (
            <div className="text-center py-4">
              <Spinner animation="border" role="status">
                <span className="visually-hidden">Loading...</span>
              </Spinner>
              <p className="mt-2">Loading ticket places...</p>
            </div>
          ) : (
            <Table responsive hover>
              <thead>
                <tr>
                  <th>Name</th>
                  <th>Description</th>
                  <th>Type</th>
                  <th>Status</th>
                  <th className="text-center">Priority</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {renderTicketPlaces()}
              </tbody>
            </Table>
          )}
        </Card.Body>
      </Card>

      {/* Ticket Place Modal */}
      <Modal show={showModal} onHide={handleCloseModal} size="lg">
        <Modal.Header closeButton>
          <Modal.Title>
            {currentPlace ? 'Edit Ticket Place' : 'Add Ticket Place'}
          </Modal.Title>
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
                    type="number"
                    name="priority"
                    value={formData.priority}
                    onChange={handleChange}
                    min="0"
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
                  <Form.Control
                    type="text"
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
                  <Form.Control
                    type="text"
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
                  <Form.Label>Gallery Images</Form.Label>
                  <Form.Control
                    as="textarea"
                    name="gallery"
                    value={formData.gallery.join('\n')}
                    onChange={(e) => {
                      const urls = e.target.value.split('\n').filter(url => url.trim() !== '');
                      setFormData(prev => ({
                        ...prev,
                        gallery: urls
                      }));
                    }}
                    rows={3}
                    placeholder="Enter image URLs (one per line)"
                  />
                  <Form.Text className="text-muted">
                    Enter one image URL per line for the gallery
                  </Form.Text>
                </Form.Group>
              </Col>
            </Row>

            <Row className="mb-3">
              <Col md={12}>
                <Form.Group>
                  <Form.Label>Terms & Conditions</Form.Label>
                  <Form.Control
                    as="textarea"
                    name="termsAndConditions"
                    value={formData.termsAndConditions.join('\n')}
                    onChange={handleChange}
                    rows={3}
                    placeholder="Enter terms and conditions (one per line)"
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
          </Form>
        </Modal.Body>
        <Modal.Footer>
          <Button variant="secondary" onClick={handleCloseModal}>
            Cancel
          </Button>
          <Button variant="primary" onClick={handleSubmit}>
            {currentPlace ? 'Save Changes' : 'Create Ticket Place'}
          </Button>
        </Modal.Footer>
      </Modal>
    </>
  );
};

export default TicketPlaceList;
