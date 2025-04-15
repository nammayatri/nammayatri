import React, { useState, useEffect } from 'react';
import { Modal, Button, Form, Row, Col } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import {
  getDaysOfWeek,
  getSpecialOccasionTypes,
  getSpecialOccasionDateTypes,
  getSpecialOccasionApplyToTypes
} from '../../utils/helpers';

const SpecialOccasionModal = ({ show, onHide, occasion }) => {
  const { getActiveTicketPlace, addSpecialOccasion, updateSpecialOccasion } = useTicket();
  const activePlace = getActiveTicketPlace();
  const [formData, setFormData] = useState({
    name: '',
    description: '',
    specialDayType: 'Open',
    date: '',
    dayOfWeek: null,
    entityType: 'category',
    entityId: '',
    businessHours: [],
    placeId: ''
  });

  // Initialize form data when editing an existing occasion
  useEffect(() => {
    if (occasion) {
      setFormData({
        name: occasion.name || '',
        description: occasion.description || '',
        businessHours: Array.isArray(occasion.businessHours) ? occasion.businessHours : [],
        dayOfWeek: occasion.dayOfWeek || null,
        date: occasion.date || '',
        type: occasion.type || 'HOLIDAY',
        appliesTo: occasion.appliesTo || 'ALL'
      });
    } else {
      setFormData({
        name: '',
        description: '',
        businessHours: [],
        dayOfWeek: null,
        date: '',
        type: 'HOLIDAY',
        appliesTo: 'ALL'
      });
    }
  }, [occasion]);

  const handleChange = (e) => {
    const { name, value } = e.target;

    // Special handling for applyTo changes
    if (name === 'applyTo') {
      setFormData(prev => ({
        ...prev,
        [name]: value,
        serviceId: value === 'service' ? prev.serviceId : '',
        categoryId: value === 'category' ? prev.categoryId : ''
      }));
    } else {
      setFormData(prev => ({
        ...prev,
        [name]: value
      }));
    }
  };

  const handleBusinessHourChange = (hourId) => {
    setFormData(prev => {
      const updatedHours = prev.businessHours.includes(hourId)
        ? prev.businessHours.filter(id => id !== hourId)
        : [...prev.businessHours, hourId];
      return {
        ...prev,
        businessHours: updatedHours
      };
    });
  };

  const handleSubmit = () => {
    // Validate form
    if (!formData.name) {
      alert('Please enter a name for the special occasion');
      return;
    }

    // Validate date or dayOfWeek - date is required if dayOfWeek is not provided
    if (!formData.dayOfWeek && !formData.date) {
      alert('Please select a specific date when weekly occurrence is not selected');
      return;
    }

    if (!formData.entityId) {
      alert(`Please select a ${formData.entityType === 'service' ? 'service' : 'service category'}`);
      return;
    }

    if (formData.businessHours.length === 0) {
      alert('Please select at least one business hour');
      return;
    }

    // Prepare data for saving
    const occasionData = {
      name: formData.name,
      description: formData.description,
      specialDayType: formData.specialDayType,
      date: formData.dayOfWeek ? null : formData.date, // Only include date if not using dayOfWeek
      dayOfWeek: formData.dayOfWeek,
      entityId: formData.entityId,
      businessHours: formData.businessHours,
      placeId: formData.placeId
    };

    // Save occasion
    if (occasion) {
      updateSpecialOccasion(occasion.id, occasionData);
    } else {
      addSpecialOccasion(occasionData);
    }

    onHide();
  };

  return (
    <Modal show={show} onHide={onHide} size="lg">
      <Modal.Header closeButton>
        <Modal.Title>
          {occasion ? 'Edit Special Occasion' : 'Add Special Occasion'}
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form>
          <Row className="mb-3">
            <Col md={6}>
              <Form.Group>
                <Form.Label>Name <span className="text-danger">*</span></Form.Label>
                <Form.Control
                  type="text"
                  name="name"
                  value={formData.name}
                  onChange={handleChange}
                  placeholder="e.g., Christmas Holiday"
                />
              </Form.Group>
            </Col>
            <Col md={6}>
              <Form.Group>
                <Form.Label>Type</Form.Label>
                <Form.Select
                  name="specialDayType"
                  value={formData.specialDayType}
                  onChange={handleChange}
                >
                  <option value="Open">Open</option>
                  <option value="Closed">Closed</option>
                </Form.Select>
              </Form.Group>
            </Col>
          </Row>

          <Form.Group className="mb-3">
            <Form.Label>Description</Form.Label>
            <Form.Control
              as="textarea"
              name="description"
              value={formData.description}
              onChange={handleChange}
              rows={2}
              placeholder="Enter a description of the special occasion"
            />
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Occurrence Type</Form.Label>
            <Form.Select 
              value={formData.dayOfWeek ? 'weekly' : 'specific'}
              onChange={(e) => {
                const isWeekly = e.target.value === 'weekly';
                setFormData(prev => ({
                  ...prev,
                  dayOfWeek: isWeekly ? 'Monday' : null,
                  date: isWeekly ? '' : prev.date
                }));
              }}
            >
              <option value="weekly">Weekly Occurrence</option>
              <option value="specific">Specific Date</option>
            </Form.Select>
            <Form.Text className="text-muted">
              Select whether this occasion occurs weekly or on a specific date
            </Form.Text>
          </Form.Group>

          {formData.dayOfWeek ? (
            <Form.Group className="mb-3">
              <Form.Label>Day of Week</Form.Label>
              <Form.Select
                value={formData.dayOfWeek || 'Monday'}
                onChange={(e) => handleChange({ target: { name: 'dayOfWeek', value: e.target.value } })}
              >
                <option value="Monday">Monday</option>
                <option value="Tuesday">Tuesday</option>
                <option value="Wednesday">Wednesday</option>
                <option value="Thursday">Thursday</option>
                <option value="Friday">Friday</option>
                <option value="Saturday">Saturday</option>
                <option value="Sunday">Sunday</option>
              </Form.Select>
            </Form.Group>
          ) : (
            <Form.Group className="mb-3">
              <Form.Label>Date</Form.Label>
              <Form.Control
                type="date"
                name="date"
                value={formData.date || ''}
                onChange={handleChange}
              />
            </Form.Group>
          )}

          <Row className="mb-3">
            <Col md={6}>
              <Form.Group>
                <Form.Label>Entity Type</Form.Label>
                <Form.Select
                  name="entityType"
                  value={formData.entityType}
                  onChange={(e) => {
                    setFormData(prev => ({
                      ...prev,
                      entityType: e.target.value,
                      entityId: '' // Reset entityId when type changes
                    }));
                  }}
                >
                  <option value="service">Service</option>
                  <option value="category">Service Category</option>
                </Form.Select>
              </Form.Group>
            </Col>
            <Col md={6}>
              <Form.Group>
                <Form.Label>{formData.entityType === 'service' ? 'Service' : 'Service Category'} <span className="text-danger">*</span></Form.Label>
                <Form.Select
                  name="entityId"
                  value={formData.entityId}
                  onChange={handleChange}
                >
                  <option value="">Select {formData.entityType === 'service' ? 'a service' : 'a category'}</option>
                  {formData.entityType === 'service' ? (
                    activePlace?.services?.map(service => (
                      <option key={service.id} value={service.id}>
                        {service.service || service.name} ({service.id})
                      </option>
                    ))
                  ) : (
                    activePlace?.serviceCategories?.map(category => (
                      <option key={category.id} value={category.id}>
                        {category.name} ({category.id})
                      </option>
                    ))
                  )}
                </Form.Select>
              </Form.Group>
            </Col>
          </Row>

          <Form.Group className="mb-3">
            <Form.Label>Business Hours <span className="text-danger">*</span></Form.Label>
            <div className="border rounded p-3" style={{ maxHeight: '200px', overflowY: 'auto' }}>
              {activePlace?.businessHours?.length > 0 ? (
                activePlace.businessHours.map(hour => (
                  <Form.Check
                    key={hour.id}
                    type="checkbox"
                    id={`special-occasion-business-hour-${hour.id}`}
                    label={`${hour.name} (${hour.id})`}
                    checked={formData.businessHours.includes(hour.id)}
                    onChange={() => {
                      const updatedHours = formData.businessHours.includes(hour.id)
                        ? formData.businessHours.filter(id => id !== hour.id)
                        : [...formData.businessHours, hour.id];
                      setFormData(prev => ({
                        ...prev,
                        businessHours: updatedHours
                      }));
                    }}
                    className="mb-2"
                  />
                ))
              ) : (
                <p className="text-muted mb-0">No business hours available. Please create some first.</p>
              )}
            </div>
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Cancel
        </Button>
        <Button variant="primary" onClick={handleSubmit}>
          {occasion ? 'Update' : 'Add'}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default SpecialOccasionModal;
