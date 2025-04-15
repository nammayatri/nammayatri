import React, { useState, useEffect } from 'react';
import { Modal, Button, Form, Row, Col } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import { getDaysOfWeek } from '../../utils/helpers';

const ServiceModal = ({ show, onHide, service }) => {
  const { getActiveTicketPlace, addService, updateService } = useTicket();
  const activePlace = getActiveTicketPlace();
  const [formData, setFormData] = useState({
    service: '',
    shortDesc: '',
    operationalDays: [],
    operationalDate: {
      startDate: '',
      eneDate: ''
    },
    maxVerification: 1,
    allowFutureBooking: true,
    allowCancellation: false,
    expiryType: 'VisitDate',
    expiryTime: '12:00:00',
    expiryMinutes: 120,
    businessHours: []
  });

  // Initialize form data when editing an existing service
  useEffect(() => {
    if (service) {
      setFormData({
        service: service.service || '',
        shortDesc: service.shortDesc || '',
        operationalDays: service.operationalDays || [],
        operationalDate: service.operationalDate || { startDate: '', eneDate: '' },
        maxVerification: service.maxVerification || 1,
        allowFutureBooking: service.allowFutureBooking !== false,
        allowCancellation: service.allowCancellation === true,
        expiryType: service.expiry?.tag || 'VisitDate',
        expiryTime: service.expiry?.tag === 'VisitDate' ? service.expiry?.contents || '12:00:00' : '12:00:00',
        expiryMinutes: service.expiry?.tag === 'InstantExpiry' ? service.expiry?.contents || 120 : 120,
        businessHours: service.businessHours || []
      });
    } else {
      // Reset form for new service
      setFormData({
        service: '',
        shortDesc: '',
        operationalDays: [],
        operationalDate: { startDate: '', eneDate: '' },
        maxVerification: 1,
        allowFutureBooking: true,
        allowCancellation: false,
        expiryType: 'VisitDate',
        expiryTime: '12:00:00',
        expiryMinutes: 120,
        businessHours: []
      });
    }
  }, [service, show]);

  const handleChange = (e) => {
    const { name, value, type, checked } = e.target;

    if (type === 'checkbox') {
      setFormData(prev => ({
        ...prev,
        [name]: checked
      }));
    } else if (name === 'maxVerification') {
      setFormData(prev => ({
        ...prev,
        [name]: parseInt(value, 10) || 1
      }));
    } else if (name === 'expiryMinutes') {
      setFormData(prev => ({
        ...prev,
        [name]: parseInt(value, 10) || 120
      }));
    } else {
      setFormData(prev => ({
        ...prev,
        [name]: value
      }));
    }
  };

  const handleDayChange = (day) => {
    setFormData(prev => {
      const days = [...prev.operationalDays];

      if (days.includes(day)) {
        // Remove day
        return {
          ...prev,
          operationalDays: days.filter(d => d !== day)
        };
      } else {
        // Add day
        return {
          ...prev,
          operationalDays: [...days, day]
        };
      }
    });
  };

  // Business hours are now handled with checkboxes in the UI

  const handleSubmit = () => {
    // Validate form
    if (!formData.service) {
      alert('Please enter a name for the service');
      return;
    }

    // Prepare expiry data
    const expiry = formData.expiryType === 'VisitDate'
      ? { tag: 'VisitDate', contents: formData.expiryTime }
      : { tag: 'InstantExpiry', contents: parseInt(formData.expiryMinutes) };

    // Format operational date if both dates are provided
    let operationalDate = null;
    if (formData.operationalDate?.startDate && formData.operationalDate?.eneDate) {
      operationalDate = {
        startDate: formData.operationalDate.startDate,
        eneDate: formData.operationalDate.eneDate
      };
    }

    // Prepare service data
    const serviceData = {
      service: formData.service,
      shortDesc: formData.shortDesc,
      operationalDays: formData.operationalDays,
      operationalDate,
      maxVerification: formData.maxVerification,
      allowFutureBooking: formData.allowFutureBooking,
      allowCancellation: formData.allowCancellation,
      expiry,
      businessHours: formData.businessHours,
      placesId: activePlace.id
    };

    // Save service
    if (service) {
      updateService(service.id, serviceData);
    } else {
      addService(serviceData);
    }

    onHide();
  };

  return (
    <Modal show={show} onHide={onHide} size="lg">
      <Modal.Header closeButton>
        <Modal.Title>
          {service ? 'Edit Service' : 'Add Service'}
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
                  name="service"
                  value={formData.service}
                  onChange={handleChange}
                  placeholder="e.g., Museum Entry"
                />
              </Form.Group>
            </Col>
            <Col md={6}>
              <Form.Group>
                <Form.Label>Max Verification</Form.Label>
                <Form.Control
                  type="number"
                  name="maxVerification"
                  value={formData.maxVerification}
                  onChange={handleChange}
                  min="1"
                />
                <Form.Text className="text-muted">
                  Maximum number of times this ticket can be verified
                </Form.Text>
              </Form.Group>
            </Col>
          </Row>

          <Form.Group className="mb-3">
            <Form.Label>Description</Form.Label>
            <Form.Control
              as="textarea"
              name="shortDesc"
              value={formData.shortDesc}
              onChange={handleChange}
              rows={2}
              placeholder="Enter a description of the service"
            />
          </Form.Group>

          <Row className="mb-3">
            <Col md={6}>
              <Form.Group>
                <Form.Check
                  type="checkbox"
                  id="allowFutureBooking"
                  name="allowFutureBooking"
                  label="Allow Future Booking"
                  checked={formData.allowFutureBooking}
                  onChange={handleChange}
                />
              </Form.Group>
            </Col>
            <Col md={6}>
              <Form.Group>
                <Form.Check
                  type="checkbox"
                  id="allowCancellation"
                  name="allowCancellation"
                  label="Allow Cancellation"
                  checked={formData.allowCancellation}
                  onChange={handleChange}
                />
              </Form.Group>
            </Col>
          </Row>

          <Form.Group className="mb-3">
            <Form.Label>Operational Days</Form.Label>
            <div>
              {getDaysOfWeek().map(day => (
                <Form.Check
                  key={day}
                  type="checkbox"
                  id={`day-${day}`}
                  label={day}
                  checked={formData.operationalDays.includes(day)}
                  onChange={() => handleDayChange(day)}
                  inline
                />
              ))}
            </div>
            <Form.Text className="text-muted">
              Operational days can be left empty. If no days are selected, the service will be available on all days.
            </Form.Text>
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Operational Dates</Form.Label>
            <div className="mb-2">
              <Form.Text className="text-muted">
                Leave empty to make the service available without date restrictions
              </Form.Text>
            </div>
            <Row>
              <Col md={5}>
                <Form.Label>Start Date</Form.Label>
                <Form.Control
                  type="date"
                  name="operationalDateStart"
                  value={formData.operationalDate?.startDate || ''}
                  onChange={(e) => {
                    const date = e.target.value;
                    setFormData(prev => ({
                      ...prev,
                      operationalDate: {
                        ...prev.operationalDate,
                        startDate: date,
                        eneDate: date // Set end date same as start date by default
                      }
                    }));
                  }}
                  placeholder="Select start date"
                />
              </Col>
              <Col md={5}>
                <Form.Label>End Date</Form.Label>
                <Form.Control
                  type="date"
                  name="operationalDateEnd"
                  value={formData.operationalDate?.eneDate || ''}
                  onChange={(e) => {
                    setFormData(prev => ({
                      ...prev,
                      operationalDate: {
                        ...prev.operationalDate,
                        eneDate: e.target.value
                      }
                    }));
                  }}
                  min={formData.operationalDate?.startDate || ''}
                  placeholder="Select end date"
                />
              </Col>
              <Col md={2} className="d-flex align-items-end">
                <Button 
                  variant="outline-danger" 
                  size="sm"
                  className="mb-3"
                  onClick={() => {
                    setFormData(prev => ({
                      ...prev,
                      operationalDate: { startDate: '', eneDate: '' }
                    }));
                  }}
                >
                  Clear Dates
                </Button>
              </Col>
            </Row>
          </Form.Group>

          <Row className="mb-3">
            <Col md={6}>
              <Form.Group>
                <Form.Label>Expiry Type</Form.Label>
                <Form.Select
                  name="expiryType"
                  value={formData.expiryType}
                  onChange={handleChange}
                >
                  <option value="VisitDate">Visit Date</option>
                  <option value="InstantExpiry">Instant Expiry</option>
                </Form.Select>
              </Form.Group>
            </Col>
            <Col md={6}>
              {formData.expiryType === 'VisitDate' ? (
                <Form.Group>
                  <Form.Label>Expiry Time</Form.Label>
                  <Form.Control
                    type="time"
                    name="expiryTime"
                    value={formData.expiryTime}
                    onChange={handleChange}
                  />
                </Form.Group>
              ) : (
                <Form.Group>
                  <Form.Label>Expiry Minutes</Form.Label>
                  <Form.Control
                    type="number"
                    name="expiryMinutes"
                    value={formData.expiryMinutes}
                    onChange={handleChange}
                    min="1"
                  />
                </Form.Group>
              )}
            </Col>
          </Row>

          <Form.Group className="mb-3">
            <Form.Label>Business Hours</Form.Label>
            <div className="border rounded p-3" style={{ maxHeight: '200px', overflowY: 'auto' }}>
              {activePlace && activePlace.businessHours && activePlace.businessHours.length > 0 ? (
                activePlace.businessHours.map(hour => (
                  <Form.Check
                    key={hour.id}
                    type="checkbox"
                    id={hour.id}
                    label={
                      <>
                        {hour.name ? (
                          <>
                            {hour.name}
                            <span className="text-muted ms-2">({hour.id})</span>
                          </>
                        ) : (
                          hour.id
                        )}
                      </>
                    }
                    checked={formData.businessHours.includes(hour.id)}
                    onChange={() => {
                      const updatedHours = formData.businessHours.includes(hour.id)
                        ? formData.businessHours.filter(id => id !== hour.id)
                        : [...formData.businessHours, hour.id];
                      setFormData({ ...formData, businessHours: updatedHours });
                    }}
                    className="mb-2"
                  />
                ))
              ) : (
                <p className="text-muted mb-0">No business hours available. Please create some first.</p>
              )}
            </div>
            {activePlace && activePlace.businessHours && activePlace.businessHours.length > 0 && (
              <div className="d-flex justify-content-between mt-2">
                <Form.Text className="text-muted">
                  Select all applicable business hours for this service
                </Form.Text>
                <Button
                  variant="link"
                  size="sm"
                  onClick={() => {
                    // Toggle all: If all are selected, deselect all. Otherwise, select all.
                    const allIds = activePlace.businessHours.map(hour => hour.id);
                    const allSelected = allIds.every(id => formData.businessHours.includes(id));
                    setFormData({
                      ...formData,
                      businessHours: allSelected ? [] : allIds
                    });
                  }}
                >
                  {formData.businessHours.length === activePlace.businessHours.length ? 'Deselect All' : 'Select All'}
                </Button>
              </div>
            )}
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Cancel
        </Button>
        <Button variant="primary" onClick={handleSubmit}>
          {service ? 'Update' : 'Add'}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default ServiceModal;
