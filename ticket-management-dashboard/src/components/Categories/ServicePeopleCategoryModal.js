import React, { useState, useEffect } from 'react';
import { Modal, Button, Form, Row, Col, Table } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import { v4 as uuidv4 } from 'uuid';

const ServicePeopleCategoryModal = ({ show, onHide, peopleCategory }) => {
  const { addServicePeopleCategory, updateServicePeopleCategory } = useTicket();
  const [formData, setFormData] = useState({
    name: '',
    description: '',
    priceAmount: '',
    priceCurrency: 'INR',
    pricingType: 'AllDays',
    timeBounds: { tag: 'Unbounded' },
    vendorSplitDetails: []
  });

  const [timeBoundType, setTimeBoundType] = useState('Unbounded');
  const [selectedDate, setSelectedDate] = useState('');
  
  // Define weekday order constant
  const WEEKDAYS = [
    'monday',
    'tuesday',
    'wednesday',
    'thursday',
    'friday',
    'saturday',
    'sunday'
  ];

  const [weekdayTimes, setWeekdayTimes] = useState({
    monday: [['09:00:00', '18:00:00']],
    tuesday: [['09:00:00', '18:00:00']],
    wednesday: [['09:00:00', '18:00:00']],
    thursday: [['09:00:00', '18:00:00']],
    friday: [['09:00:00', '18:00:00']],
    saturday: [['09:00:00', '18:00:00']],
    sunday: [['09:00:00', '18:00:00']]
  });
  const [dayTimeRanges, setDayTimeRanges] = useState([['09:00:00', '18:00:00']]);

  // Vendor split state
  const [vendorId, setVendorId] = useState('');
  const [splitAmount, setSplitAmount] = useState('');

  // Initialize form data when editing an existing people category
  useEffect(() => {
    if (peopleCategory) {
      const btypeType = peopleCategory.timeBounds?.tag || 'Unbounded';
      setTimeBoundType(btypeType);
      
      if (btypeType === 'BoundedByDay' && peopleCategory.timeBounds?.contents) {
        // Handle BoundedByDay format
        const [date, ranges] = peopleCategory.timeBounds.contents[0] || [];
        setSelectedDate(date || '');
        setDayTimeRanges(ranges || [['09:00:00', '18:00:00']]);
      } else if (btypeType === 'BoundedByWeekday' && peopleCategory.timeBounds?.contents) {
        // Handle BoundedByWeekday format
        setWeekdayTimes(peopleCategory.timeBounds.contents);
      }

      setFormData({
        name: peopleCategory.name || '',
        description: peopleCategory.description || '',
        priceAmount: peopleCategory.priceAmount?.toString() || '',
        priceCurrency: peopleCategory.priceCurrency || 'INR',
        pricingType: peopleCategory.pricingType || 'AllDays',
        timeBounds: peopleCategory.timeBounds || { tag: 'Unbounded' },
        vendorSplitDetails: peopleCategory.vendorSplitDetails || []
      });
    } else {
      // Reset form for new people category
      setFormData({
        name: '',
        description: '',
        priceAmount: '',
        priceCurrency: 'INR',
        pricingType: 'AllDays',
        timeBounds: { tag: 'Unbounded' },
        vendorSplitDetails: []
      });
      setTimeBoundType('Unbounded');
      setSelectedDate('');
      setDayTimeRanges([['09:00:00', '18:00:00']]);
      setWeekdayTimes({
        monday: [['09:00:00', '18:00:00']],
        tuesday: [['09:00:00', '18:00:00']],
        wednesday: [['09:00:00', '18:00:00']],
        thursday: [['09:00:00', '18:00:00']],
        friday: [['09:00:00', '18:00:00']],
        saturday: [['09:00:00', '18:00:00']],
        sunday: [['09:00:00', '18:00:00']]
      });
      setVendorId('');
      setSplitAmount('');
    }
  }, [peopleCategory, show]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  const handleTimeBoundTypeChange = (e) => {
    const type = e.target.value;
    setTimeBoundType(type);
    
    if (type === 'Unbounded') {
      setFormData(prev => ({
        ...prev,
        timeBounds: { tag: 'Unbounded' }
      }));
    } else if (type === 'BoundedByDay') {
      setFormData(prev => ({
        ...prev,
        timeBounds: {
          tag: 'BoundedByDay',
          contents: [[selectedDate, dayTimeRanges]]
        }
      }));
    } else if (type === 'BoundedByWeekday') {
      setFormData(prev => ({
        ...prev,
        timeBounds: {
          tag: 'BoundedByWeekday',
          contents: weekdayTimes
        }
      }));
    }
  };

  const handleWeekdayTimeChange = (day, index, timeType, value) => {
    const updatedWeekdayTimes = { ...weekdayTimes };
    const timeRanges = [...updatedWeekdayTimes[day]];
    timeRanges[index] = [
      timeType === 'start' ? `${value}:00` : timeRanges[index][0],
      timeType === 'end' ? `${value}:00` : timeRanges[index][1]
    ];
    updatedWeekdayTimes[day] = timeRanges;
    setWeekdayTimes(updatedWeekdayTimes);

    if (timeBoundType === 'BoundedByWeekday') {
      setFormData(prev => ({
        ...prev,
        timeBounds: {
          tag: 'BoundedByWeekday',
          contents: updatedWeekdayTimes
        }
      }));
    }
  };

  const handleDayTimeChange = (index, timeType, value) => {
    const updatedRanges = [...dayTimeRanges];
    updatedRanges[index] = [
      timeType === 'start' ? `${value}:00` : updatedRanges[index][0],
      timeType === 'end' ? `${value}:00` : updatedRanges[index][1]
    ];
    setDayTimeRanges(updatedRanges);

    if (timeBoundType === 'BoundedByDay' && selectedDate) {
      setFormData(prev => ({
        ...prev,
        timeBounds: {
          tag: 'BoundedByDay',
          contents: [[selectedDate, updatedRanges]]
        }
      }));
    }
  };

  const handleDateChange = (value) => {
    setSelectedDate(value);
    if (timeBoundType === 'BoundedByDay') {
      setFormData(prev => ({
        ...prev,
        timeBounds: {
          tag: 'BoundedByDay',
          contents: [[value, dayTimeRanges]]
        }
      }));
    }
  };

  const handleAddVendorSplit = () => {
    if (!vendorId || !splitAmount || isNaN(parseFloat(splitAmount))) {
      alert('Please enter valid vendor ID and split amount');
      return;
    }

    const newSplit = {
      vendorId,
      splitAmount: parseFloat(splitAmount),
      splitType: 'FIXED'
    };

    setFormData(prev => ({
      ...prev,
      vendorSplitDetails: [...prev.vendorSplitDetails, newSplit]
    }));

    // Reset vendor split inputs
    setVendorId('');
    setSplitAmount('');
  };

  const handleRemoveVendorSplit = (index) => {
    setFormData(prev => ({
      ...prev,
      vendorSplitDetails: prev.vendorSplitDetails.filter((_, i) => i !== index)
    }));
  };

  const validateVendorSplits = () => {
    if (formData.vendorSplitDetails.length === 0) return true;
    
    const totalSplit = formData.vendorSplitDetails.reduce(
      (sum, split) => sum + split.splitAmount,
      0
    );
    
    const priceAmount = parseFloat(formData.priceAmount);
    return totalSplit <= priceAmount;
  };

  const handleSubmit = () => {
    // Validate form
    if (!formData.name) {
      alert('Please enter a name for the people category');
      return;
    }

    if (!formData.priceAmount || isNaN(parseFloat(formData.priceAmount))) {
      alert('Please enter a valid price amount');
      return;
    }

    if (!validateVendorSplits()) {
      alert('Total vendor split amounts must be less than or equal to the price amount');
      return;
    }

    // Prepare people category data
    const peopleCategoryData = {
      name: formData.name,
      description: formData.description,
      priceAmount: parseFloat(formData.priceAmount),
      priceCurrency: formData.priceCurrency,
      pricingType: formData.pricingType,
      timeBounds: formData.timeBounds,
      vendorSplitDetails: formData.vendorSplitDetails
    };

    // Save people category
    if (peopleCategory) {
      updateServicePeopleCategory(peopleCategory.id, peopleCategoryData);
    } else {
      addServicePeopleCategory(peopleCategoryData);
    }

    onHide();
  };

  return (
    <Modal show={show} onHide={onHide} size="lg">
      <Modal.Header closeButton>
        <Modal.Title>
          {peopleCategory ? 'Edit People Category' : 'Add People Category'}
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group className="mb-3">
            <Form.Label>Name <span className="text-danger">*</span></Form.Label>
            <Form.Control
              type="text"
              name="name"
              value={formData.name}
              onChange={handleChange}
              placeholder="e.g., Adult"
            />
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Description</Form.Label>
            <Form.Control
              as="textarea"
              name="description"
              value={formData.description}
              onChange={handleChange}
              rows={2}
              placeholder="Enter a description of the people category"
            />
          </Form.Group>

          <Row className="mb-3">
            <Col md={8}>
              <Form.Group>
                <Form.Label>Price Amount <span className="text-danger">*</span></Form.Label>
                <Form.Control
                  type="number"
                  name="priceAmount"
                  value={formData.priceAmount}
                  onChange={handleChange}
                  min="0"
                  step="0.01"
                  placeholder="e.g., 50.00"
                />
              </Form.Group>
            </Col>
            <Col md={4}>
              <Form.Group>
                <Form.Label>Currency</Form.Label>
                <Form.Select
                  name="priceCurrency"
                  value={formData.priceCurrency}
                  onChange={handleChange}
                >
                  <option value="INR">INR</option>
                  <option value="USD">USD</option>
                  <option value="EUR">EUR</option>
                  <option value="GBP">GBP</option>
                </Form.Select>
              </Form.Group>
            </Col>
          </Row>

          <Form.Group className="mb-3">
            <Form.Label>Pricing Type</Form.Label>
            <Form.Select
              name="pricingType"
              value={formData.pricingType}
              onChange={handleChange}
            >
              <option value="AllDays">All Days</option>
              <option value="SameDay">Same Day</option>
            </Form.Select>
            <Form.Text className="text-muted">
              AllDays: Same price for all days, SameDay: Price applies only for the same day
            </Form.Text>
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Time Bounds</Form.Label>
            <Form.Select
              value={timeBoundType}
              onChange={handleTimeBoundTypeChange}
              className="mb-2"
            >
              <option value="Unbounded">Unbounded (No Time Restrictions)</option>
              <option value="BoundedByDay">Bounded By Day (Specific Date)</option>
              <option value="BoundedByWeekday">Bounded By Weekday (Weekly Schedule)</option>
            </Form.Select>

            {timeBoundType === 'BoundedByDay' && (
              <>
                <Form.Group className="mb-2">
                  <Form.Label>Date</Form.Label>
                  <Form.Control
                    type="date"
                    value={selectedDate}
                    onChange={(e) => handleDateChange(e.target.value)}
                  />
                </Form.Group>
                <Row>
                  <Col>
                    <Form.Group>
                      <Form.Label>Start Time</Form.Label>
                      <Form.Control
                        type="time"
                        value={dayTimeRanges[0][0].split(':').slice(0, 2).join(':')}
                        onChange={(e) => handleDayTimeChange(0, 'start', e.target.value)}
                      />
                    </Form.Group>
                  </Col>
                  <Col>
                    <Form.Group>
                      <Form.Label>End Time</Form.Label>
                      <Form.Control
                        type="time"
                        value={dayTimeRanges[0][1].split(':').slice(0, 2).join(':')}
                        onChange={(e) => handleDayTimeChange(0, 'end', e.target.value)}
                      />
                    </Form.Group>
                  </Col>
                </Row>
              </>
            )}

            {timeBoundType === 'BoundedByWeekday' && (
              <div className="border rounded p-3">
                {WEEKDAYS.map((day) => (
                  <div key={day} className="mb-3">
                    <Form.Label className="text-capitalize">{day}</Form.Label>
                    <Row>
                      <Col>
                        <Form.Control
                          type="time"
                          value={weekdayTimes[day][0][0].split(':').slice(0, 2).join(':')}
                          onChange={(e) => handleWeekdayTimeChange(day, 0, 'start', e.target.value)}
                        />
                      </Col>
                      <Col>
                        <Form.Control
                          type="time"
                          value={weekdayTimes[day][0][1].split(':').slice(0, 2).join(':')}
                          onChange={(e) => handleWeekdayTimeChange(day, 0, 'end', e.target.value)}
                        />
                      </Col>
                    </Row>
                  </div>
                ))}
              </div>
            )}
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Vendor Split Details</Form.Label>
            <div className="border rounded p-3 mb-2">
              <Row className="mb-2">
                <Col>
                  <Form.Control
                    type="text"
                    placeholder="Vendor ID"
                    value={vendorId}
                    onChange={(e) => setVendorId(e.target.value)}
                  />
                </Col>
                <Col>
                  <Form.Control
                    type="number"
                    step="0.01"
                    min="0"
                    placeholder="Split Amount"
                    value={splitAmount}
                    onChange={(e) => setSplitAmount(e.target.value)}
                  />
                </Col>
                <Col xs="auto">
                  <Button onClick={handleAddVendorSplit}>Add Split</Button>
                </Col>
              </Row>

              {formData.vendorSplitDetails.length > 0 ? (
                <Table size="sm">
                  <thead>
                    <tr>
                      <th>Vendor ID</th>
                      <th>Split Amount</th>
                      <th>Type</th>
                      <th>Action</th>
                    </tr>
                  </thead>
                  <tbody>
                    {formData.vendorSplitDetails.map((split, index) => (
                      <tr key={index}>
                        <td>{split.vendorId}</td>
                        <td>{split.splitAmount.toFixed(2)}</td>
                        <td>{split.splitType}</td>
                        <td>
                          <Button
                            variant="danger"
                            size="sm"
                            onClick={() => handleRemoveVendorSplit(index)}
                          >
                            Remove
                          </Button>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                  <tfoot>
                    <tr>
                      <td><strong>Total:</strong></td>
                      <td colSpan="3">
                        <strong>
                          {formData.vendorSplitDetails
                            .reduce((sum, split) => sum + split.splitAmount, 0)
                            .toFixed(2)}
                        </strong>
                      </td>
                    </tr>
                  </tfoot>
                </Table>
              ) : (
                <p className="text-muted mb-0">
                  No vendor splits configured. Default vendor will receive 100% of the amount.
                </p>
              )}
            </div>
            <Form.Text className="text-muted">
              Total vendor split amounts should be less than or equal to the price amount
            </Form.Text>
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Cancel
        </Button>
        <Button variant="primary" onClick={handleSubmit}>
          {peopleCategory ? 'Update' : 'Add'}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default ServicePeopleCategoryModal;
