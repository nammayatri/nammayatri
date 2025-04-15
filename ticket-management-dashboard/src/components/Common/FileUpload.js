import React, { useRef } from 'react';
import { Button } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';

const FileUpload = () => {
  const { loadJsonFromFile } = useTicket();
  const fileInputRef = useRef(null);

  const handleButtonClick = () => {
    fileInputRef.current.click();
  };

  const handleFileChange = (event) => {
    const file = event.target.files[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        const jsonData = JSON.parse(e.target.result);
        const success = loadJsonFromFile(jsonData);
        
        if (success) {
          alert('JSON data loaded successfully!');
        } else {
          alert('Error loading JSON data. Please check the file format.');
        }
      } catch (error) {
        console.error('Error parsing JSON file:', error);
        alert('Error parsing JSON file: ' + error.message);
      }
    };
    
    reader.readAsText(file);
    
    // Reset the file input so the same file can be selected again
    event.target.value = null;
  };

  return (
    <>
      <input
        type="file"
        ref={fileInputRef}
        style={{ display: 'none' }}
        accept=".json"
        onChange={handleFileChange}
      />
      <Button 
        variant="outline-secondary" 
        size="sm" 
        onClick={handleButtonClick}
        className="me-2"
      >
        Load JSON
      </Button>
    </>
  );
};

export default FileUpload;
