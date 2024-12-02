# 3D to ASCII Art Converter

A web-based tool that converts 3D models into ASCII art representations in real-time. This application allows users to view, rotate, and export 3D models as ASCII art with various customization options.

## Features

- **Real-time 3D to ASCII Conversion**: View 3D models as ASCII art in real-time
- **Multiple File Format Support**: Supports STL, OBJ, FBX, GLTF, and GLB files
- **Interactive Controls**:
  - Model rotation with mouse control
  - Auto-rotation toggle
  - Light/Dark mode switch
  - Text color customization
- **Export Options**:
  - Screenshot capture
  - Text file export
  - Animated SVG export
  - Multi-frame ASCII animation

## Tech Stack

- Frontend:
  - Three.js for 3D rendering
  - Custom ASCII effect implementation
  - HTML5 Canvas for export functionality
- Backend:
  - Node.js
  - Express
  - Python (via Pyodide) for image processing
- Additional Libraries:
  - `multer` for file uploads
  - `gif.js` for GIF generation
  - Various Three.js loaders (STL, OBJ, FBX, GLTF)

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/3d-ascii-converter.git
cd 3d-ascii-converter
```

2. Install dependencies:
```bash
npm install
```

3. Start the server:
```bash
npm start
```

4. Open your browser and navigate to `http://localhost:3000`

## Usage

1. Click the "Choose 3D Model" button to select your 3D file
2. Use mouse controls to rotate the view:
   - Left click + drag to rotate
   - Right click + drag to pan
   - Scroll to zoom
3. Use the control buttons to:
   - Toggle auto-rotation
   - Switch between light and dark mode
   - Change ASCII text color
   - Export the current view or animation

## Export Options

### Screenshot
- Captures the current view as a PNG file
- Preserves ASCII characters and current color scheme

### Text File
- Exports the current view as a plain text file
- Maintains exact ASCII character positioning

### Animated SVG
- Creates an animated SVG of the model rotating
- Preserves ASCII art style with smooth animation
- Configurable animation duration and frame count

## Customization

### Color Schemes
- Default light mode: Black text on white background
- Default dark mode: White text on black background
- Custom color picker for ASCII text
- Available colors: White, Yellow, Green, Cyan, Magenta, Red, Blue, Orange

### ASCII Characters
The application uses a custom set of ASCII characters for different shading levels:
```
' .:-=+*#%@'  (From lightest to darkest)
```

## Browser Compatibility

Tested and working on:
- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+

## Known Limitations

- Large 3D models may take longer to process
- Animation performance depends on system capabilities
- Some complex models may result in reduced ASCII detail
- GIF export currently limited to smaller frame counts

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Three.js community for 3D rendering capabilities
- ASCII art community for inspiration and character sets
- Contributors and testers who helped improve the project