# 3D to ASCII Art Converter

A web-based tool that converts 3D models into ASCII art representations in real-time. This application allows users to view, rotate, and export 3D models as ASCII art with various customization options.

## Features

- **Real-time 3D to ASCII Conversion**: View 3D models as ASCII art in real-time
- **Multiple File Format Support**: Supports STL file format (primary format)
- **Interactive Controls**:
  - Model rotation with mouse control
  - Auto-rotation toggle
  - Light/Dark mode switch
  - Text color customization
- **Export Options**:
  - Screenshot capture
  - Text file export
  - Animated SVG export
  - Animated GIF generation

## Tech Stack

- Frontend:
  - Three.js for 3D rendering
  - Custom ASCII effect implementation
  - HTML5 Canvas for export functionality
- Backend:
  - Haskell with Scotty web framework
  - Warp server
  - Binary STL file processing
  - Custom ASCII conversion algorithm
- Additional Libraries:
  - `gif.js` for GIF generation
  - Three.js AsciiEffect for real-time rendering
  - Vector operations for 3D transformations

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/3d-ascii-converter.git
cd 3d-ascii-converter
```

2. Install Haskell dependencies:
```bash
cabal update
cabal build
```

3. Install Node.js dependencies:
```bash
npm install
```

4. Start the server:
```bash
cabal run
```

5. Open your browser and navigate to `http://localhost:3000`

## Usage

1. Click the "Choose 3D Model" button to select your STL file
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

### Animated GIF
- Generates an animated GIF of the rotating model
- Customizable frame rate and quality
- Maintains ASCII character fidelity

## ASCII Implementation Details

The ASCII conversion process uses the following character set for shading:
```
' .:-=+*#%@'  (From lightest to darkest)
```

The conversion algorithm in `STLProcessor.hs` implements:
- Z-buffer rendering
- Perspective projection
- Custom character mapping based on depth
- Triangle rasterization using Bresenham's algorithm

## Browser Compatibility

Tested and working on:
- Chrome 90+
- Firefox 88+
- Safari 14+
- Edge 90+

## Known Limitations

- Currently only supports binary STL files
- Large STL models may take longer to process
- Animation performance depends on system capabilities
- GIF export limited to 30 frames for performance
- ASCII resolution fixed at 80x40 characters

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE file for details.

## Acknowledgments

- Three.js community for 3D rendering capabilities
- ASCII art community for inspiration and character sets
- Haskell community for vector math and binary processing libraries
- Contributors and testers who helped improve the project