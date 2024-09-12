import express from 'express';
import multer from 'multer';
import path from 'path';
import { fileURLToPath } from 'url';
import { convertStlToAscii, convertStlToGif } from './stlToAscii.js';
import cors from 'cors';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
const upload = multer({ dest: path.join(__dirname, '..', 'uploads') });

app.use(cors());
app.use((req, res, next) => {
    if (req.url.endsWith('.js')) {
      res.type('application/javascript');
    }
    next();
  });
  
  // Update the static file serving to point to the correct directory
  app.use(express.static(path.join(__dirname, '..', 'public')));
  
  app.use(express.json());

app.post('/convert', upload.single('stl'), async (req, res) => {
    if (!req.file) {
        return res.status(400).send('No file uploaded.');
    }

    const width = parseInt(req.body.width) || 80;
    const height = parseInt(req.body.height) || 40;

    try {
        console.log(`Converting file: ${req.file.path}`);
        console.log(`Dimensions: ${width}x${height}`);
        const frames = await convertStlToAscii(req.file.path, width, height);
        console.log(`Conversion completed. ${frames.length} frames generated.`);
        res.json(frames);
    } catch (error) {
        console.error('Conversion error:', error);
        res.status(500).json({ error: 'Error converting STL to ASCII', details: error.message });
    }
});

app.post('/convert-gif', upload.single('stl'), async (req, res) => {
    if (!req.file) {
        return res.status(400).send('No file uploaded.');
    }

    const width = parseInt(req.body.width) || 80;
    const height = parseInt(req.body.height) || 40;

    try {
        console.log(`Converting file to GIF: ${req.file.path}`);
        console.log(`Dimensions: ${width}x${height}`);
        const gifBuffer = await convertStlToGif(req.file.path, width, height);
        console.log('GIF conversion completed.');
        res.contentType('image/gif');
        res.send(gifBuffer);
    } catch (error) {
        console.error('GIF Conversion error:', error);
        res.status(500).json({ error: 'Error converting STL to GIF', details: error.message });
    }
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => console.log(`Server running on port ${PORT}`));
