import { STLLoader } from 'three/examples/jsm/loaders/STLLoader.js';
import * as THREE from 'three';
import fs from 'fs';

function createRotatingAsciiModel(geometry, width = 120, height = 60) {
    console.log('Creating rotating ASCII model...');
    console.log(`Geometry vertices: ${geometry.attributes.position.count}`);
    
    const positions = geometry.attributes.position.array;
    const normals = geometry.attributes.normal.array;

    // Calculate bounding box
    const bbox = new THREE.Box3().setFromBufferAttribute(geometry.attributes.position);
    const center = new THREE.Vector3();
    bbox.getCenter(center);
    const size = bbox.getSize(new THREE.Vector3());
    const maxDim = Math.max(size.x, size.y, size.z);

    console.log(`Bounding box: min(${bbox.min.x}, ${bbox.min.y}, ${bbox.min.z}), max(${bbox.max.x}, ${bbox.max.y}, ${bbox.max.z})`);
    console.log(`Center: (${center.x}, ${center.y}, ${center.z})`);
    console.log(`Max dimension: ${maxDim}`);

    // Calculate scaling factors
    const modelAspectRatio = size.x / size.y;
    const outputAspectRatio = width / height;
    let scaleX, scaleY, offsetX, offsetY;
    if (modelAspectRatio > outputAspectRatio) {
        scaleX = width / size.x * 0.8;
        scaleY = scaleX;
        offsetX = width * 0.1;
        offsetY = (height - size.y * scaleY) / 2;
    } else {
        scaleY = height / size.y * 0.8;
        scaleX = scaleY;
        offsetX = (width - size.x * scaleX) / 2;
        offsetY = height * 0.1;
    }

    // Function to rotate a point around Y and X axes
    function rotateYX(point, angleY, angleX) {
        let rotated = new THREE.Vector3(point.x, point.y, point.z);
        rotated.applyAxisAngle(new THREE.Vector3(0, 1, 0), angleY);
        rotated.applyAxisAngle(new THREE.Vector3(1, 0, 0), angleX);
        return rotated;
    }

    // Function to apply Sobel filter (unchanged)
    function applySobelFilter(depthMap) {
        const sobelX = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]];
        const sobelY = [[-1, -2, -1], [0, 0, 0], [1, 2, 1]];
        const edgeMap = new Float32Array(width * height);

        for (let y = 1; y < height - 1; y++) {
            for (let x = 1; x < width - 1; x++) {
                let pixelX = 0, pixelY = 0;
                for (let j = -1; j <= 1; j++) {
                    for (let i = -1; i <= 1; i++) {
                        const idx = (y + j) * width + (x + i);
                        pixelX += depthMap[idx] * sobelX[j + 1][i + 1];
                        pixelY += depthMap[idx] * sobelY[j + 1][i + 1];
                    }
                }
                edgeMap[y * width + x] = Math.sqrt(pixelX * pixelX + pixelY * pixelY);
            }
        }
        return edgeMap;
    }

    // Function to detect edges based on normal differences (unchanged)
    function detectNormalEdges(normalMap) {
        const edgeMap = new Float32Array(width * height);
        const edgeThreshold = 0.1;

        for (let y = 1; y < height - 1; y++) {
            for (let x = 1; x < width - 1; x++) {
                const idx = y * width + x;
                const normal = new THREE.Vector3(normalMap[idx * 3], normalMap[idx * 3 + 1], normalMap[idx * 3 + 2]);
                
                let maxAngleDiff = 0;
                for (let j = -1; j <= 1; j++) {
                    for (let i = -1; i <= 1; i++) {
                        if (i === 0 && j === 0) continue;
                        const neighborIdx = (y + j) * width + (x + i);
                        const neighborNormal = new THREE.Vector3(normalMap[neighborIdx * 3], normalMap[neighborIdx * 3 + 1], normalMap[neighborIdx * 3 + 2]);
                        const angleDiff = normal.angleTo(neighborNormal);
                        maxAngleDiff = Math.max(maxAngleDiff, angleDiff);
                    }
                }
                edgeMap[idx] = Math.min(maxAngleDiff / edgeThreshold, 1);
            }
        }
        return edgeMap;
    }

    // Function to create a single frame
    function createFrame(angleY, angleX) {
        const depthMap = new Float32Array(width * height);
        const normalMap = new Float32Array(width * height * 3);
        const colorMap = new Float32Array(width * height * 3);

        // Project 3D points to 2D and calculate depth, normal, and color maps
        for (let i = 0; i < positions.length; i += 3) {
            const vertex = new THREE.Vector3(positions[i], positions[i+1], positions[i+2]).sub(center);
            const normal = new THREE.Vector3(normals[i], normals[i+1], normals[i+2]);
            
            const rotatedVertex = rotateYX(vertex, angleY, angleX);
            const rotatedNormal = rotateYX(normal, angleY, angleX);

            const x = Math.floor(rotatedVertex.x * scaleX + offsetX);
            const y = Math.floor(height - (rotatedVertex.y * scaleY + offsetY));

            if (x >= 0 && x < width && y >= 0 && y < height) {
                const index = y * width + x;
                if (rotatedVertex.z < depthMap[index] || depthMap[index] === 0) {
                    depthMap[index] = rotatedVertex.z;
                    normalMap[index * 3] = rotatedNormal.x;
                    normalMap[index * 3 + 1] = rotatedNormal.y;
                    normalMap[index * 3 + 2] = rotatedNormal.z;
                    // Generate a color based on position (you can modify this to use actual colors if available)
                    colorMap[index * 3] = (rotatedVertex.x / size.x + 0.5);
                    colorMap[index * 3 + 1] = (rotatedVertex.y / size.y + 0.5);
                    colorMap[index * 3 + 2] = (rotatedVertex.z / size.z + 0.5);
                }
            }
        }

        const sobelEdgeMap = applySobelFilter(depthMap);
        const normalEdgeMap = detectNormalEdges(normalMap);

        const output = new Array(width * height);
        const asciiChars = ' .,:;i1tfLCG08@';

        for (let i = 0; i < width * height; i++) {
            const normal = new THREE.Vector3(normalMap[i * 3], normalMap[i * 3 + 1], normalMap[i * 3 + 2]);
            const luminance = Math.max(0, normal.dot(new THREE.Vector3(0, 0, 1)));
            const sobelEdgeFactor = Math.min(sobelEdgeMap[i] / 0.1, 1);
            const normalEdgeFactor = normalEdgeMap[i];
            
            const edgeFactor = Math.max(sobelEdgeFactor, normalEdgeFactor);
            const detailFactor = (luminance * 0.3 + edgeFactor * 0.7);

            // Apply depth-based effects
            const depth = (depthMap[i] - bbox.min.z) / (bbox.max.z - bbox.min.z);
            const depthFactor = Math.exp(-depth * 2); // Adjust the multiplier to control depth falloff

            // Combine all factors
            const combinedFactor = detailFactor * depthFactor;
            
            const charIndex = Math.floor(combinedFactor * (asciiChars.length - 1));
            output[i] = asciiChars[charIndex];
        }

        return { ascii: output.join(''), colorMap: colorMap };
    }

    // Generate frames for a full rotation
    const frames = [];
    const rotationSteps = 120;
    for (let i = 0; i < rotationSteps; i++) {
        const angleY = (Math.PI * 2 * i) / rotationSteps;
        const angleX = (Math.PI / 4) * Math.sin((Math.PI * 2 * i) / rotationSteps);
        frames.push(createFrame(angleY, angleX));
    }

    console.log(`Generated ${frames.length} frames`);
    console.log(`Sample frame:\n${frames[0].ascii}`);

    return frames;
}

function convertStlToAscii(filePath, width = 120, height = 60) {
    return new Promise((resolve, reject) => {
        console.log(`Converting STL file: ${filePath}`);
        const loader = new STLLoader();
        const stlData = fs.readFileSync(filePath);
        
        try {
            const geometry = loader.parse(stlData.buffer);
            console.log('STL file parsed successfully');
            const frames = createRotatingAsciiModel(geometry, width, height);
            console.log(`Conversion completed. ${frames.length} frames generated.`);
            resolve(frames);
        } catch (error) {
            console.error('Error during STL conversion:', error);
            reject(error);
        }
    });
}

async function convertAsciiToGif(asciiFrames, width, height) {
    const encoder = new GIFEncoder(width * 6, height * 6);
    encoder.start();
    encoder.setRepeat(0);   // 0 for repeat, -1 for no-repeat
    encoder.setDelay(100);  // frame delay in ms
    encoder.setQuality(10); // image quality. 10 is default.

    const pixelSize = 6; // Size of each ASCII character in pixels

    for (let frame of asciiFrames) {
        const imageData = new Uint8ClampedArray(width * pixelSize * height * pixelSize * 4);

        const lines = frame.split('\n');
        for (let y = 0; y < lines.length; y++) {
            for (let x = 0; x < lines[y].length; x++) {
                const char = lines[y][x];
                const brightness = char === ' ' ? 0 : 255;

                for (let py = 0; py < pixelSize; py++) {
                    for (let px = 0; px < pixelSize; px++) {
                        const index = ((y * pixelSize + py) * width * pixelSize + (x * pixelSize + px)) * 4;
                        imageData[index] = brightness;     // R
                        imageData[index + 1] = brightness; // G
                        imageData[index + 2] = brightness; // B
                        imageData[index + 3] = 255;        // A
                    }
                }
            }
        }

        encoder.addFrame(imageData);
    }

    encoder.finish();
    return encoder.out.getData();
}

async function convertStlToGif(filePath, width = 120, height = 60) {
    try {
        const asciiFrames = await convertStlToAscii(filePath, width, height);
        const gifBuffer = await convertAsciiToGif(asciiFrames, width, height);
        return gifBuffer;
    } catch (error) {
        console.error('Error during STL to GIF conversion:', error);
        throw error;
    }
}

export { convertStlToAscii, convertStlToGif };