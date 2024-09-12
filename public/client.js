import { OrbitControls } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/controls/OrbitControls.js';
import { STLLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/STLLoader.js';
import { OBJLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/OBJLoader.js';
import { FBXLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/FBXLoader.js';
import { GLTFLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/GLTFLoader.js';
import { AsciiEffect } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/effects/AsciiEffect.js';

let scene, camera, renderer, effect, myMesh, controls;
let rotateModel = false;
let ASCIIColor = '#ffffff';
let backgroundColor = 'black';
const characters = ' .:-+*=%@#';

function init() {
    scene = new THREE.Scene();
    scene.background = new THREE.Color(0x1a1b26);

    camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
    camera.position.z = 4;

    renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight);

    const pointLight1 = new THREE.PointLight(0xffffff, 1);
    pointLight1.position.set(100, 100, 400);
    scene.add(pointLight1);

    const pointLight2 = new THREE.PointLight(0xffffff, 0.5);
    pointLight2.position.set(-500, 100, -400);
    scene.add(pointLight2);

    createEffect();

    controls = new OrbitControls(camera, effect.domElement);

    document.getElementById('ascii-output').appendChild(effect.domElement);

    window.addEventListener('resize', onWindowResize, false);
}

function createEffect() {
    effect = new AsciiEffect(renderer, characters, { 
        invert: true,
        resolution: 0.25
    });
    effect.setSize(window.innerWidth, window.innerHeight);
    effect.domElement.style.color = ASCIIColor;
    effect.domElement.style.backgroundColor = backgroundColor;
}

function loadModel(file) {
    const reader = new FileReader();
    reader.onload = function (event) {
        const extension = file.name.split('.').pop().toLowerCase();
        let loader;

        switch (extension) {
            case 'stl':
                loader = new STLLoader();
                break;
            case 'obj':
                loader = new OBJLoader();
                break;
            case 'fbx':
                loader = new FBXLoader();
                break;
            case 'gltf':
            case 'glb':
                loader = new GLTFLoader();
                break;
            default:
                console.error('Unsupported file format');
                return;
        }

        loader.load(
            URL.createObjectURL(file),
            function (object) {
                if (myMesh) scene.remove(myMesh);

                if (object.scene) {
                    myMesh = object.scene;
                } else if (object.isGroup) {
                    myMesh = object;
                } else {
                    const material = new THREE.MeshStandardMaterial({ color: 0xc0caf5, flatShading: true, side: THREE.DoubleSide });
                    myMesh = new THREE.Mesh(object, material);
                }

                myMesh.position.set(0, 0, 0);
                myMesh.rotation.x = -Math.PI / 2;

                const bbox = new THREE.Box3().setFromObject(myMesh);
                const size = bbox.getSize(new THREE.Vector3());
                const center = bbox.getCenter(new THREE.Vector3());
                const maxDim = Math.max(size.x, size.y, size.z);
                
                myMesh.scale.multiplyScalar(2 / maxDim);
                myMesh.position.sub(center.multiplyScalar(2 / maxDim));

                camera.position.set(0, 0, 3);
                camera.lookAt(0, 0, 0);
                controls.update();

                scene.add(myMesh);
            },
            undefined,
            function (error) {
                console.error('An error occurred while loading the model:', error);
                showError('Error loading the model. Please try again.');
            }
        );
    };
    reader.readAsArrayBuffer(file);
}

function animate() {
    requestAnimationFrame(animate);
    if (rotateModel && myMesh) {
        myMesh.rotation.z += 0.01;
    }
    effect.render(scene, camera);
}

function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
    effect.setSize(window.innerWidth, window.innerHeight);
}

function showError(message) {
    const errorElement = document.getElementById('error-message') || createErrorElement();
    errorElement.textContent = message;
}

function createErrorElement() {
    const newErrorElement = document.createElement('div');
    newErrorElement.id = 'error-message';
    document.querySelector('.container').appendChild(newErrorElement);
    return newErrorElement;
}

function showProgress(message, progress) {
    const progressContainer = document.getElementById('progress-container') || createProgressContainer();
    const progressBar = document.getElementById('progress-bar');
    const progressText = document.getElementById('progress-text');

    if (progress !== undefined) {
        progressBar.style.width = `${progress}%`;
    }
    progressText.textContent = message;
}

function createProgressContainer() {
    const container = document.createElement('div');
    container.id = 'progress-container';
    container.style.width = '100%';
    container.style.marginTop = '10px';
    container.innerHTML = `
        <div id="progress-bar" style="width: 0%; height: 20px; background-color: #7aa2f7; transition: width 0.3s;"></div>
        <div id="progress-text" style="text-align: center; margin-top: 5px;"></div>
    `;
    document.querySelector('.container').appendChild(container);
    return container;
}

function captureAsciiFrames(frames = 30) {
    const asciiFrames = [];
    const angleStep = (Math.PI * 2) / frames;
    const originalRotation = new THREE.Euler().copy(myMesh.rotation);

    for (let i = 0; i < frames; i++) {
        myMesh.rotation.y = originalRotation.y + i * angleStep;
        renderer.render(scene, camera);
        effect.render(scene, camera);
        
        const asciiContent = effect.domElement.innerText;
        asciiFrames.push(asciiContent);
        
        showProgress('Capturing frames...', (i / frames) * 50);
    }

    myMesh.rotation.copy(originalRotation);
    renderer.render(scene, camera);
    effect.render(scene, camera);

    return asciiFrames;
}

function takeScreenshot() {
    if (!effect || !effect.domElement) {
        console.error('ASCII effect not initialized');
        return;
    }

    // Create a canvas element to draw the ASCII art
    const canvas = document.createElement('canvas');
    canvas.width = effect.domElement.clientWidth;
    canvas.height = effect.domElement.clientHeight;
    const ctx = canvas.getContext('2d');

    // Set the background
    ctx.fillStyle = backgroundColor;
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    // Draw the ASCII art
    ctx.fillStyle = ASCIIColor;
    ctx.font = '9.7561px monospace';
    const lines = effect.domElement.innerText.split('\n');
    lines.forEach((line, index) => {
        ctx.fillText(line, 0, (index + 1) * 9.7561);
    });

    // Convert the canvas to a data URL and trigger download
    const dataURL = canvas.toDataURL('image/png');
    const link = document.createElement('a');
    link.href = dataURL;
    link.download = 'ascii_screenshot.png';
    link.click();
}

function changeTextColor() {
    const colors = [
        '#ffffff', '#ffff00', '#00ff00', '#00ffff', 
        '#ff00ff', '#ff0000', '#0000ff', '#ffa500'
    ];
    const colorNames = [
        'White', 'Yellow', 'Green', 'Cyan', 
        'Magenta', 'Red', 'Blue', 'Orange'
    ];

    let colorTable = '<table>';
    for (let i = 0; i < colors.length; i += 4) {
        colorTable += '<tr>';
        for (let j = i; j < i + 4 && j < colors.length; j++) {
            colorTable += `<td style="background-color: ${colors[j]}; 
                            color: ${colors[j] === '#ffffff' ? '#000000' : '#ffffff'}; 
                            cursor: pointer; padding: 10px;" 
                            onclick="selectColor('${colors[j]}')">${colorNames[j]}</td>`;
        }
        colorTable += '</tr>';
    }
    colorTable += '</table>';

    const colorSelector = document.createElement('div');
    colorSelector.id = 'color-selector';
    colorSelector.innerHTML = colorTable;
    colorSelector.style.position = 'fixed';
    colorSelector.style.top = '50%';
    colorSelector.style.left = '50%';
    colorSelector.style.transform = 'translate(-50%, -50%)';
    colorSelector.style.backgroundColor = '#ffffff';
    colorSelector.style.padding = '20px';
    colorSelector.style.border = '2px solid #000000';

    const closeButton = document.createElement('button');
    closeButton.textContent = 'Close';
    closeButton.style.marginTop = '10px';
    closeButton.onclick = closeColorSelector;
    colorSelector.appendChild(closeButton);

    document.body.appendChild(colorSelector);
}

function generateAnimatedGIF() {
    if (typeof window.GIF === 'undefined') {
        console.error('GIF library is not loaded. Please check your script inclusions.');
        showProgress('Error: GIF library not loaded', 0);
        return;
    }

    if (!myMesh) {
        showProgress('Please load a 3D model first.', 0);
        console.log('No model loaded');
        return;
    }

    console.log('Starting ASCII GIF animation generation process');
    showProgress('Preparing to generate ASCII GIF animation...', 0);

    const frames = 30;
    const originalRotation = myMesh.rotation.z;

    // Create a canvas element to draw our ASCII frames
    const canvas = document.createElement('canvas');
    canvas.width = effect.domElement.clientWidth;
    canvas.height = effect.domElement.clientHeight;
    const ctx = canvas.getContext('2d');

    // Initialize gif.js with local worker
    const gif = new GIF({
        workers: 2,
        quality: 10,
        width: canvas.width,
        height: canvas.height,
        workerScript: '/js/gif.worker.js'
      });

    // Capture ASCII frames
    for (let i = 0; i < frames; i++) {
        myMesh.rotation.z = originalRotation + (i / frames) * Math.PI * 2;
        effect.render(scene, camera);
        
        const asciiContent = effect.domElement.innerText;

        // Clear canvas and set background
        ctx.fillStyle = backgroundColor;
        ctx.fillRect(0, 0, canvas.width, canvas.height);

        // Draw ASCII content
        ctx.fillStyle = ASCIIColor;
        ctx.font = '9.7561px monospace';
        asciiContent.split('\n').forEach((line, index) => {
            ctx.fillText(line, 0, (index + 1) * 9.7561);
        });

        // Add frame to GIF
        gif.addFrame(ctx, {copy: true, delay: 100});
        
        showProgress('Capturing ASCII frames...', (i / frames) * 90);
    }

    // Reset rotation
    myMesh.rotation.z = originalRotation;

    // Render GIF
    gif.on('finished', function(blob) {
        const url = URL.createObjectURL(blob);
        const link = document.createElement('a');
        link.href = url;
        link.download = 'animated_ascii_3d_model.gif';
        link.click();
        URL.revokeObjectURL(url);
        showProgress('ASCII GIF animation generated successfully!', 100);
    });

    gif.render();
}

document.getElementById('download-gif-btn').addEventListener('click', generateAnimatedGIF);

function closeColorSelector() {
    const colorSelector = document.getElementById('color-selector');
    if (colorSelector) {
        document.body.removeChild(colorSelector);
    }
}

function selectColor(color) {
    ASCIIColor = color;
    effect.domElement.style.color = ASCIIColor;
    closeColorSelector();
}

function saveAsTextFile() {
    if (!myMesh) {
        showProgress('Please load a 3D model first.', 0);
        console.log('No model loaded');
        return;
    }

    console.log('Starting text file generation process');
    showProgress('Preparing to generate text file...', 0);

    const frames = 15;
    const angleStep = (Math.PI * 2) / frames;
    let allFrames = '';

    try {
        console.log(`Capturing ${frames} frames`);
        for (let i = 0; i < frames; i++) {
            myMesh.rotation.y = i * angleStep;
            effect.render(scene, camera);
            
            const asciiContent = effect.domElement.textContent;
            const frameHeader = `Frame ${i + 1}:\n${'='.repeat(80)}\n`;
            const formattedContent = asciiContent.split('\n').map(line => line.trimRight()).join('\n');
            allFrames += frameHeader + formattedContent + '\n\n';
            
            console.log(`Captured frame ${i + 1}/${frames}`);
            showProgress('Capturing frames...', (i / frames) * 100);
        }

        console.log('Frame capture complete');
        showProgress('Generating text file...', 90);

        const blob = new Blob([allFrames], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const link = document.createElement('a');
        link.href = url;
        link.download = 'ascii_model_frames.txt';
        link.click();
        URL.revokeObjectURL(url);

        console.log('Text file generation complete');
        showProgress('Text file generated successfully!', 100);

    } catch (error) {
        console.error('Error during text file generation:', error);
        showProgress(`Error: ${error.message}. Please try again.`, 0);
    }
}

function generateAnimatedSVG() {
    if (!myMesh) {
        showProgress('Please load a 3D model first.', 0);
        console.log('No model loaded');
        return;
    }

    console.log('Starting ASCII SVG animation generation process');
    showProgress('Preparing to generate ASCII SVG animation...', 0);

    const frames = 30;
    const svgFrames = [];
    const originalRotation = myMesh.rotation.z;

    // Capture ASCII frames
    for (let i = 0; i < frames; i++) {
        myMesh.rotation.z = originalRotation + (i / frames) * Math.PI * 2;
        effect.render(scene, camera);
        
        const asciiContent = effect.domElement.innerText;
        svgFrames.push(asciiContent);
        
        showProgress('Capturing ASCII frames...', (i / frames) * 90);
    }

    // Reset rotation
    myMesh.rotation.z = originalRotation;

    // Generate SVG with ASCII text
    const svgWidth = effect.domElement.clientWidth;
    const svgHeight = effect.domElement.clientHeight;
    let svgContent = `<svg xmlns="http://www.w3.org/2000/svg" width="${svgWidth}" height="${svgHeight}">
    <style>
        @keyframes rotate {
            0%, 100% { opacity: 1; }
            3% { opacity: 0; }
            97% { opacity: 0; }
        }
        text {
            font-family: monospace;
            font-size: 9.7561px;
            fill: ${ASCIIColor};
        }
    </style>
    <rect width="100%" height="100%" fill="${backgroundColor}"/>
    `;

    svgFrames.forEach((frame, index) => {
        svgContent += `
    <g style="animation: rotate 3.1s linear ${index * (3.1 / frames)}s infinite;">
        ${frame.split('\n').map((line, y) => 
            `<text x="0" y="${(y + 1) * 9.7561}">${escapeHTML(line)}</text>`
        ).join('')}
    </g>`;
    });

    svgContent += '</svg>';

    // Create downloadable SVG file
    const blob = new Blob([svgContent], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'animated_ascii_3d_model.svg';
    link.click();
    URL.revokeObjectURL(url);

    showProgress('ASCII SVG animation generated successfully!', 100);
}

function escapeHTML(str) {
    return str.replace(/&/g, '&amp;')
              .replace(/</g, '&lt;')
              .replace(/>/g, '&gt;')
              .replace(/"/g, '&quot;')
              .replace(/'/g, '&#039;');
}

document.addEventListener('DOMContentLoaded', () => {
    init();
    animate();

    const addEventListenerSafely = (id, event, handler) => {
        const element = document.getElementById(id);
        if (element) {
            element.addEventListener(event, handler);
        } else {
            console.error(`Element with id '${id}' not found in the DOM`);
        }
    };

    addEventListenerSafely('model-file', 'change', (event) => {
        const file = event.target.files[0];
        if (file) {
            const fileNameElement = document.getElementById('file-name');
            if (fileNameElement) {
                fileNameElement.textContent = file.name;
            }
            loadModel(file);
        }
    });

    addEventListenerSafely('rotate-btn', 'click', () => {
        rotateModel = !rotateModel;
    });

    addEventListenerSafely('light-dark-btn', 'click', () => {
        const lightMode = backgroundColor === 'black';
        backgroundColor = lightMode ? 'white' : 'black';
        ASCIIColor = lightMode ? 'black' : 'white';
        if (effect && effect.domElement) {
            effect.domElement.style.color = ASCIIColor;
            effect.domElement.style.backgroundColor = backgroundColor;
        }
    });

    addEventListenerSafely('screenshot-btn', 'click', takeScreenshot);

    addEventListenerSafely('download-txt-btn', 'click', () => {
        if (effect && effect.domElement) {
            const ascii = effect.domElement.textContent;
            const blob = new Blob([ascii], { type: 'text/plain' });
            const url = URL.createObjectURL(blob);
            const link = document.createElement('a');
            link.download = 'ascii_model.txt';
            link.href = url;
            link.click();
            URL.revokeObjectURL(url);
        }
    });

    addEventListenerSafely('change-color-btn', 'click', changeTextColor);
    
    addEventListenerSafely('download-svg-btn', 'click', generateAnimatedSVG);
});

// Add these functions to the global scope
window.selectColor = selectColor;
window.closeColorSelector = closeColorSelector;