import { OrbitControls } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/controls/OrbitControls.js';
import { STLLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/STLLoader.js';
import { OBJLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/OBJLoader.js';
import { FBXLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/FBXLoader.js';
import { GLTFLoader } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/loaders/GLTFLoader.js';
import { AsciiEffect } from 'https://cdn.jsdelivr.net/npm/three@0.121.1/examples/jsm/effects/AsciiEffect.js';

// Server configuration
const SERVER_URL = 'http://localhost:3000';

// Scene setup variables
let scene, camera, renderer, effect, myMesh, controls;
let rotateModel = false;
let lightMode = false;
let characters = ' .:-=+*#%@';
let ASCIIColor = '#ffffff';
let backgroundColor = 'black';
let currentView = '3d'; 

function init() {
    scene = new THREE.Scene();
    scene.background = new THREE.Color(0x1a1b26);

    camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
    camera.position.z = 4;

    renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight);

    // Add lights
    const pointLight1 = new THREE.PointLight(0xffffff, 1);
    pointLight1.position.set(100, 100, 400);
    scene.add(pointLight1);

    const pointLight2 = new THREE.PointLight(0xffffff, 0.5);
    pointLight2.position.set(-500, 100, -400);
    scene.add(pointLight2);

    createEffect();
    initControls();

    document.getElementById('ascii-output').appendChild(effect.domElement);
    window.addEventListener('resize', onWindowResize, false);
}

function initControls() {
    controls = new OrbitControls(camera, effect.domElement);
    controls.enableDamping = true;
    controls.dampingFactor = 0.05;
    controls.screenSpacePanning = true;
    controls.minDistance = 0.1;
    controls.maxDistance = 10;
    controls.maxPolarAngle = Math.PI;
    
    controls.touches = {
        ONE: THREE.TOUCH.ROTATE,
        TWO: THREE.TOUCH.DOLLY_PAN
    };
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
    reader.onload = function(event) {
        const extension = file.name.split('.').pop().toLowerCase();
        let loader;

        showProgress('Loading model...', 0);

        switch (extension) {
            case 'stl': loader = new STLLoader(); break;
            case 'obj': loader = new OBJLoader(); break;
            case 'fbx': loader = new FBXLoader(); break;
            case 'gltf':
            case 'glb': loader = new GLTFLoader(); break;
            default:
                showError('Unsupported file format');
                return;
        }

        const url = URL.createObjectURL(file);
        loader.load(url,
            function(object) {
                if (myMesh) scene.remove(myMesh);

                if (object.scene) {
                    myMesh = object.scene;
                } else if (object.isGroup) {
                    myMesh = object;
                } else {
                    const material = new THREE.MeshStandardMaterial({
                        color: 0xc0caf5,
                        flatShading: true,
                        side: THREE.DoubleSide
                    });
                    myMesh = new THREE.Mesh(object, material);
                }

                optimizeModel(myMesh);
                scene.add(myMesh);
                URL.revokeObjectURL(url);
                showProgress('Model loaded successfully!', 100);
            },
            function(xhr) {
                const percent = (xhr.loaded / xhr.total) * 100;
                showProgress('Loading model...', percent);
            },
            function(error) {
                console.error('Error loading model:', error);
                showError('Error loading the model. Please try again.');
                URL.revokeObjectURL(url);
            }
        );
    };
    reader.readAsArrayBuffer(file);
}

function optimizeModel(mesh) {
    const bbox = new THREE.Box3().setFromObject(mesh);
    const size = bbox.getSize(new THREE.Vector3());
    const center = bbox.getCenter(new THREE.Vector3());
    const maxDim = Math.max(size.x, size.y, size.z);
    const scale = 2 / maxDim;

    mesh.scale.multiplyScalar(scale);
    mesh.position.sub(center.multiplyScalar(scale));
    mesh.rotation.x = -Math.PI / 2;

    camera.position.set(0, 0, 3);
    camera.lookAt(0, 0, 0);
    controls.update();
}

function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
    effect.setSize(window.innerWidth, window.innerHeight);
}

function animate() {
    requestAnimationFrame(animate);
    if (rotateModel && myMesh && currentView === '3d') {
        myMesh.rotation.z += 0.01;
    }
    if (currentView === '3d') {
        effect.render(scene, camera);
    }
}

function showError(message) {
    const errorElement = document.getElementById('error-message') || createErrorElement();
    errorElement.textContent = message;
    errorElement.style.opacity = '1';
    setTimeout(() => {
        errorElement.style.opacity = '0';
    }, 3000);
}

function createErrorElement() {
    const errorElement = document.createElement('div');
    errorElement.id = 'error-message';
    document.querySelector('.container').appendChild(errorElement);
    return errorElement;
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

// Color selector and management
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
    Object.assign(colorSelector.style, {
        position: 'fixed',
        top: '50%',
        left: '50%',
        transform: 'translate(-50%, -50%)',
        backgroundColor: '#ffffff',
        padding: '20px',
        border: '2px solid #000000',
        zIndex: '1000',
    });

    const closeButton = document.createElement('button');
    closeButton.textContent = 'Close';
    closeButton.style.marginTop = '10px';
    closeButton.onclick = closeColorSelector;
    colorSelector.appendChild(closeButton);

    document.body.appendChild(colorSelector);
}

function saveAsTextFile() {
    const content = document.getElementById('ascii-output').textContent;
    const blob = new Blob([content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'ascii_model.txt';
    link.click();
    URL.revokeObjectURL(url);
}

function closeColorSelector() {
    const colorSelector = document.getElementById('color-selector');
    if (colorSelector) {
        document.body.removeChild(colorSelector);
    }
}

function selectColor(color) {
    ASCIIColor = color;
    if (effect?.domElement) {
        effect.domElement.style.color = ASCIIColor;
    }
    const asciiOutput = document.getElementById('ascii-output');
    if (currentView === 'ascii') {
        asciiOutput.style.color = ASCIIColor;
    }
    closeColorSelector();
}

function takeScreenshot() {
    if (!effect) {
        console.error('ASCII effect not initialized');
        return;
    }

    // Create canvas matching the effect dimensions
    const canvas = document.createElement('canvas');
    canvas.width = effect.domElement.clientWidth;
    canvas.height = effect.domElement.clientHeight;
    const ctx = canvas.getContext('2d');

    // Make sure we have the latest render
    effect.render(scene, camera);

    // Get ASCII content directly from effect
    const asciiContent = effect.domElement.innerText;

    // Fill background
    ctx.fillStyle = backgroundColor;
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    // Draw ASCII text
    ctx.fillStyle = ASCIIColor;
    ctx.font = '9.7561px monospace';  // Match the font used in GIF generation
    asciiContent.split('\n').forEach((line, index) => {
        ctx.fillText(line, 0, (index + 1) * 9.7561);  // Same spacing as GIF
    });

    // Save image
    const dataURL = canvas.toDataURL('image/png');
    const link = document.createElement('a');
    link.href = dataURL;
    link.download = 'ascii_screenshot.png';
    link.click();
}

async function generateGif() {
    if (!myMesh) {
        showProgress('Please load a 3D model first.', 0);
        console.log('No model loaded');
        return;
    }

    console.log('Starting GIF generation process');
    showProgress('Preparing to generate GIF...', 0);

    const frames = 30;
    const originalRotation = myMesh.rotation.z;
    const originalPosition = myMesh.position.clone();

    // **Removed the extra centering step here**.
    // The model is already centered in optimizeModel().
    // We trust that the model is centered in the viewport as desired.
    // const bbox = new THREE.Box3().setFromObject(myMesh);
    // const center = bbox.getCenter(new THREE.Vector3());
    // myMesh.position.sub(center);

    const canvas = document.createElement('canvas');
    canvas.width = effect.domElement.clientWidth;
    canvas.height = effect.domElement.clientHeight;
    const ctx = canvas.getContext('2d');

    const gif = new GIF({
        workers: 2,
        quality: 10,
        width: canvas.width,
        height: canvas.height,
        workerScript: '/js/gif.worker.js'
    });

    for (let i = 0; i < frames; i++) {
        myMesh.rotation.z = originalRotation + (i / frames) * Math.PI * 2;
        effect.render(scene, camera);

        const asciiContent = effect.domElement.innerText;

        ctx.fillStyle = backgroundColor;
        ctx.fillRect(0, 0, canvas.width, canvas.height);

        ctx.fillStyle = ASCIIColor;
        ctx.font = '9.7561px monospace';
        asciiContent.split('\n').forEach((line, index) => {
            ctx.fillText(line, 0, (index + 1) * 9.7561);
        });

        gif.addFrame(ctx, { copy: true, delay: 100 });
        showProgress('Capturing ASCII frames...', (i / frames) * 90);
    }

    // Restore original position and rotation
    myMesh.position.copy(originalPosition);
    myMesh.rotation.z = originalRotation;
    effect.render(scene, camera);

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

function generateAnimatedSVG() {
    if (!myMesh) {
        showProgress('Please load a 3D model first.', 0);
        return;
    }

    showProgress('Preparing to generate ASCII SVG animation...', 0);

    const frames = 30;
    const svgFrames = [];
    const originalRotation = myMesh.rotation.z;

    for (let i = 0; i < frames; i++) {
        myMesh.rotation.z = originalRotation + (i / frames) * Math.PI * 2;
        effect.render(scene, camera);
        
        const asciiContent = effect.domElement.innerText;
        svgFrames.push(asciiContent);
        
        showProgress('Capturing ASCII frames...', (i / frames) * 90);
    }

    myMesh.rotation.z = originalRotation;

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

// Event listeners initialization
document.addEventListener('DOMContentLoaded', () => {
    init();
    animate();

    const fileInput = document.getElementById('model-file');
    if (fileInput) {
        fileInput.addEventListener('change', function(event) {
            const file = event.target.files[0];
            if (file) {
                document.getElementById('file-name').textContent = file.name;
                loadModel(file);
            }
        });
    }

    // Add event listeners for other controls
    const addEventListenerSafely = (id, event, handler) => {
        const element = document.getElementById(id);
        if (element) {
            element.addEventListener(event, handler);
        }
    };

    addEventListenerSafely('rotate-btn', 'click', () => {
        rotateModel = !rotateModel;
    });

    addEventListenerSafely('light-dark-btn', 'click', () => {
        lightMode = !lightMode;
        backgroundColor = lightMode ? 'white' : 'black';
        ASCIIColor = lightMode ? 'black' : 'white';
        if (effect?.domElement) {
            effect.domElement.style.color = ASCIIColor;
            effect.domElement.style.backgroundColor = backgroundColor;
        }
    });

    addEventListenerSafely('screenshot-btn', 'click', takeScreenshot);
    addEventListenerSafely('change-color-btn', 'click', changeTextColor);
    document.getElementById('download-gif-btn').addEventListener('click', generateGif);
    document.getElementById('download-svg-btn').addEventListener('click', generateAnimatedSVG);
});

// Handle device orientation changes
window.addEventListener('orientationchange', () => {
    setTimeout(() => {
        onWindowResize();
        if (myMesh) {
            const bbox = new THREE.Box3().setFromObject(myMesh);
            const center = bbox.getCenter(new THREE.Vector3());
            myMesh.position.sub(center);
            camera.lookAt(0, 0, 0);
        }
    }, 100);
});

// Make necessary functions available globally
window.selectColor = selectColor;
window.closeColorSelector = closeColorSelector;