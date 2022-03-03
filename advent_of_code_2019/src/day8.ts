import { readFileSync } from 'fs';

export function splitLayers(layerWidth: number, layerHeight: number, data: string) {
  const layerSize = layerHeight * layerWidth;

  const layerCount = data.length / layerSize;

  const layers: string[] = [];
  for (let i = 0; i < layerCount; i += 1) {
    layers.push(data.substr(i * layerSize, layerSize));
  }

  return layers;
}

function countDigits(digit: string, layer: string) {
  let count = 0;

  for (let i = 0; i < layer.length; i += 1) {
    if (layer[i] === digit) {
      count += 1;
    }
  }

  return count;
}

export function drawLayers(layerWidth: number, layers: string[]) {
  let final = '';

  for (let i = 0; i < layers[0].length; i += 1) {
    for (let l = 0; l < layers.length; l += 1) {
      if (layers[l][i] !== '2') {
        final += layers[l][i];
        break;
      }
    }
  }
  return splitLayers(layerWidth, 1, final);
}

export function day8Solve() {
  const data = readFileSync('data/day8').toString();

  const layers = splitLayers(25, 6, data);

  let minZeroLayer = layers[0];
  let minZeroCount = countDigits('0', layers[0]);

  for (let i = 1; i < layers.length; i += 1) {
    const zeroCount = countDigits('0', layers[i]);
    if (zeroCount < minZeroCount) {
      minZeroLayer = layers[i];
      minZeroCount = zeroCount;
    }
  }

  return {
    solution1: countDigits('1', minZeroLayer) * countDigits('2', minZeroLayer),
    solution2: drawLayers(25, layers),
  };
}
