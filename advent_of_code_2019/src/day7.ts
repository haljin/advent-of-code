import { readFileSync } from 'fs';
import { startProgram } from './computer';

export function chainProgram(program: number[], phaseSettings: number[]) {
  return phaseSettings.reduce((previousStage, phaseSetting) => {
    const { output } = startProgram([...program]).continue(phaseSetting).continue(previousStage);
    return output[0];
  }, 0);
}

export function allPermutations(inputs: number[]): number[][] {
  if (inputs.length <= 2) return inputs.length === 2 ? [inputs, [inputs[1], inputs[0]]] : [inputs];
  return inputs.reduce(
    (acc: number[][], item, i) => acc.concat(
      allPermutations([...inputs.slice(0, i), ...inputs.slice(i + 1)])
        .map((val: number[]) => [item, ...val]),
    ),
    [],
  );
}

export function day7Solve() {
  const data = readFileSync('data/day7').toString()
    .split(',')
    .filter((line) => line !== '')
    .map((cmd) => +cmd);

  return {
    solution1: Math.max(...allPermutations([0, 1, 2, 3, 4])
      .map((perm) => chainProgram(data, perm))),
  };
}
