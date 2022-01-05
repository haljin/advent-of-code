import { readFileSync } from 'fs';
import { runProgram } from './computer';

export function day5Solve() {
  const data = readFileSync('data/day5').toString()
    .split(',')
    .filter((line) => line !== '')
    .map((cmd) => +cmd);

  const output: number[] = [];
  runProgram([...data], 1, output);
  const output2: number[] = [];
  runProgram([...data], 5, output2);

  return { solution1: output.at(-1), solution2: output2.at(-1) };
}
