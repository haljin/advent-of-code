import { readFileSync } from 'fs';
import { startProgram } from './computer';

export function day5Solve() {
  const data = readFileSync('data/day5').toString()
    .split(',')
    .filter((line) => line !== '')
    .map((cmd) => +cmd);

  const { output } = startProgram([...data]).continue(1);
  const { output: output2 } = startProgram([...data]).continue(5);
  return { solution1: output.at(-1), solution2: output2.at(-1) };
}
