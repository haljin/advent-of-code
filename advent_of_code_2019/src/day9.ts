import { readFileSync } from 'fs';
import { startProgram } from './computer';

export function day9Solve() {
  const data = readFileSync('data/day9').toString()
    .split(',')
    .filter((line) => line !== '')
    .map((cmd) => +cmd);

  return {
    solution1: startProgram([...data]).continue(1).output,
    solution2: startProgram([...data]).continue(2).output,
  };
}
