import { readFileSync } from 'fs';
import { runProgram } from './computer';

function findTerms(program: number[]) {
  for (let noun = 0; noun <= 99; noun += 1) {
    for (let verb = 0; verb <= 99; verb += 1) {
      const newProgram = [...program];
      newProgram[1] = noun;
      newProgram[2] = verb;
      runProgram(newProgram);
      if (newProgram[0] === 19690720) {
        return { noun, verb };
      }
    }
  }

  return { noun: -1, verb: -1 };
}

export function day2Solve() {
  const data = readFileSync('data/day2').toString()
    .split(',')
    .filter((line) => line !== '')
    .map((cmd) => +cmd);

  data[1] = 12;
  data[2] = 2;

  const final = runProgram([...data]);
  const { noun, verb } = findTerms(data);

  return { solution1: final[0], solution2: 100 * noun + verb };
}
