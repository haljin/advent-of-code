import { readFileSync } from 'fs';

export function processCommand(index: number, program : number[]) {
  const newProgram = program;
  switch (program[index]) {
    case 99:
      return { next: -1, program };
    case 1:
      newProgram[program[index + 3]] = program[program[index + 1]] + program[program[index + 2]];
      return { next: index + 4, program: newProgram };
    case 2:
      newProgram[program[index + 3]] = program[program[index + 1]] * program[program[index + 2]];
      return { next: index + 4, program: newProgram };
    default:
      throw new Error('Unexpected opcode');
  }
}

export function runProgram(program: number[]) {
  let index = 0;
  let runningProgram = program;
  do {
    const result = processCommand(index, runningProgram);
    index = result.next;
    runningProgram = result.program;
  }
  while (index !== -1);
  return program;
}

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

  // return { solution1, solution2 };
}
