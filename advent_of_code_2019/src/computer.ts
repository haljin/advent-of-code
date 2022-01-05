function getData(command: number, position: number, index: number, program: number[]) {
  const addressMode = command.toString().at(-(position + 2));
  if (addressMode === '1') {
    return program[index + position];
  }
  return program[program[index + position]];
}

export function processCommand(
  index: number,
  program : number[],
  input = 0,
  output: number[] = [],
) {
  const newProgram = program;

  const opcode = newProgram[index];
  const instruction = opcode % 100;
  switch (instruction) {
    case 99:
    // Halt
      return { next: -1, program };
    case 1:
    // Add
      newProgram[program[index + 3]] = getData(opcode, 1, index, program)
       + getData(opcode, 2, index, program);
      return { next: index + 4, program: newProgram };
    case 2:
    // Multiply
      newProgram[program[index + 3]] = getData(opcode, 1, index, program)
      * getData(opcode, 2, index, program);
      return { next: index + 4, program: newProgram };
    case 3:
    // Input
      newProgram[program[index + 1]] = input;
      return { next: index + 2, program: newProgram };
    case 4:
    // Output
      output.push(getData(opcode, 1, index, program));
      return { next: index + 2, program: newProgram };
    case 5:
    // JumpIfTrue
      if (getData(opcode, 1, index, program) !== 0) {
        return { next: getData(opcode, 2, index, program), program: newProgram };
      }
      return { next: index + 3, program: newProgram };
    case 6:
    // JumpIfFalse
      if (getData(opcode, 1, index, program) === 0) {
        return { next: getData(opcode, 2, index, program), program: newProgram };
      }
      return { next: index + 3, program: newProgram };
    case 7:
    // LessThan
      if (getData(opcode, 1, index, program) < getData(opcode, 2, index, program)) {
        newProgram[program[index + 3]] = 1;
      } else {
        newProgram[program[index + 3]] = 0;
      }
      return { next: index + 4, program: newProgram };
    case 8:
    // Equals
      if (getData(opcode, 1, index, program) === getData(opcode, 2, index, program)) {
        newProgram[program[index + 3]] = 1;
      } else {
        newProgram[program[index + 3]] = 0;
      }
      return { next: index + 4, program: newProgram };
    default:
      throw new Error('Unexpected opcode');
  }
}

export function runProgram(
  program: number[],
  input = 0,
  output: number[] = [],
) {
  let index = 0;
  let runningProgram = program;
  do {
    const result = processCommand(index, runningProgram, input, output);
    index = result.next;
    runningProgram = result.program;
  }
  while (index !== -1);
  return program;
}
