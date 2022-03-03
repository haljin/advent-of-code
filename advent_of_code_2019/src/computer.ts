export interface ProgramState {
  index: number,
  program: number[],
  output: number[],
  continue: ((input: number) => ProgramState),
  relativeBase: number
}

function readMemory(index: number, program: number[]) {
  const before = program.length;
  if (index < 0) {
    console.error({ index, program });
    throw new Error('wrong');
  }
  if (index >= before) {
    for (let i = 0; i <= (index - before); i += 1) {
      program.push(0);
    }
  }
  return program[index];
}

function writeMemory(index: number, data: number, program: number[]) {
  const before = program.length;
  const prog = program;
  if (index < 0) {
    console.error({ index, program });
    throw new Error('wrong');
  }
  if (index >= before) {
    for (let i = 0; i <= (index - before); i += 1) {
      prog.push(0);
    }
  }
  prog[index] = data;
}

function getData(
  { program, relativeBase, index }: ProgramState,
  position: number,
) {
  const command = program[index];
  const addressMode = command.toString().at(-(position + 2));
  if (addressMode === '2') {
    // Relative mode
    return readMemory(relativeBase + readMemory(index + position, program), program);
  }
  if (addressMode === '1') {
    // Immediate mode
    return readMemory(index + position, program);
  }
  // Position mode
  return readMemory(readMemory(index + position, program), program);
}

function putData(
  { program, relativeBase, index }: ProgramState,
  position: number,
  data: number,
) {
  const command = program[index];
  const addressMode = command.toString().at(-(position + 2));
  if (addressMode === '2') {
    // Relative mode
    return writeMemory(relativeBase + readMemory(index + position, program), data, program);
  }
  if (addressMode === '1') {
    // Immediate mode
    return writeMemory(index + position, data, program);
  }
  // Position mode
  return writeMemory(readMemory(index + position, program), data, program);
}

function processCommand(programState: ProgramState): ProgramState {
  const {
    index, program, output, relativeBase,
  } = programState;

  const opcode = readMemory(index, program);
  const instruction = opcode % 100;
  switch (instruction) {
    case 99:
      // Halt
      return { ...programState, index: -1 };
    case 1:
      // Add
      putData(programState, 3, getData(programState, 1) + getData(programState, 2));
      return { ...programState, index: index + 4 };
    case 2:
      // Multiply
      putData(programState, 3, getData(programState, 1) * getData(programState, 2));
      return { ...programState, index: index + 4 };
    case 3:
      // Input
      return {
        ...programState,
        index: -2,
        continue: (newInput: number) => {
          putData(programState, 1, newInput);
          return runProgram({
            ...programState, program, index: index + 2, continue: () => programState,
          });
        },
      };
    case 4:
      // Output
      output.push(getData(programState, 1));
      return { ...programState, index: index + 2 };
    case 5:
      // JumpIfTrue
      if (getData(programState, 1) !== 0) {
        return { ...programState, index: getData(programState, 2) };
      }
      return { ...programState, index: index + 3 };
    case 6:
      // JumpIfFalse
      if (getData(programState, 1) === 0) {
        return { ...programState, index: getData(programState, 2) };
      }
      return { ...programState, index: index + 3 };
    case 7:
      // LessThan
      if (getData(programState, 1)
      < getData(programState, 2)) {
        putData(programState, 3, 1);
      } else {
        putData(programState, 3, 0);
      }
      return { ...programState, index: index + 4 };
    case 8:
      // Equals
      if (getData(programState, 1)
      === getData(programState, 2)) {
        putData(programState, 3, 1);
      } else {
        putData(programState, 3, 0);
      }
      return { ...programState, index: index + 4 };
    case 9:
      // Adjust Base
      return {
        ...programState,
        index: index + 2,
        relativeBase: relativeBase + getData(programState, 1),
      };
    default:
      console.log(programState);
      throw new Error(`Unexpected opcode ${opcode}`);
  }
}

function runProgram(state: ProgramState): ProgramState {
  let processState = state;
  do {
    processState = processCommand(processState);
  }
  while (processState.index !== -1 && processState.index !== -2);
  return processState;
}

export function startProgram(program: number[]): ProgramState {
  const output: number[] = [];
  const state: ProgramState = {
    index: 0, program, output, continue: () => state, relativeBase: 0,
  };

  return runProgram(state);
}
