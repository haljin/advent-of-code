interface ProgramState {
  index: number,
  program : number[],
  output: number[],
  continue: ((input: number) => ProgramState)
}

function getData(command: number, position: number, index: number, program: number[]) {
  const addressMode = command.toString().at(-(position + 2));
  if (addressMode === '1') {
    return program[index + position];
  }
  return program[program[index + position]];
}

function processCommand(programState : ProgramState): ProgramState {
  const { index, program, output } = programState;

  const opcode = program[index];
  const instruction = opcode % 100;
  switch (instruction) {
    case 99:
    // Halt
      return { ...programState, index: -1 };
    case 1:
    // Add
      program[program[index + 3]] = getData(opcode, 1, index, program)
       + getData(opcode, 2, index, program);
      return { ...programState, index: index + 4 };
    case 2:
    // Multiply
      program[program[index + 3]] = getData(opcode, 1, index, program)
      * getData(opcode, 2, index, program);
      return { ...programState, index: index + 4 };
    case 3:
    // Input
      return {
        ...programState,
        index: -2,
        continue: (newInput: number) => {
          program[program[index + 1]] = newInput;
          return runProgram({
            ...programState, program, index: index + 2, continue: () => programState,
          });
        },
      };
    case 4:
    // Output
      output.push(getData(opcode, 1, index, program));
      return { ...programState, index: index + 2 };
    case 5:
    // JumpIfTrue
      if (getData(opcode, 1, index, program) !== 0) {
        return { ...programState, index: getData(opcode, 2, index, program) };
      }
      return { ...programState, index: index + 3 };
    case 6:
    // JumpIfFalse
      if (getData(opcode, 1, index, program) === 0) {
        return { ...programState, index: getData(opcode, 2, index, program) };
      }
      return { ...programState, index: index + 3 };
    case 7:
    // LessThan
      if (getData(opcode, 1, index, program) < getData(opcode, 2, index, program)) {
        program[program[index + 3]] = 1;
      } else {
        program[program[index + 3]] = 0;
      }
      return { ...programState, index: index + 4 };
    case 8:
    // Equals
      if (getData(opcode, 1, index, program) === getData(opcode, 2, index, program)) {
        program[program[index + 3]] = 1;
      } else {
        program[program[index + 3]] = 0;
      }
      return { ...programState, index: index + 4 };
    default:
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
    index: 0, program, output, continue: () => state,
  };

  return runProgram(state);
}
