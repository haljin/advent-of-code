import { readFileSync } from 'fs';

export function checkPassword(password : string) {
  const { doubleFound: df, increasing: inc } = [...password]
    .reduce(({ doubleFound, increasing, previousChar }, elem) => ({
      doubleFound: doubleFound || (previousChar === elem),
      increasing: increasing && (elem >= previousChar),
      previousChar: elem,
    }), { doubleFound: false, increasing: true, previousChar: '' });

  return df && inc;
}

export function checkPasswordStrict(password : string) {
  const { groupLengths: gl, increasing: inc } = [...password]
    .reduce(
      ({ groupLengths, increasing, previousChar }, elem) => {
        const lengths = groupLengths;
        if (elem === previousChar) {
          (lengths[lengths.length - 1] += 1);
        } else {
          lengths.push(1);
        }
        return {
          increasing: increasing && (elem >= previousChar),
          groupLengths: lengths,
          previousChar: elem,
        };
      },
      { groupLengths: [0], increasing: true, previousChar: '' },
    );

  return gl.includes(2) && inc;
}

export function day4Solve() {
  const data = readFileSync('data/day4').toString()
    .split('-');

  let matches = 0;
  for (let i = +data[0]; i <= +data[1]; i += 1) {
    if (checkPassword(i.toString())) { matches += 1; }
  }
  let matchesStrict = 0;
  for (let i = +data[0]; i <= +data[1]; i += 1) {
    if (checkPasswordStrict(i.toString())) {
      matchesStrict += 1;
    }
  }

  return { solution1: matches, solution2: matchesStrict };
}
