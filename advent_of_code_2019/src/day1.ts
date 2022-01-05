import { readFileSync } from 'fs';

export function calculateFuel(value: number) {
  return Math.floor(value / 3) - 2;
}

export function day1Solve() {
  const data = readFileSync('data/day1').toString()
    .split('\n')
    .filter((line) => line !== '');

  const initialFuel = data
    .map((line) => calculateFuel(+line));

  const solution1 = initialFuel.reduce((acc, line) => line + acc);

  let solution2 = 0;
  let fuelData = initialFuel;

  while (fuelData.length > 0) {
    solution2 += fuelData.reduce((acc, fuel) => fuel + acc);

    fuelData = fuelData
      .map((line) => calculateFuel(line))
      .filter((fuel) => fuel > 0);
  }
  return { solution1, solution2 };
}
