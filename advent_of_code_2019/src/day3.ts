import { readFileSync } from 'fs';

interface WireCoords {x: number, y: number}

function findBends(wire: string[]) {
  return wire
    .map((dir) => ({ direction: dir[0], length: +(dir.slice(1)) }))
    .reduce((points, bend) => {
      const { x, y } = points[points.length - 1];
      switch (bend.direction) {
        case 'U':
          points.push({ x, y: y + bend.length });
          break;
        case 'D':
          points.push({ x, y: y - bend.length });
          break;
        case 'R':
          points.push({ x: x + bend.length, y });
          break;
        case 'L':
          points.push({ x: x - bend.length, y });
          break;
        default:
          throw new Error();
      }

      return points;
    }, [{ x: 0, y: 0 }]);
}

function checkIntersect(
  { x: x1, y: y1 }: WireCoords,
  { x: x2, y: y2 }: WireCoords,
  { x: x3, y: y3 }: WireCoords,
  { x: x4, y: y4 }: WireCoords,
) {
  const denom = ((x2 - x1) * (y4 - y3)) - ((y2 - y1) * (x4 - x3));
  if (denom === 0) return null;

  const u = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denom;
  const t = ((x2 - x1) * (y1 - y3) - (y2 - y3) * (x1 - x3)) / denom;
  if (u < 0 || u > 1 || t < 0 || t > 1) {
    return null;
  }

  return { x: (x1 + u * (x2 - x1)), y: y1 + u * (y2 - y1) };
}

function findIntersections(wire1: WireCoords[], wire2: WireCoords[]) {
  const results = [];

  for (let i = 1; i < wire1.length; i += 1) {
    for (let j = 1; j < wire2.length; j += 1) {
      results.push(checkIntersect(wire1[i - 1], wire1[i], wire2[j - 1], wire2[j]));
    }
  }

  return results.flat().filter((val): val is WireCoords => val != null);
}

function distanceToIntersect(wire: WireCoords[], intersects: WireCoords[]) {
  return intersects.map(({ x, y }) => {
    let found = false;
    let distance = 0;
    let i = 1;
    do {
      const point1 = wire[i - 1];
      const point2 = wire[i];

      if (point1.x === x && point2.x === x
      && ((y > point1.y && y < point2.y) || (y < point1.y && y > point2.y))) {
        found = true;
        distance += Math.abs(y - point1.y);
      } else if (point1.y === y && point2.y === y
      && ((x > point1.x && x < point2.x) || (x < point1.x && x > point2.x))) {
        found = true;
        distance += Math.abs(x - point1.x);
      } else {
        distance += (Math.abs(point2.x - point1.x) + Math.abs(point2.y - point1.y));
      }

      i += 1;
    } while (!found && i < wire.length);
    return distance;
  });
}

export function findClosestIntersectLength(wire1: string[], wire2: string[]) {
  const wireCoords1 = findBends(wire1);
  const wireCoords2 = findBends(wire2);
  const intersections = findIntersections(wireCoords1, wireCoords2);

  const distances = distanceToIntersect(wireCoords1, intersections);

  return Math.min(...distanceToIntersect(wireCoords2, intersections)
    .map((dist2, i) => distances[i] + dist2));
}

export function findClosestIntersectManhattan(wire1: string[], wire2: string[]) {
  const intersections = findIntersections(findBends(wire1), findBends(wire2));

  return intersections
    .map(({ x, y }) => Math.abs(x) + Math.abs(y))
    .reduce(
      (prev, current) => (current !== 0 && (current < prev || prev === -1) ? current : prev),
      -1,
    );
}
export function day3Solve() {
  const data = readFileSync('data/day3').toString()
    .split('\n')
    .filter((line) => line !== '')
    .map((wire) => wire.split(','));

  return {
    solution1: findClosestIntersectManhattan(data[0], data[1]),
    solution2: findClosestIntersectLength(data[0], data[1]),
  };
}
