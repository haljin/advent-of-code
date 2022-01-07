import { readFileSync } from 'fs';

interface UOMEntry {center: string, body: string}
interface Orbit {center: string, bodies: Orbit[]}

function insertOrbits(root: Orbit, data: UOMEntry[]) {
  const orbiters = data.filter(({ center }) => center === root.center);

  if (orbiters.length === 0) {
    return root;
  }
  orbiters.forEach(({ body }) => root.bodies.push(
    insertOrbits({ center: body, bodies: [] }, data),
  ));
  return root;
}

export function buildOrbits(data: string[]) {
  const uomEntries = data.map((line) => {
    const [center, body] = line.split(')');
    return { center, body };
  });
  return insertOrbits({ center: 'COM', bodies: [] }, uomEntries);
}

function countOrbits(root: Orbit, base: number): number {
  return root.bodies
    .map((body) => countOrbits(body, base + 1) + base)
    .reduce((acc, val) => acc + val, 1);
}

export function countAllOrbits(root: Orbit) {
  return countOrbits(root, 0) - 1;
}

function findPath(root: Orbit, lookingFor: string, pathSoFar: string[]) : null | string[] {
  if (root.center === lookingFor) { return pathSoFar; }
  if (root.bodies.length === 0) return null;
  const subPaths = root.bodies
    .map((body) => findPath(body, lookingFor, [...pathSoFar, root.center]))
    .filter((emptyPath): emptyPath is string[] => emptyPath !== null);

  if (subPaths.length === 0) return null;
  return subPaths[0];
}

export function findCommonSubPath(orbits: Orbit) {
  const path1 = findPath(orbits, 'YOU', [])?.reverse() || [];
  const path2 = findPath(orbits, 'SAN', [])?.reverse() || [];
  const firstCommonIndex = path1.findIndex(
    (elem) => path2.includes(elem),
  );

  return firstCommonIndex + path2.findIndex((el) => el === path1[firstCommonIndex]);
}

export function day6Solve() {
  const data = readFileSync('data/day6').toString()
    .split('\n');

  const orbits = buildOrbits(data);

  return {
    solution1: countAllOrbits(orbits),
    solution2: findCommonSubPath(orbits),
  };
}
