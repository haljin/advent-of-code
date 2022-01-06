import request from 'supertest';
import { buildOrbits, countAllOrbits, findCommonSubPath } from '../src/day6';
import app from '../src/app';

describe('Counting orbits', () => {
  it('should match examples', () => {
    expect(countAllOrbits(buildOrbits([
      'COM)B',
      'B)C',
      'C)D',
      'D)E',
      'E)F',
      'B)G',
      'G)H',
      'D)I',
      'E)J',
      'J)K',
      'K)L',
    ]))).toEqual(42);
  });
  it('should match examples in part 2', () => {
    expect(findCommonSubPath(buildOrbits([
      'COM)B',
      'B)C',
      'C)D',
      'D)E',
      'E)F',
      'B)G',
      'G)H',
      'D)I',
      'E)J',
      'J)K',
      'K)L',
      'K)YOU',
      'I)SAN',
    ]))).toEqual(4);
  });
});

describe('GET /day6', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day6');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 140608, solution2: 337 });
  });
});
