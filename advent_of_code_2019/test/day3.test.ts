import request from 'supertest';
import { findClosestIntersectLength, findClosestIntersectManhattan } from '../src/day3';
import app from '../src/app';

describe('Processing Wires', () => {
  it('should match examples', () => {
    expect(findClosestIntersectManhattan(['R8', 'U5', 'L5', 'D3'], ['U7', 'R6', 'D4', 'L4'])).toEqual(6);

    expect(findClosestIntersectManhattan(
      ['R75', 'D30', 'R83', 'U83', 'L12', 'D49', 'R71', 'U7', 'L72'],
      ['U62', 'R66', 'U55', 'R34', 'D71', 'R55', 'D58', 'R83'],
    )).toEqual(159);

    expect(findClosestIntersectManhattan(
      ['R98', 'U47', 'R26', 'D63', 'R33', 'U87', 'L62', 'D20', 'R33', 'U53', 'R51'],
      ['U98', 'R91', 'D20', 'R16', 'D67', 'R40', 'U7', 'R15', 'U6', 'R7'],
    )).toEqual(135);
  });
  it('should match examples for part 2', () => {
    expect(findClosestIntersectLength(['R8', 'U5', 'L5', 'D3'], ['U7', 'R6', 'D4', 'L4'])).toEqual(30);

    expect(findClosestIntersectLength(
      ['R75', 'D30', 'R83', 'U83', 'L12', 'D49', 'R71', 'U7', 'L72'],
      ['U62', 'R66', 'U55', 'R34', 'D71', 'R55', 'D58', 'R83'],
    )).toEqual(610);

    expect(findClosestIntersectLength(
      ['R98', 'U47', 'R26', 'D63', 'R33', 'U87', 'L62', 'D20', 'R33', 'U53', 'R51'],
      ['U98', 'R91', 'D20', 'R16', 'D67', 'R40', 'U7', 'R15', 'U6', 'R7'],
    )).toEqual(410);
  });
});

describe('GET /day3', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day3');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 248, solution2: 28580 });
  });
});
