import request from 'supertest';
import { runProgram } from '../src/computer';
import app from '../src/app';

describe('Processing programs', () => {
  it('should match examples', () => {
    expect(runProgram([1, 0, 0, 0, 99])).toEqual([2, 0, 0, 0, 99]);
    expect(runProgram([2, 3, 0, 3, 99])).toEqual([2, 3, 0, 6, 99]);
    expect(runProgram([2, 4, 4, 5, 99, 0])).toEqual([2, 4, 4, 5, 99, 9801]);
    expect(runProgram([1, 1, 1, 4, 99, 5, 6, 0, 99])).toEqual([30, 1, 1, 4, 2, 5, 6, 0, 99]);
    expect(runProgram([1, 9, 10, 3,
      2, 3, 11, 0,
      99,
      30, 40, 50])).toEqual([3500, 9, 10, 70,
      2, 3, 11, 0,
      99,
      30, 40, 50]);
  });
});

describe('GET /day2', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day2');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 3409710, solution2: 7912 });
  });
});
