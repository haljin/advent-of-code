import request from 'supertest';
import { startProgram } from '../src/computer';
import app from '../src/app';

describe('Processing programs', () => {
  it('should match examples', () => {
    expect(startProgram([1, 0, 0, 0, 99])).toHaveProperty('program', [2, 0, 0, 0, 99]);
    expect(startProgram([2, 3, 0, 3, 99])).toHaveProperty('program', [2, 3, 0, 6, 99]);
    expect(startProgram([2, 4, 4, 5, 99, 0])).toHaveProperty('program', [2, 4, 4, 5, 99, 9801]);
    expect(startProgram([1, 1, 1, 4, 99, 5, 6, 0, 99])).toHaveProperty('program', [30, 1, 1, 4, 2, 5, 6, 0, 99]);
    expect(startProgram([1, 9, 10, 3,
      2, 3, 11, 0,
      99,
      30, 40, 50])).toHaveProperty('program', [3500, 9, 10, 70,
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
