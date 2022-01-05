import request from 'supertest';
import { runProgram } from '../src/computer';
import app from '../src/app';

describe('Checking passwords', () => {
  it('should match examples', () => {
    expect(runProgram([1002, 4, 3, 4, 33])).toEqual([1002, 4, 3, 4, 99]);
  });
});

describe('GET /day3', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day5');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 6069343, solution2: 3188550 });
  });
});
