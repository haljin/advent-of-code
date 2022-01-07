import request from 'supertest';
import { chainProgram } from '../src/day7';
import app from '../src/app';

describe('Checking passwords', () => {
  it('should match examples', () => {
    expect(chainProgram(
      [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0],
      [4, 3, 2, 1, 0],
    ))
      .toEqual(43210);
    expect(chainProgram(
      [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
        101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0],
      [0, 1, 2, 3, 4],
    ))
      .toEqual(54321);
    expect(chainProgram(
      [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
        1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0],
      [1, 0, 4, 3, 2],
    ))
      .toEqual(65210);
  });
});

describe('GET /day7', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day7');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 18812 });
  });
});
