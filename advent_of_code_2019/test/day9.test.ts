import request from 'supertest';
import { startProgram } from '../src/computer';
import app from '../src/app';

describe('Checking programs', () => {
  it('should match examples', () => {
    expect(startProgram([109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]))
      .toHaveProperty('output', [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]);
    expect(startProgram([1102, 34915192, 34915192, 7, 4, 7, 99, 0]))
      .toHaveProperty('output', [1219070632396864]);
    expect(startProgram([104, 1125899906842624, 99]))
      .toHaveProperty('output', [1125899906842624]);
  });
});
describe('GET /day9', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day9');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: [3013554615], solution2: [50158] });
  });
});
