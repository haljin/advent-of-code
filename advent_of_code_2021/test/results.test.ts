import request from 'supertest';
import app from '../src/app';

describe('GET /day1', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day1');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 3330521, solution2: 4992931 });
  });
});
