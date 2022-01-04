import request from 'supertest';
import app from '../src/app';
import { calculateFuel } from '../src/day1';

describe('Fuel calculations', () => {
  it('should match examples', () => {
    expect(calculateFuel(12)).toEqual(2);
    expect(calculateFuel(14)).toEqual(2);
    expect(calculateFuel(1969)).toEqual(654);
    expect(calculateFuel(100756)).toEqual(33583);
  });
});

describe('GET /day1', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day1');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 3330521, solution2: 4992931 });
  });
});
