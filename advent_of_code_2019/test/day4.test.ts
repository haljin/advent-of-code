import request from 'supertest';
import { checkPassword, checkPasswordStrict } from '../src/day4';
import app from '../src/app';

describe('Checking passwords', () => {
  it('should match examples', () => {
    expect(checkPassword('111111')).toBe(true);
    expect(checkPassword('223450')).toBe(false);
    expect(checkPassword('123789')).toBe(false);
  });
  it('should match examples for part2', () => {
    expect(checkPasswordStrict('112233')).toBe(true);
    expect(checkPasswordStrict('123444')).toBe(false);
    expect(checkPasswordStrict('111122')).toBe(true);
    expect(checkPasswordStrict('777789')).toBe(false);
  });
});

describe('GET /day3', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day4');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({ solution1: 966, solution2: 628 });
  });
});
