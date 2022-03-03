import request from 'supertest';
import { splitLayers } from '../src/day8';
import app from '../src/app';

describe('Splitting layers', () => {
  it('should match examples', () => {
    expect(splitLayers(3, 2, '123456789012'))
      .toEqual(['123456', '789012']);
  });
});

describe('GET /day8', () => {
  it('should return 200 OK', async () => {
    const res = await request(app).get('/day8');

    expect(res.status).toEqual(200);
    expect(res.body).toEqual({
      solution1: 2250,
      solution2: [
        '1111010010001101001010000',
        '1000010010000101001010000',
        '1110011110000101001010000',
        '1000010010000101001010000',
        '1000010010100101001010000',
        '1000010010011000110011110',
      ],
    });
  });
});
