import express from 'express';
import { day1Solve } from './day1';
import { day2Solve } from './day2';
import { day3Solve } from './day3';
import { day4Solve } from './day4';
import { day5Solve } from './day5';

const app = express();

// define a route handler for the default home page
app.get('/', (_req: any, res: any) => {
  res.send('Hello world!');
});

app.get('/day1', (_req: any, res: any) => {
  res.send(day1Solve());
});
app.get('/day2', (_req: any, res: any) => {
  res.send(day2Solve());
});
app.get('/day3', (_req: any, res: any) => {
  res.send(day3Solve());
});
app.get('/day4', (_req: any, res: any) => {
  res.send(day4Solve());
});
app.get('/day5', (_req: any, res: any) => {
  res.send(day5Solve());
});

export default app;
