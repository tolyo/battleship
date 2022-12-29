import {test, expect} from '@playwright/test';
import {add} from '../src/index';

test('add function', async () => {
  await expect(add(1, 2)).toEqual(3);
});
