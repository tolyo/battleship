import { TargetService } from './target-service.js';
import { SessionPhase } from '../domain/room-state.js';

describe('TargetService', () => {
  it('returns target moves when match state allows moves', () => {
    const target = new TargetService({
      phase: SessionPhase.PLAYING,
      canSubmitMove: true,
    });

    expect(
      target.moveForTile({
        row: 4,
        column: 5,
        state: 'empty',
      })
    ).toEqual({ row: 4, column: 5 });
  });

  it('rejects target moves when match state is not accepting moves', () => {
    const target = new TargetService({
      phase: SessionPhase.WAITING,
      canSubmitMove: true,
    });

    expect(
      target.moveForTile({
        row: 4,
        column: 5,
        state: 'empty',
      })
    ).toBeUndefined();
  });

  it('rejects already resolved target tiles', () => {
    const target = new TargetService({
      phase: SessionPhase.PLAYING,
      canSubmitMove: true,
    });

    expect(
      target.moveForTile({
        row: 4,
        column: 5,
        state: 'hit',
      })
    ).toBeUndefined();
  });
});
