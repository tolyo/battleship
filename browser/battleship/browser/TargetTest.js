import { create } from '../../../_build/browser/js/modules/battleship/browser/Target.js';
import { SessionPhase } from '../../../assets/game/domain/room-state.js';

describe('TargetService', () => {
  it('reads live match permissions on each attempt and leaves the tile unchanged', () => {
    let phase = SessionPhase.SETUP;
    let canSubmitMove = false;
    const state = {
      get phase() {
        return phase;
      },
      get canSubmitMove() {
        return canSubmitMove;
      },
    };
    const target = Reflect.construct(create, [state]);
    const tile = Object.freeze({ row: 2, column: 7, state: 'empty' });
    expect(target.moveForTile(tile).allowed).toBeFalse();
    phase = SessionPhase.PLAYING;
    canSubmitMove = true;
    expect(target.moveForTile(tile)).toEqual({
      allowed: true,
      row: 2,
      column: 7,
    });
    canSubmitMove = false;
    expect(target.moveForTile(tile).allowed).toBeFalse();
    expect(tile).toEqual({ row: 2, column: 7, state: 'empty' });
  });
});
