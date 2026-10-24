import { create } from '../../../_build/browser/js/modules/battleship/client/MatchSnapshots.js';
import { matchViewModelFromView } from '../../../assets/game/domain/match-view-model.js';
import { CellState } from '../../../assets/game/domain/constants.js';

describe('MatchViewService', () => {
  it('stores match view snapshots until player id is known', () => {
    const matchState = matchStateFake({ playerId: undefined });
    const view = { id: 1 };

    expect(
      Reflect.construct(create, [
        matchState,
        { project: matchViewModelFromView },
      ]).receiveSnapshot(view)
    ).toBe(false);

    expect(matchState.data.pendingView).toBe(view);
    expect(matchState.applyViewModel).not.toHaveBeenCalled();
  });

  it('retains null and undefined snapshots while waiting for player identity', () => {
    for (const payload of [null, undefined]) {
      const state = matchStateFake({ playerId: undefined });
      const projection = { project: jasmine.createSpy('project') };
      const service = Reflect.construct(create, [state, projection]);
      expect(service.receiveSnapshot(payload)).toBeFalse();
      expect(state.data.pendingView).toBe(payload);
      expect(Object.hasOwn(state.data, 'pendingView')).toBeTrue();
      expect(projection.project).not.toHaveBeenCalled();
    }
  });

  it('projects match view snapshots into match state view models', () => {
    const matchState = matchStateFake({ playerId: 'p1' });

    expect(
      Reflect.construct(create, [
        matchState,
        { project: matchViewModelFromView },
      ]).receiveSnapshot(matchStateFixture())
    ).toBe(true);

    expect(matchState.applyViewModel).toHaveBeenCalledOnceWith(
      jasmine.objectContaining({
        status: 'Your turn',
        isMyTurn: true,
        unitCoordinatesById: {
          0: [{ row: 0, column: 0 }],
        },
      })
    );
  });

  it('rejects malformed match view snapshots', () => {
    const matchState = matchStateFake({ playerId: 'p1' });

    expect(
      Reflect.construct(create, [
        matchState,
        { project: matchViewModelFromView },
      ]).receiveSnapshot(undefined)
    ).toBe(false);

    expect(matchState.applyViewModel).not.toHaveBeenCalled();
  });

  it('preserves pending payload identity and reads changing player identity', () => {
    let playerId = '';
    const writes = [];
    const calls = [];
    const model = { id: 'model' };
    const data = new Proxy(
      {},
      {
        set(target, key, value, receiver) {
          writes.push({ key, value, receiver });
          return Reflect.set(target, key, value, receiver);
        },
      }
    );
    const state = {
      get playerId() {
        return playerId;
      },
      data,
      applyViewModel(view) {
        expect(this).toBe(state);
        calls.push(view);
        return false;
      },
    };
    const projection = {
      project(view) {
        expect(this).toBe(projection);
        expect(view).toBe(payload);
        return model;
      },
    };
    const service = Reflect.construct(create, [state, projection]);
    const payload = Object.freeze({ id: 7 });
    expect(service.receiveSnapshot(payload)).toBeFalse();
    expect(writes).toEqual([
      { key: 'pendingView', value: payload, receiver: data },
    ]);
    expect(calls).toEqual([]);
    playerId = 'p1';
    expect(service.receiveSnapshot(payload)).toBeFalse();
    expect(calls).toEqual([model]);
    expect(writes.length).toBe(1);
  });
});

function matchStateFake({ playerId }) {
  return {
    playerId,
    data: {
      pendingView: undefined,
    },
    applyViewModel: jasmine.createSpy('applyViewModel').and.returnValue(true),
  };
}

function matchStateFixture() {
  const ownBoard = emptyBoard();
  ownBoard[0][0] = '0';
  return {
    own_player: { id: 'p1', board: ownBoard },
    opponent: { id: 'p2', board: emptyBoard() },
    allowed_actions: [{ action: 'move', target: 'opponent_board' }],
  };
}

function emptyBoard() {
  return Array.from({ length: 10 }, () =>
    Array.from({ length: 10 }, () => CellState.EMPTY)
  );
}
