import { MatchViewService } from './match-view-service.js';
import { CellState } from '../domain/constants.js';

describe('MatchViewService', () => {
  it('stores match view snapshots until player id is known', () => {
    const matchState = matchStateFake({ playerId: undefined });
    const view = { id: 1 };

    expect(new MatchViewService(matchState).receiveSnapshot(view)).toBe(false);

    expect(matchState.data.pendingView).toBe(view);
    expect(matchState.applyViewModel).not.toHaveBeenCalled();
  });

  it('projects match view snapshots into match state view models', () => {
    const matchState = matchStateFake({ playerId: 'p1' });

    expect(
      new MatchViewService(matchState).receiveSnapshot(matchStateFixture())
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

    expect(new MatchViewService(matchState).receiveSnapshot(undefined)).toBe(
      false
    );

    expect(matchState.applyViewModel).not.toHaveBeenCalled();
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
