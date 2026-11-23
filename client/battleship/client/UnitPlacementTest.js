import { create } from '../../../_build/browser/js/modules/battleship/client/UnitPlacement.js';

describe('UnitPlacement', () => {
  it('clears setup units and previews through setup service', () => {
    const setup = setupServiceFake();
    const placement = create(setup, undefined);
    const unit = { id: '2', size: 3 };

    placement.clearUnit({ unit });
    placement.clearPreview();
    placement.previewAt({ unit }, '4', '5', 'VERTICAL');

    expect(setup.clearUnit).toHaveBeenCalledOnceWith('2');
    expect(setup.clearPreview).toHaveBeenCalledOnceWith();
    expect(setup.previewUnitAt).toHaveBeenCalledOnceWith(
      '2',
      '4',
      '5',
      'VERTICAL',
      3
    );
  });

  it('places and commits through setup service', () => {
    const setup = setupServiceFake();
    const placement = create(setup, undefined);
    const unit = { id: '2', size: 3 };
    const coordinates = [{ row: 1, column: 2 }];

    placement.placeAt({ unit }, '4', '5', 'HORIZONTAL');
    placement.commit({ unit }, coordinates);

    expect(setup.placeUnitAt).toHaveBeenCalledOnceWith(
      '2',
      '4',
      '5',
      'HORIZONTAL',
      3
    );
    expect(setup.clearPreview).toHaveBeenCalledOnceWith();
    expect(setup.placeUnit).toHaveBeenCalledOnceWith('2', coordinates);
  });

  it('reads changing unit bindings and preserves placement result identity', () => {
    const setup = setupServiceFake();
    const placement = create(setup, undefined);
    const points = Object.freeze([{ row: 2, column: 3 }]);
    setup.previewUnitAt.and.returnValue(points);
    let selected = { id: '2', size: 3 };
    const source = {
      get unit() {
        return selected;
      },
    };
    expect(placement.previewAt(source, '2', '3', 'HORIZONTAL')).toBe(points);
    selected = { id: '9', size: 1 };
    placement.placeAt(source, '7', '8', 'VERTICAL');
    expect(setup.placeUnitAt).toHaveBeenCalledOnceWith(
      '9',
      '7',
      '8',
      'VERTICAL',
      1
    );
    selected = undefined;
    expect(placement.previewAt(source, '2', '3', 'HORIZONTAL')).toBeUndefined();
    expect(setup.previewUnitAt).toHaveBeenCalledTimes(1);
  });

  it('keeps the selected unit when clearing the preview changes its binding', () => {
    const setup = setupServiceFake();
    const source = { unit: { id: '9', size: 1 } };
    const calls = [];
    setup.clearPreview.and.callFake(() => {
      calls.push('preview');
      source.unit = undefined;
    });
    setup.placeUnit.and.callFake((id) => calls.push(id));
    const points = [{ row: 2, column: 3 }];
    create(setup, undefined).commit(source, points);
    expect(setup.placeUnit).toHaveBeenCalledOnceWith('9', points);
    expect(calls).toEqual(['preview', '9']);
  });

  it('ignores missing units', () => {
    const setup = setupServiceFake();
    const placement = create(setup, undefined);

    expect(placement.previewAt({}, '4', '5', 'VERTICAL')).toBeUndefined();
    expect(placement.placeAt({}, '4', '5', 'VERTICAL')).toBeUndefined();
    placement.clearUnit({});
    placement.commit({}, []);

    expect(setup.clearUnit).not.toHaveBeenCalled();
    expect(setup.previewUnitAt).not.toHaveBeenCalled();
    expect(setup.placeUnitAt).not.toHaveBeenCalled();
    expect(setup.placeUnit).not.toHaveBeenCalled();
    expect(setup.clearPreview).toHaveBeenCalledOnceWith();
  });
});

function setupServiceFake() {
  return {
    clearPreview: jasmine.createSpy('clearPreview'),
    clearUnit: jasmine.createSpy('clearUnit'),
    placeUnit: jasmine.createSpy('placeUnit'),
    placeUnitAt: jasmine.createSpy('placeUnitAt'),
    previewUnitAt: jasmine.createSpy('previewUnitAt'),
  };
}
