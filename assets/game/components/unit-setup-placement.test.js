import { UnitSetupPlacement } from './unit-setup-placement.js';

describe('UnitSetupPlacement', () => {
  it('clears setup units and previews through setup service', () => {
    const setup = setupServiceFake();
    const placement = new UnitSetupPlacement(setup);
    const unit = { id: '2', size: 3 };

    placement.clearUnit(unit);
    placement.clearPreview();
    placement.previewAt(unit, '4', '5', 'VERTICAL');

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
    const placement = new UnitSetupPlacement(setup);
    const unit = { id: '2', size: 3 };
    const coordinates = [{ row: 1, column: 2 }];

    placement.placeAt(unit, '4', '5', 'HORIZONTAL');
    placement.commit(unit, coordinates);

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

  it('ignores missing units', () => {
    const setup = setupServiceFake();
    const placement = new UnitSetupPlacement(setup);

    expect(
      placement.previewAt(undefined, '4', '5', 'VERTICAL')
    ).toBeUndefined();
    expect(placement.placeAt(undefined, '4', '5', 'VERTICAL')).toBeUndefined();
    placement.clearUnit(undefined);
    placement.commit(undefined, []);

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
