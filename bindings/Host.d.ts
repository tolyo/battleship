/** Values crossing the boundary between browser services and Terlan controllers. */
export interface RoomIdentity<View> {
  readonly roomId: string;
  readonly playerId: string;
  readonly opponentId?: string;
  readonly view?: View;
}

export interface DashboardScope {
  authenticated?: boolean;
}

export interface PendingSnapshot<View> {
  pendingView?: View;
}

export interface MatchSnapshotState<View, Model> {
  readonly playerId?: string;
  readonly data: PendingSnapshot<View>;
  applyViewModel(model: Model): boolean;
}

export interface SnapshotProjection<View, Model> {
  project(view?: View): Model | undefined;
}

export interface ClientSignal {
  readonly type: string;
}

export interface ClientMove {
  readonly type: string;
  readonly row: number;
  readonly column: number;
}

export interface TimerHost {
  setTimeout(callback: () => void, delay: number): number;
  clearTimeout(handle: number): void;
}

export interface TimerState<Handle> {
  timerId?: Handle;
}

export interface RestoreStore<Entry> {
  currentRoomId(): string | null | undefined;
  restoreEntry(roomId: string): Entry | null | undefined;
}

export interface RoomEventEnvelope<Entry, View> {
  readonly type: string;
  readonly entry?: Entry;
  readonly updateUrl?: boolean;
  readonly view?: View;
  readonly reason?: string | null;
}
