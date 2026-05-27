/**
 * @typedef {object} ServerMessage
 * @property {string} type
 * @property {string=} room_id
 * @property {string=} player_id
 * @property {string=} opponent_id
 * @property {string=} reason
 * @property {unknown=} game
 */

/**
 * @param {unknown} value
 * @returns {value is ServerMessage}
 */
function isServerMessage(value) {
  return (
    typeof value === 'object' &&
    value !== null &&
    'type' in value &&
    typeof value.type === 'string'
  );
}

/**
 * @param {unknown} data
 * @returns {ServerMessage}
 */
export function decodeServerMessage(data) {
  if (typeof data !== 'string') {
    return { type: 'unknown' };
  }

  try {
    const parsed = /** @type {unknown} */ (JSON.parse(data));
    if (isServerMessage(parsed)) {
      return parsed;
    }
  } catch {
    return { type: 'invalid_payload' };
  }

  return { type: 'unknown' };
}
