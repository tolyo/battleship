export default (() => {
  let cache = {};

  const subscribe = (topic, cb) => {
    if (!cache[topic]) {
      cache[topic] = [];
    }
    cache[topic].push(cb);
    return [topic, cb];
  };

  const publish = (topic, args) => {
    if (cache[topic]) {
      cache[topic].forEach((cb) => {
        cb.apply(null, args || []);
      });
    }
  };

  const unsubscribe = (handle) => {
    const t = handle[0];
    if (cache[t]) {
      for (let x = 0; x < cache[t].length; x++) {
        if (cache[t][x] === handle[1]) {
          cache[t].splice(x, 1);
        }
      }
    }
  };

  const reset = () => {
    cache = {};
  };

  return {
    publish,
    subscribe,
    unsubscribe,
    reset,
  };
})();
