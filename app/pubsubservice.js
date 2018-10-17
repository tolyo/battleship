
export default (() => {
  const cache = {}

  const subscribe = (topic, callback) => {
    if (!cache[topic]) {
      cache[topic] = []
    }
    cache[topic].push(callback)
    return [topic, callback]
  }

  const publish = (topic, args) => {
    if (cache[topic]) {
      cache[topic].forEach(callback => {
        callback.apply(null, ...(args || []))
      })
    }
  }

  const unsubscribe = handle => {
    const t = handle[0]
    if (cache[t]) {
      for (let x = 0; x < cache[t].length; x++) {
        if (cache[t][x] === handle[1]) {
          cache[t].splice(x, 1)
        }
      }
    }
  }

  return {
    publish,
    subscribe,
    unsubscribe
  }
})()
