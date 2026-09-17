// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { mulberry32 } from '../sample_data'
import { partitionStats, splitDisconnected } from '../welcome'

/* ---------- label propagation ---------- */
export function labelPropagation(g, opts = {}) {
  const rand = mulberry32(opts.seed ?? 7)
  const n = g.n
  const label = Int32Array.from({ length: n }, (_, i) => i)
  const order = Array.from({ length: n }, (_, i) => i)
  for (let it = 0; it < 200; it++) {
    for (let i = n - 1; i > 0; i--) {
      const j = Math.floor(rand() * (i + 1))
      ;[order[i], order[j]] = [order[j], order[i]]
    }
    let changed = false
    for (const v of order) {
      const a = g.adj[v]
      if (!a.length) continue
      const counts = new Map()
      let best = 0
      for (let k = 0; k < a.length; k++) {
        const c = (counts.get(label[a[k]]) || 0) + 1
        counts.set(label[a[k]], c)
        if (c > best) best = c
      }
      const top = []
      counts.forEach((c, l) => {
        if (c === best) top.push(l)
      })
      if (top.includes(label[v])) continue // already a most frequent label
      label[v] = top[Math.floor(rand() * top.length)]
      changed = true
    }
    if (!changed) break
  }
  return partitionStats(g, splitDisconnected(g, label))
}
