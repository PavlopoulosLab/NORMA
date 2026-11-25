/* ---------- how well a layout keeps the network's own structure ----------
   Group-aware layouts separate annotated groups on purpose, so a group
   separation score cannot tell whether that separation reflects the
   network. These two scores measure what a layout gives up to achieve it,
   independently of any annotation:
   - neighbourhood preservation: for each node with d neighbours in the
     network, the share of them among its d nearest nodes on screen
     (Gansner et al. 2013); 1 means the picture keeps every neighbourhood.
   - distance stress: how far on-screen distances depart from shortest-path
     distances, after the best uniform scaling (Gansner, Koren & North 2005),
     normalized to 0 (perfect) … 1; only pairs in the same component count.
   Both are computed on a fixed sample of source nodes for large networks. */

export interface FidelityEdge {
  source: string
  target: string
}

export interface TopologyFidelity {
  neighbourhood: number // 0..1, higher is better
  stress: number // 0..1, lower is better
  nodes: number // nodes with at least one neighbour that were scored
}

export interface FidelityOptions {
  sample?: number // at most this many source nodes (default 400)
  seed?: number
}

function mulberry(seed: number) {
  let a = seed >>> 0
  return () => {
    a = (a + 0x6d2b79f5) >>> 0
    let t = a
    t = Math.imul(t ^ (t >>> 15), t | 1)
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61)
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

export function topologyFidelity(
  positions: Record<string, { x: number; y: number }>,
  nodeIds: string[],
  edges: FidelityEdge[],
  opts: FidelityOptions = {}
): TopologyFidelity {
  const ids = nodeIds.filter((id) => positions[id])
  const n = ids.length
  const out: TopologyFidelity = { neighbourhood: NaN, stress: NaN, nodes: 0 }
  if (n < 3) return out
  const index = new Map(ids.map((id, i) => [id, i]))
  const adj: number[][] = ids.map(() => [])
  const seen = new Set<string>()
  edges.forEach((e) => {
    const s = index.get(e.source)
    const t = index.get(e.target)
    if (s === undefined || t === undefined || s === t) return
    const key = s < t ? `${s}\t${t}` : `${t}\t${s}`
    if (seen.has(key)) return
    seen.add(key)
    adj[s].push(t)
    adj[t].push(s)
  })
  const X = new Float64Array(n)
  const Y = new Float64Array(n)
  ids.forEach((id, i) => {
    X[i] = positions[id].x
    Y[i] = positions[id].y
  })

  // source nodes: all, or a fixed random sample
  const limit = opts.sample ?? 400
  let sources = ids.map((_, i) => i).filter((i) => adj[i].length > 0)
  if (sources.length > limit) {
    const rand = mulberry(opts.seed ?? 7)
    for (let i = sources.length - 1; i > 0; i--) {
      const j = Math.floor(rand() * (i + 1))
      ;[sources[i], sources[j]] = [sources[j], sources[i]]
    }
    sources = sources.slice(0, limit)
  }
  if (!sources.length) return out

  // --- neighbourhood preservation
  let kept = 0
  const d2 = new Float64Array(n)
  const order = new Int32Array(n)
  sources.forEach((s) => {
    const k = adj[s].length
    for (let i = 0; i < n; i++) {
      const dx = X[i] - X[s]
      const dy = Y[i] - Y[s]
      d2[i] = i === s ? Infinity : dx * dx + dy * dy
      order[i] = i
    }
    // the k nearest on screen (partial selection is enough: sort a copy)
    const nearest = Array.from(order)
      .sort((a, b) => d2[a] - d2[b])
      .slice(0, k)
    const neighbours = new Set(adj[s])
    let hit = 0
    nearest.forEach((i) => {
      if (neighbours.has(i)) hit++
    })
    kept += hit / k
  })
  out.neighbourhood = kept / sources.length
  out.nodes = sources.length

  // --- distance stress with the best uniform scale alpha:
  // minimise sum w (e - a*d)^2 with w = 1/d^2, giving a = sum(e/d) / sum(1)
  const dist = new Int32Array(n)
  const queue = new Int32Array(n)
  let sumRatio = 0
  let pairs = 0
  const eucl: number[] = []
  const hops: number[] = []
  sources.forEach((s) => {
    dist.fill(-1)
    dist[s] = 0
    let head = 0
    let tail = 0
    queue[tail++] = s
    while (head < tail) {
      const v = queue[head++]
      const dv = dist[v]
      adj[v].forEach((w) => {
        if (dist[w] < 0) {
          dist[w] = dv + 1
          queue[tail++] = w
        }
      })
    }
    for (let i = 0; i < n; i++) {
      if (i === s || dist[i] <= 0) continue
      const e = Math.hypot(X[i] - X[s], Y[i] - Y[s])
      eucl.push(e)
      hops.push(dist[i])
      sumRatio += e / dist[i]
      pairs++
    }
  })
  if (pairs) {
    const alpha = sumRatio / pairs
    let num = 0
    let den = 0
    for (let p = 0; p < pairs; p++) {
      const d = hops[p]
      const diff = eucl[p] - alpha * d
      num += (diff * diff) / (d * d)
      den += alpha * alpha
    }
    out.stress = den > 0 ? Math.min(1, num / den) : NaN
  }
  return out
}
