// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { WALKTRAP_NODE_LIMIT, partitionStats } from '../welcome'

/* ---------- Walktrap ---------- */
export function walktrap(g, opts = {}) {
  const t = opts.steps ?? 4
  const n = g.n
  if (n > WALKTRAP_NODE_LIMIT)
    throw new Error(
      `Walktrap is limited to ${WALKTRAP_NODE_LIMIT.toLocaleString('en-US')} nodes in the browser.`
    )
  if (!g.m)
    return partitionStats(
      g,
      Int32Array.from({ length: n }, (_, i) => i)
    )
  // random walk with a self-loop on every node (as in the original method)
  const deg = g.adj.map((a) => a.length + 1)
  const step = (vec) => {
    const out = new Map()
    vec.forEach((p, u) => {
      const share = p / deg[u]
      out.set(u, (out.get(u) || 0) + share)
      const a = g.adj[u]
      for (let k = 0; k < a.length; k++) out.set(a[k], (out.get(a[k]) || 0) + share)
    })
    return out
  }
  // P^t for every node (sparse)
  const P = new Array(n)
  for (let i = 0; i < n; i++) {
    let v = new Map([[i, 1]])
    for (let s = 0; s < t; s++) v = step(v)
    // negligible probabilities are dropped to keep memory in check
    v.forEach((p, k) => {
      if (p < 1e-7) v.delete(k)
    })
    P[i] = v
  }
  const dist2 = (a, b) => {
    // sum_k (a_k - b_k)^2 / d_k
    let s = 0
    a.forEach((x, k) => {
      const y = b.get(k) || 0
      s += ((x - y) * (x - y)) / deg[k]
    })
    b.forEach((y, k) => {
      if (!a.has(k)) s += (y * y) / deg[k]
    })
    return s
  }
  const size = new Int32Array(2 * n).fill(0)
  for (let i = 0; i < n; i++) size[i] = 1
  const vec = P // community vectors, extended as merges happen
  const nbrs = Array.from({ length: n }, (_, i) => new Map()) // community -> Map(neighbour -> delta sigma)
  const alive = new Uint8Array(2 * n)
  for (let i = 0; i < n; i++) alive[i] = 1
  // min-heap of [delta, a, b]
  const heap = []
  const push = (item) => {
    heap.push(item)
    let i = heap.length - 1
    while (i > 0) {
      const p = (i - 1) >> 1
      if (heap[p][0] <= heap[i][0]) break
      ;[heap[p], heap[i]] = [heap[i], heap[p]]
      i = p
    }
  }
  const pop = () => {
    const top = heap[0]
    const last = heap.pop()
    if (heap.length) {
      heap[0] = last
      let i = 0
      for (;;) {
        const l = 2 * i + 1,
          r = l + 1
        let m = i
        if (l < heap.length && heap[l][0] < heap[m][0]) m = l
        if (r < heap.length && heap[r][0] < heap[m][0]) m = r
        if (m === i) break
        ;[heap[m], heap[i]] = [heap[i], heap[m]]
        i = m
      }
    }
    return top
  }
  const deltaSigma = (a, b) =>
    (((size[a] * size[b]) / (size[a] + size[b])) * dist2(vec[a], vec[b])) / n
  for (let u = 0; u < n; u++) {
    g.adj[u].forEach((v) => {
      if (v <= u) return
      const d = deltaSigma(u, v)
      nbrs[u].set(v, d)
      nbrs[v].set(u, d)
      push([d, u, v])
    })
  }
  // modularity bookkeeping on the original graph
  const twoM = 2 * g.m
  const inW = new Float64Array(2 * n),
    totW = new Float64Array(2 * n)
  for (let i = 0; i < n; i++) totW[i] = g.adj[i].length
  const between = Array.from({ length: n }, (_, u) => {
    const mp = new Map()
    g.adj[u].forEach((v) => mp.set(v, (mp.get(v) || 0) + 1))
    return mp
  })
  let q = 0
  for (let i = 0; i < n; i++) q -= Math.pow(totW[i] / twoM, 2)
  const merges = []
  let bestQ = q,
    bestStep = 0
  let next = n
  while (heap.length) {
    const [d, a, b] = pop()
    if (!alive[a] || !alive[b] || nbrs[a].get(b) !== d) continue
    const c = next++
    alive[a] = 0
    alive[b] = 0
    alive[c] = 1
    size[c] = size[a] + size[b]
    const va = vec[a],
      vb = vec[b]
    const vc = new Map()
    va.forEach((x, k) => vc.set(k, (x * size[a]) / size[c]))
    vb.forEach((x, k) => vc.set(k, (vc.get(k) || 0) + (x * size[b]) / size[c]))
    vec[c] = vc
    vec[a] = null
    vec[b] = null
    // modularity change
    const eab = between[a].get(b) || 0
    q += (2 * eab) / twoM - (2 * totW[a] * totW[b]) / (twoM * twoM)
    inW[c] = inW[a] + inW[b] + 2 * eab
    totW[c] = totW[a] + totW[b]
    const bc = new Map()
    ;[a, b].forEach((x) =>
      between[x].forEach((w, y) => {
        if (y !== a && y !== b) bc.set(y, (bc.get(y) || 0) + w)
      })
    )
    between[c] = bc
    bc.forEach((w, y) => {
      between[y].delete(a)
      between[y].delete(b)
      between[y].set(c, w)
    })
    merges.push([a, b, c])
    if (q > bestQ + 1e-12) {
      bestQ = q
      bestStep = merges.length
    }
    // new distances to neighbouring communities (Lance-Williams where possible)
    const nc = new Map()
    const neighbours = new Set([...nbrs[a].keys(), ...nbrs[b].keys()])
    neighbours.delete(a)
    neighbours.delete(b)
    neighbours.forEach((x) => {
      let dnew
      if (nbrs[a].has(x) && nbrs[b].has(x)) {
        dnew =
          ((size[a] + size[x]) * nbrs[a].get(x) +
            (size[b] + size[x]) * nbrs[b].get(x) -
            size[x] * d) /
          (size[a] + size[b] + size[x])
      } else {
        dnew = deltaSigma(c, x)
      }
      nc.set(x, dnew)
      nbrs[x].delete(a)
      nbrs[x].delete(b)
      nbrs[x].set(c, dnew)
      push([dnew, Math.min(c, x), Math.max(c, x)])
    })
    nbrs[c] = nc
    nbrs[a] = new Map()
    nbrs[b] = new Map()
  }
  // replay merges up to the step with the highest modularity
  const parent = Int32Array.from({ length: 2 * n }, (_, i) => i)
  const find = (x) => {
    while (parent[x] !== x) {
      parent[x] = parent[parent[x]]
      x = parent[x]
    }
    return x
  }
  for (let s = 0; s < bestStep; s++) {
    const [a, b, c] = merges[s]
    parent[a] = c
    parent[b] = c
  }
  const raw = new Int32Array(n)
  for (let i = 0; i < n; i++) raw[i] = find(i)
  return partitionStats(g, raw)
}
