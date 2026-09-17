// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { mulberry32 } from '../sample_data'
import { partitionStats } from '../welcome'

/* ---------- Leiden ---------- */
export function leidenCommunities(g, opts = {}) {
  const gamma = opts.resolution ?? 1
  const theta = opts.theta ?? 0.01
  const rand = mulberry32(opts.seed ?? 42)
  const n = g.n
  if (!g.m)
    return partitionStats(
      g,
      Int32Array.from({ length: n }, (_, i) => i)
    )
  // weighted graph: nbr[i] = Map(j -> w), kw[i] = weighted degree
  let nbr = g.adj.map((a) => {
    const mp = new Map()
    a.forEach((v) => mp.set(v, 1))
    return mp
  })
  let N = n
  const twoM = 2 * g.m
  let nodeOf = Int32Array.from({ length: n }, (_, i) => i) // original node -> aggregate node
  let part = Int32Array.from({ length: n }, (_, i) => i) // aggregate node -> community

  const shuffle = (arr) => {
    for (let i = arr.length - 1; i > 0; i--) {
      const j = Math.floor(rand() * (i + 1))
      ;[arr[i], arr[j]] = [arr[j], arr[i]]
    }
    return arr
  }

  for (let level = 0; level < 50; level++) {
    const kw = new Float64Array(N)
    nbr.forEach((mp, i) => {
      let s = 0
      mp.forEach((w, j) => {
        s += j === i ? 2 * w : w
      })
      kw[i] = s
    })
    // --- move nodes fast
    const tot = new Float64Array(N)
    for (let i = 0; i < N; i++) tot[part[i]] += kw[i]
    const queue = shuffle(Array.from({ length: N }, (_, i) => i))
    const inQueue = new Uint8Array(N).fill(1)
    let head = 0
    let changed = false
    while (head < queue.length) {
      const v = queue[head++]
      inQueue[v] = 0
      const cv = part[v]
      const links = new Map()
      nbr[v].forEach((w, u) => {
        if (u !== v) {
          const cu = part[u]
          links.set(cu, (links.get(cu) || 0) + w)
        }
      })
      tot[cv] -= kw[v]
      let best = cv
      let bestGain = (links.get(cv) || 0) - (gamma * kw[v] * tot[cv]) / twoM
      links.forEach((w, c) => {
        const gain = w - (gamma * kw[v] * tot[c]) / twoM
        if (gain > bestGain + 1e-12) {
          bestGain = gain
          best = c
        }
      })
      tot[best] += kw[v]
      if (best !== cv) {
        part[v] = best
        changed = true
        nbr[v].forEach((w, u) => {
          if (u !== v && part[u] !== best && !inQueue[u]) {
            inQueue[u] = 1
            queue.push(u)
          }
        })
      }
    }
    // communities of the current level
    const commMembers = new Map()
    for (let i = 0; i < N; i++) {
      if (!commMembers.has(part[i])) commMembers.set(part[i], [])
      commMembers.get(part[i]).push(i)
    }
    if (commMembers.size === N) break // every community is a single node: done
    // --- refinement: well-connected sub-communities inside each community
    const refined = Int32Array.from({ length: N }, (_, i) => i)
    const rTot = Float64Array.from(kw)
    const rSingle = new Uint8Array(N).fill(1)
    commMembers.forEach((members, c) => {
      if (members.length === 1) return
      const totC = members.reduce((s, i) => s + kw[i], 0)
      const inC = new Set(members)
      // links from each node to the rest of its community
      const kIn = new Map()
      members.forEach((v) => {
        let s = 0
        nbr[v].forEach((w, u) => {
          if (u !== v && inC.has(u)) s += w
        })
        kIn.set(v, s)
      })
      // links from each refined community to the rest of C (start: singletons)
      const extC = new Map(members.map((v) => [v, kIn.get(v)]))
      shuffle([...members]).forEach((v) => {
        if (!rSingle[v]) return
        const kv = kw[v]
        if (kIn.get(v) < (gamma * kv * (totC - kv)) / twoM) return // v not well connected
        const links = new Map()
        nbr[v].forEach((w, u) => {
          if (u !== v && inC.has(u)) {
            const r = refined[u]
            links.set(r, (links.get(r) || 0) + w)
          }
        })
        const own = refined[v]
        const candidates = []
        let maxGain = 0
        links.forEach((w, r) => {
          if (r === own) return
          const totR = rTot[r]
          // only well-connected refined communities are eligible
          if (extC.get(r) < (gamma * totR * (totC - totR)) / twoM) return
          const gain = w - (gamma * kv * totR) / twoM
          if (gain >= 0) {
            candidates.push([r, gain])
            if (gain > maxGain) maxGain = gain
          }
        })
        if (!candidates.length) return
        // choose randomly, favouring larger gains (theta controls randomness)
        const weights = candidates.map(([, gain]) => Math.exp((gain - maxGain) / theta))
        let pick = rand() * weights.reduce((s, w) => s + w, 0)
        let chosen = candidates[candidates.length - 1][0]
        for (let i = 0; i < candidates.length; i++) {
          pick -= weights[i]
          if (pick <= 0) {
            chosen = candidates[i][0]
            break
          }
        }
        // move v from its singleton into `chosen`
        const linksToChosen = links.get(chosen) || 0
        extC.set(chosen, extC.get(chosen) + kIn.get(v) - 2 * linksToChosen)
        extC.delete(own)
        rTot[chosen] += kv
        rTot[own] -= kv
        refined[v] = chosen
        rSingle[v] = 0
        rSingle[chosen] = 0 // refined ids are node indices, so this is the community's first node
      })
    })
    // --- aggregate on the refined partition; each aggregate node keeps its
    // (non-refined) community as its starting community
    const rmap = new Map()
    for (let i = 0; i < N; i++) if (!rmap.has(refined[i])) rmap.set(refined[i], rmap.size)
    const nextN = rmap.size
    if (nextN === N && !changed) break
    const agg = Array.from({ length: nextN }, () => new Map())
    const nextPart = new Int32Array(nextN)
    for (let i = 0; i < N; i++) {
      const ri = rmap.get(refined[i])
      nextPart[ri] = part[i]
      nbr[i].forEach((w, j) => {
        const rj = rmap.get(refined[j])
        // self-loops are stored once with their full weight
        if (ri === rj) {
          if (j >= i) agg[ri].set(ri, (agg[ri].get(ri) || 0) + (i === j ? w : w))
        } else agg[ri].set(rj, (agg[ri].get(rj) || 0) + w)
      })
    }
    nodeOf = nodeOf.map((x) => rmap.get(refined[x]))
    // community ids of the aggregate start partition
    const cmap = new Map()
    for (let i = 0; i < nextN; i++) {
      if (!cmap.has(nextPart[i])) cmap.set(nextPart[i], cmap.size)
      nextPart[i] = cmap.get(nextPart[i])
    }
    nbr = agg
    part = nextPart
    N = nextN
    if (nextN === cmap.size) break // aggregation changes nothing more
  }
  const raw = new Int32Array(n)
  for (let i = 0; i < n; i++) raw[i] = part[nodeOf[i]]
  return partitionStats(g, raw)
}
