// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { PROFILE_PATH_LIMIT, formatStat } from './wiring'
import { escapeHtml } from './network_state'

/* ---------- directed statistics (Profiler and Compare) ---------- */
export const PROFILE_DIRECTED_STATS = [
  { key: 'dirEdges', label: 'Directed edges', desc: 'Distinct source → target connections.' },
  {
    key: 'reciprocity',
    label: 'Reciprocity',
    desc: 'Share of directed edges whose reverse edge also exists.',
  },
  { key: 'maxIn', label: 'Maximum in-degree', desc: 'Most incoming neighbors of any node.' },
  { key: 'maxOut', label: 'Maximum out-degree', desc: 'Most outgoing neighbors of any node.' },
  { key: 'sources', label: 'Source nodes', desc: 'Nodes with outgoing but no incoming edges.' },
  { key: 'sinks', label: 'Sink nodes', desc: 'Nodes with incoming but no outgoing edges.' },
  {
    key: 'scc',
    label: 'Strongly connected components',
    desc: 'Groups of nodes that can all reach each other along edge directions.',
  },
  {
    key: 'largestScc',
    label: 'Largest strongly connected component',
    desc: 'Nodes in the biggest such group, with its share of all nodes.',
  },
  {
    key: 'reachable',
    label: 'Reachable node pairs',
    desc: 'Share of ordered pairs (A, B) where B can be reached from A along edge directions.',
  },
  {
    key: 'dirAvgPath',
    label: 'Average directed path length',
    desc: 'Mean shortest path following edge directions, over reachable pairs.',
  },
  {
    key: 'dirDiameter',
    label: 'Directed diameter',
    desc: 'Longest shortest path following edge directions.',
  },
]

// edges: [source, target, directed]; undirected edges can be walked both ways
export function profileDirected(ids, edges) {
  const n = ids.length
  const index = new Map(ids.map((id, i) => [id, i]))
  const out = Array.from({ length: n }, () => new Set())
  const arcKeys = new Set()
  edges.forEach(([s, t, d]) => {
    const a = index.get(s),
      b = index.get(t)
    if (a === undefined || b === undefined || a === b) return
    out[a].add(b)
    if (d) arcKeys.add(a + ',' + b)
    else out[b].add(a)
  })
  const r = {}
  r.dirEdges = arcKeys.size
  let recip = 0
  arcKeys.forEach((k) => {
    const [a, b] = k.split(',')
    if (arcKeys.has(b + ',' + a)) recip++
  })
  r.reciprocity = arcKeys.size ? recip / arcKeys.size : NaN
  const indeg = new Int32Array(n)
  const adj = out.map((set) => Int32Array.from(set))
  adj.forEach((list) =>
    list.forEach((w) => {
      indeg[w]++
    })
  )
  r.maxIn = n ? Math.max(...indeg) : NaN
  r.maxOut = n ? Math.max(...adj.map((l) => l.length)) : NaN
  r.sources = 0
  r.sinks = 0
  for (let v = 0; v < n; v++) {
    if (indeg[v] === 0 && adj[v].length > 0) r.sources++
    if (adj[v].length === 0 && indeg[v] > 0) r.sinks++
  }

  // strongly connected components (iterative Tarjan)
  const idx = new Int32Array(n).fill(-1),
    low = new Int32Array(n),
    onStack = new Uint8Array(n)
  const stack = [],
    sccSizes = []
  let counter = 0
  for (let root = 0; root < n; root++) {
    if (idx[root] >= 0) continue
    const call = [[root, 0]]
    idx[root] = low[root] = counter++
    stack.push(root)
    onStack[root] = 1
    while (call.length) {
      const frame = call[call.length - 1]
      const v = frame[0]
      if (frame[1] < adj[v].length) {
        const w = adj[v][frame[1]++]
        if (idx[w] < 0) {
          idx[w] = low[w] = counter++
          stack.push(w)
          onStack[w] = 1
          call.push([w, 0])
        } else if (onStack[w]) low[v] = Math.min(low[v], idx[w])
      } else {
        call.pop()
        if (call.length) {
          const u = call[call.length - 1][0]
          low[u] = Math.min(low[u], low[v])
        }
        if (low[v] === idx[v]) {
          let size = 0,
            w
          do {
            w = stack.pop()
            onStack[w] = 0
            size++
          } while (w !== v)
          sccSizes.push(size)
        }
      }
    }
  }
  r.scc = sccSizes.length
  const big = sccSizes.length ? Math.max(...sccSizes) : 0
  r.largestScc = n ? `${big.toLocaleString('en-US')} (${((100 * big) / n).toFixed(1)}%)` : '—'

  // directed shortest paths
  if (n <= PROFILE_PATH_LIMIT) {
    const dist = new Int32Array(n),
      queue = new Int32Array(Math.max(1, n))
    let pairs = 0,
      total = 0,
      diameter = 0
    for (let s = 0; s < n; s++) {
      dist.fill(-1)
      dist[s] = 0
      let head = 0,
        tail = 0
      queue[tail++] = s
      while (head < tail) {
        const v = queue[head++]
        const nb = adj[v]
        for (let k = 0; k < nb.length; k++) {
          const w = nb[k]
          if (dist[w] < 0) {
            dist[w] = dist[v] + 1
            queue[tail++] = w
            pairs++
            total += dist[w]
            if (dist[w] > diameter) diameter = dist[w]
          }
        }
      }
    }
    r.reachable = n > 1 ? `${((100 * pairs) / (n * (n - 1))).toFixed(1)}%` : '—'
    r.dirAvgPath = pairs ? total / pairs : NaN
    r.dirDiameter = pairs ? diameter : NaN
  } else {
    r.reachable = r.dirAvgPath = r.dirDiameter = 'skipped'
  }
  return r
}

export function directedStatsTable(results) {
  let html =
    '<div class="table-wrap"><table class="data"><thead><tr><th scope="col">Statistic</th>' +
    results.map((r) => `<th scope="col">${escapeHtml(r.name)}</th>`).join('') +
    '</tr></thead><tbody>'
  PROFILE_DIRECTED_STATS.forEach((st) => {
    html +=
      `<tr><td>${escapeHtml(st.label)}<span class="stat-desc">${escapeHtml(st.desc)}</span></td>` +
      results
        .map(
          (r) =>
            `<td class="num">${escapeHtml(r.directed ? formatStat(r.directed[st.key]) : '—')}</td>`
        )
        .join('') +
      '</tr>'
  })
  return html + '</tbody></table></div>'
}
