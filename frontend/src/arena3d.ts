// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import {
  UNGROUPED,
  effectiveGroupsFor,
  escapeHtml,
  getUsedGroups,
  groupLabel,
  refreshNodeVisual,
  sanitizeColor,
} from './network_state'
import { activeView } from './profiler'
import { applyGroupVisibility, buildGroupLegend } from './hulls'
import { cy } from './cy'
import { downloadText, fileStem, normaCols, normaLines, plural } from './layouts/controls'
import { groupAnalysisState } from './enrichment'
import { partitionStats } from './welcome'
import { shownEdges, shownNodes } from './metrics'
import { simpleGraph } from './wiring'

/* ---------- reading Arena3D network files ---------- */
// SourceNode, SourceLayer, TargetNode, TargetLayer, Weight, Channel:
// becomes a NORMA network (one channel per Channel value when there are
// several) and an annotation with one group per layer.
export function isArena3dNetworkText(text) {
  const first = normaLines(text).find((l) => l.trim())
  if (!first) return false
  const h = normaCols(first).map((c) => c.trim().toLowerCase())
  return (
    h[0] === 'sourcenode' &&
    h[1] === 'sourcelayer' &&
    h[2] === 'targetnode' &&
    h[3] === 'targetlayer'
  )
}

export function convertArena3dNetwork(text) {
  const lines = normaLines(text).filter((l) => l.trim())
  const head = normaCols(lines[0]).map((c) => c.trim().toLowerCase())
  const wCol = head.indexOf('weight'),
    cCol = head.indexOf('channel')
  const members = new Map()
  const channels = new Set()
  const rows = []
  lines.slice(1).forEach((line) => {
    const c = normaCols(line)
    if (c.length < 4) return
    const [sn, sl, tn, tl] = c.map((x) => x.trim())
    ;[
      [sn, sl],
      [tn, tl],
    ].forEach(([n, l]) => {
      if (!members.has(l)) members.set(l, new Set())
      members.get(l).add(n)
    })
    const ch = cCol >= 0 ? (c[cCol] || '').trim() : ''
    if (ch) channels.add(ch)
    rows.push([sn, tn, wCol >= 0 ? (c[wCol] || '').trim() : '', ch])
  })
  const typed = channels.size > 1
  const weighted = wCol >= 0
  const header = ['Source', 'Target', ...(weighted ? ['Weight'] : []), ...(typed ? ['Type'] : [])]
  const network =
    [
      header.join('\t'),
      ...rows.map((r) =>
        [r[0], r[1], ...(weighted ? [r[2] || '1'] : []), ...(typed ? [r[3] || '1'] : [])].join('\t')
      ),
    ].join('\n') + '\n'
  const annotation = [...members].map(([l, set]) => `${l}\t${[...set].join(',')}`).join('\n') + '\n'
  return { network, annotation, layers: members.size }
}

/* ============================================================
   GROUP ANALYSIS (Network Profiler page, current view)
     - per-group statistics and the modularity of the grouping
     - the group network: one node per group, edges weighted by the
       connections between groups, opened as a new view
     - enrichment: hypergeometric test of annotation terms in selected
       nodes or in each group of another annotation, with
       Benjamini-Hochberg FDR
   All of it uses the shown part of the view (ticked groups and channels).
   ============================================================ */

// log n! with a cached table
const LOG_FACT = [0]

function logFact(n) {
  for (let i = LOG_FACT.length; i <= n; i++) LOG_FACT[i] = LOG_FACT[i - 1] + Math.log(i)
  return LOG_FACT[n]
}

function logChoose(n, k) {
  return k < 0 || k > n ? -Infinity : logFact(n) - logFact(k) - logFact(n - k)
}

// P(X >= k) for X ~ Hypergeometric(N population, K successes, n draws)
export function hypergeomUpper(k, N, K, n) {
  const hi = Math.min(K, n)
  if (k > hi) return 0
  const lo = Math.max(k, 0)
  const denom = logChoose(N, n)
  const logs = []
  for (let x = lo; x <= hi; x++) logs.push(logChoose(K, x) + logChoose(N - K, n - x) - denom)
  const m = Math.max(...logs)
  const p = Math.exp(m) * logs.reduce((s, l) => s + Math.exp(l - m), 0)
  return Math.min(1, p)
}

export function benjaminiHochberg(ps) {
  const idx = ps.map((p, i) => [p, i]).sort((a, b) => a[0] - b[0])
  const q = new Array(ps.length)
  let min = 1
  for (let r = idx.length - 1; r >= 0; r--) {
    const [p, i] = idx[r]
    min = Math.min(min, (p * idx.length) / (r + 1))
    q[i] = min
  }
  return q
}

// the shown part of the view as a simple undirected graph
function shownSimpleGraph() {
  const nodes = shownNodes()
  const ids = nodes.map((n) => n.id())
  const idSet = new Set(ids)
  const pairs = []
  shownEdges(true).forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    if (s !== t && idSet.has(s) && idSet.has(t)) pairs.push([s, t])
  })
  return { nodes, ids, graph: simpleGraph(ids, pairs) }
}

function groupStatistics() {
  const { nodes, ids, graph } = shownSimpleGraph()
  const index = new Map(ids.map((id, i) => [id, i]))
  const groupsOf = ids.map((id) => effectiveGroupsFor(cy.$id(id)).filter((g) => g !== UNGROUPED))
  const order = getUsedGroups().filter((g) => S.activeGroups.has(g) && g !== UNGROUPED)
  const members = new Map(order.map((g) => [g, []]))
  groupsOf.forEach((gs, i) =>
    gs.forEach((g) => {
      if (members.has(g)) members.get(g).push(i)
    })
  )
  const m = graph.m,
    twoM = 2 * m
  const deg = graph.adj.map((a) => a.length)
  const rows = []
  members.forEach((list, g) => {
    if (!list.length) return
    const inG = new Set(list)
    let internal2 = 0,
      boundary = 0,
      vol = 0
    list.forEach((i) => {
      vol += deg[i]
      graph.adj[i].forEach((j) => {
        if (inG.has(j)) internal2++
        else boundary++
      })
    })
    const internal = internal2 / 2
    const k = list.length
    const pairsIn = (k * (k - 1)) / 2
    const volRest = twoM - vol
    rows.push({
      group: g,
      label: groupLabel(g),
      size: k,
      internal,
      boundary,
      density: pairsIn ? internal / pairsIn : NaN,
      avgInternalDegree: k ? internal2 / k : NaN,
      conductance: Math.min(vol, volRest) > 0 ? boundary / Math.min(vol, volRest) : NaN,
      share: twoM ? internal / m - Math.pow(vol / twoM, 2) : NaN, // modularity term
    })
  })
  // modularity of the grouping: nodes in several groups count for their first group, nodes in none are singletons
  const raw = new Int32Array(ids.length)
  const gIndex = new Map(order.map((g, i) => [g, i]))
  groupsOf.forEach((gs, i) => {
    raw[i] = gs.length ? gIndex.get(gs[0]) : order.length + i
  })
  const part = partitionStats(graph, raw)
  const overlapping = groupsOf.filter((gs) => gs.length > 1).length
  const ungrouped = groupsOf.filter((gs) => !gs.length).length
  // how much denser groups are inside than the network as a whole
  const netDensity = ids.length > 1 ? m / ((ids.length * (ids.length - 1)) / 2) : NaN
  return {
    rows,
    modularity: part.modularity,
    overlapping,
    ungrouped,
    nodes: ids.length,
    edges: m,
    netDensity,
  }
}

export function renderGroupStats() {
  const root = document.getElementById('gaStats')
  if (!cy.nodes().length) {
    root.innerHTML = '<p class="sub">Show a network first.</p>'
    return
  }
  const st = groupStatistics()
  if (!st.rows.length) {
    root.innerHTML = '<p class="sub">The current view has no ticked groups.</p>'
    return
  }
  const f = (v, d = 3) => (Number.isFinite(v) ? v.toFixed(d) : '—')
  const notes = []
  if (st.overlapping)
    notes.push(
      `${plural(st.overlapping, 'node')} in several groups ${st.overlapping === 1 ? 'counts' : 'count'} for ${st.overlapping === 1 ? 'its' : 'their'} first group`
    )
  if (st.ungrouped)
    notes.push(
      `${plural(st.ungrouped, 'node')} in no group ${st.ungrouped === 1 ? 'counts as a group of its own' : 'count as groups of their own'}`
    )
  root.innerHTML = `
    <p class="sub"><b>Modularity of the grouping: ${f(st.modularity)}</b> on ${plural(st.nodes, 'shown node')} and ${plural(st.edges, 'edge')} (network density ${f(st.netDensity, 4)})${notes.length ? `; ${notes.join(', ')}` : ''}. Values above about 0.3 mean the groups follow the network's structure.</p>
    <div class="table-wrap"><table class="data">
      <thead><tr><th scope="col">Group</th><th scope="col">Nodes</th><th scope="col">Edges inside</th><th scope="col">Edges leaving</th><th scope="col">Density inside</th><th scope="col">Mean degree inside</th><th scope="col">Conductance ↓</th><th scope="col">Modularity share</th></tr></thead>
      <tbody>${st.rows
        .map(
          (r) => `<tr>
        <td><span class="nodedot" style="background:${S.nodeColorMap[r.group] || '#888'}"></span> ${escapeHtml(r.label)}</td>
        <td class="num">${r.size}</td><td class="num">${r.internal}</td><td class="num">${r.boundary}</td>
        <td class="num">${f(r.density)}</td><td class="num">${f(r.avgInternalDegree, 2)}</td>
        <td class="num">${f(r.conductance)}</td><td class="num">${f(r.share, 4)}</td></tr>`
        )
        .join('')}</tbody>
    </table></div>`
  document.getElementById('btnGaStatsTsv').disabled = false
  groupAnalysisState.stats = st
}

export function groupStatsTsv() {
  const st = groupAnalysisState.stats
  if (!st) return
  const f = (v) => (Number.isFinite(v) ? String(Math.round(v * 1e6) / 1e6) : '')
  const lines = [
    `# modularity of the grouping\t${f(st.modularity)}`,
    [
      'Group',
      'Nodes',
      'Edges inside',
      'Edges leaving',
      'Density inside',
      'Mean degree inside',
      'Conductance',
      'Modularity share',
    ].join('\t'),
    ...st.rows.map((r) =>
      [
        r.label,
        r.size,
        r.internal,
        r.boundary,
        f(r.density),
        f(r.avgInternalDegree),
        f(r.conductance),
        f(r.share),
      ].join('\t')
    ),
  ]
  const v = activeView()
  downloadText(`group-statistics${v ? '-' + fileStem(v.name) : ''}.tsv`, lines.join('\n') + '\n')
}

// Group colors given with generated data win over the palette that a
// data config re-applies.
export function applyGroupColorOverrides(colors) {
  Object.entries(colors || {}).forEach(([g, c]) => {
    const sc = sanitizeColor(c)
    if (sc) S.nodeColorMap[g] = sc
  })
  cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
  buildGroupLegend()
  applyGroupVisibility()
}
