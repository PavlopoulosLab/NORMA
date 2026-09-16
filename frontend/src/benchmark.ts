// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { activeView } from './profiler'
import {
  applyLocalLayoutsAfter,
  blockGroupLayout,
  collectLayoutGraph,
  strategyGravity,
  strategySuperNodes,
  strategyVirtualNodes,
} from './layouts/input'
import {
  computeSubLayout,
  computeSubLayoutAsync,
  normalizeSpacing,
  targetNodeSpacing,
} from './metrics'
import { downloadText, fileStem, plural, setLayoutMode, setStatus } from './layouts/controls'
import { escapeHtml } from './network_state'
import { groupSeparation } from './clustering/mapping'
import { runActiveLayout } from './layouts/run'
import { switchTab } from './wiring'

/* ---------- layout benchmark (Network Profiler) ---------- */
const BENCH_LAYOUTS = [
  ['conn', 'cose', 'Force-directed (cose)'],
  ['conn', 'fr', 'Force-directed, weighted'],
  ['conn', 'kk', 'Kamada–Kawai'],
  ['conn', 'stress', 'Stress majorization'],
  ['conn', 'circle', 'Circle'],
  ['conn', 'concentric', 'Concentric'],
  ['conn', 'breadthfirst', 'Hierarchical'],
  ['conn', 'grid', 'Grid'],
  ['conn', 'random', 'Random (baseline)'],
  ['groups', 'fr', 'Groups: force-directed, weighted'],
  ['groups', 'cose', 'Groups: force-directed (cose)'],
  ['groups', 'circle', 'Groups: circle'],
  ['groups', 'grid', 'Groups: grid'],
  ['groups', 'breadthfirst', 'Groups: hierarchical'],
  ['groups', 'concentric', 'Groups: concentric'],
  ['groups', 'virtual', 'NORMA-2.0 strategy 1: virtual nodes'],
  ['groups', 'gravity', 'NORMA-2.0 strategy 2: group gravity'],
  ['groups', 'supernodes', 'NORMA-2.0 strategy 3: super nodes'],
]

const benchState = { rows: [], running: false, cancel: false }

async function benchmarkPositions(kind, name, graph) {
  const spacing = targetNodeSpacing()
  if (kind === 'conn') {
    const pos = ['fr', 'kk', 'stress'].includes(name)
      ? await computeSubLayoutAsync(graph.nodeIds, graph.edges, name)
      : computeSubLayout(graph.nodeIds, graph.edges, name)
    return pos ? normalizeSpacing(pos, spacing) : null
  }
  const algorithm = document.getElementById('strategyAlgorithm').value
  const force = parseFloat(document.getElementById('groupForce').value) || 10
  const clusterScale = parseFloat(document.getElementById('groupClusterRadius').value) || 1
  let localName = document.getElementById('localGroupLayout').value
  const keepAllowed = name === 'virtual' || name === 'gravity'
  if (localName === 'keep' && !keepAllowed) localName = 'circle'
  if (name === 'virtual')
    return applyLocalLayoutsAfter(
      graph,
      await strategyVirtualNodes(graph, algorithm),
      localName,
      force,
      clusterScale
    )
  if (name === 'gravity')
    return applyLocalLayoutsAfter(
      graph,
      await strategyGravity(graph, algorithm, force),
      localName,
      force,
      clusterScale
    )
  if (name === 'supernodes')
    return strategySuperNodes(graph, algorithm, localName, force, clusterScale)
  return blockGroupLayout(graph, name, localName, force, clusterScale)
}

export async function runLayoutBenchmark() {
  if (benchState.running) {
    benchState.cancel = true
    return
  }
  const graph = collectLayoutGraph()
  const btn = document.getElementById('btnBench')
  if (graph.nodeIds.length < 3) {
    setStatus('benchStatus', [
      { level: 'error', text: 'Show a network with at least three nodes first.' },
    ])
    return
  }
  if (graph.groupMembers.size < 2) {
    setStatus('benchStatus', [
      {
        level: 'error',
        text: 'The current view needs at least two ticked groups to measure separation.',
      },
    ])
    return
  }
  const repeats = parseInt(document.getElementById('benchRepeats').value, 10) || 1
  benchState.running = true
  benchState.cancel = false
  btn.textContent = 'Stop'
  const rows = []
  const started = performance.now()
  try {
    for (const [kind, name, label] of BENCH_LAYOUTS) {
      if (benchState.cancel) break
      setStatus('benchStatus', [
        {
          level: 'busy',
          text: `Running ${label} (${rows.length + 1} of ${BENCH_LAYOUTS.length})…`,
        },
      ])
      await new Promise((r) => setTimeout(r, 20))
      const runs = []
      for (let k = 0; k < repeats; k++) {
        const t0 = performance.now()
        let pos
        try {
          pos = await benchmarkPositions(kind, name, graph)
        } catch (err) {
          pos = null
        }
        const ms = performance.now() - t0
        if (!pos) continue
        runs.push({ ms, ...groupSeparation(pos, graph.groupsOf) })
      }
      const agg = (key) => {
        const vals = runs.map((r) => r[key]).filter(Number.isFinite)
        if (!vals.length) return { mean: NaN, sd: NaN }
        const mean = vals.reduce((s, v) => s + v, 0) / vals.length
        const sd =
          vals.length > 1
            ? Math.sqrt(vals.reduce((s, v) => s + (v - mean) ** 2, 0) / (vals.length - 1))
            : NaN
        return { mean, sd }
      }
      rows.push({
        kind,
        name,
        label,
        runs: runs.length,
        ms: agg('ms'),
        silhouette: agg('silhouette'),
        intruders: agg('intruders'),
        overlap: agg('overlap'),
      })
      benchState.rows = rows
      renderBenchmark(graph)
    }
    const secs = ((performance.now() - started) / 1000).toFixed(1)
    setStatus('benchStatus', [
      {
        level: benchState.cancel ? 'warn' : 'ok',
        text: `${benchState.cancel ? 'Stopped after' : 'Benchmarked'} ${plural(rows.length, 'layout')} on ${plural(graph.nodeIds.length, 'node')} and ${plural(graph.groupMembers.size, 'group')} in ${secs} s. The view itself was not changed.`,
      },
    ])
    document.getElementById('btnBenchTsv').disabled = !rows.length
  } finally {
    benchState.running = false
    btn.textContent = 'Benchmark layouts'
  }
}

function renderBenchmark(graph) {
  const root = document.getElementById('benchResults')
  const rows = benchState.rows
  if (!rows.length) {
    root.innerHTML = ''
    return
  }
  const best = (key) => {
    const vals = rows.map((r) => r[key].mean).filter(Number.isFinite)
    if (!vals.length) return null
    return key === 'silhouette' ? Math.max(...vals) : Math.min(...vals)
  }
  const bS = best('silhouette'),
    bI = best('intruders'),
    bO = best('overlap')
  const cell = (v, fmt, isBest) =>
    `<td class="num${isBest ? ' cmp-max' : ''}">${Number.isFinite(v.mean) ? fmt(v.mean) + (Number.isFinite(v.sd) ? ` <span class="sd">± ${fmt(v.sd)}</span>` : '') : '—'}</td>`
  const pct = (v) => `${(v * 100).toFixed(1)}%`
  const num = (v) => v.toFixed(3)
  const ms = (v) => (v < 1000 ? `${Math.round(v)} ms` : `${(v / 1000).toFixed(1)} s`)
  root.innerHTML = `<div class="table-wrap"><table class="data bench-table">
    <thead><tr><th scope="col">Layout</th><th scope="col">Time</th><th scope="col">Silhouette ↑</th><th scope="col">Inside other outlines ↓</th><th scope="col">Outline overlap ↓</th><th scope="col"></th></tr></thead>
    <tbody>${rows
      .map(
        (r, i) => `<tr>
      <td>${escapeHtml(r.label)}</td>
      ${cell(r.ms, ms, false)}
      ${cell(r.silhouette, num, r.silhouette.mean === bS)}
      ${cell(r.intruders, pct, r.intruders.mean === bI)}
      ${cell(r.overlap, pct, r.overlap.mean === bO)}
      <td><button type="button" class="bench-use" data-row="${i}" title="Apply this layout to the view">Use</button></td>
    </tr>`
      )
      .join('')}</tbody></table></div>`
  root
    .querySelectorAll('.bench-use')
    .forEach((b) => b.addEventListener('click', () => useBenchLayout(rows[+b.dataset.row])))
}

function useBenchLayout(row) {
  if (row.kind === 'conn') {
    setLayoutMode('connections')
    document.getElementById('layoutSelect').value = row.name
  } else {
    setLayoutMode('groups')
    document.getElementById('groupArrangement').value = row.name
    document
      .getElementById('groupArrangement')
      .dispatchEvent(new Event('change', { bubbles: true }))
  }
  switchTab('network')
  runActiveLayout()
}

export function benchmarkTsv() {
  const rows = benchState.rows
  if (!rows.length) return
  const f = (v, d = 4) => (Number.isFinite(v) ? v.toFixed(d) : '')
  const lines = [
    [
      'Layout',
      'Runs',
      'Time ms (mean)',
      'Time ms (sd)',
      'Silhouette (mean)',
      'Silhouette (sd)',
      'Inside other outlines (mean)',
      'Inside other outlines (sd)',
      'Outline overlap (mean)',
      'Outline overlap (sd)',
    ].join('\t'),
  ]
  rows.forEach((r) =>
    lines.push(
      [
        r.label,
        r.runs,
        f(r.ms.mean, 1),
        f(r.ms.sd, 1),
        f(r.silhouette.mean),
        f(r.silhouette.sd),
        f(r.intruders.mean),
        f(r.intruders.sd),
        f(r.overlap.mean),
        f(r.overlap.sd),
      ].join('\t')
    )
  )
  const v = typeof activeView === 'function' && activeView()
  downloadText(`layout-benchmark${v ? '-' + fileStem(v.name) : ''}.tsv`, lines.join('\n') + '\n')
}
