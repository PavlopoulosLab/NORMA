// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from '../state'
import {
  applyLocalLayoutsAfter,
  blockGroupLayout,
  collectLayoutGraph,
  strategyGravity,
  strategySuperNodes,
  strategyVirtualNodes,
} from './input'
import {
  applyPositions,
  cancelFrJobs,
  computeSubLayoutAsync,
  layoutOptsFor,
  makeLayoutProgress,
  normalizeSpacing,
  shownEdges,
  shownNodes,
  targetNodeSpacing,
} from '../metrics'
import { cy } from '../cy'
import { setStatus } from './controls'
import CoseWorker from './cose.worker?worker&inline'

/* ---------- running layouts ---------- */
export const STRATEGIES = new Set(['virtual', 'gravity', 'supernodes'])

export function layoutMode() {
  return document.getElementById('layoutMode').value === 'groups' ? 'groups' : 'connections'
}

export function groupArrangement() {
  return document.getElementById('groupArrangement').value
}

export function setLayoutBusy(busy) {
  document.getElementById('btnRunLayout').disabled = busy
  setStatus(
    'layoutStatus',
    busy ? [{ level: 'busy', text: 'Computing layout…', progress: null }] : []
  )
}

async function runComputedLayout(compute) {
  cancelFrJobs()
  cancelCoseJob()
  const run = ++S.layoutRunSeq
  const nodeCount = cy.nodes().length
  setLayoutBusy(true)
  S.layoutProgress = makeLayoutProgress((f) => {
    if (run === S.layoutRunSeq)
      setStatus('layoutStatus', [{ level: 'busy', text: 'Computing layout…', progress: f }])
  })
  // let the busy note paint before any work on the page starts
  await new Promise((r) => setTimeout(r, 0))
  try {
    const positions = await compute()
    if (run !== S.layoutRunSeq || cy.nodes().length !== nodeCount) return
    if (positions) applyPositions(positions)
  } catch (err) {
    if (run === S.layoutRunSeq)
      setStatus('layoutStatus', [
        { level: 'error', text: `The layout couldn't be computed: ${err.message}` },
      ])
    return
  } finally {
    if (run === S.layoutRunSeq) {
      document.getElementById('btnRunLayout').disabled = false
      S.layoutProgress = null
    }
  }
  if (run === S.layoutRunSeq) setStatus('layoutStatus', [])
}

// Layout by connections: groups play no part.
export function runLayout(name) {
  if (name === 'fr' || name === 'kk' || name === 'stress') {
    runComputedLayout(async () => {
      const graph = collectLayoutGraph()
      if (!graph.nodeIds.length) return null
      const pos = await computeSubLayoutAsync(graph.nodeIds, graph.edges, name)
      return pos ? normalizeSpacing(pos, targetNodeSpacing()) : null
    })
    return
  }
  // a Cytoscape layout replaces any computed one still running
  cancelFrJobs()
  cancelCoseJob()
  S.layoutRunSeq++
  setLayoutBusy(false)
  // Animating every force-directed iteration is slow on bigger graphs;
  // there, compute first and animate only to the final positions.
  const nodes = shownNodes()
  const animate = name === 'cose' && nodes.length > 150 ? 'end' : true
  const opts = { ...layoutOptsFor(name), animate, animationDuration: 500, padding: 50 }

  // Only the shown part is arranged: nodes of ticked groups and, with
  // "Use checked channels only", edges of ticked channels. Hidden nodes
  // keep their positions. Shown nodes without a counted edge still take
  // part (repulsion only).
  const channelOnly = document.getElementById('layoutOnActiveOnly').checked
  if (!nodes.length) return
  const eles = nodes.union(shownEdges(channelOnly))
  if (animate !== 'end') {
    eles.layout(opts).run()
    return
  }
  // Computed in one go, in a worker, then animated to the final positions.
  // Once the page has painted a frame, Cytoscape's cose runs 5-7x slower on
  // the main thread (a V8 effect, reproducible with plain Cytoscape); a
  // worker is a fresh isolate, so it stays fast and the page stays usable.
  const run = S.layoutRunSeq
  document.getElementById('btnRunLayout').disabled = true
  setStatus('layoutStatus', [{ level: 'busy', text: 'Computing layout…', progress: null }])
  const finish = () => {
    if (run !== S.layoutRunSeq) return
    document.getElementById('btnRunLayout').disabled = false
    setStatus('layoutStatus', [])
  }
  coseInWorker(eles, opts)
    .then((positions) => {
      if (run !== S.layoutRunSeq) return
      if (positions) {
        eles
          .nodes()
          .layout({
            name: 'preset',
            positions,
            fit: true,
            padding: opts.padding,
            animate: true,
            animationDuration: opts.animationDuration,
          })
          .run()
      } else eles.layout(opts).run() // no worker (very old browser): as before
    })
    .finally(finish)
}

/* ---------- cose in a worker ---------- */
let coseWorker = null
let coseJob = null // { resolve } of the run in progress

function coseInWorker(eles, opts) {
  cancelCoseJob()
  if (!coseWorker) {
    try {
      coseWorker = new CoseWorker()
    } catch (e) {
      return Promise.resolve(null)
    }
    coseWorker.onmessage = (e) => {
      const job = coseJob
      coseJob = null
      if (job) job.resolve(e.data.error ? null : e.data.positions)
    }
    coseWorker.onerror = (e) => {
      e.preventDefault()
      const job = coseJob
      coseJob = null
      coseWorker = null
      if (job) job.resolve(null)
    }
  }
  // what cose reads: node ids, positions and outer sizes, edges, the viewport
  const elements = eles.nodes().map((n) => ({
    data: { id: n.id(), w: n.outerWidth(), h: n.outerHeight() },
    position: n.position(),
  }))
  eles
    .edges()
    .forEach((e) =>
      elements.push({ data: { id: e.id(), source: e.source().id(), target: e.target().id() } })
    )
  const { animate, animationDuration, ...rest } = opts
  const layout = { ...rest, boundingBox: { x1: 0, y1: 0, w: cy.width(), h: cy.height() } }
  return new Promise((resolve) => {
    coseJob = { resolve }
    coseWorker.postMessage({
      elements,
      style: [{ selector: 'node', style: { width: 'data(w)', height: 'data(h)' } }],
      opts: layout,
    })
  })
}

// Stops a cose computation still running in the worker; its caller sees null.
export function cancelCoseJob() {
  if (!coseJob) return
  const job = coseJob
  coseJob = null
  coseWorker.terminate()
  coseWorker = null
  job.resolve(null)
}

// Layout by groups: block arrangements or a NORMA-2.0 strategy, each
// followed by the chosen layout inside every group.
export function runGroupLayout() {
  const graph = collectLayoutGraph()
  if (!graph.nodeIds.length) return
  if (!graph.groupMembers.size) {
    setStatus('layoutStatus', [
      {
        level: 'warn',
        text: 'This view has no active groups to arrange, so the nodes were arranged by their connections instead.',
      },
    ])
    runLayout('fr')
    return
  }
  const arrangement = groupArrangement()
  const algorithm = document.getElementById('strategyAlgorithm').value
  const force = parseFloat(document.getElementById('groupForce').value) || 10
  const clusterScale = parseFloat(document.getElementById('groupClusterRadius').value) || 1
  let localName = document.getElementById('localGroupLayout').value
  const keepAllowed = arrangement === 'virtual' || arrangement === 'gravity'
  if (localName === 'keep' && !keepAllowed) localName = 'circle'
  runComputedLayout(async () => {
    if (arrangement === 'virtual') {
      return applyLocalLayoutsAfter(
        graph,
        await strategyVirtualNodes(graph, algorithm),
        localName,
        force,
        clusterScale
      )
    }
    if (arrangement === 'gravity') {
      return applyLocalLayoutsAfter(
        graph,
        await strategyGravity(graph, algorithm, force),
        localName,
        force,
        clusterScale
      )
    }
    if (arrangement === 'supernodes') {
      return strategySuperNodes(graph, algorithm, localName, force, clusterScale)
    }
    return blockGroupLayout(graph, arrangement, localName, force, clusterScale)
  })
}

// Runs whichever layout mode is active.
export function runActiveLayout() {
  if (layoutMode() === 'groups') runGroupLayout()
  else runLayout(document.getElementById('layoutSelect').value)
}

// Older names still called elsewhere (loading data, switching annotations).
export function runStrategyLayout() {
  runActiveLayout()
}

function runGroupedLayout() {
  runActiveLayout()
}
