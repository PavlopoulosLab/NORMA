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
  normalizeSpacing,
  shownEdges,
  shownNodes,
  targetNodeSpacing,
} from '../metrics'
import { cy } from '../cy'
import { setStatus } from './controls'

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
  setStatus('layoutStatus', busy ? [{ level: 'busy', text: 'Computing layout…' }] : [])
}

async function runComputedLayout(compute) {
  cancelFrJobs()
  const run = ++S.layoutRunSeq
  const nodeCount = cy.nodes().length
  setLayoutBusy(true)
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
    if (run === S.layoutRunSeq) document.getElementById('btnRunLayout').disabled = false
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
  nodes.union(shownEdges(channelOnly)).layout(opts).run()
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
