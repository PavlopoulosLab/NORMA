// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import { UNGROUPED, effectiveGroupsFor, getUsedGroups, groupLabel } from './network_state'
import { activeView, openInNewView } from './profiler'
import { applyGroupColorOverrides } from './arena3d'
import { cy } from './cy'
import { loadData } from './hulls'
import { plural, setStatus } from './layouts/controls'
import { shownEdges, shownNodes } from './metrics'
import { switchTab } from './wiring'

/* ---------- group network ---------- */
export function openGroupNetwork() {
  if (!cy.nodes().length) return
  const order = getUsedGroups().filter((g) => S.activeGroups.has(g) && g !== UNGROUPED)
  if (order.length < 2) {
    setStatus('gaStatus', [
      { level: 'error', text: 'The group network needs at least two ticked groups.' },
    ])
    return
  }
  const nodes = shownNodes()
  const groupsOf = new Map(
    nodes.map((n) => [n.id(), effectiveGroupsFor(n).filter((g) => g !== UNGROUPED)])
  )
  const size = new Map(order.map((g) => [g, 0]))
  groupsOf.forEach((gs) => gs.forEach((g) => size.set(g, (size.get(g) || 0) + 1)))
  const inside = new Map(order.map((g) => [g, 0]))
  const between = new Map()
  const seenPairs = new Set()
  shownEdges(true).forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    if (s === t || !groupsOf.has(s) || !groupsOf.has(t)) return
    const pk = s < t ? s + '\t' + t : t + '\t' + s
    if (seenPairs.has(pk)) return // parallel channels count once
    seenPairs.add(pk)
    const gs = groupsOf.get(s),
      gt = groupsOf.get(t)
    gs.forEach((a) =>
      gt.forEach((b) => {
        if (a === b) {
          inside.set(a, inside.get(a) + 1)
          return
        }
        const key = a < b ? a + '\u0000' + b : b + '\u0000' + a
        between.set(key, (between.get(key) || 0) + 1)
      })
    )
  })
  const maxSize = Math.max(...size.values())
  const data = {
    nodes: order
      .filter((g) => size.get(g))
      .map((g) => ({
        id: groupLabel(g),
        groups: [groupLabel(g)],
        size: Math.round(30 + 70 * Math.sqrt(size.get(g) / maxSize)),
        members: size.get(g),
        'edges inside': inside.get(g),
      })),
    edges: [...between].map(([key, w]) => {
      const [a, b] = key.split('\u0000')
      return {
        source: groupLabel(a),
        target: groupLabel(b),
        weight: w,
        type: 'connections',
        connections: w,
      }
    }),
    groupOrder: order.filter((g) => size.get(g)).map((g) => groupLabel(g)),
    nodeColors: Object.fromEntries(
      order.map((g) => [groupLabel(g), S.nodeColorMap[g] || '#888888'])
    ),
    edgeColors: { connections: '#64748b' },
    config: {
      layoutSelect: 'fr',
      edgeWidthMode: 'weight',
      edgeWidthMin: '1',
      edgeWidthMax: '10',
      sizeMetric: 'fixed',
      showEdgeLabels: true,
      edgeLabelContent: 'attr:connections',
      edgeLabelSize: '11',
    },
  }
  const v = activeView()
  openInNewView(`${v ? v.name : 'View'}: group network`, () => {
    loadData(data)
    applyGroupColorOverrides(data.nodeColors)
  })
  switchTab('network')
  setStatus('gaStatus', [
    {
      level: 'ok',
      text: `Opened the group network: ${plural(data.nodes.length, 'group')}, ${plural(data.edges.length, 'connection')} between groups. Node size follows group size; edge width follows the number of connections.`,
    },
  ])
}
