// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { PIE_MAX_SLICES, effectiveGroupsFor } from '../network_state'
import { cy } from '../cy'
import { net3d } from './state'

/* ---------- element cache: everything read from the 2D elements ---------- */
export function buildCache3d() {
  const nodes = []
  const byId = new Map()
  cy.nodes().forEach((n) => {
    if (!n.visible()) return
    const pies = []
    if (n.style('shape') === 'ellipse') {
      for (let i = 1; i <= PIE_MAX_SLICES; i++) {
        const size = parseFloat(n.data('pieSize' + i)) || 0
        if (size > 0) pies.push([n.data('pieColor' + i), size / 100])
      }
    }
    const rec = {
      id: n.id(),
      el: n,
      size: n.width(),
      color: n.style('background-color'),
      pies,
      shape: n.style('shape'),
      border: n.style('border-color'),
      borderW: n.pstyle('border-width').pfValue,
      opacity: n.effectiveOpacity(),
      underlay:
        n.pstyle('underlay-opacity').value > 0
          ? {
              color: n.style('underlay-color'),
              alpha: n.pstyle('underlay-opacity').value,
              pad: n.pstyle('underlay-padding').pfValue,
            }
          : null,
      label: n.style('label'),
      labelColor: n.style('color'),
      halo: n.style('text-outline-color'),
      haloW: n.pstyle('text-outline-width').pfValue,
      fontPx: n.pstyle('font-size').pfValue,
      valign: n.style('text-valign'),
      halign: n.style('text-halign'),
      marginX: n.pstyle('text-margin-x').pfValue || 0,
      marginY: n.pstyle('text-margin-y').pfValue || 0,
      groups: effectiveGroupsFor(n),
      highlighted: n.hasClass('highlighted'),
    }
    nodes.push(rec)
    byId.set(rec.id, rec)
  })
  const edges = []
  const pairCount = new Map()
  cy.edges().forEach((e) => {
    if (!e.visible()) return
    const s = e.data('source'),
      t = e.data('target')
    if (!byId.has(s) || !byId.has(t) || s === t) return
    const pair = s < t ? s + '\t' + t : t + '\t' + s
    const k = pairCount.get(pair) || 0
    pairCount.set(pair, k + 1)
    const arrow = e.style('target-arrow-shape')
    edges.push({
      id: e.id(),
      el: e,
      s,
      t,
      pair,
      index: k,
      color: e.style('line-color'),
      width: e.pstyle('width').pfValue,
      opacity: e.effectiveOpacity(),
      arrow: arrow && arrow !== 'none' ? arrow : null,
      arrowColor: e.style('target-arrow-color'),
      arrowScale: e.pstyle('arrow-scale').value || 1,
      label: e.style('label'),
      labelColor: e.style('color'),
      labelBg: e.style('text-background-color'),
      fontPx: e.pstyle('font-size').pfValue,
      rotate: e.style('text-rotation') === 'autorotate',
      underlay: e.pstyle('underlay-opacity').value > 0 ? e.style('underlay-color') : null,
    })
  })
  edges.forEach((e) => {
    e.siblings = pairCount.get(e.pair)
  })
  net3d.cache = { nodes, edges, byId }
  net3d.cacheDirty = false
}

/* ---------- positions ---------- */
export function pos3dOf(id) {
  let p = net3d.pos.get(id)
  if (!p) {
    const n = cy.getElementById(id)
    const q = n.length ? n.position() : { x: 0, y: 0 }
    p = [q.x, q.y, 0]
    net3d.pos.set(id, p)
  }
  return p
}
