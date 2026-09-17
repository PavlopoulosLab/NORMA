// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import { applyEdgeLabels, scheduleBundling } from './profiler'
import { applyNodeSizing, invalidateFullMetrics } from './metrics'
import { currentTheme } from './themes'
import { cy } from './cy'
import { edgeIsDirected } from './export/dialog'

/* ---------- parallel edges: merge per node pair ---------- */
// In merge mode each node pair keeps one visible edge (its first edge in an
// active channel). That edge is widened by the number of active channels
// on the pair and turns gray when they disagree; the popup lists them all.
// Uses per-edge style bypasses so it layers over whatever width and color
// rules the stylesheet currently has.
function edgeMergeOn() {
  const el = document.getElementById('edgeMergeMode')
  return !!el && el.value === 'merge'
}

export function applyEdgeMerge() {
  if (typeof cy === 'undefined') return
  const on = edgeMergeOn()
  cy.batch(() => {
    cy.edges('.merged-rep').forEach((e) => {
      e.removeStyle('width line-color target-arrow-color')
      e.removeClass('merged-rep')
    })
    cy.edges('.merged-hidden').removeClass('merged-hidden')
    cy.edges().forEach((e) => {
      if (e.data('mergedWith')) e.data('mergedWith', null)
    })
    if (!on) return
    const byPair = new Map()
    cy.edges().forEach((e) => {
      if (e.hasClass('hidden-type')) return
      const s = e.data('source'),
        t = e.data('target')
      // directed edges merge only with edges running the same way
      const key = edgeIsDirected(e)
        ? 'D\t' + s + '\t' + t
        : 'U\t' + (s < t ? s + '\t' + t : t + '\t' + s)
      const list = byPair.get(key)
      if (list) list.push(e)
      else byPair.set(key, [e])
    })
    const baseFixed = parseFloat(document.getElementById('edgeWidthFixed').value) || 2
    const byWeight = document.getElementById('edgeWidthMode').value === 'weight'
    byPair.forEach((list) => {
      if (list.length < 2) return
      const [rep, ...rest] = list
      rest.forEach((e) => e.addClass('merged-hidden'))
      rep.data(
        'mergedWith',
        list.map((e) => e.id())
      )
      const colors = new Set(list.map((e) => e.data('color')))
      const base = byWeight ? rep.data('edgeWidth') || baseFixed : baseFixed
      rep.addClass('merged-rep')
      rep.style({
        width: Math.min(base * (1 + 0.7 * (list.length - 1)), base * 6),
        'line-color': colors.size === 1 ? rep.data('color') : currentTheme.muted,
        'target-arrow-color': colors.size === 1 ? rep.data('color') : currentTheme.muted,
      })
    })
  })
  applyEdgeLabels()
  if (typeof scheduleBundling === 'function') scheduleBundling()
}

export function applyTypeVisibility() {
  cy.edges().forEach((e) => {
    if (S.activeTypes.has(e.data('type'))) e.removeClass('hidden-type')
    else e.addClass('hidden-type')
  })
  applyEdgeMerge()
  invalidateFullMetrics()
  // if node sizes are being driven by a channel-scoped metric, keep them live
  if (document.getElementById('sizeChannelOnly').checked) applyNodeSizing()
}
