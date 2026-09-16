// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import { UNGROUPED, getUsedGroups, groupLabel } from './network_state'
import { cy } from './cy'
import { downloadText, listSample, normalizeCssColor, plural, setStatus } from './layouts/controls'
import { edgeIsDirected } from './export/dialog'
import { exportEdges, exportNodes, exportShownOnly } from './metrics'
import { getUsedTypes } from './hulls'

/* ---------- NORMA export ---------- */
// NORMA can't read tabs or line breaks inside names, and node names can't
// contain commas (the annotation separator). Group names may keep commas.
function normaSafeName(name, renamed, isNode = true) {
  let safe = String(name).replace(/[\t\r\n]+/g, ' ')
  if (isNode) safe = safe.replace(/,/g, ';')
  if (safe !== String(name)) renamed.add(String(name))
  return safe
}

function renamedNote(renamed) {
  return renamed.size
    ? [
        {
          level: 'warn',
          text: `Changed ${plural(renamed.size, 'name')} that NORMA can't read (commas in node names became semicolons, tabs became spaces): ${listSample([...renamed])}.`,
        },
      ]
    : []
}

function connectedNodeIds() {
  const ids = new Set()
  exportEdges().forEach((e) => {
    if (e.data('source') !== e.data('target')) {
      ids.add(e.data('source'))
      ids.add(e.data('target'))
    }
  })
  return ids
}

function requireLoadedNetwork() {
  if (exportEdges().length) return true
  setStatus('saveStatus', [
    {
      level: 'error',
      text: cy.edges().length
        ? 'No edges are shown: tick at least one group and channel, or untick "Only ticked groups and channels".'
        : 'Load a network with at least one edge before saving NORMA files.',
    },
  ])
  return false
}

export function exportNormaNetwork() {
  if (!requireLoadedNetwork()) return
  const renamed = new Set()
  const types = getUsedTypes()
  const keepTypes = document.getElementById('normaKeepTypes').checked && types.length > 1
  const rowsByKey = new Map()
  let selfLoops = 0,
    merged = 0
  exportEdges().forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    if (s === t) {
      selfLoops++
      return
    }
    const type = e.data('type')
    const directed = edgeIsDirected(e)
    const pair = directed ? 'D\t' + s + '\t' + t : 'U\t' + (s < t ? s + '\t' + t : t + '\t' + s)
    const key = pair + (keepTypes ? '\t' + type : '')
    const w = e.data('weight')
    const row = rowsByKey.get(key)
    if (row) {
      merged++
      if (typeof w === 'number') row.weight = row.weight === undefined ? w : Math.max(row.weight, w)
    } else {
      rowsByKey.set(key, {
        source: s,
        target: t,
        type,
        directed,
        weight: typeof w === 'number' ? w : undefined,
      })
    }
  })
  const rows = [...rowsByKey.values()]
  const withDirection = rows.some((r) => r.directed)
  const weighted = rows.some((r) => r.weight !== undefined)
  const unweighted = weighted ? rows.filter((r) => r.weight === undefined).length : 0
  const header = ['Source', 'Target']
  if (weighted) header.push('Weight')
  if (keepTypes) header.push('Type')
  if (withDirection) header.push('Direction')
  const lines = [header.join('\t')]
  rows.forEach((r) => {
    const cols = [normaSafeName(r.source, renamed), normaSafeName(r.target, renamed)]
    if (weighted) cols.push(String(r.weight === undefined ? 1 : r.weight))
    if (keepTypes) cols.push(normaSafeName(r.type, renamed, false))
    if (withDirection) cols.push(r.directed ? 'directed' : 'undirected')
    lines.push(cols.join('\t'))
  })
  downloadText('norma-network.txt', lines.join('\n') + '\n')

  const notes = [
    {
      level: 'ok',
      text: `Saved norma-network.txt with ${plural(rows.length, 'row')}${weighted ? ', weighted' : ''}${keepTypes ? `, ${plural(types.length, 'type')} in a Type column` : ''}${withDirection ? ', with a Direction column' : ''}.`,
    },
  ]
  if (merged)
    notes.push({
      level: 'ok',
      text: keepTypes
        ? `Merged ${plural(merged, 'repeated edge')} with the same pair and channel.`
        : `Merged ${plural(merged, 'parallel edge')} into single connections${weighted ? ', keeping the highest weight' : ''}. Tick "Keep channels as a Type column" to keep them apart.`,
    })
  if (document.getElementById('normaKeepTypes').checked && types.length <= 1)
    notes.push({
      level: 'ok',
      text: 'The view has a single channel, so no Type column was needed.',
    })
  if (keepTypes || withDirection)
    notes.push({
      level: 'warn',
      text: `The ${[keepTypes && 'Type', withDirection && 'Direction'].filter(Boolean).join(' and ')} column${keepTypes && withDirection ? 's are' : ' is'} new in NORMA 3.0; earlier versions of NORMA read only Source, Target and Weight.`,
    })
  if (unweighted)
    notes.push({
      level: 'warn',
      text: `${plural(unweighted, 'row')} had no weight and got weight 1.`,
    })
  if (selfLoops) notes.push({ level: 'ok', text: `Left out ${plural(selfLoops, 'self-loop')}.` })
  const isolated = exportNodes().length - connectedNodeIds().size
  if (isolated)
    notes.push({
      level: 'warn',
      text: `${plural(isolated, 'node has', 'nodes have')} no edges and can't appear in a network file.`,
    })
  setStatus('saveStatus', [...notes, ...renamedNote(renamed)])
}

export function exportNormaAnnotation() {
  if (!requireLoadedNetwork()) return
  const groups = getUsedGroups().filter(
    (g) => g !== UNGROUPED && (!exportShownOnly() || S.activeGroups.has(g))
  )
  if (!groups.length) {
    setStatus('saveStatus', [
      {
        level: 'error',
        text: 'This view has no ticked groups, so there is no annotation to save.',
      },
    ])
    return
  }
  const renamed = new Set()
  const connected = connectedNodeIds()
  // A group's display label is its NORMA name, unless two groups share a
  // label; then the group key keeps them apart.
  const labelCounts = {}
  groups.forEach((g) => {
    const l = groupLabel(g)
    labelCounts[l] = (labelCounts[l] || 0) + 1
  })
  const membersOf = {}
  exportNodes().forEach((n) =>
    (n.data('groups') || []).forEach((g) => (membersOf[g] = membersOf[g] || []).push(n.id()))
  )
  const skippedGroups = []
  const droppedNodes = new Set()
  const lines = []
  groups.forEach((g) => {
    const members = membersOf[g] || []
    const present = members.filter((id) => connected.has(id))
    members.forEach((id) => {
      if (!connected.has(id)) droppedNodes.add(id)
    })
    const name = labelCounts[groupLabel(g)] > 1 ? g : groupLabel(g)
    if (!present.length) {
      skippedGroups.push(name)
      return
    }
    lines.push(
      `${normaSafeName(name, renamed, false)}\t${present.map((id) => normaSafeName(id, renamed)).join(',')}`
    )
  })
  if (!lines.length) {
    setStatus('saveStatus', [
      {
        level: 'error',
        text: 'None of the grouped nodes have edges, so the annotation would be empty.',
      },
    ])
    return
  }
  downloadText('norma-annotation.txt', lines.join('\n') + '\n')
  const notes = [
    { level: 'ok', text: `Saved norma-annotation.txt with ${plural(lines.length, 'group')}.` },
  ]
  if (droppedNodes.size)
    notes.push({
      level: 'warn',
      text: `Left out ${plural(droppedNodes.size, 'node')} without edges, since NORMA needs every annotated node in the network file: ${listSample([...droppedNodes])}.`,
    })
  if (skippedGroups.length)
    notes.push({
      level: 'warn',
      text: `Left out ${plural(skippedGroups.length, 'group')} whose nodes have no edges: ${listSample(skippedGroups)}.`,
    })
  setStatus('saveStatus', [...notes, ...renamedNote(renamed)])
}

export function exportNormaColors() {
  if (!requireLoadedNetwork()) return
  const renamed = new Set()
  const connected = connectedNodeIds()
  const nodes = exportNodes().filter((n) => connected.has(n.id()))
  const useOwn = nodes.some((n) => n.data('nodeColor'))
  const lines = []
  nodes.forEach((n) => {
    let color
    if (useOwn) {
      color = n.data('nodeColor')
    } else {
      const g = (n.data('groups') || []).find((x) => x !== UNGROUPED)
      color = g ? S.nodeColorMap[g] : null
    }
    if (!color) return
    // canvas-normalized rgba() values aren't NORMA colors; keep hex only
    const hex = String(color).startsWith('#') ? color : normalizeCssColor(color) || ''
    if (!hex.startsWith('#')) return
    lines.push(`${normaSafeName(n.id(), renamed)}\t${hex}`)
  })
  if (!lines.length) {
    setStatus('saveStatus', [
      {
        level: 'error',
        text: 'No node has a color to save. Load an expression file, or give nodes groups so their group colors can be used.',
      },
    ])
    return
  }
  downloadText('norma-expression.txt', lines.join('\n') + '\n')
  setStatus('saveStatus', [
    {
      level: 'ok',
      text: useOwn
        ? `Saved norma-expression.txt with the expression colors of ${plural(lines.length, 'node')}.`
        : `Saved norma-expression.txt using each node's first group color (${plural(lines.length, 'node')}), since no expression colors are loaded.`,
    },
    ...renamedNote(renamed),
  ])
}
