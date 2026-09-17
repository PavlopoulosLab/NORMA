// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import {
  UNGROUPED,
  UNGROUPED_LABEL,
  buildAttrSchema,
  colorForGroup,
  getUsedGroups,
  renderAttrSchema,
  setGroupOrder,
} from './network_state'
import {
  annotationText,
  downloadText,
  fileStem,
  libEntry,
  libSelection,
  listSample,
  plural,
  selectedNetworks,
  selectionKey,
  setStatus,
  updateRefreshState,
} from './layouts/controls'
import { applyGroupVisibility, buildGroupLegend, drawGroupHulls, loadData } from './hulls'
import { applyValueColors } from './clustering/mapping'
import { bumpDataVersion, noteChange } from './demo_downloads'
import { cy } from './cy'
import { hideEdgePopup, hideInfo } from './profiler'
import { layoutMode, runGroupLayout } from './layouts/run'
import { nextLoadPositions } from './api/tester'
import { updateContextInfo } from './recording'

/* ---------- combining library files into a view ---------- */
// Merges the chosen networks into one node/edge set. With one network its
// edges keep their Type (or the network name) as channel; with several,
// every network is its own channel ("name" or "name: type"), so pairs
// found in more than one network become parallel edges.
function combineNetworks(netEntries) {
  const multi = netEntries.length > 1
  const nodeOrder = []
  const nodeNetworks = new Map()
  const edges = []
  netEntries.forEach((ne) => {
    ne.parsed.nodes.forEach((id) => {
      if (!nodeNetworks.has(id)) {
        nodeNetworks.set(id, [])
        nodeOrder.push(id)
      }
      nodeNetworks.get(id).push(ne.name)
    })
    ne.parsed.edges.forEach((e, i) => {
      const type = e.type ? (multi ? `${ne.name}: ${e.type}` : e.type) : ne.name
      edges.push({
        id: `${ne.id}-${i}`,
        source: e.source,
        target: e.target,
        type,
        ...(e.weight !== undefined ? { weight: e.weight } : {}),
        ...(e.directed ? { directed: true } : {}),
        ...(multi ? { network: ne.name } : {}),
      })
    })
  })
  return { nodeOrder, nodeNetworks, edges, multi }
}

// Resolves an annotation and an expression file against a set of node ids.
function resolveGroupsAndColors(nodeIds, viewName, annEntry, colEntry) {
  const nodeSet = new Set(nodeIds)
  const notes = []
  const groupsOf = {}
  let groupCount = 0
  let groupOrder = null

  if (annEntry) {
    const missing = new Set()
    const emptied = []
    const kept = []
    annEntry.parsed.groups.forEach(({ name, members }) => {
      const present = members.filter((m) => nodeSet.has(m))
      members.forEach((m) => {
        if (!nodeSet.has(m)) missing.add(m)
      })
      if (!present.length) {
        emptied.push(name)
        return
      }
      kept.push({ name, members: present })
      present.forEach((m) => (groupsOf[m] = groupsOf[m] || []).push(name))
    })
    groupCount = kept.length
    groupOrder = kept.map((g) => g.name)
    if (!kept.length) {
      notes.push({
        level: 'error',
        text: `None of the names in "${annEntry.name}" are in ${viewName}, so no groups are shown. Check that the files belong together.`,
      })
    } else {
      if (missing.size || emptied.length) {
        const parts = []
        if (missing.size)
          parts.push(
            `Dropped ${plural(missing.size, 'annotation name')} not found in ${viewName}: ${listSample([...missing])}.`
          )
        if (emptied.length)
          parts.push(
            `Left out ${plural(emptied.length, 'group')} with no nodes in the view: ${listSample(emptied)}.`
          )
        const text = annotationText(kept)
        notes.push({
          level: 'warn',
          text: parts.join(' '),
          action: {
            label: 'Download corrected annotation',
            run: () => downloadText(`${fileStem(annEntry.name)}-corrected.txt`, text),
          },
        })
      }
      const ungrouped = nodeIds.filter((n) => !groupsOf[n]).length
      if (ungrouped)
        notes.push({
          level: 'ok',
          text: `${plural(ungrouped, 'node is', 'nodes are')} in no group and listed as "${UNGROUPED_LABEL}".`,
        })
    }
  }

  const colorOf = new Map()
  const valuesOf = new Map()
  if (colEntry && colEntry.parsed.numeric) {
    const unknown = []
    colEntry.parsed.values.forEach((row, node) => {
      if (nodeSet.has(node)) valuesOf.set(node, row)
      else unknown.push(node)
    })
    if (unknown.length)
      notes.push({
        level: 'warn',
        text: `Ignored values for ${plural(unknown.length, 'node')} not found in ${viewName}: ${listSample(unknown)}.`,
      })
    const without = nodeIds.length - valuesOf.size
    if (without)
      notes.push({
        level: 'ok',
        text: `${plural(without, 'node has', 'nodes have')} no value and ${without === 1 ? 'is' : 'are'} drawn in the "no value" color.`,
      })
  } else if (colEntry) {
    const unknown = []
    colEntry.parsed.colors.forEach((color, node) => {
      if (nodeSet.has(node)) colorOf.set(node, color)
      else unknown.push(node)
    })
    if (unknown.length)
      notes.push({
        level: 'warn',
        text: `Ignored colors for ${plural(unknown.length, 'node')} not found in ${viewName}: ${listSample(unknown)}.`,
      })
    const uncolored = nodeIds.length - colorOf.size
    if (uncolored)
      notes.push({
        level: 'ok',
        text: `${plural(uncolored, 'node has', 'nodes have')} no expression color and ${uncolored === 1 ? 'is' : 'are'} gray.`,
      })
  }
  return { groupsOf, colorOf, valuesOf, notes, groupCount, groupOrder }
}

// NORMA's look: node fill from expression colors with groups shaded behind;
// without an expression file, fill nodes with their group colors.
function applyLibraryDisplayDefaults(annEntry, colEntry) {
  document.getElementById('nodeFillSelect').value = colEntry
    ? colEntry.parsed.numeric
      ? 'values'
      : 'data'
    : 'groups'
  if (colEntry && annEntry) {
    document.getElementById('showGroupHulls').checked = true
    document.getElementById('hullControls').style.display = 'block'
  }
}

// Changes groups and colors of the nodes already on screen, keeping their
// positions (unless grouped layout is on, which depends on the groups).
export function applyGroupsAndColorsInPlace(groupsOf, colorOf, groupOrder, meta) {
  bumpDataVersion()
  S.nodeColorMap = {}
  S.groupAttrs = meta ? JSON.parse(JSON.stringify(meta)) : {}
  setGroupOrder(groupOrder)
  let anyUngrouped = false
  cy.batch(() => {
    cy.nodes().forEach((n) => {
      const own = groupsOf[n.id()]
      const groups = own && own.length ? own : [UNGROUPED]
      if (!own || !own.length) anyUngrouped = true
      n.data({ groups, group: groups.join(', '), nodeColor: colorOf.get(n.id()) || null })
    })
  })
  if (anyUngrouped)
    S.groupAttrs[UNGROUPED] = {
      label: UNGROUPED_LABEL,
      description: 'Nodes that no group lists as a member',
    }
  const used = getUsedGroups()
  used.forEach((g) => colorForGroup(g))
  S.activeGroups = new Set(used)
  buildGroupLegend()
  applyGroupVisibility()
  buildAttrSchema()
  renderAttrSchema()
  hideInfo()
  hideEdgePopup()
  cy.elements().removeClass('dimmed highlighted')
  if (layoutMode() === 'groups') runGroupLayout()
  drawGroupHulls()
}

export function refreshLibraryView() {
  noteChange()
  const nets = selectedNetworks()
  if (!nets.length) {
    setStatus('normaStatus', [
      {
        level: 'error',
        text: 'Tick at least one network, then show it in this view or open it in a new one.',
      },
    ])
    return
  }
  const annEntry = libEntry('annotation', libSelection.annotation)
  const colEntry = libEntry('colors', libSelection.colors)
  const key = selectionKey()
  const sameNetworks =
    S.currentLibView && S.currentLibView.nets === key.nets && cy.nodes().length > 0

  const combined = combineNetworks(nets)
  const viewName = nets.length === 1 ? `"${nets[0].name}"` : 'the selected networks'
  const { groupsOf, colorOf, valuesOf, notes, groupCount, groupOrder } = resolveGroupsAndColors(
    combined.nodeOrder,
    viewName,
    annEntry,
    colEntry
  )
  applyLibraryDisplayDefaults(annEntry, colEntry)

  // extra details carried by library entries (e.g. from STRING)
  const groupMeta = annEntry && annEntry.groupMeta ? annEntry.groupMeta : null
  const nodeExtra = (id) => {
    const out = {}
    nets.forEach((e) => {
      if (e.nodeAttrs && e.nodeAttrs[id]) Object.assign(out, e.nodeAttrs[id])
    })
    return out
  }
  const keptPositions = nets.length === 1 && nets[0].nodePositions ? nets[0].nodePositions : null
  if (sameNetworks) {
    cy.batch(() => cy.nodes().forEach((n) => n.data('values', valuesOf.get(n.id()) || null)))
    applyGroupsAndColorsInPlace(groupsOf, colorOf, groupOrder, groupMeta)
    applyValueColors()
  } else {
    loadData(
      {
        groupOrder,
        ...(groupMeta ? { groupAttrs: JSON.parse(JSON.stringify(groupMeta)) } : {}),
        nodes: combined.nodeOrder.map((id) => ({
          ...nodeExtra(id),
          id,
          groups: groupsOf[id] || [],
          ...(colorOf.has(id) ? { color: colorOf.get(id) } : {}),
          ...(valuesOf.has(id) ? { values: valuesOf.get(id) } : {}),
          ...(combined.multi ? { networks: combined.nodeNetworks.get(id) } : {}),
        })),
        edges: combined.edges,
      },
      nextLoadPositions || keptPositions ? { positions: nextLoadPositions || keptPositions } : {}
    )
  }
  S.currentLibView = key
  updateRefreshState()
  updateContextInfo()

  const shownNets =
    nets.length === 1
      ? `"${nets[0].name}" (${nets[0].parsed.summary})`
      : `${nets.length} networks overlaid as channels: ${combined.nodeOrder.length} nodes, ${combined.edges.length} edges`
  const parts = [`Showing ${shownNets}`]
  if (annEntry) parts.push(`${plural(groupCount, 'group')} from "${annEntry.name}"`)
  if (colEntry) parts.push(`colors from "${colEntry.name}"`)
  const lead = (sameNetworks ? 'Updated groups and colors in place. ' : '') + parts.join(', ') + '.'
  if (groupCount > 30 && document.getElementById('showGroupHulls').checked) {
    notes.push({
      level: 'ok',
      text: `With ${groupCount} groups shaded at once the view gets crowded. Type in the Node groups filter and use "Deactivate matches" / "Activate matches" to focus on a few.`,
    })
  }
  setStatus('normaStatus', [{ level: 'ok', text: lead }, ...notes])
}
