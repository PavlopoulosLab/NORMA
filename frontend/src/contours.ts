// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { EDGE_TYPES } from './palette'
import { NORMA_CFG } from './config'
import { S } from './state'
import {
  UNGROUPED,
  effectiveGroupsFor,
  getUsedGroups,
  groupLabel,
  nodeFillMode,
} from './network_state'
import { activeView } from './profiler'
import { bubbleContour, bubblePathD } from './clustering/wiring'
import { buildCompareArena3dModel } from './enrichment'
import { cy } from './cy'
import { dataVersion } from './demo_downloads'
import { downloadText, fileStem, plural, setStatus } from './layouts/controls'
import { drawGroupHulls, hexToRgba, hullCanvasCss } from './hulls'
import { edgeIsDirected } from './export/dialog'
import { resolveStringRoute } from './string/requests'
import { rgbOf } from './view3d/state'
import { shownEdges, shownNodes } from './metrics'

/* ---------- 2D: cached contours in model coordinates ---------- */
var bubbleCache = { key: '', paths: new Map(), computedAt: 0, timer: null }

function bubbleSettings(zoom) {
  // screen-pixel margins turned into model units at this zoom
  const mScale = Math.max(0.15, Math.min(1, zoom))
  return {
    margin: (36 * mScale) / zoom, // contour about 12 px outside the nodes
    edgeRadius: (24 * mScale) / zoom, // links about 8 px wide on each side
    avoid: (14 * mScale) / zoom,
  }
}

function bubbleKey(nodes, zoom) {
  let h = 0
  nodes.forEach((n, i) => {
    const p = n.position()
    h =
      (h * 31 + Math.round(p.x * 2) * 7 + Math.round(p.y * 2) * 13 + Math.round(n.width()) + i) %
      1000000007
  })
  const zb = Math.round(Math.log2(zoom) * 4)
  return `${dataVersion}|${zb}|${h}|${nodes.length}|${[...S.activeGroups].join('\u0001')}`
}

// Contours for every ticked group, as SVG path data in model coordinates.
export function bubblePathsModel(force) {
  const shown = cy.nodes().filter((n) => !n.hasClass('hidden-group'))
  const zoom = cy.zoom()
  const key = bubbleKey(shown, zoom)
  if (!force && key === bubbleCache.key) return bubbleCache.paths
  // while things move, recompute at most every 120 ms (large networks: only
  // once they stop) and redraw when they stop
  const now = performance.now()
  const big = shown.length > 1500
  const sameData = bubbleCache.key.split('|')[0] === String(dataVersion)
  if (!force && bubbleCache.key && sameData && (big || now - bubbleCache.computedAt < 120)) {
    clearTimeout(bubbleCache.timer)
    // redraw inside an animation frame: a canvas drawn from a bare timer is
    // not always shown until something else repaints (seen in Chrome on macOS)
    bubbleCache.timer = setTimeout(
      () =>
        requestAnimationFrame(() => {
          bubbleCache.computedAt = 0
          bubbleCache.settled = true
          drawGroupHulls()
        }),
      big ? 300 : 140
    )
    if (!(big && bubbleCache.settled)) return bubbleCache.paths
  }
  bubbleCache.settled = false
  const zb = Math.pow(2, Math.round(Math.log2(zoom) * 4) / 4)
  const opts = bubbleSettings(zb)
  const all = shown.map((n) => ({
    id: n.id(),
    x: n.position().x,
    y: n.position().y,
    r: n.width() / 2,
    groups: effectiveGroupsFor(n),
  }))
  const paths = new Map()
  getUsedGroups()
    .filter((g) => S.activeGroups.has(g) && g !== UNGROUPED)
    .forEach((g) => {
      const members = all.filter((p) => p.groups.includes(g))
      if (!members.length) return
      const others = all.filter((p) => !p.groups.includes(g))
      const loops = bubbleContour(members, others, opts)
      paths.set(g, bubblePathD(loops))
    })
  bubbleCache.key = key
  bubbleCache.paths = paths
  bubbleCache.computedAt = now
  return paths
}

export function drawBubbleSets(ctx, opacity) {
  const paths = bubblePathsModel(false)
  const z = cy.zoom(),
    pan = cy.pan()
  // the scale the canvas was actually sized with, not a pixel ratio that
  // may have changed since
  const dpr = hullCanvasCss.dpr || 1
  ctx.save()
  ctx.setTransform(z * dpr, 0, 0, z * dpr, pan.x * dpr, pan.y * dpr)
  paths.forEach((d, g) => {
    if (!d) return
    const color = S.nodeColorMap[g] || '#888888'
    const path = new Path2D(d)
    ctx.fillStyle = hexToRgba(color, opacity)
    ctx.fill(path, 'evenodd')
    ctx.lineWidth = 1.6 / z
    ctx.strokeStyle = hexToRgba(color, Math.min(1, opacity + 0.45))
    ctx.stroke(path)
  })
  ctx.restore()
}

/* ============================================================
   ARENA3D EXPORT
   Arena3D (https://arena3d.org) shows multilayer networks in 3D. Each
   ticked group of the current view becomes one layer; nodes keep their
   2D positions inside their layer, edges inside a group stay in its layer
   and edges between groups run between layers. A node in several groups
   appears once in each of its layers.
   Three outputs:
     - Arena3D JSON (Arena3D's own export format)
     - Arena3D network file (SourceNode, SourceLayer, TargetNode,
       TargetLayer, Weight, Channel)
     - Open in Arena3D: the JSON is POSTed to <server>/api/external,
       which answers { token, url }; the url is opened in a new tab.
   ============================================================ */
export const ARENA3D_LAYER_SPACING = 960

export const ARENA3D_LAYER_WIDTH = 947

const ARENA3D_COPY_CHANNEL = 'same node'

export const ARENA3D_MAX_LAYERS = 20

function arena3dBase() {
  const raw = (document.getElementById('arenaAddress').value || 'https://arena3d.org')
    .trim()
    .replace(/\/+$/, '')
  return /^https?:\/\//i.test(raw) ? raw : 'https://' + raw
}

export function hexColor(c) {
  const [r, g, b] = rgbOf(c)
  const h = (x) =>
    Math.max(0, Math.min(255, Math.round(x)))
      .toString(16)
      .padStart(2, '0')
  return '#' + h(r) + h(g) + h(b)
}

// Arena3D layer names can't contain underscores (node ids are name_layer).
export function arenaSafe(s) {
  return (
    String(s)
      .replace(/_/g, '-')
      .replace(/[\t\r\n]+/g, ' ')
      .trim() || '-'
  )
}

function buildArena3dModel() {
  const connectCopies = document.getElementById('arenaCopies').checked
  const colorMode = document.getElementById('arenaNodeColors').value
  const nodes = shownNodes()
  const edges = shownEdges(true).filter((e) => e.data('source') !== e.data('target'))
  if (!nodes.length) throw new Error('Show a network first.')

  // layers: ticked groups in legend order, then one for nodes in no group
  const groupOrder = getUsedGroups().filter((g) => S.activeGroups.has(g) && g !== UNGROUPED)
  const layersOf = new Map()
  let ungrouped = false
  nodes.forEach((n) => {
    let gs = effectiveGroupsFor(n).filter((g) => g !== UNGROUPED)
    if (!gs.length) {
      gs = [UNGROUPED]
      ungrouped = true
    }
    layersOf.set(n.id(), gs)
  })
  let layerKeys = [...groupOrder.filter((g) => nodes.some((n) => layersOf.get(n.id()).includes(g)))]
  if (ungrouped) layerKeys.push(UNGROUPED)
  // Arena3D takes up to 20 layers: keep the first 20 (group list order)
  let droppedLayers = 0,
    droppedNodes = 0
  if (layerKeys.length > ARENA3D_MAX_LAYERS) {
    droppedLayers = layerKeys.length - ARENA3D_MAX_LAYERS
    layerKeys = layerKeys.slice(0, ARENA3D_MAX_LAYERS)
    const kept = new Set(layerKeys)
    ;[...layersOf].forEach(([id, gs]) => {
      const left = gs.filter((g) => kept.has(g))
      if (left.length) layersOf.set(id, left)
      else {
        layersOf.delete(id)
        droppedNodes++
      }
    })
  }
  const usedNames = new Set()
  const layerName = new Map(
    layerKeys.map((g) => {
      let name = arenaSafe(g === UNGROUPED ? 'No group' : groupLabel(g))
      while (usedNames.has(name)) name += '′'
      usedNames.add(name)
      return [g, name]
    })
  )
  const nodeName = (id) => arenaSafe(id)

  // positions: each layer's nodes are fitted into the layer's square
  const layers = layerKeys.map((g, i) => ({
    key: g,
    name: layerName.get(g),
    position_x: String((i - (layerKeys.length - 1) / 2) * ARENA3D_LAYER_SPACING),
    position_y: '0',
    position_z: '0',
    last_layer_scale: '1',
    rotation_x: '0',
    rotation_y: '0',
    rotation_z: '0',
    floor_current_color: g === UNGROUPED ? '#777777' : hexColor(S.nodeColorMap[g] || '#777777'),
    geometry_parameters_width: String(ARENA3D_LAYER_WIDTH),
  }))
  const outNodes = []
  const half = ARENA3D_LAYER_WIDTH * 0.42
  layers.forEach((layer) => {
    const members = nodes.filter(
      (n) => layersOf.has(n.id()) && layersOf.get(n.id()).includes(layer.key)
    )
    const xs = members.map((n) => n.position('x')),
      ys = members.map((n) => n.position('y'))
    const cx = (Math.min(...xs) + Math.max(...xs)) / 2,
      cy0 = (Math.min(...ys) + Math.max(...ys)) / 2
    const span = Math.max(Math.max(...xs) - Math.min(...xs), Math.max(...ys) - Math.min(...ys)) || 1
    const k = members.length > 1 ? (2 * half) / span : 0
    members.forEach((n) => {
      // pie nodes (several groups) take the color of the layer they are drawn in
      const multi = nodeFillMode() === 'groups' && effectiveGroupsFor(n).length > 1
      const color =
        (colorMode === 'group' || multi) && layer.key !== UNGROUPED
          ? S.nodeColorMap[layer.key] || '#777777'
          : n.style('background-color')
      const attrs = n.data('attrs') || {}
      outNodes.push({
        name: nodeName(n.id()),
        layer: layer.name,
        position_x: '0',
        position_y: String(-(n.position('y') - cy0) * k),
        position_z: String((n.position('x') - cx) * k),
        scale: String(Math.round(Math.max(0.3, Math.min(3, (n.width() || 42) / 42)) * 100) / 100),
        color: hexColor(color),
        url:
          typeof attrs.url === 'string'
            ? attrs.url
            : attrs.stringId
              ? `https://string-db.org/network/${attrs.stringId}`
              : '',
        descr: typeof attrs.description === 'string' ? attrs.description : '',
      })
    })
  })

  // edges
  const weights = edges.map((e) => e.data('weight')).filter((w) => typeof w === 'number')
  const weighted = weights.length > 0
  const wMin = weighted ? Math.min(...weights) : 0,
    wMax = weighted ? Math.max(...weights) : 1
  const opacityOf = (w) => {
    if (!weighted || typeof w !== 'number' || wMax === wMin) return 1
    return Math.round((0.2 + (0.8 * (w - wMin)) / (wMax - wMin)) * 1000) / 1000
  }
  const multiChannel = new Set(edges.map((e) => e.data('type'))).size > 1
  const outEdges = []
  const rows = []
  const seen = new Set()
  let interLayer = 0
  edges.forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    const ls = layersOf.get(s),
      lt = layersOf.get(t)
    if (!ls || !lt) return
    const shared = ls.filter((g) => lt.includes(g))
    const pairs = shared.length ? shared.map((g) => [g, g]) : [[ls[0], lt[0]]]
    if (!shared.length) interLayer++
    const channelLabel = (EDGE_TYPES[e.data('type')] || { label: e.data('type') }).label
    const channel = multiChannel ? arenaSafe(channelLabel) : ''
    pairs.forEach(([gs, gt]) => {
      const src = `${nodeName(s)}_${layerName.get(gs)}`,
        trg = `${nodeName(t)}_${layerName.get(gt)}`
      const key = [src, trg, channel].join('\u0000')
      if (seen.has(key)) return
      seen.add(key)
      outEdges.push({
        src,
        trg,
        opacity: String(opacityOf(e.data('weight'))),
        color: hexColor(e.style('line-color') || e.data('color')),
        channel,
      })
      rows.push([
        nodeName(s),
        layerName.get(gs),
        nodeName(t),
        layerName.get(gt),
        typeof e.data('weight') === 'number' ? e.data('weight') : 1,
        channel || '1',
      ])
    })
  })
  // optional edges linking the copies of a node in different layers
  let copyEdges = 0
  if (connectCopies) {
    layersOf.forEach((gs, id) => {
      for (let i = 0; i + 1 < gs.length; i++) {
        const src = `${nodeName(id)}_${layerName.get(gs[i])}`,
          trg = `${nodeName(id)}_${layerName.get(gs[i + 1])}`
        outEdges.push({
          src,
          trg,
          opacity: '0.6',
          color: '#FFFFFF',
          channel: multiChannel ? ARENA3D_COPY_CHANNEL : '',
        })
        rows.push([
          nodeName(id),
          layerName.get(gs[i]),
          nodeName(id),
          layerName.get(gs[i + 1]),
          1,
          multiChannel ? ARENA3D_COPY_CHANNEL : '1',
        ])
        copyEdges++
      }
    })
  }
  const json = {
    scene: {
      position_x: '0',
      position_y: '0',
      scale: String(
        layers.length > 4 ? Math.round(((0.6561 * 4) / layers.length) * 1e4) / 1e4 : 0.6561
      ),
      color: '#000000',
      rotation_x: '0.261799387799149',
      rotation_y: '0.261799387799149',
      rotation_z: '0.0872664625997165',
    },
    layers: layers.map(({ key, ...rest }) => rest),
    nodes: outNodes,
    edges: outEdges,
    universalLabelColor: '#FFFFFF',
    direction: edges.some((e) => edgeIsDirected(e)),
    edgeOpacityByWeight: weighted,
    edgeWidthByWeight: false,
  }
  const multiLayerNodes = [...layersOf.values()].filter((gs) => gs.length > 1).length
  return {
    json,
    rows,
    stats: {
      layers: layers.length,
      nodes: outNodes.length,
      edges: outEdges.length,
      interLayer,
      copyEdges,
      multiLayerNodes,
      distinctNodes: layersOf.size,
      droppedLayers,
      droppedNodes,
    },
  }
}

function arena3dNetworkText(rows) {
  return (
    [
      'SourceNode\tSourceLayer\tTargetNode\tTargetLayer\tWeight\tChannel',
      ...rows.map((r) => r.join('\t')),
    ].join('\n') + '\n'
  )
}

function arena3dSummary(st) {
  const parts = [
    `${plural(st.layers, 'layer')}, ${plural(st.nodes, 'node')} (${st.distinctNodes.toLocaleString()} distinct), ${plural(st.edges, 'edge')}`,
  ]
  if (st.interLayer) parts.push(`${plural(st.interLayer, 'edge')} between layers`)
  if (st.multiLayerNodes) parts.push(`${plural(st.multiLayerNodes, 'node')} in several layers`)
  if (st.droppedLayers)
    parts.push(
      `Arena3D takes up to ${ARENA3D_MAX_LAYERS} layers, so only the first ${ARENA3D_MAX_LAYERS} groups were exported (${plural(st.droppedLayers, 'group')} left out${st.droppedNodes ? `, with ${plural(st.droppedNodes, 'node')} that ${st.droppedNodes === 1 ? 'is' : 'are'} only in those` : ''})`
    )
  return parts.join('; ')
}

// The current view, or the Network Comparison (`src`), as an Arena3D network.
const ARENA3D_SOURCES = {
  view: {
    build: async () => buildArena3dModel(),
    status: 'arenaStatus',
    button: 'btnArenaOpen',
    stem: () => {
      const v = typeof activeView === 'function' && activeView()
      return `arena3d-${v ? fileStem(v.name) : 'network'}`
    },
  },
  compare: {
    build: async () => buildCompareArena3dModel(),
    status: 'cmpArenaStatus',
    button: 'btnCmpArenaOpen',
    stem: () => 'arena3d-comparison',
  },
}

export async function exportArena3d(kind, src = 'view') {
  const S = ARENA3D_SOURCES[src]
  try {
    setStatus(S.status, [{ level: 'busy', text: 'Preparing the Arena3D network…' }])
    const model = await S.build()
    const stem = S.stem()
    if (kind === 'json') {
      downloadText(`${stem}.json`, JSON.stringify(model.json, null, 2))
      setStatus(S.status, [
        { level: 'ok', text: `Saved ${stem}.json: ${arena3dSummary(model.stats)}.` },
      ])
    } else {
      downloadText(`${stem}.txt`, arena3dNetworkText(model.rows))
      setStatus(S.status, [
        {
          level: 'ok',
          text: `Saved ${stem}.txt (Arena3D network file): ${arena3dSummary(model.stats)}. Use Upload Network in Arena3D to open it.`,
        },
      ])
    }
  } catch (err) {
    setStatus(S.status, [{ level: 'error', text: err.message }])
  }
}

export async function openInArena3d(src = 'view') {
  const S = ARENA3D_SOURCES[src]
  // open the tab now, while the click still counts as a user action, so
  // pop-up blockers let it through; it is pointed at Arena3D once the
  // server answers
  const tab = window.open('', '_blank')
  if (tab) {
    try {
      tab.document.write(
        '<title>Opening Arena3D…</title><p style="font-family:sans-serif;padding:2em">Sending the network to Arena3D…</p>'
      )
    } catch (e) {}
  }
  const btn = document.getElementById(S.button)
  btn.disabled = true
  let model
  try {
    model = await S.build()
  } catch (err) {
    if (tab && !tab.closed) tab.close()
    btn.disabled = false
    setStatus(S.status, [{ level: 'error', text: err.message }])
    return
  }
  setStatus(S.status, [
    { level: 'busy', text: `Sending ${arena3dSummary(model.stats)} to Arena3D…` },
  ])
  try {
    const route =
      NORMA_CFG.features.relays.arena3d === false ? 'direct' : await resolveStringRoute()
    const body = JSON.stringify(model.json)
    const url =
      route === 'proxy'
        ? `arena3d-api/external?upstream=${encodeURIComponent(arena3dBase())}`
        : `${arena3dBase()}/api/external`
    let response
    try {
      response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body,
      })
    } catch (err) {
      throw new Error(
        route === 'proxy'
          ? `The server relay could not be reached (${err.message}). Check that server.py is still running.`
          : `Arena3D could not be reached (${err.message}). Browsers may block direct calls to other sites; run NORMA with server.py, or save the Arena3D JSON and load it in Arena3D yourself.`
      )
    }
    const text = await response.text()
    if (!response.ok) {
      if (route === 'proxy' && !/^\s*[\[{]/.test(text) && response.status !== 502)
        throw new Error(
          'This server has no Arena3D relay. Start NORMA with server.py, or save the Arena3D JSON instead.'
        )
      throw new Error(`Arena3D answered with an error (${response.status}): ${text.slice(0, 200)}`)
    }
    let answer
    try {
      answer = JSON.parse(text)
    } catch (e) {
      throw new Error('Arena3D sent an answer that is not JSON.')
    }
    if (!answer || !answer.url) throw new Error('Arena3D did not return a link to the network.')
    if (tab && !tab.closed) tab.location.href = answer.url
    else window.open(answer.url, '_blank')
    setStatus(S.status, [
      { level: 'ok', text: `Opened in Arena3D: ${arena3dSummary(model.stats)}.` },
      {
        level: 'ok',
        text: 'If no new tab appeared, open the link:',
        action: { label: 'Open Arena3D', run: () => window.open(answer.url, '_blank') },
      },
    ])
  } catch (err) {
    if (tab && !tab.closed) tab.close()
    setStatus(S.status, [
      { level: 'error', text: err.message },
      {
        level: 'ok',
        text: 'You can still save the Arena3D JSON and load it in Arena3D.',
        action: { label: 'Save Arena3D JSON', run: () => exportArena3d('json', src) },
      },
    ])
  } finally {
    btn.disabled = false
  }
}

// page wiring, run by main.ts in the original order
export function init() {
  document.getElementById('btnArenaOpen').addEventListener('click', () => openInArena3d('view'))

  document
    .getElementById('btnArenaJson')
    .addEventListener('click', () => exportArena3d('json', 'view'))

  document
    .getElementById('btnArenaTsv')
    .addEventListener('click', () => exportArena3d('tsv', 'view'))
}
