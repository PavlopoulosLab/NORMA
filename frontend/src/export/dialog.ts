// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from '../state'
import { activeView, applyEdgeCurveStyle } from '../profiler'
import { applyEdgeMerge } from '../parallel_edges'
import {
  applyNodeSizing,
  frPending,
  getFrWorker,
  invalidateFullMetrics,
  updateStats,
  workStep,
} from '../metrics'
import { buildSvg } from './svg'
import { buildSvg3d, export3dSize, renderExportCanvas3d } from '../view3d/export'
import { canvasToBlob, pdfFromCanvas, renderExportCanvas, withDpi } from './raster'
import {
  composeCanvasWithLegend,
  composeSvgWithLegend,
  exportLegendLayout,
  legendIsEmpty,
  legendModel,
} from '../clustering/mapping'
import { currentTheme } from '../themes'
import { cy, setStyle } from '../cy'
import { exportFrame, exportPixelSize, exportSizeText, withLegendSize } from './draw'
import { markDirty3d, net3d } from '../view3d/state'
import { mulberry32 } from '../sample_data'
import { nextPaint, setStatus } from '../layouts/controls'

/* ---------- dialog ---------- */
const EXPORT_FORMATS = {
  png: { label: 'PNG', ext: 'png', raster: true, transparent: true },
  jpeg: { label: 'JPEG', ext: 'jpg', raster: true, transparent: false },
  webp: { label: 'WebP', ext: 'webp', raster: true, transparent: true },
  svg: { label: 'SVG', ext: 'svg', raster: false, transparent: true },
  pdf: { label: 'PDF', ext: 'pdf', raster: true, transparent: false },
}

function exportOptions() {
  const format = document.querySelector('input[name="imgFormat"]:checked').value
  const bgMode = document.getElementById('imgBackground').value
  const fmtInfo = EXPORT_FORMATS[format]
  let bg = null
  if (bgMode === 'theme') bg = currentTheme.bg
  else if (bgMode === 'white') bg = '#ffffff'
  else if (bgMode === 'custom') bg = document.getElementById('imgBgColor').value
  if (!bg && !fmtInfo.transparent) bg = '#ffffff'
  return {
    format,
    area: document.getElementById('imgArea').value,
    scale: parseFloat(document.getElementById('imgScale').value) || 2,
    bg,
    hulls:
      document.getElementById('imgHulls').checked &&
      document.getElementById('showGroupHulls').checked,
    clean: document.getElementById('imgClean').checked,
    legend: document.getElementById('imgLegend').checked,
    fileName: (document.getElementById('imgFileName').value.trim() || 'network').replace(
      /[\\/:*?"<>|]+/g,
      '_'
    ),
  }
}

function updateExportDialog() {
  const o = exportOptions()
  const info = EXPORT_FORMATS[o.format]
  const bgSel = document.getElementById('imgBackground')
  const transparentOpt = bgSel.querySelector('option[value="transparent"]')
  transparentOpt.disabled = !info.transparent
  if (!info.transparent && bgSel.value === 'transparent') bgSel.value = 'white'
  document.getElementById('imgBgColor').hidden = bgSel.value !== 'custom'
  document.getElementById('imgHullsRow').hidden = !document.getElementById('showGroupHulls').checked
  document.getElementById('imgLegendRow').hidden = legendIsEmpty(legendModel())
  document.getElementById('imgScaleLabel').textContent = info.raster
    ? 'Resolution'
    : 'Size in the file'
  document.getElementById('exportTitle').textContent = net3d.active
    ? 'Export 3D image'
    : 'Export image'
  const size = document.getElementById('imgSize')
  if (net3d.active) {
    document.getElementById('btnImgSave').disabled = !cy.nodes().length
    const s3 = export3dSize(o.scale)
    const all3 = withLegendSize(
      s3.outW,
      s3.outH,
      o.format === 'svg' ? Math.min(o.scale, 8) : s3.k,
      o
    )
    let t3 = exportSizeText(all3.W, all3.H, s3.k, o.format)
    if (all3.W !== s3.outW) t3 += ' · legend included'
    t3 += ' · 3D view from the current angle'
    if (s3.limited) t3 += ' · reduced to the largest size this browser can draw'
    size.textContent = t3
    document.getElementById('imgExt').textContent = '.' + info.ext
    return
  }
  const frame = exportFrame(o.area)
  if (!frame) {
    size.textContent = 'Nothing is shown to export.'
    document.getElementById('btnImgSave').disabled = true
    return
  }
  document.getElementById('btnImgSave').disabled = false
  const { W, H, k, limited } = exportPixelSize(frame, o.scale)
  const all = withLegendSize(W, H, k / frame.unitPx, o)
  let text = exportSizeText(all.W, all.H, k / frame.unitPx, o.format)
  if (all.W !== W) text += ' · legend included'
  if (limited) text += ' · reduced to the largest size this browser can draw'
  size.textContent = text
  document.getElementById('imgExt').textContent = '.' + info.ext
}

function openExportDialog() {
  const dlg = document.getElementById('exportDialog')
  const v = typeof activeView === 'function' && activeView()
  const nameInput = document.getElementById('imgFileName')
  nameInput.value = (v ? v.name : 'network').replace(/[\\/:*?"<>|]+/g, '_')
  setStatus('imgStatus', [])
  updateExportDialog()
  if (typeof dlg.showModal === 'function') dlg.showModal()
  else dlg.setAttribute('open', '')
}

async function saveExportImage() {
  const o = exportOptions()
  const info = EXPORT_FORMATS[o.format]
  const btn = document.getElementById('btnImgSave')
  btn.disabled = true
  setStatus('imgStatus', [{ level: 'busy', text: 'Drawing the image…', progress: null }])
  await nextPaint()
  // optionally hide selection and search/click highlighting while drawing
  let restore = null
  if (o.clean) {
    const selected = cy.elements(':selected')
    const dimmed = cy.elements('.dimmed'),
      highlighted = cy.elements('.highlighted')
    cy.batch(() => {
      selected.unselect()
      dimmed.removeClass('dimmed')
      highlighted.removeClass('highlighted')
    })
    restore = () =>
      cy.batch(() => {
        selected.select()
        dimmed.addClass('dimmed')
        highlighted.addClass('highlighted')
      })
  }
  try {
    let blob
    // the legend (optional) is drawn to the right, at the picture's scale
    const legendLayout = exportLegendLayout(o)
    const mime =
      o.format === 'jpeg' ? 'image/jpeg' : o.format === 'webp' ? 'image/webp' : 'image/png'
    if (net3d.active) {
      net3d.cacheDirty = true
      const s3 = export3dSize(o.scale)
      if (o.format === 'svg') {
        const text = await buildSvg3d({ ...o, title: o.fileName }).text()
        blob = new Blob([composeSvgWithLegend(text, legendLayout, Math.min(o.scale, 8), o.bg)], {
          type: 'image/svg+xml',
        })
      } else {
        const canvas = composeCanvasWithLegend(renderExportCanvas3d(o), legendLayout, s3.k, o.bg)
        blob =
          o.format === 'pdf'
            ? await pdfFromCanvas(canvas, canvas.width / s3.k, canvas.height / s3.k)
            : await withDpi(await canvasToBlob(canvas, mime, 0.95), 96 * s3.k)
      }
    } else if (o.format === 'svg') {
      cy.forceRender && cy.forceRender()
      const frame = exportFrame(o.area)
      const { k } = exportPixelSize(frame, o.scale)
      const text = await buildSvg({ ...o, title: o.fileName }).text()
      blob = new Blob([composeSvgWithLegend(text, legendLayout, k / frame.unitPx, o.bg)], {
        type: 'image/svg+xml',
      })
    } else {
      cy.forceRender && cy.forceRender()
      const frame = exportFrame(o.area)
      const { k } = exportPixelSize(frame, o.scale)
      const legendScale = k / frame.unitPx
      const canvas = composeCanvasWithLegend(renderExportCanvas(o), legendLayout, legendScale, o.bg)
      if (o.format === 'pdf') {
        // one screen pixel = one point, so the page matches the on-screen size
        blob = await pdfFromCanvas(canvas, canvas.width / legendScale, canvas.height / legendScale)
      } else {
        blob = await withDpi(await canvasToBlob(canvas, mime, 0.95), 96 * legendScale)
      }
    }
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `${o.fileName}.${info.ext}`
    document.body.appendChild(a)
    a.click()
    a.remove()
    setTimeout(() => URL.revokeObjectURL(url), 2000)
    setStatus('imgStatus', [
      {
        level: 'ok',
        text: `Saved ${o.fileName}.${info.ext} (${(blob.size / 1024 / 1024).toFixed(blob.size > 1024 * 1024 ? 1 : 2)} MB).`,
      },
    ])
  } catch (err) {
    setStatus('imgStatus', [
      { level: 'error', text: `The image couldn't be saved: ${err.message}` },
    ])
  } finally {
    if (restore) restore()
    if (net3d.active) markDirty3d()
    btn.disabled = false
  }
}

/* ============================================================
   EDGE DIRECTION
   Each edge may be directed (source -> target) or undirected, from its
   file or JSON. The view's Direction setting decides how edges are
   handled: as in the data, all as directed, or all as undirected.
   Arrows, merging, node degrees and centralities, the breadthfirst
   layout, node and edge details, the Profiler, Compare and exports all
   follow the effective direction.
   ============================================================ */
export function directionMode() {
  const el = document.getElementById('edgeDirection')
  return el ? el.value : 'data'
}

export function edgeIsDirected(e) {
  const mode = directionMode()
  if (mode === 'directed') return true
  if (mode === 'undirected') return false
  return !!e.data('directed')
}

// 'undirected', 'directed' or 'mixed' for a collection of edges
export function directionOf(edges) {
  let d = 0,
    n = 0
  edges.forEach((e) => {
    n++
    if (edgeIsDirected(e)) d++
  })
  return d === 0 ? 'undirected' : d === n ? 'directed' : 'mixed'
}

export function anyEdgeDirected(edges) {
  return (edges || cy.edges()).some((e) => edgeIsDirected(e))
}

export function arrowsShown() {
  return directionMode() !== 'undirected' && anyEdgeDirected(cy.edges())
}

export function applyEdgeDirection() {
  const shape = document.getElementById('arrowShape').value
  const scale = parseFloat(document.getElementById('arrowScale').value) || 1
  document.getElementById('arrowScaleValue').textContent = scale.toFixed(1) + '×'
  const anyDirectedData = cy.edges().some((e) => e.data('directed'))
  document.getElementById('edgeDirectionHint').textContent =
    directionMode() === 'data'
      ? anyDirectedData
        ? 'Arrows show the edges marked as directed in the data.'
        : 'No edge in this view is marked as directed.'
      : directionMode() === 'directed'
        ? 'Every edge is treated as pointing from its source to its target.'
        : anyDirectedData
          ? 'Direction is ignored (the default). This network has directed edges: choose "As in the data" to show them.'
          : 'Direction is ignored everywhere (the default), as in earlier versions of NORMA.'
  cy.batch(() => {
    cy.edges().forEach((e) => {
      const arrow = edgeIsDirected(e) ? shape : 'none'
      if (e.data('arrow') !== arrow) e.data('arrow', arrow)
    })
  })
  document.getElementById('arrowControls').hidden = !arrowsShown()
  setStyle('edge', {
    'target-arrow-shape': 'data(arrow)',
    'target-arrow-color': 'data(color)',
    'arrow-scale': scale,
  })
  // merged edges and haystack edges depend on direction
  applyEdgeMerge()
  applyEdgeCurveStyle()
  invalidateFullMetrics()
  if (document.getElementById('sizeMetric').value !== 'fixed') applyNodeSizing()
  updateStats()
}

/* ============================================================
   3D NETWORK
   A second view of the same network, drawn in 3D on a plain canvas.
   Everything visual is read from the 2D (Cytoscape) elements, so groups,
   colors, pies, shapes, labels, edge labels, arrows, selection, search
   highlighting and ticked groups/channels look and behave the same.
   Only node positions (x, y, z) and the camera are 3D-specific; both are
   kept per view.
   Coordinates: x to the right, y down (as in 2D), z away from the viewer.
   ============================================================ */

/* ---------- 3D force-directed layout (weighted Fruchterman-Reingold) ----------
   Self-contained so it can also run in the background worker.
   edges: [{ s, t, w }] with node ids; init: optional { id: [x, y, z] }. */
export function fr3dLayout(ids, edges, opts) {
  opts = opts || {}
  const n = ids.length
  const out = {}
  if (!n) return out
  const rand = mulberry32(opts.seed || 123)
  const idx = new Map(ids.map((id, i) => [id, i]))
  const K = opts.spacing || 60 // ideal edge length
  const E = []
  let maxW = 0
  edges.forEach((e) => {
    const a = idx.get(e.s),
      b = idx.get(e.t)
    if (a === undefined || b === undefined || a === b) return
    const w = Number.isFinite(e.w) && e.w > 0 ? e.w : 1
    if (w > maxW) maxW = w
    E.push([a, b, w])
  })
  // connected components (union-find); each is laid out on its own, as in
  // the 2D weighted layout, so unconnected pieces don't drift away
  const parent = Int32Array.from({ length: n }, (_, i) => i)
  const find = (x) => {
    while (parent[x] !== x) {
      parent[x] = parent[parent[x]]
      x = parent[x]
    }
    return x
  }
  E.forEach(([a, b]) => {
    const ra = find(a),
      rb = find(b)
    if (ra !== rb) parent[ra] = rb
  })
  const compMap = new Map()
  for (let i = 0; i < n; i++) {
    const r = find(i)
    if (!compMap.has(r)) compMap.set(r, [])
    compMap.get(r).push(i)
  }
  const comps = [...compMap.values()].sort((x, y) => y.length - x.length)
  const compOf = new Int32Array(n),
    local = new Int32Array(n)
  comps.forEach((m, c) =>
    m.forEach((g, i) => {
      compOf[g] = c
      local[g] = i
    })
  )
  const compEdges = comps.map(() => [])
  E.forEach(([a, b, w]) => compEdges[compOf[a]].push([local[a], local[b], w]))

  function layoutComponent(m, CE) {
    const N = m
    const X = new Float64Array(N),
      Y = new Float64Array(N),
      Z = new Float64Array(N)
    const R0 = K * Math.cbrt(N) * 0.9
    for (let i = 0; i < N; i++) {
      let x, y, z
      do {
        x = rand() * 2 - 1
        y = rand() * 2 - 1
        z = rand() * 2 - 1
      } while (x * x + y * y + z * z > 1)
      X[i] = x * R0
      Y[i] = y * R0
      Z[i] = z * R0
    }
    if (N === 1) {
      X[0] = Y[0] = Z[0] = 0
      return { X, Y, Z, R: K * 0.5 }
    }
    const iters = opts.iterations || (N <= 300 ? 400 : N <= 1500 ? 250 : 120)
    const DX = new Float64Array(N),
      DY = new Float64Array(N),
      DZ = new Float64Array(N)
    const K2 = K * K
    const useGrid = N > 1500
    let temp = R0 * 0.5
    const cool = temp / (iters + 1)
    for (let it = 0; it < iters; it++) {
      if ((it & 3) === 0) workStep(it / iters)
      DX.fill(0)
      DY.fill(0)
      DZ.fill(0)
      if (!useGrid) {
        for (let i = 0; i < N; i++) {
          for (let j = i + 1; j < N; j++) {
            let dx = X[i] - X[j],
              dy = Y[i] - Y[j],
              dz = Z[i] - Z[j]
            let d2 = dx * dx + dy * dy + dz * dz
            if (d2 < 0.01) {
              dx = rand() - 0.5
              dy = rand() - 0.5
              dz = rand() - 0.5
              d2 = 0.01
            }
            const f = K2 / d2
            DX[i] += dx * f
            DY[i] += dy * f
            DZ[i] += dz * f
            DX[j] -= dx * f
            DY[j] -= dy * f
            DZ[j] -= dz * f
          }
        }
      } else {
        // repulsion only from nodes in the neighbouring grid cells
        const cell = 2 * K
        const grid = new Map()
        const key = (p, q, r) => p + ',' + q + ',' + r
        for (let i = 0; i < N; i++) {
          const k = key(Math.floor(X[i] / cell), Math.floor(Y[i] / cell), Math.floor(Z[i] / cell))
          const list = grid.get(k)
          if (list) list.push(i)
          else grid.set(k, [i])
        }
        for (let i = 0; i < N; i++) {
          const cx = Math.floor(X[i] / cell),
            cy = Math.floor(Y[i] / cell),
            cz = Math.floor(Z[i] / cell)
          for (let p = -1; p <= 1; p++)
            for (let q = -1; q <= 1; q++)
              for (let r = -1; r <= 1; r++) {
                const list = grid.get(key(cx + p, cy + q, cz + r))
                if (!list) continue
                for (let t = 0; t < list.length; t++) {
                  const j = list[t]
                  if (j === i) continue
                  let dx = X[i] - X[j],
                    dy = Y[i] - Y[j],
                    dz = Z[i] - Z[j]
                  let d2 = dx * dx + dy * dy + dz * dz
                  if (d2 < 0.01) {
                    dx = rand() - 0.5
                    dy = rand() - 0.5
                    dz = rand() - 0.5
                    d2 = 0.01
                  }
                  const f = K2 / d2
                  DX[i] += dx * f
                  DY[i] += dy * f
                  DZ[i] += dz * f
                }
              }
        }
      }
      for (let e = 0; e < CE.length; e++) {
        const [p, q, w] = CE[e]
        const dx = X[p] - X[q],
          dy = Y[p] - Y[q],
          dz = Z[p] - Z[q]
        const d = Math.sqrt(dx * dx + dy * dy + dz * dz) || 0.01
        const f = (d / K) * (w / maxW) // (d^2 / K) / d, scaled by weight
        DX[p] -= dx * f
        DY[p] -= dy * f
        DZ[p] -= dz * f
        DX[q] += dx * f
        DY[q] += dy * f
        DZ[q] += dz * f
      }
      for (let i = 0; i < N; i++) {
        DX[i] -= X[i] * 0.01
        DY[i] -= Y[i] * 0.01
        DZ[i] -= Z[i] * 0.01
        const d = Math.sqrt(DX[i] * DX[i] + DY[i] * DY[i] + DZ[i] * DZ[i]) || 1
        const step = Math.min(d, temp)
        X[i] += (DX[i] / d) * step
        Y[i] += (DY[i] / d) * step
        Z[i] += (DZ[i] / d) * step
      }
      temp = Math.max(K * 0.02, temp - cool)
    }
    let mx = 0,
      my = 0,
      mz = 0
    for (let i = 0; i < N; i++) {
      mx += X[i]
      my += Y[i]
      mz += Z[i]
    }
    mx /= N
    my /= N
    mz /= N
    let R = 0
    for (let i = 0; i < N; i++) {
      X[i] -= mx
      Y[i] -= my
      Z[i] -= mz
      R = Math.max(R, Math.sqrt(X[i] * X[i] + Y[i] * Y[i] + Z[i] * Z[i]))
    }
    return { X, Y, Z, R: R + K * 0.5 }
  }

  let laidOut = 0
  const parts = comps.map((m, c) => {
    globalThis.__normaWorkRange = [laidOut / n, (laidOut + m.length) / n]
    laidOut += m.length
    return layoutComponent(m.length, compEdges[c])
  })
  globalThis.__normaWorkRange = [0, 1]
  // pack: the largest piece in the middle, the others on a shell around it
  const centers = [[0, 0, 0]]
  if (parts.length > 1) {
    const others = parts.slice(1)
    const maxR = Math.max(...others.map((p) => p.R))
    const area = others.reduce((s, p) => s + Math.PI * (p.R + K * 0.5) ** 2, 0) * 1.4
    const shell = Math.max(parts[0].R + maxR + K, Math.sqrt(area / (4 * Math.PI)))
    const golden = Math.PI * (3 - Math.sqrt(5))
    others.forEach((p, i) => {
      const m = others.length
      const y = m === 1 ? 0 : 1 - (i / (m - 1)) * 2
      const r = Math.sqrt(Math.max(0, 1 - y * y))
      const t = golden * i
      centers.push([Math.cos(t) * r * shell, y * shell, Math.sin(t) * r * shell])
    })
  }
  comps.forEach((m, c) => {
    const p = parts[c],
      ctr = centers[c]
    m.forEach((g, i) => {
      out[ids[g]] = [p.X[i] + ctr[0], p.Y[i] + ctr[1], p.Z[i] + ctr[2]]
    })
  })
  return out
}

export function fr3dAsync(ids, edges, opts, onProgress) {
  const worker = ids.length > 150 ? getFrWorker() : null
  if (!worker) return Promise.resolve(fr3dLayout(ids, edges, opts))
  const id = ++S.frRequestSeq
  return new Promise((resolve) => {
    frPending.set(id, { resolve, onProgress, local: () => fr3dLayout(ids, edges, opts) })
    worker.postMessage({ id, kind: 'fr3d', ids, edges, opts })
  })
}

// Points spread evenly on a unit sphere (Fibonacci lattice).
export function fibonacciSphere(count) {
  const pts = []
  const golden = Math.PI * (3 - Math.sqrt(5))
  for (let i = 0; i < count; i++) {
    const y = count === 1 ? 0 : 1 - (i / (count - 1)) * 2
    const r = Math.sqrt(Math.max(0, 1 - y * y))
    const t = golden * i
    pts.push([Math.cos(t) * r, y, Math.sin(t) * r])
  }
  return pts
}

// Points filling a ball of the given radius, first ones in the middle.
export function fibonacciBall(count, radius) {
  const shell = fibonacciSphere(count)
  return shell.map((p, i) => {
    const r = radius * Math.cbrt((i + 0.5) / count)
    return [p[0] * r, p[1] * r, p[2] * r]
  })
}

// page wiring, run by main.ts in the original order
export function init() {
  ;['imgArea', 'imgScale', 'imgBackground', 'imgHulls', 'imgClean', 'imgLegend'].forEach((id) => {
    document.getElementById(id).addEventListener('change', updateExportDialog)
  })

  document
    .querySelectorAll('input[name="imgFormat"]')
    .forEach((r) => r.addEventListener('change', updateExportDialog))

  document.getElementById('btnImgSave').addEventListener('click', saveExportImage)

  document
    .getElementById('btnImgCancel')
    .addEventListener('click', () => document.getElementById('exportDialog').close())

  document.getElementById('btnExportImage').addEventListener('click', openExportDialog)

  document.getElementById('btnExportImage2').addEventListener('click', openExportDialog)

  ;(function () {
    const tag = document.getElementById('statDirection')
    const useData = () => {
      if (!tag.classList.contains('stat-dir-off')) return
      const sel = document.getElementById('edgeDirection')
      sel.value = 'data'
      // a real change event, so undo and the view's settings follow as usual
      sel.dispatchEvent(new Event('change', { bubbles: true }))
    }
    tag.addEventListener('click', useData)
    tag.addEventListener('keydown', (ev) => {
      if (ev.key === 'Enter' || ev.key === ' ') {
        ev.preventDefault()
        useData()
      }
    })
  })()

  ;['edgeDirection', 'arrowShape'].forEach((id) => {
    document.getElementById(id).addEventListener('change', applyEdgeDirection)
  })

  document.getElementById('arrowScale').addEventListener('input', applyEdgeDirection)
}
