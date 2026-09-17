// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { PROFILE_DIRECTED_STATS } from '../directed_stats'
import { PROFILE_STATS, formatStat } from '../wiring'
import {
  UPSET_MAX_COLUMNS,
  compareState,
  exclusiveCounts,
  jaccard,
  membership,
  runComparison,
  spearman,
} from '../label_colors'
import { cy } from '../cy'
import { downloadText } from '../layouts/controls'
import { escapeHtml } from '../network_state'
import { layoutLegend, legendIsEmpty, legendModel } from '../clustering/mapping'
import { profSection } from '../profiler'

/* ---------- drawing ---------- */
function legendHtml(nets) {
  return `<div class="cmp-legend">${nets
    .map(
      (n, i) =>
        `<span><i style="background:${n.color}"></i>${String.fromCharCode(65 + i)}: ${escapeHtml(n.name)}</span>`
    )
    .join('')}</div>`
}

// Venn diagram for 2 or 3 sets. Regions are labelled with exact counts;
// circle sizes are schematic, not proportional.
function vennSvg(nets, sets) {
  const k = sets.length
  const counts = exclusiveCounts(membership(sets))
  const c = (m) => (counts.get(m) || 0).toLocaleString()
  const W = 360,
    H = k === 2 ? 220 : 300
  const circles =
    k === 2
      ? [
          { x: 140, y: 110, r: 88 },
          { x: 220, y: 110, r: 88 },
        ]
      : [
          { x: 145, y: 115, r: 88 },
          { x: 215, y: 115, r: 88 },
          { x: 180, y: 180, r: 88 },
        ]
  const regions =
    k === 2
      ? [
          [1, 95, 114],
          [2, 265, 114],
          [3, 180, 114],
        ]
      : [
          [1, 105, 100],
          [2, 255, 100],
          [4, 180, 240],
          [3, 180, 72],
          [5, 128, 178],
          [6, 232, 178],
          [7, 180, 140],
        ]
  let svg = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Venn diagram">`
  circles.forEach((ci, i) => {
    svg += `<circle cx="${ci.x}" cy="${ci.y}" r="${ci.r}" fill="${nets[i].color}" fill-opacity="0.16" stroke="${nets[i].color}" stroke-width="1.5"/>`
  })
  const labelPos =
    k === 2
      ? [
          [12, 18, 'start'],
          [348, 18, 'end'],
        ]
      : [
          [12, 18, 'start'],
          [348, 18, 'end'],
          [180, 294, 'middle'],
        ]
  labelPos.forEach(([x, y, anchor], i) => {
    svg += `<text x="${x}" y="${y}" text-anchor="${anchor}" class="venn-set" fill="${nets[i].color}">${String.fromCharCode(65 + i)} (${sets[i].size.toLocaleString()})</text>`
  })
  regions.forEach(([m, x, y]) => {
    svg += `<text x="${x}" y="${y + 4}" text-anchor="middle" class="venn-count">${c(m)}</text>`
  })
  return svg + '</svg>'
}

// UpSet plot: every non-empty exclusive intersection as a column.
function upsetSvg(nets, sets) {
  const k = sets.length
  const allCounts = [...exclusiveCounts(membership(sets))].sort(
    (a, b) => b[1] - a[1] || a[0] - b[0]
  )
  // with many networks there can be hundreds of combinations: show the largest
  const counts = allCounts.slice(0, UPSET_MAX_COLUMNS)
  const colW = k > 6 ? 22 : 26,
    rowH = k > 6 ? 18 : 22,
    leftW = 220,
    barH = 130,
    top = 16
  const W = leftW + counts.length * colW + 20
  const H = top + barH + 10 + k * rowH + 10
  const maxC = Math.max(...counts.map((x) => x[1]), 1)
  const maxS = Math.max(...sets.map((s) => s.size), 1)
  let svg = `<svg viewBox="0 0 ${W} ${H}" style="max-width:${W}px" role="img" aria-label="UpSet plot">`
  // intersection bars
  counts.forEach(([m, n], j) => {
    const x = leftW + j * colW + 5
    const h = (n / maxC) * barH
    const y = top + barH - h
    svg += `<rect class="bar" x="${x}" y="${y.toFixed(1)}" width="${colW - 10}" height="${h.toFixed(1)}"><title>${n.toLocaleString()} in exactly: ${sets
      .map((_, i) => ((m >> i) & 1 ? String.fromCharCode(65 + i) : ''))
      .filter(Boolean)
      .join(' + ')}</title></rect>`
    svg += `<text x="${x + (colW - 10) / 2}" y="${(y - 3).toFixed(1)}" text-anchor="middle" class="upset-num">${n.toLocaleString()}</text>`
  })
  svg += `<line class="axis" x1="${leftW}" x2="${W - 10}" y1="${top + barH}" y2="${top + barH}"/>`
  // set rows: size bars (right-aligned toward the matrix) and names
  const matrixTop = top + barH + 10
  sets.forEach((set, i) => {
    const y = matrixTop + i * rowH
    if (i % 2 === 0)
      svg += `<rect x="${leftW}" y="${y}" width="${counts.length * colW}" height="${rowH}" class="upset-stripe"/>`
    const bw = (set.size / maxS) * 60
    svg += `<rect x="${leftW - 6 - bw}" y="${y + 6}" width="${bw}" height="${rowH - 12}" fill="${nets[i].color}" fill-opacity="0.7"><title>${set.size.toLocaleString()}</title></rect>`
    const short = nets[i].name.length > 20 ? nets[i].name.slice(0, 19) + '…' : nets[i].name
    svg += `<text x="${leftW - 72}" y="${y + rowH / 2 + 4}" text-anchor="end" class="upset-set"><title>${escapeHtml(nets[i].name)}</title>${String.fromCharCode(65 + i)} ${escapeHtml(short)}</text>`
  })
  // dot matrix
  counts.forEach(([m], j) => {
    const cx = leftW + j * colW + colW / 2
    const on = []
    for (let i = 0; i < k; i++) {
      const cy0 = matrixTop + i * rowH + rowH / 2
      const member = (m >> i) & 1
      if (member) on.push(cy0)
      svg += `<circle cx="${cx}" cy="${cy0}" r="5" class="${member ? 'upset-on' : 'upset-off'}"/>`
    }
    if (on.length > 1)
      svg += `<line x1="${cx}" x2="${cx}" y1="${on[0]}" y2="${on[on.length - 1]}" class="upset-link"/>`
  })
  svg += '</svg>'
  if (allCounts.length > counts.length) {
    const rest = allCounts.slice(counts.length).reduce((t, c) => t + c[1], 0)
    svg += `<p class="sub" style="margin:6px 0 0;">Showing the ${counts.length} largest of ${allCounts.length} combinations; the other ${allCounts.length - counts.length} hold ${rest.toLocaleString()} items in total (all are in the downloaded table).</p>`
  }
  return svg
}

// Square matrix with shaded cells; values in [0, 1] unless `signed`.
function matrixHtml(nets, value, format, signed) {
  let html =
    '<table class="cmp-matrix"><thead><tr><th></th>' +
    nets
      .map(
        (n, i) =>
          `<th title="${escapeHtml(n.name)}"><i style="background:${n.color}"></i>${String.fromCharCode(65 + i)}</th>`
      )
      .join('') +
    '</tr></thead><tbody>'
  nets.forEach((a, i) => {
    html += `<tr><th title="${escapeHtml(a.name)}"><i style="background:${a.color}"></i>${String.fromCharCode(65 + i)}</th>`
    nets.forEach((b, j) => {
      const v = value(i, j)
      if (v == null || Number.isNaN(v.v)) {
        html += '<td class="na">–</td>'
        return
      }
      const t = signed ? Math.abs(v.v) : v.v
      const hue = signed && v.v < 0 ? '#dc2626' : '#2563eb'
      html += `<td style="background:color-mix(in srgb, ${hue} ${Math.round(t * 55)}%, var(--panel))" title="${escapeHtml(v.title || '')}">${format(v)}</td>`
    })
    html += '</tr>'
  })
  return html + '</tbody></table>'
}

// Complementary cumulative degree distribution, log-log, one line per network.
function ccdfSvg(nets) {
  const W = 520,
    H = 260,
    L = 48,
    R = 12,
    T = 12,
    B = 34
  const pw = W - L - R,
    ph = H - T - B
  let maxDeg = 1
  nets.forEach((n) =>
    n.stats.degree.forEach((d) => {
      if (d > maxDeg) maxDeg = d
    })
  )
  const lx = Math.log10(Math.max(maxDeg, 10))
  const minP = Math.min(
    ...nets.map((n) => 1 / Math.max(1, n.stats.degree.filter((d) => d > 0).length))
  )
  const ly = Math.log10(minP)
  const X = (d) => L + (Math.log10(d) / lx) * pw
  const Y = (p) => T + (Math.log10(p) / ly) * ph
  let svg = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Degree distributions">`
  svg += `<line class="axis" x1="${L}" x2="${W - R}" y1="${T + ph}" y2="${T + ph}"/><line class="axis" x1="${L}" x2="${L}" y1="${T}" y2="${T + ph}"/>`
  for (let e = 0; e <= Math.ceil(lx); e++) {
    const d = 10 ** e
    if (d > 10 ** lx * 1.001) break
    svg += `<text x="${X(d)}" y="${H - 18}" text-anchor="middle">${d.toLocaleString()}</text>`
  }
  for (let e = 0; e >= Math.ceil(ly - 1e-9); e--) {
    const p = 10 ** e
    svg += `<text x="${L - 6}" y="${Y(p) + 3}" text-anchor="end">${p >= 0.01 ? p : p.toExponential(0)}</text>`
  }
  svg += `<text x="${L + pw / 2}" y="${H - 3}" text-anchor="middle">degree k</text>`
  svg += `<text x="12" y="${T + ph / 2}" text-anchor="middle" transform="rotate(-90 12 ${T + ph / 2})">share of nodes with degree ≥ k</text>`
  nets.forEach((n) => {
    const degs = n.stats.degree.filter((d) => d > 0).sort((a, b) => a - b)
    if (!degs.length) return
    const pts = []
    const total = degs.length
    for (let i = 0; i < degs.length; i++) {
      if (i > 0 && degs[i] === degs[i - 1]) continue
      pts.push([degs[i], (total - i) / total])
    }
    const path = pts
      .map(([d, p], i) => `${i ? 'L' : 'M'}${X(d).toFixed(1)},${Y(p).toFixed(1)}`)
      .join('')
    svg += `<path d="${path}" fill="none" stroke="${n.color}" stroke-width="2"/>`
  })
  return svg + '</svg>'
}

export function renderComparison(nets) {
  const root = document.getElementById('cmpResults')
  root.innerHTML = ''
  const letter = (i) => String.fromCharCode(65 + i)
  const nodeSets = nets.map((n) => n.nodeSet)
  const edgeSets = nets.map((n) => n.edgeSet)

  // overview
  const ov = profSection(
    'Networks compared',
    document.getElementById('cmpDirected').checked
      ? 'Nodes are matched by name, edges by the nodes they join and their direction (channels ignored): A→B, B→A and an undirected A–B are different edges.'
      : 'Nodes are matched by name, edges by the pair of nodes they join (channels and direction ignored).'
  )
  ov.insertAdjacentHTML(
    'beforeend',
    `<div class="cmp-overview">${nets
      .map(
        (n, i) => `
    <div class="cmp-card" style="--c:${n.color}">
      <div class="cmp-letter">${letter(i)}</div>
      <div class="cmp-card-body"><div class="cmp-card-name" title="${escapeHtml(n.name)}">${escapeHtml(n.name)}</div>
      <div class="cmp-card-meta">${n.kind === 'view' ? 'view' : 'file'} · ${n.nodeSet.size.toLocaleString()} nodes · ${n.edgeSet.size.toLocaleString()} edges</div></div>
    </div>`
      )
      .join('')}</div>`
  )
  const shareNodes = [...membership(nodeSets).values()].filter(
    (m) => m === (1 << nets.length) - 1
  ).length
  const shareEdges = [...membership(edgeSets).values()].filter(
    (m) => m === (1 << nets.length) - 1
  ).length
  ov.insertAdjacentHTML(
    'beforeend',
    `<p class="sub" style="margin:10px 0 0;">${shareNodes.toLocaleString()} nodes and ${shareEdges.toLocaleString()} edges are in all ${nets.length} networks.</p>`
  )
  root.appendChild(ov)

  // overlaps
  ;[
    ['Node overlap', nodeSets, 'nodes'],
    ['Edge overlap', edgeSets, 'edges'],
  ].forEach(([title, sets, what]) => {
    const sec = profSection(
      title,
      nets.length <= 3
        ? `Left: Venn diagram with the number of ${what} in each region (circle sizes are schematic). Right: the same regions as an UpSet plot, largest first.`
        : `UpSet plot: each column is a group of ${what} found in exactly the networks marked below it, largest first. Venn diagrams are shown for two or three networks.`
    )
    sec.insertAdjacentHTML('beforeend', legendHtml(nets))
    const cards = document.createElement('div')
    cards.className = 'cmp-row'
    if (nets.length <= 3)
      cards.insertAdjacentHTML(
        'beforeend',
        `<div class="card chart cmp-venn">${vennSvg(nets, sets)}</div>`
      )
    cards.insertAdjacentHTML(
      'beforeend',
      `<div class="card chart cmp-upset">${upsetSvg(nets, sets)}</div>`
    )
    sec.appendChild(cards)
    sec.insertAdjacentHTML(
      'beforeend',
      `<h4 class="cmp-subhead">Jaccard similarity of ${what}</h4><p class="sub">Shared ${what} divided by ${what} in either network; hover a cell for the counts.</p>` +
        matrixHtml(
          nets,
          (i, j) => {
            const r = jaccard(sets[i], sets[j])
            return {
              v: r.j,
              title: `${letter(i)} and ${letter(j)}: ${r.inter.toLocaleString()} shared of ${r.union.toLocaleString()}`,
            }
          },
          (v) => v.v.toFixed(2)
        )
    )
    root.appendChild(sec)
  })

  // degree agreement
  const dsec = profSection(
    'Degree agreement on shared nodes',
    'Spearman correlation between the degrees a node has in two networks, over the nodes both contain. Near 1: hubs stay hubs; near 0: unrelated; negative: roles swap.'
  )
  dsec.insertAdjacentHTML(
    'beforeend',
    matrixHtml(
      nets,
      (i, j) => {
        if (i === j) return { v: 1, title: 'same network' }
        const shared = [...nets[i].nodeSet].filter((x) => nets[j].nodeSet.has(x))
        const rho = spearman(
          shared.map((x) => nets[i].degreeOf.get(x)),
          shared.map((x) => nets[j].degreeOf.get(x))
        )
        return { v: rho, title: `${shared.length.toLocaleString()} shared nodes` }
      },
      (v) => v.v.toFixed(2),
      true
    )
  )
  root.appendChild(dsec)

  // degree distributions
  const ccdf = profSection(
    'Degree distributions',
    'Share of nodes with at least a given degree, on log–log axes. Straight, shallow lines indicate heavy-tailed (hub-dominated) networks.'
  )
  ccdf.insertAdjacentHTML(
    'beforeend',
    legendHtml(nets) + `<div class="card chart cmp-ccdf">${ccdfSvg(nets)}</div>`
  )
  root.appendChild(ccdf)

  // topology table
  const tsec = profSection(
    'Topology side by side',
    'The profiler\u2019s statistics for each network. The highest value in each row is marked.'
  )
  let html =
    '<div class="table-wrap"><table class="data cmp-topo"><thead><tr><th scope="col">Statistic</th>' +
    nets
      .map(
        (n, i) =>
          `<th scope="col"><i style="background:${n.color}"></i>${letter(i)}: ${escapeHtml(n.name)}</th>`
      )
      .join('') +
    '</tr></thead><tbody>'
  PROFILE_STATS.forEach((st) => {
    const vals = nets.map((n) => n.stats[st.key])
    const nums = vals.filter((v) => typeof v === 'number' && Number.isFinite(v))
    const max = nums.length > 1 && new Set(nums).size > 1 ? Math.max(...nums) : null
    html +=
      `<tr><td>${escapeHtml(st.label)}<span class="stat-desc">${escapeHtml(st.desc)}</span></td>` +
      vals
        .map(
          (v) =>
            `<td class="num${max !== null && v === max ? ' cmp-max' : ''}">${escapeHtml(formatStat(v))}</td>`
        )
        .join('') +
      '</tr>'
  })
  if (nets.some((n) => n.directed)) {
    html += `<tr><th colspan="${nets.length + 1}" class="cmp-rowhead">Direction</th></tr>`
    PROFILE_DIRECTED_STATS.forEach((st) => {
      const vals = nets.map((n) => (n.directed ? n.directed[st.key] : undefined))
      const nums = vals.filter((v) => typeof v === 'number' && Number.isFinite(v))
      const max = nums.length > 1 && new Set(nums).size > 1 ? Math.max(...nums) : null
      html +=
        `<tr><td>${escapeHtml(st.label)}<span class="stat-desc">${escapeHtml(st.desc)}</span></td>` +
        vals
          .map(
            (v) =>
              `<td class="num${max !== null && v === max ? ' cmp-max' : ''}">${v === undefined ? '—' : escapeHtml(formatStat(v))}</td>`
          )
          .join('') +
        '</tr>'
    })
  }
  html += '</tbody></table></div>'
  tsec.insertAdjacentHTML('beforeend', html)
  root.appendChild(tsec)
}

function comparisonTsv() {
  const nets = compareState.results
  if (!nets) return
  const letter = (i) => String.fromCharCode(65 + i)
  const header = nets.map((n, i) => `${letter(i)} ${n.name}`)
  const nodeMask = membership(nets.map((n) => n.nodeSet))
  const edgeMask = membership(nets.map((n) => n.edgeSet))
  const lines = ['# Node membership (1 = present)', ['Node', ...header].join('\t')]
  ;[...nodeMask]
    .sort((a, b) => (a[0] < b[0] ? -1 : 1))
    .forEach(([id, m]) => lines.push([id, ...nets.map((_, i) => (m >> i) & 1)].join('\t')))
  lines.push(
    '',
    '# Edge membership (1 = present)',
    ['Source', 'Target', 'Direction', ...header].join('\t')
  )
  ;[...edgeMask]
    .sort((a, b) => (a[0] < b[0] ? -1 : 1))
    .forEach(([key, m]) =>
      lines.push([...key.split('\t'), ...nets.map((_, i) => (m >> i) & 1)].join('\t'))
    )
  lines.push('', '# Topology', ['Statistic', ...header].join('\t'))
  PROFILE_STATS.forEach((st) =>
    lines.push([st.label, ...nets.map((n) => formatStat(n.stats[st.key]))].join('\t'))
  )
  if (nets.some((n) => n.directed))
    PROFILE_DIRECTED_STATS.forEach((st) =>
      lines.push(
        [st.label, ...nets.map((n) => (n.directed ? formatStat(n.directed[st.key]) : ''))].join(
          '\t'
        )
      )
    )
  downloadText('network-comparison.tsv', lines.join('\n') + '\n')
}

/* ============================================================
   IMAGE EXPORT
   PNG, JPEG and WebP are drawn by Cytoscape's own renderer onto a
   canvas of the chosen resolution, with group shading drawn underneath
   (it lives on a separate layer on screen). SVG is written element by
   element, so it stays sharp at any size and can be edited. PDF wraps
   a high-resolution JPEG in a single page sized to the image.
   Area: the whole shown network, or exactly what is on screen.
   ============================================================ */
export const EXPORT_MAX_SIDE = 16384
// common browser canvas limit
export const EXPORT_MAX_PIXELS = (16384 * 16384) / 2.2

export function exportFrame(area) {
  if (area === 'view') {
    const z = cy.zoom(),
      pan = cy.pan()
    return { x1: -pan.x / z, y1: -pan.y / z, w: cy.width() / z, h: cy.height() / z, unitPx: z }
  }
  const eles = cy.elements().filter((e) => e.visible())
  if (!eles.length) return null
  const bb = eles.boundingBox({ includeLabels: true, includeOverlays: false })
  // room for group shading around the outermost nodes
  const shading = document.getElementById('showGroupHulls').checked ? hullMarginModel() + 8 : 0
  const pad = Math.max(16, shading)
  return { x1: bb.x1 - pad, y1: bb.y1 - pad, w: bb.w + 2 * pad, h: bb.h + 2 * pad, unitPx: 1 }
}

// Shading margin in model units, matching what the screen shows.
export function hullMarginModel() {
  const z = cy.zoom()
  return (30 * Math.max(0.15, Math.min(1, z))) / z
}

// Size of the whole exported picture, including the legend when it is added.
export function withLegendSize(W, H, factor, o) {
  if (!o.legend || document.getElementById('imgLegendRow').hidden) return { W, H }
  const m = legendModel()
  if (legendIsEmpty(m)) return { W, H }
  const lay = layoutLegend(m)
  return { W: W + Math.round(lay.w * factor), H: Math.max(H, Math.round(lay.h * factor)) }
}

// "W × H px · N dpi at on-screen size · print size at 300 dpi" for the export dialog.
export function exportSizeText(W, H, factor, format) {
  const px = `${W.toLocaleString('en-US')} × ${H.toLocaleString('en-US')} px`
  if (format === 'svg')
    return `${px} (the picture's size in the file); vector graphics stay sharp at any size and print resolution`
  const dpi = Math.round(96 * factor)
  const inch = (v) => v.toFixed(v < 10 ? 1 : 0)
  const at = (d) =>
    `${inch(W / d)} × ${inch(H / d)} in (${((W / d) * 2.54).toFixed(1)} × ${((H / d) * 2.54).toFixed(1)} cm)`
  let text = `${px} · ${dpi} dpi when printed at the on-screen size, ${at(dpi)} · ${at(300)} at 300 dpi`
  if (format === 'pdf') text += ' · one page at the on-screen size'
  return text
}

// Pixel size for a frame at a given scale, reduced to stay within canvas limits.
export function exportPixelSize(frame, scale) {
  let k = frame.unitPx * scale
  const W = frame.w * k,
    H = frame.h * k
  const shrink = Math.min(
    1,
    EXPORT_MAX_SIDE / W,
    EXPORT_MAX_SIDE / H,
    Math.sqrt(EXPORT_MAX_PIXELS / (W * H))
  )
  k *= shrink
  return {
    k,
    W: Math.max(1, Math.round(frame.w * k)),
    H: Math.max(1, Math.round(frame.h * k)),
    limited: shrink < 1,
  }
}

// page wiring, run by main.ts in the original order
export function init() {
  document.getElementById('btnCompare').addEventListener('click', runComparison)

  document.getElementById('btnCompareTsv').addEventListener('click', comparisonTsv)
}
