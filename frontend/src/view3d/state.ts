// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { cy } from '../cy'
import { normalizeCssColor } from '../layouts/controls'
import { render3d } from './draw'

/* ---------- state ---------- */
export const net3d = {
  active: false,
  pos: new Map(), // node id -> [x, y, z]
  cam: { yaw: 0.6, pitch: 0.35, dist: 900, tx: 0, ty: 0, tz: 0 },
  hasLayout: false,
  cache: null,
  cacheDirty: true,
  frameQueued: false,
  interacting: false,
  interactTimer: null,
  autoTimer: null,
  lastFrame: 0,
  proj: [], // last projected nodes, near first
  projEdges: [],
  layoutSeq: 0,
  anim: null,
  spreadApplied: 0,
}

export function el3(id) {
  return document.getElementById(id)
}

export function canvas3d() {
  return el3('cy3d')
}

export function markDirty3d() {
  net3d.cacheDirty = true
  requestRender3d()
}

export function requestRender3d() {
  if (!net3d.active || net3d.frameQueued) return
  net3d.frameQueued = true
  requestAnimationFrame(() => {
    net3d.frameQueued = false
    render3d()
  })
}

/* ---------- colors ---------- */
const colorCache3d = new Map()

export function rgbOf(color) {
  if (colorCache3d.has(color)) return colorCache3d.get(color)
  let rgb = [136, 136, 136]
  const s = String(color || '').trim()
  let m
  if ((m = /^#([0-9a-f]{3})$/i.exec(s))) rgb = [...m[1]].map((c) => parseInt(c + c, 16))
  else if ((m = /^#([0-9a-f]{6})/i.exec(s)))
    rgb = [0, 2, 4].map((i) => parseInt(m[1].slice(i, i + 2), 16))
  else if ((m = /rgba?\(([^)]+)\)/i.exec(s)))
    rgb = m[1]
      .split(',')
      .slice(0, 3)
      .map((v) => parseFloat(v))
  else {
    const hex = typeof normalizeCssColor === 'function' ? normalizeCssColor(s) : null
    if (hex) return rgbOf(hex)
  }
  colorCache3d.set(color, rgb)
  return rgb
}

export function mixColor(color, bg, t) {
  if (t <= 0) return color
  const a = rgbOf(color),
    b = rgbOf(bg)
  return `rgb(${Math.round(a[0] + (b[0] - a[0]) * t)},${Math.round(a[1] + (b[1] - a[1]) * t)},${Math.round(a[2] + (b[2] - a[2]) * t)})`
}

// page wiring, run by main.ts in the original order
export function init() {
  cy.on('add remove data select unselect style class', () => {
    if (net3d.active) markDirty3d()
    else net3d.cacheDirty = true
  })
}
