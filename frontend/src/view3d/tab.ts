// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { cy } from '../cy'
import { el3, net3d, requestRender3d } from './state'
import { fit3d, wrapAngle } from './camera'
import { render3d } from './draw'
import { runLayout3d, updateSpread3dReadout } from './layouts'
import { updateZoomReadout } from '../profiler'

/* ---------- entering and leaving the 3D tab ---------- */
export function setMode3d(on) {
  net3d.active = on
  document.getElementById('canvas').classList.toggle('mode3d', on)
  el3('cy3d').hidden = !on
  el3('nav3d').hidden = !on
  el3('section3d').hidden = !on
  const layout2d = document.getElementById('layoutSection')
  if (layout2d) layout2d.hidden = on
  document.getElementById('hint').textContent = on
    ? 'Drag to rotate · drag a node to move it · right-drag to pan · scroll to zoom · Shift-drag to select · double-click to fit'
    : 'Drag nodes · Shift-drag or Shift-click to select several · scroll to zoom · double-click to recenter'
  if (on) {
    net3d.cacheDirty = true
    if (!net3d.hasLayout && cy.nodes().length) {
      // first visit for this view: start from the 2D picture, then lay out in 3D
      cy.nodes().forEach((n) => {
        if (!net3d.pos.has(n.id())) {
          const p = n.position()
          net3d.pos.set(n.id(), [p.x, p.y, 0])
        }
      })
      fit3d(null, false)
      runLayout3d(el3('layout3d').value)
    } else {
      render3d()
    }
    toggleAutoRotate3d(el3('autoRotate3d').checked)
  } else {
    toggleAutoRotate3d(false)
    updateZoomReadout()
  }
}

// per-view state
export function capture3d() {
  const pos = {}
  net3d.pos.forEach((p, id) => {
    pos[id] = p
  })
  return { pos, cam: { ...net3d.cam }, hasLayout: net3d.hasLayout, spread: net3d.spreadApplied }
}

export function restore3d(state) {
  net3d.pos = new Map(state && state.pos ? Object.entries(state.pos) : [])
  net3d.cam =
    state && state.cam
      ? { ...state.cam }
      : { yaw: 0.6, pitch: 0.35, dist: 900, tx: 0, ty: 0, tz: 0 }
  net3d.hasLayout = !!(state && state.hasLayout)
  net3d.spreadApplied = (state && state.spread) || 0
  el3('spread3d').value = net3d.spreadApplied
  updateSpread3dReadout()
  net3d.cacheDirty = true
  net3d.layoutSeq++
  if (net3d.active) {
    if (!net3d.hasLayout && cy.nodes().length) {
      cy.nodes().forEach((n) => {
        if (!net3d.pos.has(n.id())) {
          const p = n.position()
          net3d.pos.set(n.id(), [p.x, p.y, 0])
        }
      })
      fit3d(null, false)
      runLayout3d(el3('layout3d').value)
    } else render3d()
  }
}

// new data in the current view: 3D positions start over
export function reset3dForNewData() {
  net3d.pos = new Map()
  net3d.hasLayout = false
  net3d.cacheDirty = true
  net3d.layoutSeq++
  if (net3d.active && cy.nodes().length) {
    cy.nodes().forEach((n) => {
      const p = n.position()
      net3d.pos.set(n.id(), [p.x, p.y, 0])
    })
    fit3d(null, false)
    runLayout3d(el3('layout3d').value)
  }
}

/* ---------- auto-rotate ---------- */
export function toggleAutoRotate3d(on) {
  cancelAnimationFrame(net3d.autoTimer)
  net3d.autoTimer = null
  el3('btnAuto3d').setAttribute('aria-pressed', on ? 'true' : 'false')
  if (!on || !net3d.active) return
  let last = performance.now()
  const tick = (now) => {
    const speed = parseFloat(el3('autoSpeed3d').value) || 1
    net3d.cam.yaw = wrapAngle(net3d.cam.yaw + ((now - last) / 1000) * 0.35 * speed)
    last = now
    const big = net3d.cache && net3d.cache.nodes.length + net3d.cache.edges.length > 3000
    if (big) {
      net3d.interacting = true
      clearTimeout(net3d.interactTimer)
      net3d.interactTimer = setTimeout(() => {
        net3d.interacting = false
        requestRender3d()
      }, 200)
    }
    render3d()
    net3d.autoTimer = requestAnimationFrame(tick)
  }
  net3d.autoTimer = requestAnimationFrame(tick)
}
