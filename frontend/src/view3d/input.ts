// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import {
  VIEW_PRESETS_3D,
  fit3d,
  focalLength,
  moveCamera3d,
  rotate3d,
  screenToWorldDelta,
  zoom3d,
} from './camera'
import { applySpread3d, runLayout3d, updateSpread3dReadout } from './layouts'
import { canvas3d, el3, markDirty3d, net3d, requestRender3d } from './state'
import { cy, onCommitStyle } from '../cy'
import { hideEdgePopup, hideInfo } from '../profiler'
import { pos3dOf } from './cache'
import { toggleAutoRotate3d } from './tab'

/* ---------- picking ---------- */
function pickNode3d(x, y) {
  for (const p of net3d.proj) {
    if (Math.hypot(p.x - x, p.y - y) <= Math.max(p.r, 4)) return p
  }
  return null
}

function pickEdge3d(x, y) {
  let best = null,
    bestD = 6
  for (const e of net3d.projEdges) {
    let d
    if (e.cx !== undefined) {
      d = Infinity
      let px = e.ax,
        py = e.ay
      for (let i = 1; i <= 12; i++) {
        const t = i / 12
        const qx = (1 - t) * (1 - t) * e.ax + 2 * (1 - t) * t * e.cx + t * t * e.bx
        const qy = (1 - t) * (1 - t) * e.ay + 2 * (1 - t) * t * e.cy + t * t * e.by
        d = Math.min(d, segDist(x, y, px, py, qx, qy))
        px = qx
        py = qy
      }
    } else d = segDist(x, y, e.ax, e.ay, e.bx, e.by)
    if (d < bestD + e.width / 2) {
      bestD = d
      best = e
    }
  }
  return best
}

function segDist(x, y, x1, y1, x2, y2) {
  const dx = x2 - x1,
    dy = y2 - y1
  const l2 = dx * dx + dy * dy
  const t = l2 ? Math.max(0, Math.min(1, ((x - x1) * dx + (y - y1) * dy) / l2)) : 0
  return Math.hypot(x - (x1 + t * dx), y - (y1 + t * dy))
}

function updateLook3dReadouts() {
  el3('fog3dValue').textContent = Math.round((parseFloat(el3('fog3d').value) || 0) * 100) + '%'
  el3('layer3dRow').hidden = !['layers', 'hierarchy', 'degree'].includes(el3('layout3d').value)
  el3('layer3dSpacingValue').textContent = el3('layer3dSpacing').value
}

// page wiring, run by main.ts in the original order
export function init() {
  /* ---------- mouse, touch and keyboard ---------- */
  ;(function setupInput3d() {
    const cv = canvas3d()
    const box = el3('selectBox3d')
    let drag = null
    const local = (ev) => {
      const r = cv.getBoundingClientRect()
      return { x: ev.clientX - r.left, y: ev.clientY - r.top }
    }
    const startInteracting = () => {
      net3d.interacting = true
      clearTimeout(net3d.interactTimer)
    }
    const stopInteracting = () => {
      clearTimeout(net3d.interactTimer)
      net3d.interactTimer = setTimeout(() => {
        net3d.interacting = false
        requestRender3d()
      }, 160)
    }

    cv.addEventListener('contextmenu', (ev) => ev.preventDefault())
    cv.addEventListener('pointerdown', (ev) => {
      cv.focus()
      const p = local(ev)
      const node = pickNode3d(p.x, p.y)
      let mode
      if (ev.button === 2 || ev.button === 1 || ev.altKey) mode = 'pan'
      else if (ev.shiftKey && !node) mode = 'box'
      else if (node && !ev.shiftKey) mode = 'node'
      else mode = 'rotate'
      drag = {
        mode,
        x0: p.x,
        y0: p.y,
        x: p.x,
        y: p.y,
        moved: false,
        node,
        shift: ev.shiftKey,
        meta: ev.metaKey || ev.ctrlKey,
        cam: { ...net3d.cam },
      }
      if (mode === 'node') {
        const sel = cy.getElementById(node.id)
        drag.group = sel.selected() ? cy.nodes(':selected').map((n) => n.id()) : [node.id]
      }
      cv.setPointerCapture(ev.pointerId)
      ev.preventDefault()
    })
    cv.addEventListener('pointermove', (ev) => {
      if (!drag) {
        const p = local(ev)
        cv.style.cursor = pickNode3d(p.x, p.y) ? 'pointer' : 'grab'
        return
      }
      const p = local(ev)
      const dx = p.x - drag.x,
        dy = p.y - drag.y
      if (!drag.moved && Math.hypot(p.x - drag.x0, p.y - drag.y0) < 3) return
      if (!drag.moved) {
        drag.moved = true
        startInteracting()
      }
      drag.x = p.x
      drag.y = p.y
      if (drag.mode === 'rotate') {
        cv.style.cursor = 'grabbing'
        rotate3d(dx * 0.008, dy * 0.008)
      } else if (drag.mode === 'pan') {
        const H = cv.clientHeight
        const scale = focalLength(H) / net3d.cam.dist
        const d = screenToWorldDelta(-dx, -dy, scale, net3d.cam)
        net3d.cam.tx += d[0]
        net3d.cam.ty += d[1]
        net3d.cam.tz += d[2]
        requestRender3d()
      } else if (drag.mode === 'node') {
        const d = screenToWorldDelta(dx, dy, drag.node.scale, net3d.cam)
        drag.group.forEach((id) => {
          const q = pos3dOf(id)
          net3d.pos.set(id, [q[0] + d[0], q[1] + d[1], q[2] + d[2]])
        })
        requestRender3d()
      } else if (drag.mode === 'box') {
        box.hidden = false
        box.style.left = Math.min(drag.x0, p.x) + 'px'
        box.style.top = Math.min(drag.y0, p.y) + 'px'
        box.style.width = Math.abs(p.x - drag.x0) + 'px'
        box.style.height = Math.abs(p.y - drag.y0) + 'px'
      }
    })
    const finish = (ev) => {
      if (!drag) return
      const p = local(ev)
      const d = drag
      drag = null
      cv.style.cursor = 'grab'
      box.hidden = true
      if (d.moved) {
        stopInteracting()
        if (d.mode === 'box') {
          const x1 = Math.min(d.x0, p.x),
            x2 = Math.max(d.x0, p.x),
            y1 = Math.min(d.y0, p.y),
            y2 = Math.max(d.y0, p.y)
          const ids = net3d.proj
            .filter((q) => q.x >= x1 && q.x <= x2 && q.y >= y1 && q.y <= y2)
            .map((q) => q.id)
          cy.batch(() => ids.forEach((id) => cy.getElementById(id).select()))
        }
        return
      }
      // a click
      if (d.node) {
        const n = cy.getElementById(d.node.id)
        if (d.shift || d.meta) {
          n.selected() ? n.unselect() : n.select()
          return
        }
        n.emit('tap')
        return
      }
      const edge = pickEdge3d(p.x, p.y)
      if (edge) {
        cy.getElementById(edge.id).emit('tap')
        // place the edge details next to the click
        const popup = document.getElementById('edgePopup')
        const rect = document.getElementById('canvas').getBoundingClientRect()
        popup.style.left = Math.max(10, Math.min(p.x + 14, rect.width - 266)) + 'px'
        popup.style.top = Math.max(10, Math.min(p.y + 14, rect.height - 180)) + 'px'
        return
      }
      cy.elements().removeClass('dimmed highlighted')
      hideInfo()
      hideEdgePopup()
      if (!d.shift) cy.$(':selected').unselect()
    }
    cv.addEventListener('pointerup', finish)
    cv.addEventListener('pointercancel', finish)
    cv.addEventListener('dblclick', (ev) => {
      const p = local(ev)
      const node = pickNode3d(p.x, p.y)
      if (node) {
        const q = pos3dOf(node.id)
        moveCamera3d({ tx: q[0], ty: q[1], tz: q[2] })
      } else fit3d()
    })
    cv.addEventListener(
      'wheel',
      (ev) => {
        ev.preventDefault()
        startInteracting()
        zoom3d(Math.exp(-ev.deltaY * 0.0015))
        stopInteracting()
      },
      { passive: false }
    )
    cv.addEventListener('keydown', (ev) => {
      const k = ev.key
      const step = ev.shiftKey ? 0.25 : 0.08
      if (k === 'ArrowLeft') rotate3d(-step, 0)
      else if (k === 'ArrowRight') rotate3d(step, 0)
      else if (k === 'ArrowUp') rotate3d(0, -step)
      else if (k === 'ArrowDown') rotate3d(0, step)
      else if (k === '+' || k === '=') zoom3d(1.25)
      else if (k === '-' || k === '_') zoom3d(0.8)
      else if (k === '0' || k === 'f') fit3d()
      else return
      ev.preventDefault()
    })
    new ResizeObserver(() => requestRender3d()).observe(cv)
  })()

  /* ---------- controls ---------- */
  document.getElementById('zoomIn').addEventListener(
    'click',
    (ev) => {
      if (net3d.active) {
        ev.stopImmediatePropagation()
        zoom3d(1.25)
      }
    },
    true
  )

  document.getElementById('zoomOut').addEventListener(
    'click',
    (ev) => {
      if (net3d.active) {
        ev.stopImmediatePropagation()
        zoom3d(0.8)
      }
    },
    true
  )

  document.getElementById('zoomFit').addEventListener(
    'click',
    (ev) => {
      if (net3d.active) {
        ev.stopImmediatePropagation()
        fit3d()
      }
    },
    true
  )

  el3('btnRotL3d').addEventListener('click', () =>
    moveCamera3d({ yaw: net3d.cam.yaw - Math.PI / 8 })
  )

  el3('btnRotR3d').addEventListener('click', () =>
    moveCamera3d({ yaw: net3d.cam.yaw + Math.PI / 8 })
  )

  el3('btnRotU3d').addEventListener('click', () =>
    moveCamera3d({ pitch: net3d.cam.pitch - Math.PI / 8 })
  )

  el3('btnRotD3d').addEventListener('click', () =>
    moveCamera3d({ pitch: net3d.cam.pitch + Math.PI / 8 })
  )

  el3('btnReset3d').addEventListener('click', () => {
    moveCamera3d({ ...VIEW_PRESETS_3D.tilted }, false)
    fit3d()
  })

  el3('btnAuto3d').addEventListener('click', () => {
    const on = el3('btnAuto3d').getAttribute('aria-pressed') !== 'true'
    el3('autoRotate3d').checked = on
    toggleAutoRotate3d(on)
  })

  el3('autoRotate3d').addEventListener('change', () =>
    toggleAutoRotate3d(el3('autoRotate3d').checked)
  )

  document.querySelectorAll('[data-view3d]').forEach((btn) => {
    btn.addEventListener('click', () => {
      moveCamera3d({ ...VIEW_PRESETS_3D[btn.dataset.view3d] }, false)
      fit3d()
    })
  })

  el3('btnRun3d').addEventListener('click', () => runLayout3d())

  el3('btnFit3d').addEventListener('click', () => fit3d())

  el3('layout3d').addEventListener('change', () => {
    const algo = el3('layout3d').value
    el3('layer3dRow').hidden = !['layers', 'hierarchy', 'degree'].includes(algo)
  })

  el3('layer3dSpacing').addEventListener('input', () => {
    el3('layer3dSpacingValue').textContent = el3('layer3dSpacing').value
  })

  el3('spread3d').addEventListener('input', applySpread3d)

  el3('spread3d').addEventListener('dblclick', () => {
    el3('spread3d').value = 0
    applySpread3d()
  })

  ;['persp3d', 'fog3d', 'style3d', 'grid3d'].forEach((id) => {
    el3(id).addEventListener('input', () => {
      updateLook3dReadouts()
      requestRender3d()
    })
    el3(id).addEventListener('change', () => {
      updateLook3dReadouts()
      requestRender3d()
    })
  })

  updateLook3dReadouts()

  updateSpread3dReadout()

  // settings that change what the 2D view draws also change the 3D picture
  onCommitStyle(() => {
    if (net3d.active) markDirty3d()
    else net3d.cacheDirty = true
  })

  ;[
    'showGroupHulls',
    'hullStyle',
    'hullOpacity',
    'edgeCurveStyle',
    'edgeCurvature',
    'labelMinScreenSize',
  ].forEach((id) => {
    document.getElementById(id).addEventListener('input', requestRender3d)
    document.getElementById(id).addEventListener('change', requestRender3d)
  })
}
