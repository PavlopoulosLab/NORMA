// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { buildCache3d, pos3dOf } from './cache'
import { canvas3d, el3, net3d, requestRender3d } from './state'
import { cy } from '../cy'
import { formatZoom } from '../profiler'
import { render3d } from './draw'

/* ---------- camera and projection ---------- */
function perspectiveMode() {
  return el3('persp3d').value
}

export function focalLength(H) {
  const fov = perspectiveMode() === 'strong' ? 65 : 30
  return H / 2 / Math.tan((fov * Math.PI) / 360)
}

// Returns a projector for a given viewport size and camera.
export function makeProjector(W, H, cam) {
  const cyaw = Math.cos(cam.yaw),
    syaw = Math.sin(cam.yaw)
  const cp = Math.cos(cam.pitch),
    sp = Math.sin(cam.pitch)
  const f = focalLength(H)
  const ortho = perspectiveMode() === 'none'
  const orthoScale = f / cam.dist
  const near = cam.dist * 0.02
  return (p) => {
    const qx = p[0] - cam.tx,
      qy = p[1] - cam.ty,
      qz = p[2] - cam.tz
    const x1 = cyaw * qx - syaw * qz
    const z1 = syaw * qx + cyaw * qz
    const y2 = cp * qy - sp * z1
    const z2 = sp * qy + cp * z1
    const depth = cam.dist + z2
    if (!ortho && depth < near) return null
    const scale = ortho ? orthoScale : f / depth
    return { x: W / 2 + x1 * scale, y: H / 2 + y2 * scale, depth, scale }
  }
}

// Inverse of the rotation: a screen-plane vector (dx, dy) to world units at a given scale.
export function screenToWorldDelta(dx, dy, scale, cam) {
  const cyaw = Math.cos(cam.yaw),
    syaw = Math.sin(cam.yaw)
  const cp = Math.cos(cam.pitch),
    sp = Math.sin(cam.pitch)
  const x1 = dx / scale,
    y2 = dy / scale
  // camera-space vector (x1, y2, 0) back to world space
  const qy = cp * y2
  const z1 = -sp * y2
  const qx = cyaw * x1 + syaw * z1
  const qz = -syaw * x1 + cyaw * z1
  return [qx, qy, qz]
}

/* ---------- camera helpers ---------- */
export function shownIds3d(eles) {
  if (eles && eles.length)
    return eles.filter((e) => (e.isNode ? e.isNode() : true)).map((n) => n.id())
  if (net3d.cacheDirty || !net3d.cache) buildCache3d()
  return net3d.cache.nodes.map((n) => n.id)
}

export function fit3d(eles, animate = true) {
  const ids = shownIds3d(
    eles && eles.nodes
      ? eles.nodes().union(eles.connectedNodes ? eles.connectedNodes() : cy.collection())
      : eles
  )
  if (!ids.length) return
  const pts = ids.map((id) => ({ p: pos3dOf(id), r: (cy.getElementById(id).width() || 40) / 2 }))
  // aim at the middle of the bounding box
  const lo = [Infinity, Infinity, Infinity],
    hi = [-Infinity, -Infinity, -Infinity]
  pts.forEach(({ p }) => {
    for (let d = 0; d < 3; d++) {
      lo[d] = Math.min(lo[d], p[d])
      hi[d] = Math.max(hi[d], p[d])
    }
  })
  const cam = {
    ...net3d.cam,
    tx: (lo[0] + hi[0]) / 2,
    ty: (lo[1] + hi[1]) / 2,
    tz: (lo[2] + hi[2]) / 2,
  }
  const cv = canvas3d()
  const W = cv.clientWidth || 800,
    H = cv.clientHeight || 600
  const margin = 40
  cam.dist = Math.max(60, Math.hypot(hi[0] - lo[0], hi[1] - lo[1], hi[2] - lo[2]) * 1.2)
  // refine: project, measure how far the picture reaches, rescale the distance
  for (let it = 0; it < 6; it++) {
    const project = makeProjector(W, H, cam)
    let need = 0
    for (const { p, r } of pts) {
      const q = project(p)
      if (!q) {
        need = Infinity
        break
      }
      const ex = Math.abs(q.x - W / 2) + r * q.scale,
        ey = Math.abs(q.y - H / 2) + r * q.scale
      need = Math.max(need, ex / (W / 2 - margin), ey / (H / 2 - margin))
    }
    if (!Number.isFinite(need)) {
      cam.dist *= 2
      continue
    }
    if (Math.abs(need - 1) < 0.02) break
    cam.dist *= perspectiveMode() === 'none' ? need : Math.max(0.3, need)
  }
  moveCamera3d({ tx: cam.tx, ty: cam.ty, tz: cam.tz, dist: cam.dist }, animate)
}

export function moveCamera3d(target, animate = true) {
  const from = { ...net3d.cam }
  const to = { ...from, ...target }
  if (!animate) {
    Object.assign(net3d.cam, to)
    requestRender3d()
    return
  }
  // turn the short way round (both angles are unlimited)
  ;['yaw', 'pitch'].forEach((k) => {
    if (target[k] === undefined) return
    to[k] = from[k] + wrapAngle(to[k] - from[k])
  })
  const start = performance.now()
  const step = (now) => {
    const t = Math.min(1, (now - start) / 450)
    const e = 1 - Math.pow(1 - t, 3)
    Object.keys(to).forEach((key) => {
      net3d.cam[key] = from[key] + (to[key] - from[key]) * e
    })
    if (t >= 1) {
      net3d.cam.yaw = wrapAngle(net3d.cam.yaw)
      net3d.cam.pitch = wrapAngle(net3d.cam.pitch)
    }
    render3d()
    if (t < 1) requestAnimationFrame(step)
  }
  requestAnimationFrame(step)
}

export function zoom3d(factor) {
  net3d.cam.dist = Math.min(1e7, Math.max(5, net3d.cam.dist / factor))
  requestRender3d()
}

// keeps an angle in (-pi, pi]
export function wrapAngle(a) {
  a = a % (Math.PI * 2)
  if (a > Math.PI) a -= Math.PI * 2
  if (a <= -Math.PI) a += Math.PI * 2
  return a
}

export function rotate3d(dyaw, dpitch) {
  // turning and tilting have no limits: the view can go over the poles
  // as often as wanted
  net3d.cam.yaw = wrapAngle(net3d.cam.yaw + dyaw)
  net3d.cam.pitch = wrapAngle(net3d.cam.pitch + dpitch)
  requestRender3d()
}

export const VIEW_PRESETS_3D = {
  front: { yaw: 0, pitch: 0 },
  top: { yaw: 0, pitch: Math.PI / 2 - 0.02 },
  side: { yaw: Math.PI / 2, pitch: 0 },
  tilted: { yaw: 0.6, pitch: 0.45 },
}

export function updateZoomReadout3d() {
  const H = canvas3d().clientHeight || 600
  const scale = focalLength(H) / net3d.cam.dist
  document.getElementById('zoomLevel').textContent = formatZoom(scale)
}
