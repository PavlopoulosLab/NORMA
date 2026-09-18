// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)

/* ---------- side panel: drag to resize, arrow to collapse ---------- */
// Expands the side panel if it's currently collapsed, so a shortcut button
// that jumps to a sidebar section (e.g. "Import from a database" on the
// Welcome page) doesn't switch tabs behind a still-closed panel.
export function expandSidePanel() {
  const app = document.getElementById('app')
  if (app.classList.contains('side-collapsed')) document.getElementById('sideToggle').click()
}

// page wiring, run by main.ts in the original order
export function init() {
  const app = document.getElementById('app')
  const handle = document.getElementById('sideResizer')
  const toggle = document.getElementById('sideToggle')
  const DEFAULT_W = 300,
    MIN_W = 260,
    COLLAPSE_BELOW = 150
  const maxW = () => Math.max(MIN_W, Math.min(720, Math.round(window.innerWidth * 0.6)))
  let width = DEFAULT_W,
    collapsed = false
  try {
    const saved = JSON.parse(localStorage.getItem('norma3-sidebar') || 'null')
    if (saved) {
      width = Number(saved.width) || DEFAULT_W
      collapsed = !!saved.collapsed
    } else if (window.innerWidth <= 760) {
      collapsed = true
    }
  } catch (e) {}
  const save = () => {
    try {
      localStorage.setItem('norma3-sidebar', JSON.stringify({ width, collapsed }))
    } catch (e) {}
  }
  function apply() {
    width = Math.max(MIN_W, Math.min(maxW(), Math.round(width)))
    app.style.setProperty('--side-w', collapsed ? '0px' : width + 'px')
    app.classList.toggle('side-collapsed', collapsed)
    const label = collapsed ? 'Expand the side panel' : 'Collapse the side panel'
    toggle.setAttribute('aria-expanded', collapsed ? 'false' : 'true')
    toggle.setAttribute('aria-label', label)
    toggle.title = label
    handle.setAttribute('aria-valuemin', MIN_W)
    handle.setAttribute('aria-valuemax', maxW())
    handle.setAttribute('aria-valuenow', width)
  }
  function setCollapsed(c) {
    collapsed = c
    apply()
    save()
  }
  toggle.addEventListener('click', () => setCollapsed(!collapsed))
  // on small screens the sidebar is an overlay drawer with a backdrop (#app::before);
  // a tap on the backdrop reports #app itself as the target, so treat that as "close"
  app.addEventListener('click', (e) => {
    if (e.target === app && !collapsed) setCollapsed(true)
  })

  handle.addEventListener('pointerdown', (e) => {
    if (e.button !== 0) return
    e.preventDefault()
    handle.setPointerCapture(e.pointerId)
    app.classList.add('side-resizing')
    const left = app.getBoundingClientRect().left
    const beforeW = width
    const move = (ev) => {
      const w = ev.clientX - left
      if (w < COLLAPSE_BELOW) {
        if (!collapsed) {
          collapsed = true
          apply()
        }
        return
      }
      collapsed = false
      width = w
      apply()
    }
    const up = () => {
      app.classList.remove('side-resizing')
      // dragged shut: reopen later at the width it had before the drag
      if (collapsed) width = beforeW
      handle.removeEventListener('pointermove', move)
      handle.removeEventListener('pointerup', up)
      handle.removeEventListener('pointercancel', up)
      save()
    }
    handle.addEventListener('pointermove', move)
    handle.addEventListener('pointerup', up)
    handle.addEventListener('pointercancel', up)
  })
  handle.addEventListener('dblclick', () => {
    width = DEFAULT_W
    collapsed = false
    apply()
    save()
  })
  handle.addEventListener('keydown', (e) => {
    const step = e.shiftKey ? 50 : 10
    if (e.key === 'ArrowLeft') width -= step
    else if (e.key === 'ArrowRight') width += step
    else if (e.key === 'Home') width = MIN_W
    else if (e.key === 'End') width = maxW()
    else if (e.key === 'Enter') {
      setCollapsed(true)
      toggle.focus()
      e.preventDefault()
      return
    } else return
    e.preventDefault()
    apply()
    save()
  })
  window.addEventListener('resize', apply)
  apply()
}
