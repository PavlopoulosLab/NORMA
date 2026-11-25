// @ts-nocheck
/* ---------- "What next?" card after a network is loaded ----------
   Points new users to the functions they would not find on their own:
   the 3D view, edge bundling and the layouts by groups. Shown once per
   loaded network, at the bottom of the canvas, until the user ticks
   "Don't show this again" (kept in the browser). */
import { S } from './state'
import { cy } from './cy'
import { switchTab } from './wiring'
import { switchSideTab } from './side_tabs'
import { getUsedGroups, UNGROUPED } from './network_state'
import { setLayoutMode } from './layouts/controls'
import { runGroupLayout } from './layouts/run'

const STORAGE_KEY = 'norma3-next-steps'
let timer = null

function hiddenByUser() {
  try {
    return localStorage.getItem(STORAGE_KEY) === 'off'
  } catch {
    return false
  }
}

function card() {
  return document.getElementById('nextSteps')
}

export function hideNextSteps() {
  const el = card()
  if (el) el.hidden = true
  clearTimeout(timer)
}

// opens a sidebar section (Display tab) and scrolls to it
function revealControl(id) {
  switchSideTab('display')
  const el = document.getElementById(id)
  const sec = el && el.closest('.section.collapsible')
  if (sec && sec.classList.contains('collapsed')) sec.querySelector(':scope > h3').click()
  if (el) el.scrollIntoView({ block: 'center', behavior: 'smooth' })
  return el
}

const ACTIONS = {
  view3d() {
    switchTab('network3d')
  },
  bundle() {
    const sel = revealControl('edgeCurveStyle')
    if (sel && sel.value !== 'bundled') {
      sel.value = 'bundled'
      sel.dispatchEvent(new Event('change', { bubbles: true }))
    }
  },
  groups() {
    revealControl('btnRunLayout')
    setLayoutMode('groups')
    runGroupLayout()
  },
}

// Call when a network has just been loaded and laid out.
export function offerNextSteps() {
  if (hiddenByUser() || !cy.nodes().length) return
  const el = card()
  if (!el) return
  const groups = getUsedGroups().filter((g) => g !== UNGROUPED)
  el.querySelector('[data-action="groups"]').disabled = groups.length < 2
  el.querySelector('[data-action="bundle"]').disabled = cy.edges().length < 20
  el.hidden = false
  clearTimeout(timer)
  timer = setTimeout(hideNextSteps, 25000)
}

export function init() {
  const el = card()
  if (!el) return
  el.addEventListener('click', (e) => {
    const btn = e.target.closest('button')
    if (!btn) return
    const action = btn.dataset.action
    if (ACTIONS[action]) ACTIONS[action]()
    if (el.querySelector('#nextStepsOff').checked) {
      try {
        localStorage.setItem(STORAGE_KEY, 'off')
      } catch {
        /* storage blocked: the card simply shows again next time */
      }
    }
    hideNextSteps()
  })
  // the card goes away when the user starts working with the view
  cy.on('tap dragfree', () => {
    if (!el.hidden) hideNextSteps()
  })
  S.offerNextSteps = offerNextSteps
}
