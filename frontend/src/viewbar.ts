// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { activeView, createView, renderViewBar, switchView, uniqueViewName } from './profiler'

function setRenaming(on) {
  document.getElementById('viewSelectRow').hidden = on
  document.getElementById('viewRenameRow').hidden = !on
  const input = document.getElementById('viewNameInput')
  if (on) {
    input.value = activeView() ? activeView().name : ''
    input.focus()
    input.select()
  } else {
    document.getElementById('viewSelect').focus()
  }
}

function saveRename() {
  const v = activeView()
  const name = document.getElementById('viewNameInput').value.trim()
  if (v && name && name !== v.name) {
    v.name = uniqueViewName(name)
    v.autoName = false
  }
  setRenaming(false)
  renderViewBar()
}

// page wiring, run by main.ts in the original order
export function init() {
  /* ---------- view bar controls ---------- */
  document
    .getElementById('viewSelect')
    .addEventListener('change', (e) => switchView(e.target.value))

  document.getElementById('btnViewDuplicate').addEventListener('click', () => {
    const cur = activeView()
    createView(`${cur ? cur.name : 'View'} copy`, { copy: true })
  })

  document.getElementById('btnViewRename').addEventListener('click', () => setRenaming(true))

  document.getElementById('btnViewRenameSave').addEventListener('click', saveRename)

  document.getElementById('viewNameInput').addEventListener('keydown', (e) => {
    if (e.key === 'Enter') {
      e.preventDefault()
      saveRename()
    } else if (e.key === 'Escape') {
      e.preventDefault()
      setRenaming(false)
    }
  })
}
