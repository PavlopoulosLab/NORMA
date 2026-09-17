// @ts-nocheck
// ponytail: boot order of the former single-file script; typed later with the rest
import { NORMA_CFG } from './config'
import { NORMA_KINDS, normaLibrary, renderLibraryLists, updateStrategyUI } from './layouts/controls'
import { S } from './state'
import { TAB_VIEWS, currentTab, switchTab } from './wiring'
import {
  activeView,
  applyEdgeCurveStyle,
  applyEdgeOpacity,
  applyEdgeWidth,
  renderViewBar,
  updateSpreadReadout,
  updateZoomReadout,
  viewSettings,
  views,
} from './profiler'
import { apiAnnounceReady, apiFromLocation } from './api/wiring'
import { applyEdgeDirection } from './export/dialog'
import { applyTheme } from './themes'
import { askDelete, renderSiteInfo } from './api/tester'
import { cy } from './cy'
import { net3d } from './view3d/state'
import { resizeHullCanvas } from './hulls'
import { setHistoryBaseline, updateUndoButtons } from './demo_downloads'
import { updateEmptyState } from './welcome'
import { updateStats } from './metrics'

import './style.css'
import './state'
import './config'
import './palette'
import './themes'
import './sample_data'
import './cy'
import './network_state'
import { init as initHulls } from './hulls'
import './parallel_edges'
import './metrics'
import './layouts/input'
import './layouts/run'
import { init as initLayoutsControls } from './layouts/controls'
import './library'
import './uploads'
import './examples'
import './export_norma'
import { init as initWiring } from './wiring'
import { init as initProfiler } from './profiler'
import { init as initViewbar } from './viewbar'
import { init as initSideTabs } from './side_tabs'
import './directed_stats'
import { init as initDemoDownloads } from './demo_downloads'
import { init as initRecording } from './recording'
import { init as initLabelColors } from './label_colors'
import { init as initExportDraw } from './export/draw'
import './export/shading'
import './export/raster'
import './export/svg'
import { init as initExportDialog } from './export/dialog'
import { init as initView3dState } from './view3d/state'
import './view3d/cache'
import './view3d/camera'
import './view3d/painters'
import './view3d/draw'
import './view3d/layouts'
import './view3d/tab'
import { init as initView3dInput } from './view3d/input'
import './view3d/export'
import './string/requests'
import './string/ui_state'
import './string/import'
import './string/groupings'
import { init as initStringWiring } from './string/wiring'
import { init as initWelcome } from './welcome'
import './clustering/leiden'
import './clustering/label_propagation'
import './clustering/walktrap'
import './clustering/mcl'
import { init as initClusteringMapping } from './clustering/mapping'
import './benchmark'
import { init as initClusteringWiring } from './clustering/wiring'
import { init as initContours } from './contours'
import './arena3d'
import './group_network'
import { init as initEnrichment } from './enrichment'
import { init as initApiWiring } from './api/wiring'
import { init as initApiTester } from './api/tester'
import { init as initSidePanel } from './side_panel'

initHulls()
initLayoutsControls()
initWiring()
initProfiler()
initViewbar()
initSideTabs()
initDemoDownloads()
initRecording()
initLabelColors()
initExportDraw()
initExportDialog()
initView3dState()
initView3dInput()
initStringWiring()
initWelcome()
initClusteringMapping()
initClusteringWiring()
initContours()
initEnrichment()
initApiWiring()
initApiTester()

document.getElementById('btnViewDelete').addEventListener('click', () => {
  const lv = S.currentLibView
  const items = []
  if (lv) {
    String(lv.nets || '')
      .split('|')
      .filter(Boolean)
      .forEach((id) => items.push({ kind: 'network', id }))
    if (lv.annotation) items.push({ kind: 'annotation', id: lv.annotation })
    if (lv.colors) items.push({ kind: 'colors', id: lv.colors })
  }
  askDelete(items, { view: activeView() })
})
document.getElementById('btnGroupingDelete').addEventListener('click', () => {
  const id = S.currentLibView && S.currentLibView.annotation
  if (id) askDelete([{ kind: 'annotation', id }])
})
document.getElementById('btnDeleteAllFiles').addEventListener('click', () => {
  askDelete(
    NORMA_KINDS.flatMap((kind) => normaLibrary[kind].map((e) => ({ kind, id: e.id }))),
    { all: true }
  )
})
/* boot */
applyTheme(NORMA_CFG.app.theme || 'white')
if (
  document.getElementById('themeSelect') &&
  [...document.getElementById('themeSelect').options].some((o) => o.value === NORMA_CFG.app.theme)
)
  document.getElementById('themeSelect').value = NORMA_CFG.app.theme
applyEdgeCurveStyle()
applyEdgeWidth()
applyEdgeOpacity()
applyEdgeDirection()
updateSpreadReadout()
initSidePanel()

resizeHullCanvas()
updateStrategyUI()
updateZoomReadout()
renderLibraryLists()
S.DEFAULT_VIEW_CONFIG = viewSettings()
// NORMA starts with an empty view and the welcome page
views.push({
  id: 'view' + ++S.viewSeq,
  name: 'Untitled view',
  autoName: true,
  data: null,
  selection: { networks: [], annotation: '', colors: '' },
  state: { config: { ...S.DEFAULT_VIEW_CONFIG } },
})
S.activeViewId = views[0].id
updateStats()
renderViewBar()
updateEmptyState()
switchTab(TAB_VIEWS[NORMA_CFG.app.startTab] ? NORMA_CFG.app.startTab : 'welcome')
renderSiteInfo()
// the API: data in the address, and a hello to a page that opened NORMA
apiFromLocation()
apiAnnounceReady()
setHistoryBaseline()
updateUndoButtons()
// test hook: the canvas is opaque to the DOM, so end-to-end tests read state here
window.__norma = {
  cy,
  net3d,
  views,
  get currentTab() {
    return currentTab
  },
}
