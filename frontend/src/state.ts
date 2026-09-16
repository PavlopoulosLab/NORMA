import { EDGE_TYPES } from './palette'

// ponytail: mutable page state that several modules assign; ES module bindings are read-only across files
export const S = {
  currentLibView: null,
  nodeColorMap: {},
  groupShapes: {},
  groupAttrs: {},
  activeTypes: new Set(Object.keys(EDGE_TYPES)),
  activeGroups: new Set(),
  layoutRunSeq: 0,
  currentNodePalette: 'vivid',
  infoNodeId: null,
  autoEdgeIdx: 0,
  historySuspended: 0,
  dataCache: { version: -1, data: null },
  historyGestureOpen: false,
  cyLayoutsRunning: 0,
  frRequestSeq: 0,
  normaEntrySeq: 0,
  viewSeq: 0,
  activeViewId: null,
  DEFAULT_VIEW_CONFIG: null,
}
