// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import cytoscape from 'cytoscape'

/* ============================================================
   SETTINGS
   window.NORMA_CONFIG comes from server.py (/norma-config.js) or from a
   norma-config.js file next to norma.html; everything has a default, so
   NORMA also runs with no settings at all.
   ============================================================ */
export const NORMA_CFG = (() => {
  const d = {
    mode: 'local',
    version: '3.0',
    publicUrl: '',
    features: {
      restApi: null,
      relays: { string: true, arena3d: true, databases: true },
      apiTtlHours: 24,
    },
    site: {
      name: 'NORMA',
      institution: '',
      contactName: 'Pavlopoulos Lab',
      contactEmail: '',
      contactUrl: 'https://github.com/PavlopoulosLab/NORMA/issues',
      sourceUrl: 'https://github.com/PavlopoulosLab/NORMA',
      licenceName: 'MIT License',
      licenceUrl: 'https://opensource.org/licenses/MIT',
      privacyUrl: '',
      imprintUrl: '',
      maintainedUntil: '',
      notice: '',
      testedBrowsers: [],
    },
    app: { maxNodes: 5000, theme: 'white', startTab: 'welcome', cdnFallback: true },
  }
  const c = (typeof window.NORMA_CONFIG === 'object' && window.NORMA_CONFIG) || {}
  const merge = (a, b) => {
    Object.keys(b || {}).forEach((k) => {
      if (
        b[k] &&
        typeof b[k] === 'object' &&
        !Array.isArray(b[k]) &&
        a[k] &&
        typeof a[k] === 'object'
      )
        merge(a[k], b[k])
      else if (b[k] !== undefined && b[k] !== null) a[k] = b[k]
    })
    return a
  }
  merge(d, c)
  d.served = /^https?:$/.test(location.protocol)
  d.fromServer = d.served && !!c.features // server.py writes "features"
  return d
})()
