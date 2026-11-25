// Exports a figure panel from a built-in example in headless Chromium, so
// paper figures can be regenerated:
//   node scripts/figure.mjs --example norma:tau --grouping 1 --hulls fog --edges bundled \
//        --layout fr --scale 3 --out tau.png [--legend] [--tab 3d] [--layout3d layers]
// The page must be served first (python3 ../backend/server.py --port 8765).
import { chromium } from '@playwright/test'

const arg = (name, dflt) => {
  const i = process.argv.indexOf(name)
  return i > 0 ? process.argv[i + 1] : dflt
}
const url = arg('--url', 'http://127.0.0.1:8765/')
const example = arg('--example', 'norma:tau')
const grouping = parseInt(arg('--grouping', '0'), 10)
const groupingLabel = arg('--grouping-label', '') // pick the grouping whose name contains this text
const hulls = arg('--hulls', 'fog') // hull | bubble | fog | off
const edges = arg('--edges', 'bundled') // haystack | bezier | bundled
const layout = arg('--layout', '') // empty keeps the layout the example opens with
const arrangement = arg('--arrangement', '') // a group arrangement (fr, circle, grid, virtual, ...) switches to layouts by groups
const local = arg('--local', 'fr') // local layout inside groups for --arrangement
const hideUngrouped = process.argv.includes('--hide-ungrouped')
const bundleStrength = arg('--bundle-strength', '') // 1..100, the Bundling strength slider
const theme = arg('--theme', '') // white | light | dark | midnight | gray | paper
const fill = arg('--fill', '') // groups | data | values (node fill)
const labels = arg('--labels', '') // off hides node labels
const labelSize = arg('--label-size', '') // node label size in px (4 to 48)
const labelFixed = process.argv.includes('--label-fixed') // same label size for every node
const sizeBy = arg('--size-by', '') // node size: degree, betweenness, closeness, clustering
const sizeMax = arg('--size-max', '') // largest node size in px when sizing by a measure
const legendJson = arg('--legend-json', '') // write the ticked groups (label, color, shape) as JSON
const shapes = process.argv.includes('--shapes') // one node shape per group
const direction = arg('--direction', '') // undirected | data | directed (arrows)
const opacity = arg('--opacity', '') // shading opacity 0.05..0.9
const spread = arg('--spread', '') // value of the Spread slider, if any
const bg = arg('--bg', 'white') // export background: white | theme
const groups = arg('--groups', '').split('|').filter(Boolean) // tick only these groups (by id/name)
const regroup = parseInt(arg('--regroup', '-1'), 10) // switch to this grouping after the layout, keeping positions
const tab = arg('--tab', '2d') // 2d | 3d
const layout3d = arg('--layout3d', '') // a value of the 3D layout list, e.g. layers
const scale = arg('--scale', '3')
const out = arg('--out', 'figure.png')
const legend = process.argv.includes('--legend')
const width = parseInt(arg('--width', '1400'), 10)
const height = parseInt(arg('--height', '1000'), 10)

const browser = await chromium.launch()
const page = await browser.newPage({ viewport: { width, height } })
page.setDefaultTimeout(120000)

// sidebar controls may sit in collapsed sections, so values are set directly
// and the events the app listens to are dispatched
const setValue = (sel, value) =>
  page.evaluate(
    ([sel, value]) => {
      const el = document.querySelector(sel)
      if (!el) throw new Error(`no element ${sel}`)
      el.value = value
      el.dispatchEvent(new Event('input', { bubbles: true }))
      el.dispatchEvent(new Event('change', { bubbles: true }))
    },
    [sel, value]
  )
const setChecked = (sel, want) =>
  page.evaluate(
    ([sel, want]) => {
      const el = document.querySelector(sel)
      if (!el) throw new Error(`no element ${sel}`)
      if (el.checked === want) return
      el.checked = want
      el.dispatchEvent(new Event('input', { bubbles: true }))
      el.dispatchEvent(new Event('change', { bubbles: true }))
    },
    [sel, want]
  )
const idle = (id) =>
  page.waitForFunction((id) => {
    const s = document.getElementById(id)
    return !s || !/…/.test(s.textContent)
  }, id)

await page.goto(`${url}?example=${encodeURIComponent(example)}`)
await page.waitForFunction(() => window.__norma && window.__norma.cy.nodes().length > 0)
await page.waitForTimeout(800)
await idle('layoutStatus')
if (groupingLabel) {
  const idx = await page.evaluate((t) => {
    const opts = [...document.getElementById('viewGrouping').options]
    return opts.findIndex((o) => o.textContent.includes(t))
  }, groupingLabel)
  if (idx < 0) throw new Error(`no grouping contains "${groupingLabel}"`)
  await page.selectOption('#viewGrouping', { index: idx })
  await page.waitForTimeout(800)
  await idle('layoutStatus')
} else if (grouping > 0) {
  const n = await page.locator('#viewGrouping option').count()
  if (grouping < n) {
    await page.selectOption('#viewGrouping', { index: grouping })
    await page.waitForTimeout(800)
    await idle('layoutStatus')
  }
}
if (groups.length) {
  await page.evaluate((wanted) => {
    document.querySelectorAll('input[type=checkbox][data-group]').forEach((box) => {
      const want = wanted.includes(box.getAttribute('data-group'))
      if (box.checked !== want) {
        box.checked = want
        box.dispatchEvent(new Event('change', { bubbles: true }))
      }
    })
  }, groups)
  await page.waitForTimeout(500)
}
if (hideUngrouped) {
  await page.evaluate(() => {
    const box = document.querySelector('input[type=checkbox][data-group="__ungrouped__"]')
    if (box && box.checked) {
      box.checked = false
      box.dispatchEvent(new Event('change', { bubbles: true }))
    }
  })
  await page.waitForTimeout(400)
}
if (arrangement) {
  await page.evaluate(() => document.querySelector('[role=radio][data-mode="groups"]').click())
  await setValue('#groupArrangement', arrangement)
  await setValue('#localGroupLayout', local)
  await page.evaluate(() => document.getElementById('btnRunLayout').click())
  await page.waitForTimeout(800)
  await idle('layoutStatus')
  await page.waitForTimeout(800)
} else if (layout) {
  await setValue('#layoutSelect', layout)
  await page.evaluate(() => document.getElementById('btnRunLayout').click())
  await page.waitForTimeout(500)
  await idle('layoutStatus')
}
if (regroup >= 0) {
  await page.selectOption('#viewGrouping', { index: regroup })
  await page.waitForTimeout(800)
  await idle('layoutStatus')
  if (hideUngrouped) {
    await page.evaluate(() => {
      const box = document.querySelector('input[type=checkbox][data-group="__ungrouped__"]')
      if (box && box.checked) {
        box.checked = false
        box.dispatchEvent(new Event('change', { bubbles: true }))
      }
    })
    await page.waitForTimeout(400)
  }
}
if (theme) {
  await setValue('#themeSelect', theme)
  await page.waitForTimeout(400)
}
if (fill) await setValue('#nodeFillSelect', fill)
if (labels === 'off') await setChecked('#showNodeLabels', false)
else {
  await setChecked('#showNodeLabels', true)
  // exported figures show every label, however small it is on screen
  await setValue('#labelMinScreenSize', '0')
}
if (labelSize) await setValue('#nodeLabelSize', labelSize)
if (labelFixed) await setChecked('#labelScaleWithNode', false)
if (sizeBy) {
  if (sizeMax) await setValue('#sizeMax', sizeMax)
  await setValue('#sizeMetric', sizeBy)
  await setChecked('#labelScaleWithNode', true)
  await page.waitForTimeout(500)
}
if (shapes) await page.evaluate(() => document.getElementById('btnShapesAssign').click())
if (direction) await setValue('#edgeDirection', direction)
if (opacity) await setValue('#hullOpacity', opacity)
if (spread) {
  await setValue('#spreadSlider', spread)
  await page.waitForTimeout(1500)
}
await setChecked('#showGroupHulls', hulls !== 'off')
if (hulls !== 'off') await setValue('#hullStyle', hulls)
if (bundleStrength) await setValue('#bundleStrength', bundleStrength)
await setValue('#edgeCurveStyle', edges)
if (edges === 'bundled') {
  // bundling starts after a short delay in a worker: wait for it to begin, then to end
  await page
    .waitForFunction(
      () => /\u2026/.test(document.getElementById('bundleStatus').textContent),
      null,
      {
        timeout: 5000,
      }
    )
    .catch(() => {})
  await idle('bundleStatus')
}
await page.waitForTimeout(800)
if (tab === '3d') {
  await page.click('#tabNetwork3d')
  await page.waitForTimeout(800)
  if (layout3d) {
    await setValue('#layout3d', layout3d)
    await page.evaluate(() => document.getElementById('btnRun3d').click())
    await page.waitForTimeout(800)
    await idle('layout3dStatus')
    await page.waitForTimeout(1500)
  }
} else {
  await page.evaluate(() => document.getElementById('btnFit').click())
  await page.waitForTimeout(600)
}
// export
await page.click('#btnExportImage')
await page.check('input[name="imgFormat"][value="png"]')
await setValue('#imgScale', scale)
await page.evaluate((bg) => {
  const el = document.getElementById('imgBackground')
  if ([...el.options].some((o) => o.value === bg)) {
    el.value = bg
    el.dispatchEvent(new Event('change', { bubbles: true }))
  }
}, bg)
await setChecked('#imgLegend', legend)
await page.fill('#imgFileName', 'figure')
const [download] = await Promise.all([page.waitForEvent('download'), page.click('#btnImgSave')])
await download.saveAs(out)
if (legendJson) {
  const groups = await page.evaluate(() =>
    [...document.querySelectorAll('input[type=checkbox][data-group]')]
      .filter((b) => b.checked && b.getAttribute('data-group') !== '__ungrouped__')
      .map((b) => {
        const row = b.closest('.toggle-row')
        const shape = (row.querySelector('.gshape')?.getAttribute('aria-label') || '')
          .split(': ')
          .pop()
        return {
          label: row.querySelector('.label')?.textContent.trim(),
          color: row.querySelector('input[type=color]')?.value,
          shape,
        }
      })
  )
  const { writeFileSync } = await import('node:fs')
  writeFileSync(legendJson, JSON.stringify(groups, null, 1))
}
const info = await page.evaluate(() => ({
  nodes: window.__norma.cy.nodes().filter((n) => !n.hasClass('hidden-group')).length,
  edges: window.__norma.cy.edges(':visible').length,
  grouping: document.getElementById('viewGrouping').selectedOptions[0]?.textContent,
  bundle: document.getElementById('bundleStatus').textContent.trim().slice(0, 80),
}))
console.log(
  `wrote ${out}: ${info.nodes} nodes, ${info.edges} edges, grouping "${info.grouping}" ${info.bundle}`
)
await browser.close()
