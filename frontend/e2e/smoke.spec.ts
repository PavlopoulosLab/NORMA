import { expect, test } from '@playwright/test'
import { EXAMPLE, nodeCount, openExample, positions, selectValue } from './helpers'

test('welcome page loads with examples and site info', async ({ page }) => {
  await page.goto('/')
  await expect(page).toHaveTitle(/NORMA/)
  await expect(page.locator('#tabWelcome')).toHaveAttribute('aria-selected', 'true')
  expect(await page.locator('#sampleSelect option').count()).toBeGreaterThan(3)
  await expect(page.locator('#welcomeView')).toContainText('MIT License')
})

test('example opens with nodes, edges and groups', async ({ page }) => {
  await openExample(page)
  const nodes = await nodeCount(page)
  const edges = await page.evaluate(() => window.__norma.cy.edges().length)
  expect(edges).toBeGreaterThan(0)
  await expect(page.locator('#statNodes')).toHaveText(nodes.toLocaleString('en-US'))
  await expect(page.locator('#statGroups')).not.toHaveText('0')
})

test('every built-in example loads', async ({ page }) => {
  await page.goto('/')
  const keys = await page
    .locator('#sampleSelect option')
    .evaluateAll((o) => o.map((e) => (e as HTMLOptionElement).value))
  for (const key of keys.filter((k) => k.startsWith('norma:'))) {
    await openExample(page, key)
  }
})

test('layouts move the nodes', async ({ page }) => {
  await openExample(page)
  const before = await positions(page)
  await page.locator('#sideTabDisplay').click()
  await page.locator('#layoutSection h3').click()
  await expect(page.locator('#btnRunLayout')).toBeVisible()
  await selectValue(page, '#layoutSelect', 'circle')
  await page.locator('#btnRunLayout').click()
  await expect
    .poll(async () => JSON.stringify(await positions(page)) !== JSON.stringify(before))
    .toBe(true)
  const circle = await positions(page)
  await selectValue(page, '#layoutSelect', 'grid')
  await page.locator('#btnRunLayout').click()
  await expect
    .poll(async () => JSON.stringify(await positions(page)) !== JSON.stringify(circle))
    .toBe(true)
})

test('Kamada-Kawai and Stress layouts run in the background worker on a big network', async ({
  page,
  request,
}) => {
  // Regression test: getFrWorker() (metrics.ts) builds the layout worker by
  // stringifying module-private functions and dispatching them by their
  // literal source names. This suite runs against the production build
  // (see playwright.config.ts), where the bundler's minifier renames those
  // functions; the dispatcher's hardcoded names didn't follow, so "kk" and
  // "stress" threw "distanceLayout is not defined" inside the worker and
  // silently failed (runComputedLayout only reports it into #layoutStatus,
  // it never throws or logs). That only happens once a network is big
  // enough to take the worker path (> 60 nodes) - a small example never
  // exercised it.
  const n = 80
  const edges = Array.from({ length: n - 1 }, (_, i) => ({ source: `N${i}`, target: `N${i + 1}` }))
  const r = await request.post('/api/external', { data: { name: 'Layout worker test', edges } })
  expect(r.ok()).toBe(true)
  const { url } = (await r.json()) as { url: string }
  await page.goto(url)
  await expect.poll(() => nodeCount(page)).toBe(n)

  await page.locator('#sideTabDisplay').click()
  await page.locator('#layoutSection h3').click()
  await expect(page.locator('#btnRunLayout')).toBeVisible()

  for (const kind of ['kk', 'stress']) {
    const before = await positions(page)
    await selectValue(page, '#layoutSelect', kind)
    await page.locator('#btnRunLayout').click()
    await expect
      .poll(async () => JSON.stringify(await positions(page)) !== JSON.stringify(before), {
        timeout: 20_000,
      })
      .toBe(true)
    await expect(page.locator('#layoutStatus')).toBeEmpty()
  }
})

test('tabs switch views, 3D view activates', async ({ page }) => {
  await openExample(page)
  for (const [tab, view] of [
    ['tabProfiler', 'profilerView'],
    ['tabCompare', 'compareView'],
    ['tabApi', 'apiView'],
    ['tabHelp', 'helpView'],
    ['tabAbout', 'aboutView'],
  ]) {
    await page.locator(`#${tab}`).click()
    await expect(page.locator(`#${view}`)).toBeVisible()
  }
  await page.locator('#tabNetwork3d').click()
  await expect.poll(() => page.evaluate(() => window.__norma.net3d.active)).toBe(true)
  await expect(page.locator('#cy3d')).toBeVisible()
  await page.locator('#tabNetwork').click()
  await expect.poll(() => page.evaluate(() => window.__norma.net3d.active)).toBe(false)
})

for (const format of ['png', 'svg']) {
  test(`image export downloads a ${format}`, async ({ page }) => {
    await openExample(page)
    await page.locator('#btnExportImage').click()
    await expect(page.locator('#exportDialog')).toBeVisible()
    await page.locator(`input[name="imgFormat"][value="${format}"]`).check()
    const download = page.waitForEvent('download')
    await page.locator('#btnImgSave').click()
    const file = await download
    expect(file.suggestedFilename()).toMatch(new RegExp(`\\.${format}$`))
    expect(await file.failure()).toBeNull()
  })
}

test('profiler computes statistics and the layout benchmark', async ({ page }) => {
  await openExample(page)
  await page.locator('#tabProfiler').click()
  await expect(page.locator('#profNetList input')).not.toHaveCount(0)
  await page.locator('#btnProfile').click()
  await expect(page.locator('#profResults')).not.toBeEmpty({ timeout: 45_000 })
  await selectValue(page, '#benchRepeats', '1')
  await page.locator('#btnBench').click()
  await expect(page.locator('#benchResults')).not.toBeEmpty({ timeout: 45_000 })
})

test('API tab: server round trip and json link both open the payload', async ({ page }) => {
  await page.goto('/norma.html')
  await page.locator('#tabApi').click()
  await expect(page.locator('#apiPayload')).not.toBeEmpty()
  await page.locator('#btnApiServer').click()
  await expect(page.locator('#apiResultUrl')).toHaveValue(/norma\.html\?session=/)
  const url = await page.locator('#apiResultUrl').inputValue()
  await page.goto(url)
  await expect.poll(() => nodeCount(page)).toBeGreaterThan(0)

  await page.goto('/norma.html')
  await page.locator('#tabApi').click()
  await page.locator('#btnApiLink').click()
  await expect(page.locator('#apiResultUrl')).toHaveValue(/#json=/)
  const link = await page.locator('#apiResultUrl').inputValue()
  await page.goto('about:blank') // a hash-only change would not reload the page
  await page.goto(link)
  await expect.poll(() => nodeCount(page)).toBeGreaterThan(0)
})

test('REST API payload via curl-style POST opens in the page', async ({ page, request }) => {
  const r = await request.post('/api/external', {
    data: {
      name: 'Demo',
      edges: [
        { source: 'A', target: 'B' },
        { source: 'B', target: 'C' },
      ],
      groups: { G1: ['A', 'B'] },
    },
  })
  expect(r.ok()).toBe(true)
  const { url } = (await r.json()) as { url: string }
  await page.goto(url)
  await expect.poll(() => nodeCount(page)).toBe(3)
  await expect(page.locator('#statGroups')).toHaveText('1')
})

test('unknown example shows an error, not a crash', async ({ page }) => {
  await page.goto('/norma.html?example=nope')
  await expect(page.locator('body')).toContainText(/no example called/i)
  expect(await nodeCount(page)).toBe(0)
  void EXAMPLE
})

test('uploading a non-NORMA file is rejected with a warning before it reaches the canvas', async ({
  page,
}) => {
  await page.goto('/')
  await page.locator('#filesSection h3').click()
  await page.setInputFiles('#normaFileInput', {
    name: 'export.json',
    mimeType: 'application/json',
    buffer: Buffer.from('{"nodes":["A","B"],"edges":[["A","B"]]}'),
  })
  await expect(page.locator('#normaStatus .note.error')).toContainText(/looks like JSON/)
  await expect(page.locator('#libNetworks .lib-empty')).toBeVisible()
  await expect(page.locator('#libNetworks .lib-row')).toHaveCount(0)
  expect(await nodeCount(page)).toBe(0)
})

test('uploading a valid network file is accepted and ready to show', async ({ page }) => {
  await page.goto('/')
  await page.locator('#filesSection h3').click()
  await page.setInputFiles('#normaFileInput', {
    name: 'network.txt',
    mimeType: 'text/plain',
    buffer: Buffer.from('Source\tTarget\tWeight\nA\tB\t1\nB\tC\t2\n'),
  })
  await expect(page.locator('#normaStatus .note.ok').first()).toContainText(/added/i)
  await expect(page.locator('#libNetworks')).not.toBeEmpty()
})

test('"Import from a database" expands a collapsed sidebar and opens Database importers', async ({
  page,
}) => {
  await page.goto('/')
  await page.locator('#sideToggle').click()
  await expect(page.locator('#app')).toHaveClass(/side-collapsed/)
  await page.locator('#btnEmptyString').click()
  await expect(page.locator('#app')).not.toHaveClass(/side-collapsed/)
  await expect(page.locator('#sideTabDb')).toHaveAttribute('aria-selected', 'true')
  await expect(page.locator('#panelDb')).toBeVisible()
})

test('"Upload files" expands a collapsed sidebar and opens the Files section', async ({ page }) => {
  await page.goto('/')
  await page.locator('#sideToggle').click()
  await expect(page.locator('#app')).toHaveClass(/side-collapsed/)
  const chooser = page.waitForEvent('filechooser')
  await page.locator('#btnEmptyUpload').click()
  await (await chooser).setFiles([])
  await expect(page.locator('#app')).not.toHaveClass(/side-collapsed/)
  await expect(page.locator('#sideTabData')).toHaveAttribute('aria-selected', 'true')
  await expect(page.locator('#panelData')).toBeVisible()
})

test('OmniPath search is case-insensitive: a lowercase query is sent to the API in uppercase', async ({
  page,
}) => {
  await page.goto('/')
  await page.locator('#sideTabDb').click()
  await page.locator('#omnipathSection h3').click()
  await page.fill('#omnipathQuery', 'egfr')
  // keep this to the one interactions request the fix touches
  await page.locator('#omnipathGroups input[value="complexes"]').uncheck()
  await page.locator('#omnipathGroups input[value="intercell"]').uncheck()

  // e2e's webServer runs with --no-relays, so this goes straight to
  // omnipathdb.org rather than through the same-origin db-api/fetch proxy.
  let requestedUrl = ''
  await page.route('https://omnipathdb.org/**', async (route) => {
    requestedUrl = route.request().url()
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: JSON.stringify([
        {
          source: 'P00533',
          target: 'P01111',
          source_genesymbol: 'EGFR',
          target_genesymbol: 'HRAS',
          is_directed: 1,
          is_stimulation: 1,
          is_inhibition: 0,
          sources: ['SignaLink'],
          curation_effort: 3,
          type: 'post_translational',
        },
      ]),
    })
  })

  await page.locator('#btnOmnipathFetch').click()
  await expect.poll(() => requestedUrl).toContain('partners=EGFR')
  expect(requestedUrl).not.toContain('partners=egfr')
})

test('Upload Data panels report loading progress at the bottom of their own section', async ({
  page,
}) => {
  await page.goto('/')

  // Examples: opening one reports progress in #examplesStatus, not #normaStatus
  await page.locator('.section[data-tint="examples"] h3').click()
  await page.locator('#btnSample').click()
  await expect(page.locator('#examplesStatus .note.ok')).toContainText(/opened/i)

  // Files: reading an uploaded file reports progress in #normaStatus (already
  // covered structurally elsewhere; here we just confirm it still ends well)
  await page.locator('#filesSection h3').click()
  await page.setInputFiles('#normaFileInput', {
    name: 'network2.txt',
    mimeType: 'text/plain',
    buffer: Buffer.from('Source\tTarget\nX\tY\n'),
  })
  await expect(page.locator('#normaStatus .note.ok').first()).toContainText(/added/i)

  // Open saved work: a settings file reports progress in #savedWorkStatus
  await page.locator('.section[data-tint="saved"] h3').click()
  await page.setInputFiles('#configFileInput', {
    name: 'norma-settings.json',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify({ themeSelect: 'dark' })),
  })
  await expect(page.locator('#savedWorkStatus .note.ok')).toContainText(/applied settings/i)
})
