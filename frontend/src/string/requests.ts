// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { STRING_CALLER, STRING_PRIOR } from '../view3d/export'

/* ---------- requests ---------- */
export var stringState = { busy: false, abort: null, lastCall: 0, versions: new Map() }

// How requests reach STRING: through server.py's relay (same origin, so no
// cross-site restrictions) or directly from the browser.
export const stringRoute = { mode: null, checked: false }

export async function resolveStringRoute() {
  const choice = document.getElementById('stringConnect').value
  if (choice !== 'auto') {
    stringRoute.mode = choice
    return choice
  }
  if (stringRoute.checked) return stringRoute.mode
  stringRoute.checked = true
  stringRoute.mode = 'direct'
  if (/^https?:$/.test(location.protocol)) {
    try {
      const ctl = new AbortController()
      const t = setTimeout(() => ctl.abort(), 2500)
      const r = await fetch('string-api/ping', { signal: ctl.signal, cache: 'no-store' })
      clearTimeout(t)
      const j = r.ok ? await r.json() : null
      if (j && j.norma_proxy) stringRoute.mode = 'proxy'
    } catch (e) {
      /* no relay: go direct */
    }
  }
  updateStringConnectState()
  return stringRoute.mode
}

export function updateStringConnectState() {
  const el = document.getElementById('stringConnectState')
  if (!el) return
  const choice = document.getElementById('stringConnect').value
  const mode = choice === 'auto' ? stringRoute.mode : choice
  el.textContent = !mode
    ? 'Decided at the first request.'
    : mode === 'proxy'
      ? 'Requests go through this server to STRING.'
      : 'Requests go from the browser straight to STRING.'
}

export function stringBase() {
  const raw = (document.getElementById('stringAddress').value || 'https://string-db.org')
    .trim()
    .replace(/\/+$/, '')
  return /^https?:\/\//i.test(raw) ? raw : 'https://' + raw
}

export async function stringCall(method, params) {
  // one second between calls, as STRING asks
  const wait = stringState.lastCall + 1050 - Date.now()
  if (wait > 0) await new Promise((r) => setTimeout(r, wait))
  stringState.lastCall = Date.now()
  // The body is sent as a plain string: some environments pass fetch
  // requests on with postMessage, which can't copy URLSearchParams objects.
  const body = new URLSearchParams({ ...params, caller_identity: STRING_CALLER }).toString()
  const route = await resolveStringRoute()
  const url =
    route === 'proxy'
      ? `string-api/json/${method}?upstream=${encodeURIComponent(stringBase())}`
      : `${stringBase()}/api/json/${method}`
  const controller = new AbortController()
  stringState.abort = controller
  const timer = setTimeout(() => controller.abort(), 90000)
  let response
  try {
    // a form-encoded POST with a plain-text body is a "simple" request: no CORS preflight
    response = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8' },
      body,
      signal: controller.signal,
    })
  } catch (err) {
    if (err.name === 'AbortError')
      throw new Error(
        stringState.cancelled
          ? 'Cancelled.'
          : 'STRING did not answer within 90 seconds. Try again, or ask for fewer interactors.'
      )
    throw new Error(
      route === 'proxy'
        ? `The server relay could not be reached (${err.message}). Check that server.py is still running, or choose Connect: Directly.`
        : `STRING could not be reached (${err.message}). Check the internet connection. The most reliable setup is to run NORMA with server.py, which passes requests on to STRING; see the README.`
    )
  } finally {
    clearTimeout(timer)
  }
  const text = await response.text()
  if (!response.ok) {
    if (route === 'proxy') {
      if (response.status === 502 && /could not be reached from this server/.test(text))
        throw new Error(`${text.slice(0, 200)} Check the server's internet connection.`)
      // a plain web server answers relay requests with its own (non-JSON) error page
      if (!/^\s*[\[{]/.test(text))
        throw new Error(
          'This server has no STRING relay. Start NORMA with server.py, or choose Connect: Directly.'
        )
    }
    if (response.status === 404)
      throw new Error('STRING found none of the given names for this organism.')
    let detail = ''
    try {
      const j = JSON.parse(text)
      detail = (Array.isArray(j) ? j[0] : j)?.message || ''
    } catch (e) {
      detail = text.slice(0, 160)
    }
    throw new Error(
      `STRING answered with an error (${response.status})${detail ? ': ' + detail : ''}.`
    )
  }
  try {
    return JSON.parse(text)
  } catch (e) {
    throw new Error('STRING sent an answer that is not JSON. The STRING address may be wrong.')
  }
}

export async function stringVersion() {
  const base = stringBase()
  if (stringState.versions.has(base)) return stringState.versions.get(base)
  try {
    const res = await stringCall('version', {})
    const v = Array.isArray(res) ? res[0] : res
    const info = { version: v && v.string_version, address: v && v.string_stable_address }
    stringState.versions.set(base, info)
    return info
  } catch (err) {
    return { version: null, address: null }
  }
}

/* ---------- scores ---------- */
// STRING's rule for combining channel scores: remove the prior from each
// channel, combine as independent evidence, then add the prior back.
export function combineStringScores(scores) {
  let miss = 1
  scores.forEach((s) => {
    if (!(s > 0)) return
    const noPrior = Math.max(0, (s - STRING_PRIOR) / (1 - STRING_PRIOR))
    miss *= 1 - noPrior
  })
  const combined = 1 - miss
  return combined + STRING_PRIOR * (1 - combined)
}

export function scoreValue(v) {
  const x = Number(v)
  if (!Number.isFinite(x)) return 0
  return x > 1 ? x / 1000 : x // accept 0-1 or 0-1000 scales
}
