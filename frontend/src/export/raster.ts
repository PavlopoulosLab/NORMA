// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { cy } from '../cy'
import { drawHullsOnCanvas, hullShapes } from './shading'
import { exportFrame, exportPixelSize } from './draw'

/* ---------- raster ---------- */
export function renderExportCanvas(opts) {
  const frame = exportFrame(opts.area)
  if (!frame) throw new Error('Nothing is shown to export.')
  const { k, W, H } = exportPixelSize(frame, opts.scale)
  const canvas = document.createElement('canvas')
  canvas.width = W
  canvas.height = H
  const ctx = canvas.getContext('2d')
  if (opts.bg) {
    ctx.fillStyle = opts.bg
    ctx.fillRect(0, 0, W, H)
  }
  const toOut = (p) => ({ x: (p.x - frame.x1) * k, y: (p.y - frame.y1) * k })
  if (opts.hulls) drawHullsOnCanvas(ctx, hullShapes(toOut, k), k)
  const R = cy.renderer()
  if (R && typeof R.drawElements === 'function' && typeof R.getCachedZSortedEles === 'function') {
    ctx.save()
    ctx.setTransform(k, 0, 0, k, -frame.x1 * k, -frame.y1 * k)
    R.drawElements(ctx, R.getCachedZSortedEles())
    ctx.restore()
  } else {
    throw new Error('This browser build of Cytoscape cannot draw images.')
  }
  return canvas
}

// Writes the resolution into PNG (pHYs chunk) and JPEG (JFIF density)
// files, so layout and print programs open them at the right size.
const CRC_TABLE = (() => {
  const t = new Uint32Array(256)
  for (let n = 0; n < 256; n++) {
    let c = n
    for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1
    t[n] = c >>> 0
  }
  return t
})()

function crc32(bytes) {
  let c = 0xffffffff
  for (let i = 0; i < bytes.length; i++) c = CRC_TABLE[(c ^ bytes[i]) & 0xff] ^ (c >>> 8)
  return (c ^ 0xffffffff) >>> 0
}

export async function withDpi(blob, dpi) {
  if (!blob || !(dpi > 0)) return blob
  const buf = new Uint8Array(await blob.arrayBuffer())
  if (blob.type === 'image/png' && buf[12] === 0x49 && buf[13] === 0x48) {
    // IHDR first
    const ppm = Math.round(dpi / 0.0254)
    const chunk = new Uint8Array(21)
    const dv = new DataView(chunk.buffer)
    dv.setUint32(0, 9)
    chunk.set([0x70, 0x48, 0x59, 0x73], 4) // "pHYs"
    dv.setUint32(8, ppm)
    dv.setUint32(12, ppm)
    chunk[16] = 1 // per metre
    dv.setUint32(17, crc32(chunk.subarray(4, 17)))
    const ihdrEnd = 8 + 25
    const out = new Uint8Array(buf.length + chunk.length)
    out.set(buf.subarray(0, ihdrEnd), 0)
    out.set(chunk, ihdrEnd)
    out.set(buf.subarray(ihdrEnd), ihdrEnd + chunk.length)
    return new Blob([out], { type: 'image/png' })
  }
  if (
    blob.type === 'image/jpeg' &&
    buf[2] === 0xff &&
    buf[3] === 0xe0 &&
    buf[6] === 0x4a &&
    buf[7] === 0x46
  ) {
    // JFIF APP0
    const d = Math.min(65535, Math.round(dpi))
    buf[13] = 1 // dots per inch
    buf[14] = d >> 8
    buf[15] = d & 255
    buf[16] = d >> 8
    buf[17] = d & 255
    return new Blob([buf], { type: 'image/jpeg' })
  }
  return blob
}

export function canvasToBlob(canvas, type, quality) {
  return new Promise((resolve, reject) => {
    canvas.toBlob(
      (b) =>
        b
          ? resolve(b)
          : reject(
              new Error('The image is too large for this browser. Choose a smaller resolution.')
            ),
      type,
      quality
    )
  })
}

/* ---------- PDF (single page, embedded JPEG) ---------- */
export async function pdfFromCanvas(canvas, pageWidthPt, pageHeightPt) {
  const jpeg = new Uint8Array(await (await canvasToBlob(canvas, 'image/jpeg', 0.95)).arrayBuffer())
  const enc = new TextEncoder()
  const parts = []
  const offsets = []
  let length = 0
  const push = (chunk) => {
    const bytes = typeof chunk === 'string' ? enc.encode(chunk) : chunk
    parts.push(bytes)
    length += bytes.length
  }
  const w = pageWidthPt.toFixed(2),
    h = pageHeightPt.toFixed(2)
  const content = `q ${w} 0 0 ${h} 0 0 cm /Im0 Do Q`
  push('%PDF-1.4\n%\u00e2\u00e3\u00cf\u00d3\n')
  const obj = (n, body) => {
    offsets[n] = length
    push(`${n} 0 obj\n${body}\nendobj\n`)
  }
  obj(1, '<< /Type /Catalog /Pages 2 0 R >>')
  obj(2, '<< /Type /Pages /Kids [3 0 R] /Count 1 >>')
  obj(
    3,
    `<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ${w} ${h}] /Resources << /XObject << /Im0 4 0 R >> >> /Contents 5 0 R >>`
  )
  offsets[4] = length
  push(
    `4 0 obj\n<< /Type /XObject /Subtype /Image /Width ${canvas.width} /Height ${canvas.height} /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /DCTDecode /Length ${jpeg.length} >>\nstream\n`
  )
  push(jpeg)
  push('\nendstream\nendobj\n')
  obj(5, `<< /Length ${content.length} >>\nstream\n${content}\nendstream`)
  const xref = length
  let table = `xref\n0 6\n0000000000 65535 f \n`
  for (let i = 1; i <= 5; i++) table += `${String(offsets[i]).padStart(10, '0')} 00000 n \n`
  push(table + `trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n${xref}\n%%EOF\n`)
  return new Blob(parts, { type: 'application/pdf' })
}
