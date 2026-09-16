// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
/* ---------- painters: the same drawing code writes to a canvas or to SVG ---------- */
export function canvasPainter(ctx) {
  return {
    kind: 'canvas',
    clear(W, H, bg) {
      ctx.clearRect(0, 0, W, H)
      if (bg) {
        ctx.fillStyle = bg
        ctx.fillRect(0, 0, W, H)
      }
    },
    pathD(d, st) {
      const path = new Path2D(d)
      ctx.save()
      if (st.blur) ctx.filter = `blur(${st.blur}px)`
      if (st.fill) {
        ctx.globalAlpha = st.fillAlpha ?? 1
        ctx.fillStyle = st.fill
        ctx.fill(path, st.evenodd ? 'evenodd' : 'nonzero')
      }
      if (st.stroke) {
        ctx.globalAlpha = st.strokeAlpha ?? 1
        ctx.strokeStyle = st.stroke
        ctx.lineWidth = st.width || 1
        ctx.stroke(path)
      }
      ctx.restore()
    },
    line(x1, y1, x2, y2, color, width, alpha, cx, cy0) {
      ctx.globalAlpha = alpha
      ctx.strokeStyle = color
      ctx.lineWidth = width
      ctx.beginPath()
      ctx.moveTo(x1, y1)
      if (cx !== undefined) ctx.quadraticCurveTo(cx, cy0, x2, y2)
      else ctx.lineTo(x2, y2)
      ctx.stroke()
    },
    circle(x, y, r, fill, alpha, stroke, width) {
      ctx.globalAlpha = alpha
      ctx.beginPath()
      ctx.arc(x, y, r, 0, Math.PI * 2)
      if (fill) {
        ctx.fillStyle = fill
        ctx.fill()
      }
      if (stroke && width > 0) {
        ctx.strokeStyle = stroke
        ctx.lineWidth = width
        ctx.stroke()
      }
    },
    wedge(x, y, r, a0, a1, fill, alpha) {
      ctx.globalAlpha = alpha
      ctx.fillStyle = fill
      ctx.beginPath()
      ctx.moveTo(x, y)
      ctx.arc(x, y, r, a0, a1)
      ctx.closePath()
      ctx.fill()
    },
    poly(pts, fill, alpha, stroke, width) {
      ctx.globalAlpha = alpha
      ctx.beginPath()
      pts.forEach((p, i) => (i ? ctx.lineTo(p[0], p[1]) : ctx.moveTo(p[0], p[1])))
      ctx.closePath()
      if (fill) {
        ctx.fillStyle = fill
        ctx.fill()
      }
      if (stroke && width > 0) {
        ctx.strokeStyle = stroke
        ctx.lineWidth = width
        ctx.stroke()
      }
    },
    shade(x, y, r, alpha) {
      const g = ctx.createRadialGradient(x - r * 0.35, y - r * 0.35, r * 0.1, x, y, r)
      g.addColorStop(0, 'rgba(255,255,255,0.55)')
      g.addColorStop(0.45, 'rgba(255,255,255,0.05)')
      g.addColorStop(1, 'rgba(0,0,0,0.35)')
      ctx.globalAlpha = alpha
      ctx.fillStyle = g
      ctx.beginPath()
      ctx.arc(x, y, r, 0, Math.PI * 2)
      ctx.fill()
    },
    text(str, x, y, o) {
      const font = `${Math.round(o.size * 2) / 2}px Inter, Helvetica, Arial, sans-serif`
      if (!o.rotate && !o.bg) {
        // common case: no state to save
        ctx.globalAlpha = o.alpha
        if (ctx.font !== font) ctx.font = font
        ctx.textAlign = o.align
        ctx.textBaseline = o.baseline
        if (o.halo && o.haloWidth > 0) {
          ctx.lineWidth = o.haloWidth * 2
          ctx.strokeStyle = o.halo
          ctx.lineJoin = 'round'
          ctx.strokeText(str, x, y)
        }
        ctx.fillStyle = o.color
        ctx.fillText(str, x, y)
        return
      }
      ctx.save()
      ctx.globalAlpha = o.alpha
      ctx.font = font
      ctx.textAlign = o.align
      ctx.textBaseline = o.baseline
      ctx.translate(x, y)
      if (o.rotate) ctx.rotate(o.rotate)
      if (o.bg) {
        const w = ctx.measureText(str).width
        const h = o.size * 1.2
        const left = o.align === 'center' ? -w / 2 : o.align === 'right' ? -w : 0
        ctx.fillStyle = o.bg
        ctx.globalAlpha = o.alpha * 0.85
        ctx.fillRect(left - 2, -h / 2, w + 4, h)
        ctx.globalAlpha = o.alpha
      }
      if (o.halo && o.haloWidth > 0) {
        ctx.lineWidth = o.haloWidth * 2
        ctx.strokeStyle = o.halo
        ctx.lineJoin = 'round'
        ctx.strokeText(str, 0, 0)
      }
      ctx.fillStyle = o.color
      ctx.fillText(str, 0, 0)
      ctx.restore()
    },
    done() {
      ctx.globalAlpha = 1
    },
  }
}

export function svgPainter(W, H) {
  const out = []
  const f = (v) => (Math.round(v * 100) / 100).toString()
  const esc = (v) =>
    String(v).replace(
      /[&<>"]/g,
      (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]
    )
  const op = (a) => (a < 1 ? ` opacity="${f(a)}"` : '')
  let blurId = 0
  const defs = [
    '<radialGradient id="shade" cx="0.35" cy="0.35" r="0.65"><stop offset="0" stop-color="#fff" stop-opacity="0.55"/><stop offset="0.45" stop-color="#fff" stop-opacity="0.05"/><stop offset="1" stop-color="#000" stop-opacity="0.35"/></radialGradient>',
  ]
  return {
    kind: 'svg',
    clear(w, h, bg) {
      if (bg) out.push(`<rect width="${W}" height="${H}" fill="${bg}"/>`)
    },
    pathD(d, st) {
      let filter = ''
      if (st.blur) {
        const id = 'b' + ++blurId
        defs.push(
          `<filter id="${id}" x="-30%" y="-30%" width="160%" height="160%"><feGaussianBlur stdDeviation="${f(st.blur / 2)}"/></filter>`
        )
        filter = ` filter="url(#${id})"`
      }
      out.push(
        `<path d="${d}"${st.evenodd ? ' fill-rule="evenodd"' : ''} fill="${st.fill || 'none'}"${st.fill ? ` fill-opacity="${f(st.fillAlpha ?? 1)}"` : ''}${st.stroke ? ` stroke="${st.stroke}" stroke-opacity="${f(st.strokeAlpha ?? 1)}" stroke-width="${f(st.width || 1)}"` : ''}${filter}/>`
      )
    },
    line(x1, y1, x2, y2, color, width, alpha, cx, cy0) {
      const d =
        cx !== undefined
          ? `M${f(x1)},${f(y1)}Q${f(cx)},${f(cy0)} ${f(x2)},${f(y2)}`
          : `M${f(x1)},${f(y1)}L${f(x2)},${f(y2)}`
      out.push(
        `<path d="${d}" fill="none" stroke="${color}" stroke-width="${f(width)}"${op(alpha)}/>`
      )
    },
    circle(x, y, r, fill, alpha, stroke, width) {
      out.push(
        `<circle cx="${f(x)}" cy="${f(y)}" r="${f(r)}" fill="${fill || 'none'}"${stroke && width > 0 ? ` stroke="${stroke}" stroke-width="${f(width)}"` : ''}${op(alpha)}/>`
      )
    },
    wedge(x, y, r, a0, a1, fill, alpha) {
      const large = a1 - a0 > Math.PI ? 1 : 0
      out.push(
        `<path d="M${f(x)},${f(y)}L${f(x + r * Math.cos(a0))},${f(y + r * Math.sin(a0))}A${f(r)},${f(r)} 0 ${large} 1 ${f(x + r * Math.cos(a1))},${f(y + r * Math.sin(a1))}Z" fill="${fill}"${op(alpha)}/>`
      )
    },
    poly(pts, fill, alpha, stroke, width) {
      out.push(
        `<polygon points="${pts.map((p) => f(p[0]) + ',' + f(p[1])).join(' ')}" fill="${fill || 'none'}"${stroke && width > 0 ? ` stroke="${stroke}" stroke-width="${f(width)}"` : ''}${op(alpha)}/>`
      )
    },
    shade(x, y, r, alpha) {
      out.push(`<circle cx="${f(x)}" cy="${f(y)}" r="${f(r)}" fill="url(#shade)"${op(alpha)}/>`)
    },
    text(str, x, y, o) {
      const anchor = o.align === 'center' ? 'middle' : o.align === 'right' ? 'end' : 'start'
      const base =
        o.baseline === 'middle'
          ? 'central'
          : o.baseline === 'top'
            ? 'text-before-edge'
            : 'text-after-edge'
      const rot = o.rotate
        ? ` transform="rotate(${f((o.rotate * 180) / Math.PI)} ${f(x)} ${f(y)})"`
        : ''
      const halo =
        o.halo && o.haloWidth > 0
          ? ` stroke="${o.halo}" stroke-width="${f(o.haloWidth * 2)}" stroke-linejoin="round" paint-order="stroke"`
          : ''
      const bgHalo = o.bg
        ? ` stroke="${o.bg}" stroke-width="${f(o.size * 0.5)}" stroke-linejoin="round" paint-order="stroke"`
        : ''
      out.push(
        `<text x="${f(x)}" y="${f(y)}" font-size="${f(o.size)}" fill="${o.color}" text-anchor="${anchor}" dominant-baseline="${base}"${halo || bgHalo}${op(o.alpha)}${rot}>${esc(str)}</text>`
      )
    },
    done() {},
    toString(title) {
      return `<?xml version="1.0" encoding="UTF-8"?>\n<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" font-family="Inter, Helvetica, Arial, sans-serif">\n<title>${esc(title || 'NORMA 3D network')}</title>\n<defs>${defs.join('')}</defs>\n${out.join('\n')}\n</svg>`
    },
  }
}
