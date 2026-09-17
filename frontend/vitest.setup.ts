// Unit tests import modules that read the page at load (canvases, controls),
// so give jsdom the real markup and a no-op 2D canvas context.
import fs from 'node:fs'
import path from 'node:path'

const html = fs.readFileSync(path.join(process.cwd(), 'norma.html'), 'utf8')
document.body.innerHTML = html.slice(html.indexOf('<body'), html.lastIndexOf('</body>'))

const noop = () => stub
const stub: unknown = new Proxy(
  {},
  { get: (_, key) => (key === 'measureText' ? () => ({ width: 0 }) : noop), set: () => true }
)
HTMLCanvasElement.prototype.getContext = (() =>
  stub) as typeof HTMLCanvasElement.prototype.getContext
