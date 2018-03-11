const SVGBuilder = require('builder.html')
const builder = new SVGBuilder({ el: '#main' })

fetch(`/icon-sets`)
  .then(resp => resp.json())
  .then(resp => builder.set({ sets: resp }))
