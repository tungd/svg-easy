let Ractive = require('ractive/runtime')

Ractive.DEBUG = /unminified/.test(function(){/*unminified*/});
Ractive.DEBUG_PROMISES = true


const SVGBuilder = require('builder.html')
const builder = new SVGBuilder({ el: '#main' })

window.__builder = builder

fetch(`/icon-sets`)
  .then(resp => resp.json())
  .then(resp => builder.set({ sets: resp }))
