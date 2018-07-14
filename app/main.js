import './styles.less'
const Ractive = require('ractive/runtime')
import Builder from './builder.html'

Ractive.DEBUG = /unminified/.test(function(){/*unminified*/});
Ractive.DEBUG_PROMISES = true

const builder = new Builder({ el: '#main' })

fetch(`/icon-sets`)
  .then(resp => resp.json())
  .then(resp => builder.set({ sets: resp }))
