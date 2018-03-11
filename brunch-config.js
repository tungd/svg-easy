exports.files = {
  javascripts: {
    joinTo: 'app.js'
  },

  stylesheets: {
    joinTo: 'app.css'
  }
}

exports.modules = {
  autoRequire: {
    'app.js': ['main']
  }
}
