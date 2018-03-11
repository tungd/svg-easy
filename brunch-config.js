exports.files = {
  javascripts: {
    entryPoints: {
      'app/main.js': 'app.js'
    }
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

exports.plugins = {
  babel: {
    presets: [['env', {
      targets: {
        browsers: ['last 2 versions']
      }
    }]]
  }
}
