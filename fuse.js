const path = require('path')
const fb = {
  FuseBox, CopyPlugin, QuantumPlugin,
  LESSPlugin, CSSPlugin,
} = require('fuse-box')
const Ractive = require('ractive')
const rcu = require('rcu')
const typescript = require('typescript')

rcu.init(Ractive)


class RactivePlugin {

  constructor(options) {
    this.test = /\.html$/
    this.dependencies = ['ractive/runtime']
  }

  init(context) {
    context.allowExtension('.html')
  }

  transform(file) {
    if (!file.analysis.ast) {
      file.loadContents()
    }

    const { imports, script, css, template } = rcu.parse(file.contents)

    file.contents = `
const Ractive = require('ractive/runtime')
${imports.map(this.importDeclaration).join('\n')}

const component = { exports: {} }

${script}

component.exports.components = {
  ${imports.map(this.componentDeclaration).join(', ')}
}

component.exports.template = ${JSON.stringify(template)}
component.exports.css = '${this.minify(css)}'

export default Ractive.extend(component.exports)
`
    file.contents = typescript.transpileModule(
      file.contents, file.context.tsConfig.getConfig()
    ).outputText

    // TODO: sourcemap
    // TODO: support plugin chain similar to Vue Plugin
  }

  importDeclaration({ name, href }) {
    return `import ${name} from '${href}'`
  }

  componentDeclaration({ name, href }) {
    return name
  }

  minify(css) {
    return css.replace(/\s{2,}/g, ' ').replace(/\t|\r|\n/g, '').trim()
  }
}

const fuse = FuseBox.init({
  homeDir: 'app',
  output: 'public/$name.js',
  target : 'browser@es5',
  useTypescriptCompiler: false,
  plugins: [
    new RactivePlugin(),
    [LESSPlugin({
      paths: [
        path.resolve(__dirname, 'node_modules')
      ]
    }), CSSPlugin({
      outFile: file => `public/${file}`,
      minify: true
    })],
    QuantumPlugin({
      bakeApiIntoBundle: 'app',
      treeshake : true,
      uglify : true
    })
  ]
})

fuse.bundle('app')
  .instructions('> main.js **/*.html')

fuse.run()
