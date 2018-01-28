const RemoteStorage = require('remotestoragejs')
const Widget = require('remotestorage-widget')

let rs = new RemoteStorage({logging: false})
rs.access.claim('ledger', 'rw')
rs.caching.enable('/ledger/')

let widget = new Widget(rs, {
  leaveOpen: false,
  autoCloseAfter: 4000
})
setTimeout(() => {
  widget.attach('rs-widget')
}, 1000)

window.client = rs.scope('/ledger/')

window.onGet = function (fn) {
  window.getHandler = fn
}

window.retrieveFile = function (path) {
  window.client.getFile(path)
    .then(res => {
      if (res.data) {
        window.getHandler(path, res.data)
      }
    })
}
