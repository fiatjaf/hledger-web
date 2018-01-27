const RemoteStorage = require('remotestoragejs')
const Widget = require('remotestorage-widget')

let rs = new RemoteStorage({logging: true})
rs.access.claim('ledger', 'rw')
rs.caching.enable('/ledger/')

let widget = new Widget(rs, {
  leaveOpen: false,
  autoCloseAfter: 4000
})
widget.attach()

window.client = rs.scope('/ledger/')

window.setGetHandler = function (fn) {
  window.getHandler = fn
}
