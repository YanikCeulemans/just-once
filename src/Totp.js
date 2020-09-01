const crypto = require('crypto');

exports.createHmac = function (secretHexString) {
  return function (counterHexString) {
    return function () {
      const hmac = crypto.createHmac('sha1', Buffer.from(secretHexString, 'hex'));
      return hmac.update(Buffer.from(counterHexString, 'hex')).digest();
    }
  }
}