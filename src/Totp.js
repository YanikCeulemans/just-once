const crypto = require('crypto');

exports.createHmac = function (secret) {
  return function (counter) {
    return function () {
      const hmac = crypto.createHmac('sha1', Buffer.from(secret, 'hex'));
      return hmac.update(Buffer.from(counter, 'hex')).digest();
    }
  }
}