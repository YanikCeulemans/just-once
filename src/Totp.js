const crypto = require('crypto');

exports.createHmac = function (keyHexString) {
  return function () {
    const t0 = 0;
    const x = 30;
    const t = Math.floor(((Date.now() / 1000) - t0) / x);
    const hmac = crypto.createHmac('sha1', Buffer.from(keyHexString, 'hex'));
    const hexCounter = t.toString(16).padStart(16, '0');
    return hmac.update(Buffer.from(hexCounter, 'hex')).digest();
  }
}