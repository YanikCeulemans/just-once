const crypto = require('crypto');

exports.createHmac = function (key) {
  return function () {
    const t0 = 0;
    const x = 30;
    const t = Math.floor(((Date.now() / 1000) - t0) / x);
    key = '6155664d714f554b34586d78454f4a6e4b2f4e48';
    const hexKey = Buffer.from(key, 'ascii').toString('hex');
    const hmac = crypto.createHmac('sha1', Buffer.from(key, 'hex'));
    // const hmac = crypto.createHmac('sha1', Buffer.from(hexKey, 'hex'));
    const hexCounter = t.toString(16).padStart(16, '0');
    const result = hmac.update(Buffer.from(hexCounter, 'hex')).digest();
    return result;
  }
}