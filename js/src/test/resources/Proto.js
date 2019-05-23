"use strict"

exports.uint8array_length = function(xs) {
  return xs.length
}

exports.uint8array_index_impl = function(xs) {
  return function(i) {
    return xs[i]
  }
}

exports.uint8array_slice = function(xs) {
  return function(start) {
    return function(end) {
      return xs.slice(start, end)
    }
  }
}

exports.utf8_length = function(string) {
  var len = 0,
      c = 0
  for (var i = 0; i < string.length; ++i) {
    c = string.charCodeAt(i)
    if (c < 128)
      len += 1
    else if (c < 2048)
      len += 2
    else if ((c & 0xFC00) === 0xD800 && (string.charCodeAt(i + 1) & 0xFC00) === 0xDC00) {
      ++i
      len += 4
    } else
      len += 3
  }
  return len
}

exports.utf8_read = function(buffer) {
  var start = 0
  var end = buffer.length
  var len = end - start
  if (len < 1)
    return ""
  var parts = null,
    chunk = [],
    i = 0, // char offset
    t     // temporary
  while (start < end) {
    t = buffer[start++]
    if (t < 128)
      chunk[i++] = t
    else if (t > 191 && t < 224)
      chunk[i++] = (t & 31) << 6 | buffer[start++] & 63
    else if (t > 239 && t < 365) {
      t = ((t & 7) << 18 | (buffer[start++] & 63) << 12 | (buffer[start++] & 63) << 6 | buffer[start++] & 63) - 0x10000
      chunk[i++] = 0xD800 + (t >> 10)
      chunk[i++] = 0xDC00 + (t & 1023)
    } else
      chunk[i++] = (t & 15) << 12 | (buffer[start++] & 63) << 6 | buffer[start++] & 63
    if (i > 8191) {
      (parts || (parts = [])).push(String.fromCharCode.apply(String, chunk))
      i = 0
    }
  }
  if (parts) {
    if (i)
      parts.push(String.fromCharCode.apply(String, chunk.slice(0, i)))
    return parts.join("")
  }
  return String.fromCharCode.apply(String, chunk.slice(0, i))
}

exports.utf8_write = function(string) {
  return function(len) {
    var buffer = new Uint8Array(len)
    var offset = 0
    var c1, // character 1
        c2 // character 2
    for (var i = 0; i < string.length; ++i) {
      c1 = string.charCodeAt(i)
      if (c1 < 128) {
        buffer[offset++] = c1
      } else if (c1 < 2048) {
        buffer[offset++] = c1 >> 6       | 192
        buffer[offset++] = c1       & 63 | 128
      } else if ((c1 & 0xFC00) === 0xD800 && ((c2 = string.charCodeAt(i + 1)) & 0xFC00) === 0xDC00) {
        c1 = 0x10000 + ((c1 & 0x03FF) << 10) + (c2 & 0x03FF)
        ++i
        buffer[offset++] = c1 >> 18      | 240
        buffer[offset++] = c1 >> 12 & 63 | 128
        buffer[offset++] = c1 >> 6  & 63 | 128
        buffer[offset++] = c1       & 63 | 128
      } else {
        buffer[offset++] = c1 >> 12      | 224
        buffer[offset++] = c1 >> 6  & 63 | 128
        buffer[offset++] = c1       & 63 | 128
      }
    }
    return buffer
  }
}

exports.array_touint8array = function(xs) {
  return new Uint8Array(xs)
}

exports.uint8array_concatall = function(xs) {
  var zs = new Uint8Array(xs.reduce(function(acc, x) {
    return acc + x.length
  }, 0))
  xs.reduce(function(acc, x) {
    zs.set(x, acc)
    return acc + x.length
  }, 0)
  return zs
}

