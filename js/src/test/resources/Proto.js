"use strict"

exports.arrayview_length = function(xs) {
  return xs.length
}

exports.arrayview_index_impl = function(xs) {
  return function(i) {
    return xs[i]
  }
}

exports.arrayview_slice = function(xs) {
  return function(start) {
    return function(end) {
      return xs.slice(start, end)
    }
  }
}

exports.uint8array_tostring = function(buffer) {
    var start = 0
    var end = buffer.length
    var len = end - start;
    if (len < 1)
        return "";
    var parts = null,
        chunk = [],
        i = 0, // char offset
        t;     // temporary
    while (start < end) {
        t = buffer[start++];
        if (t < 128)
            chunk[i++] = t;
        else if (t > 191 && t < 224)
            chunk[i++] = (t & 31) << 6 | buffer[start++] & 63;
        else if (t > 239 && t < 365) {
            t = ((t & 7) << 18 | (buffer[start++] & 63) << 12 | (buffer[start++] & 63) << 6 | buffer[start++] & 63) - 0x10000;
            chunk[i++] = 0xD800 + (t >> 10);
            chunk[i++] = 0xDC00 + (t & 1023);
        } else
            chunk[i++] = (t & 15) << 12 | (buffer[start++] & 63) << 6 | buffer[start++] & 63;
        if (i > 8191) {
            (parts || (parts = [])).push(String.fromCharCode.apply(String, chunk));
            i = 0;
        }
    }
    if (parts) {
        if (i)
            parts.push(String.fromCharCode.apply(String, chunk.slice(0, i)));
        return parts.join("");
    }
    return String.fromCharCode.apply(String, chunk.slice(0, i));
}

exports.createWriter = function() {
  return protobuf.Writer.create()
}

exports.write_uint32 = function(writer) {
  return function(v) {
    return function() {
      return writer.uint32(v)
    }
  }
}

exports.write_string = function(writer) {
  return function(v) {
    return function() {
      return writer.string(v)
    }
  }
}

exports.write_bytes = function(writer) {
  return function(v) {
    return function() {
      return writer.bytes(v)
    }
  }
}

exports.writer_fork = function(writer) {
  return function() {
    return writer.fork()
  }
}

exports.writer_ldelim = function(writer) {
  return function() {
    return writer.ldelim()
  }
}

exports.writer_finish = function(writer) {
  return writer.finish()
}

