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

exports.uint8array_tostring = function(xs) {
  return protobuf.util.utf8.read(xs, 0, xs.length)
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

