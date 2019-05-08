"use strict"

exports.createReader = function(bytes) {
  return protobuf.Reader.create(bytes)
}

exports.len = function(reader) {
  return reader.len
}

exports.pos = function(reader) {
  return reader.pos
}

exports.uint32 = function(reader) {
  return function() {
    return reader.uint32()
  }
}

exports.string = function(reader) {
  return function() {
    return reader.string()
  }
}

exports.skipType = function(reader) {
  return function(tag) {
    return function() {
      return reader.skipType(tag)
    }
  }
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

