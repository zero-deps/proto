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
