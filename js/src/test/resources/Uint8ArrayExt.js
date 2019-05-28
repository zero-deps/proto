"use strict"

exports.length = function(xs) {
  return xs.length
}

exports.indexUnsafe = function(xs) {
  return function(i) {
    return xs[i]
  }
}

exports.slice = function(xs) {
  return function(start) {
    return function(end) {
      return xs.slice(start, end)
    }
  }
}

exports.fromArray = function(xs) {
  return new Uint8Array(xs)
}

exports.concatAll = function(xs) {
  var zs = new Uint8Array(xs.reduce(function(acc, x) {
    return acc + x.length
  }, 0))
  xs.reduce(function(acc, x) {
    zs.set(x, acc)
    return acc + x.length
  }, 0)
  return zs
}

