"use strict"

exports.splitFloat64 = function(value) {
  var sign = (value < 0) ? 1 : 0
  value = sign ? -value : value

  // Handle zeros.
  if (value === 0) {
    if ((1 / value) > 0) {
      // Positive zero.
      return { low: 0x00000000, high: 0x00000000 }
    } else {
      // Negative zero.
      return { low: 0x00000000, high: 0x80000000 }
    }
  }

  // Handle nans.
  if (isNaN(value)) {
    return { low: 0xFFFFFFFF, high: 0x7FFFFFFF }
  }

  // Handle infinities.
  if (value > jspb.BinaryConstants.FLOAT64_MAX) {
    return { low: 0, high: ((sign << 31) | (0x7FF00000)) >>> 0 }
  }

  // Handle denormals.
  if (value < jspb.BinaryConstants.FLOAT64_MIN) {
    // Number is a denormal.
    var mant = value / Math.pow(2, -1074)
    var mantHigh = (mant / jspb.BinaryConstants.TWO_TO_32)
    return { low: (mant >>> 0), high: ((sign << 31) | mantHigh) >>> 0 }
  }

  var exp = Math.floor(Math.log(value) / Math.LN2)
  if (exp == 1024) exp = 1023
  var mant = value * Math.pow(2, -exp)

  var mantHigh = (mant * jspb.BinaryConstants.TWO_TO_20) & 0xFFFFF
  var mantLow = (mant * jspb.BinaryConstants.TWO_TO_52) >>> 0

  return { low: mantLow, high: ((sign << 31) | ((exp + 1023) << 20) | mantHigh) >>> 0 }
}

