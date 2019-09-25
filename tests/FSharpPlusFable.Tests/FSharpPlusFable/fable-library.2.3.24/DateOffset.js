"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = DateTimeOffset;
exports.fromDate = fromDate;
exports.fromTicks = fromTicks;
exports.getUtcTicks = getUtcTicks;
exports.minValue = minValue;
exports.maxValue = maxValue;
exports.parse = parse;
exports.tryParse = tryParse;
exports.create = create;
exports.now = now;
exports.utcNow = utcNow;
exports.toUniversalTime = toUniversalTime;
exports.toLocalTime = toLocalTime;
exports.timeOfDay = timeOfDay;
exports.date = date;
exports.day = day;
exports.hour = hour;
exports.millisecond = millisecond;
exports.minute = minute;
exports.month = month;
exports.second = second;
exports.year = year;
exports.dayOfWeek = dayOfWeek;
exports.dayOfYear = dayOfYear;
exports.add = add;
exports.addDays = addDays;
exports.addHours = addHours;
exports.addMinutes = addMinutes;
exports.addSeconds = addSeconds;
exports.addMilliseconds = addMilliseconds;
exports.addYears = addYears;
exports.addMonths = addMonths;
exports.subtract = subtract;
exports.equals = equals;
exports.equalsExact = equalsExact;
exports.compare = compare;
exports.op_Addition = op_Addition;
exports.op_Subtraction = op_Subtraction;
exports.compareTo = void 0;

var _Date = require("./Date");

var _Long = require("./Long");

var _Util = require("./Util");

/**
 * DateTimeOffset functions.
 *
 * Note: DateOffset instances are always DateObjects in local
 * timezone (because JS dates are all kinds of messed up).
 * A local date returns UTC epoc when `.getTime()` is called.
 *
 * However, this means that in order to construct an UTC date
 * from a DateOffset with offset of +5 hours, you first need
 * to subtract those 5 hours, than add the "local" offset.
 * As said, all kinds of messed up.
 *
 * Basically; invariant: date.getTime() always return UTC time.
 */
function DateTimeOffset(value, offset) {
  const d = new Date(value);
  d.offset = offset != null ? offset : new Date().getTimezoneOffset() * -60000;
  return d;
}

function fromDate(date, offset) {
  const isUtc = date.kind === 1
  /* UTC */
  ;
  const offset2 = isUtc ? 0 : date.getTimezoneOffset() * -60000;

  if (offset != null && offset !== offset2) {
    throw new Error(isUtc ? "The UTC Offset for Utc DateTime instances must be 0." : "The UTC Offset of the local dateTime parameter does not match the offset argument.");
  }

  return DateTimeOffset(date.getTime(), offset2);
}

function fromTicks(ticks, offset) {
  ticks = (0, _Long.fromValue)(ticks);
  const epoc = (0, _Long.ticksToUnixEpochMilliseconds)(ticks) - offset;
  return DateTimeOffset(epoc, offset);
}

function getUtcTicks(date) {
  return (0, _Long.unixEpochMillisecondsToTicks)(date.getTime(), 0);
}

function minValue() {
  // This is "0001-01-01T00:00:00.000Z", actual JS min value is -8640000000000000
  return DateTimeOffset(-62135596800000, 0);
}

function maxValue() {
  // This is "9999-12-31T23:59:59.999Z", actual JS max value is 8640000000000000
  return DateTimeOffset(253402300799999, 0);
}

function parse(str) {
  const date = (0, _Date.parseRaw)(str);

  const offsetMatch = _Date.offsetRegex.exec(str);

  const offset = offsetMatch == null ? date.getTimezoneOffset() * -60000 : offsetMatch[0] === "Z" ? 0 : parseInt(offsetMatch[1], 10) * 3600000 + parseInt(offsetMatch[2], 10) * 60000;
  return DateTimeOffset(date.getTime(), offset);
}

function tryParse(v) {
  try {
    return [true, parse(v)];
  } catch (_err) {
    return [false, minValue()];
  }
}

function create(year, month, day, h, m, s, ms, offset) {
  if (offset == null) {
    offset = ms;
    ms = 0;
  }

  if (offset !== 0) {
    if (offset % 60000 !== 0) {
      throw new Error("Offset must be specified in whole minutes");
    }

    if (~~(offset / 3600000) > 14) {
      throw new Error("Offset must be within plus or minus 14 hour");
    }
  }

  let date;

  if (offset === 0) {
    date = new Date(Date.UTC(year, month - 1, day, h, m, s, ms));

    if (year <= 99) {
      date.setFullYear(year, month - 1, day);
    }
  } else {
    const str = (0, _Util.padWithZeros)(year, 4) + "-" + (0, _Util.padWithZeros)(month, 2) + "-" + (0, _Util.padWithZeros)(day, 2) + "T" + (0, _Util.padWithZeros)(h, 2) + ":" + (0, _Util.padWithZeros)(m, 2) + ":" + (0, _Util.padWithZeros)(s, 2) + "." + (0, _Util.padWithZeros)(ms, 3) + (0, _Date.dateOffsetToString)(offset);
    date = new Date(str);
  }

  const dateValue = date.getTime();

  if (isNaN(dateValue)) {
    throw new Error("The parameters describe an unrepresentable Date");
  }

  return DateTimeOffset(dateValue, offset);
}

function now() {
  const date = new Date();
  const offset = date.getTimezoneOffset() * -60000;
  return DateTimeOffset(date.getTime(), offset);
}

function utcNow() {
  const date = now();
  return DateTimeOffset(date.getTime(), 0);
}

function toUniversalTime(date) {
  return DateTimeOffset(date.getTime(), 0);
}

function toLocalTime(date) {
  return DateTimeOffset(date.getTime(), date.getTimezoneOffset() * -60000);
}

function timeOfDay(d) {
  const d2 = new Date(d.getTime() + d.offset);
  return d2.getUTCHours() * 3600000 + d2.getUTCMinutes() * 60000 + d2.getUTCSeconds() * 1000 + d2.getUTCMilliseconds();
}

function date(d) {
  const d2 = new Date(d.getTime() + d.offset);
  return (0, _Date.create)(d2.getUTCFullYear(), d2.getUTCMonth() + 1, d2.getUTCDate(), 0, 0, 0, 0);
}

function day(d) {
  return new Date(d.getTime() + d.offset).getUTCDate();
}

function hour(d) {
  return new Date(d.getTime() + d.offset).getUTCHours();
}

function millisecond(d) {
  return new Date(d.getTime() + d.offset).getUTCMilliseconds();
}

function minute(d) {
  return new Date(d.getTime() + d.offset).getUTCMinutes();
}

function month(d) {
  return new Date(d.getTime() + d.offset).getUTCMonth() + 1;
}

function second(d) {
  return new Date(d.getTime() + d.offset).getUTCSeconds();
}

function year(d) {
  return new Date(d.getTime() + d.offset).getUTCFullYear();
}

function dayOfWeek(d) {
  return new Date(d.getTime() + d.offset).getUTCDay();
}

function dayOfYear(d) {
  const d2 = new Date(d.getTime() + d.offset);

  const _year = d2.getUTCFullYear();

  const _month = d2.getUTCMonth() + 1;

  let _day = d2.getUTCDate();

  for (let i = 1; i < _month; i++) {
    _day += (0, _Date.daysInMonth)(_year, i);
  }

  return _day;
}

function add(d, ts) {
  return DateTimeOffset(d.getTime() + ts, d.offset);
}

function addDays(d, v) {
  return DateTimeOffset(d.getTime() + v * 86400000, d.offset);
}

function addHours(d, v) {
  return DateTimeOffset(d.getTime() + v * 3600000, d.offset);
}

function addMinutes(d, v) {
  return DateTimeOffset(d.getTime() + v * 60000, d.offset);
}

function addSeconds(d, v) {
  return DateTimeOffset(d.getTime() + v * 1000, d.offset);
}

function addMilliseconds(d, v) {
  return DateTimeOffset(d.getTime() + v, d.offset);
}

function addYears(d, v) {
  const newMonth = d.getUTCMonth() + 1;
  const newYear = d.getUTCFullYear() + v;

  const _daysInMonth = (0, _Date.daysInMonth)(newYear, newMonth);

  const newDay = Math.min(_daysInMonth, d.getUTCDate());
  return create(newYear, newMonth, newDay, d.getUTCHours(), d.getUTCMinutes(), d.getUTCSeconds(), d.getUTCMilliseconds(), d.offset);
}

function addMonths(d, v) {
  const d2 = new Date(d.getTime() + d.offset);
  let newMonth = d2.getUTCMonth() + 1 + v;
  let newMonth_ = 0;
  let yearOffset = 0;

  if (newMonth > 12) {
    newMonth_ = newMonth % 12;
    yearOffset = Math.floor(newMonth / 12);
    newMonth = newMonth_;
  } else if (newMonth < 1) {
    newMonth_ = 12 + newMonth % 12;
    yearOffset = Math.floor(newMonth / 12) + (newMonth_ === 12 ? -1 : 0);
    newMonth = newMonth_;
  }

  const newYear = d2.getUTCFullYear() + yearOffset;

  const _daysInMonth = (0, _Date.daysInMonth)(newYear, newMonth);

  const newDay = Math.min(_daysInMonth, d2.getUTCDate());
  return create(newYear, newMonth, newDay, d2.getUTCHours(), d2.getUTCMinutes(), d2.getUTCSeconds(), d2.getUTCMilliseconds(), d.offset);
}

function subtract(d, that) {
  return typeof that === "number" ? DateTimeOffset(d.getTime() - that, d.offset) : d.getTime() - that.getTime();
}

function equals(d1, d2) {
  return d1.getTime() === d2.getTime();
}

function equalsExact(d1, d2) {
  return d1.getTime() === d2.getTime() && d1.offset === d2.offset;
}

function compare(d1, d2) {
  return (0, _Util.compareDates)(d1, d2);
}

const compareTo = compare;
exports.compareTo = compareTo;

function op_Addition(x, y) {
  return add(x, y);
}

function op_Subtraction(x, y) {
  return subtract(x, y);
}