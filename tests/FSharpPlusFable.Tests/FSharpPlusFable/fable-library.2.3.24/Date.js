"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.dateOffsetToString = dateOffsetToString;
exports.dateToHalfUTCString = dateToHalfUTCString;
exports.toString = toString;
exports.default = DateTime;
exports.fromTicks = fromTicks;
exports.fromDateTimeOffset = fromDateTimeOffset;
exports.getTicks = getTicks;
exports.minValue = minValue;
exports.maxValue = maxValue;
exports.parseRaw = parseRaw;
exports.parse = parse;
exports.tryParse = tryParse;
exports.create = create;
exports.now = now;
exports.utcNow = utcNow;
exports.today = today;
exports.isLeapYear = isLeapYear;
exports.daysInMonth = daysInMonth;
exports.toUniversalTime = toUniversalTime;
exports.toLocalTime = toLocalTime;
exports.specifyKind = specifyKind;
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
exports.toLongDateString = toLongDateString;
exports.toShortDateString = toShortDateString;
exports.toLongTimeString = toLongTimeString;
exports.toShortTimeString = toShortTimeString;
exports.equals = equals;
exports.op_Addition = op_Addition;
exports.op_Subtraction = op_Subtraction;
exports.isDaylightSavingTime = isDaylightSavingTime;
exports.compareTo = exports.compare = exports.offsetRegex = void 0;

var _Long = require("./Long");

var _Util = require("./Util");

/**
 * DateTimeOffset functions.
 *
 * Note: Date instances are always DateObjects in local
 * timezone (because JS dates are all kinds of messed up).
 * A local date returns UTC epoc when `.getTime()` is called.
 *
 * Basically; invariant: date.getTime() always return UTC time.
 */
const offsetRegex = /(?:Z|[+-](\d+):?([0-5]?\d)?)\s*$/;
exports.offsetRegex = offsetRegex;

function dateOffsetToString(offset) {
  const isMinus = offset < 0;
  offset = Math.abs(offset);
  const hours = ~~(offset / 3600000);
  const minutes = offset % 3600000 / 60000;
  return (isMinus ? "-" : "+") + (0, _Util.padWithZeros)(hours, 2) + ":" + (0, _Util.padWithZeros)(minutes, 2);
}

function dateToHalfUTCString(date, half) {
  const str = date.toISOString();
  return half === "first" ? str.substring(0, str.indexOf("T")) : str.substring(str.indexOf("T") + 1, str.length - 1);
}

function dateToISOString(d, utc) {
  if (utc) {
    return d.toISOString();
  } else {
    // JS Date is always local
    const printOffset = d.kind == null ? true : d.kind === 2
    /* Local */
    ;
    return (0, _Util.padWithZeros)(d.getFullYear(), 4) + "-" + (0, _Util.padWithZeros)(d.getMonth() + 1, 2) + "-" + (0, _Util.padWithZeros)(d.getDate(), 2) + "T" + (0, _Util.padWithZeros)(d.getHours(), 2) + ":" + (0, _Util.padWithZeros)(d.getMinutes(), 2) + ":" + (0, _Util.padWithZeros)(d.getSeconds(), 2) + "." + (0, _Util.padWithZeros)(d.getMilliseconds(), 3) + (printOffset ? dateOffsetToString(d.getTimezoneOffset() * -60000) : "");
  }
}

function dateToISOStringWithOffset(dateWithOffset, offset) {
  const str = dateWithOffset.toISOString();
  return str.substring(0, str.length - 1) + dateOffsetToString(offset);
}

function dateToStringWithCustomFormat(date, format, utc) {
  return format.replace(/(\w)\1*/g, match => {
    let rep = match;

    switch (match.substring(0, 1)) {
      case "y":
        const y = utc ? date.getUTCFullYear() : date.getFullYear();
        rep = match.length < 4 ? y % 100 : y;
        break;

      case "M":
        rep = (utc ? date.getUTCMonth() : date.getMonth()) + 1;
        break;

      case "d":
        rep = utc ? date.getUTCDate() : date.getDate();
        break;

      case "H":
        rep = utc ? date.getUTCHours() : date.getHours();
        break;

      case "h":
        const h = utc ? date.getUTCHours() : date.getHours();
        rep = h > 12 ? h % 12 : h;
        break;

      case "m":
        rep = utc ? date.getUTCMinutes() : date.getMinutes();
        break;

      case "s":
        rep = utc ? date.getUTCSeconds() : date.getSeconds();
        break;

      case "f":
        rep = utc ? date.getUTCMilliseconds() : date.getMilliseconds();
        break;
    }

    if (rep !== match && rep < 10 && match.length > 1) {
      rep = "0" + rep;
    }

    return rep;
  });
}

function dateToStringWithOffset(date, format) {
  const d = new Date(date.getTime() + date.offset);

  if (typeof format !== "string") {
    return d.toISOString().replace(/\.\d+/, "").replace(/[A-Z]|\.\d+/g, " ") + dateOffsetToString(date.offset);
  } else if (format.length === 1) {
    switch (format) {
      case "D":
      case "d":
        return dateToHalfUTCString(d, "first");

      case "T":
      case "t":
        return dateToHalfUTCString(d, "second");

      case "O":
      case "o":
        return dateToISOStringWithOffset(d, date.offset);

      default:
        throw new Error("Unrecognized Date print format");
    }
  } else {
    return dateToStringWithCustomFormat(d, format, true);
  }
}

function dateToStringWithKind(date, format) {
  const utc = date.kind === 1
  /* UTC */
  ;

  if (typeof format !== "string") {
    return utc ? date.toUTCString() : date.toLocaleString();
  } else if (format.length === 1) {
    switch (format) {
      case "D":
      case "d":
        return utc ? dateToHalfUTCString(date, "first") : date.toLocaleDateString();

      case "T":
      case "t":
        return utc ? dateToHalfUTCString(date, "second") : date.toLocaleTimeString();

      case "O":
      case "o":
        return dateToISOString(date, utc);

      default:
        throw new Error("Unrecognized Date print format");
    }
  } else {
    return dateToStringWithCustomFormat(date, format, utc);
  }
}

function toString(date, format) {
  return date.offset != null ? dateToStringWithOffset(date, format) : dateToStringWithKind(date, format);
}

function DateTime(value, kind) {
  const d = new Date(value);
  d.kind = (kind == null ? 0
  /* Unspecified */
  : kind) | 0;
  return d;
}

function fromTicks(ticks, kind) {
  ticks = (0, _Long.fromValue)(ticks);
  kind = kind != null ? kind : 0
  /* Unspecified */
  ;
  let date = DateTime((0, _Long.ticksToUnixEpochMilliseconds)(ticks), kind); // Ticks are local to offset (in this case, either UTC or Local/Unknown).
  // If kind is anything but UTC, that means that the tick number was not
  // in utc, thus getTime() cannot return UTC, and needs to be shifted.

  if (kind !== 1
  /* UTC */
  ) {
      date = DateTime(date.getTime() - (0, _Util.dateOffset)(date), kind);
    }

  return date;
}

function fromDateTimeOffset(date, kind) {
  switch (kind) {
    case 1
    /* UTC */
    :
      return DateTime(date.getTime(), 1
      /* UTC */
      );

    case 2
    /* Local */
    :
      return DateTime(date.getTime(), 2
      /* Local */
      );

    default:
      const d = DateTime(date.getTime() + date.offset, kind);
      return DateTime(d.getTime() - (0, _Util.dateOffset)(d), kind);
  }
}

function getTicks(date) {
  return (0, _Long.unixEpochMillisecondsToTicks)(date.getTime(), (0, _Util.dateOffset)(date));
}

function minValue() {
  // This is "0001-01-01T00:00:00.000Z", actual JS min value is -8640000000000000
  return DateTime(-62135596800000, 0
  /* Unspecified */
  );
}

function maxValue() {
  // This is "9999-12-31T23:59:59.999Z", actual JS max value is 8640000000000000
  return DateTime(253402300799999, 0
  /* Unspecified */
  );
}

function parseRaw(str) {
  let date = new Date(str);

  if (isNaN(date.getTime())) {
    // Try to check strings JS Date cannot parse (see #1045, #1422)
    // tslint:disable-next-line:max-line-length
    const m = /^\s*(\d+[^\w\s:]\d+[^\w\s:]\d+)?\s*(\d+:\d+(?::\d+(?:\.\d+)?)?)?\s*([AaPp][Mm])?\s*([+-]\d+(?::\d+)?)?\s*$/.exec(str);

    if (m != null) {
      let baseDate = null;
      let timeInSeconds = 0;

      if (m[2] != null) {
        const timeParts = m[2].split(":");
        timeInSeconds = parseInt(timeParts[0], 10) * 3600 + parseInt(timeParts[1] || "0", 10) * 60 + parseFloat(timeParts[2] || "0");

        if (m[3] != null && m[3].toUpperCase() === "PM") {
          timeInSeconds += 720;
        }
      }

      if (m[4] != null) {
        // There's an offset, parse as UTC
        if (m[1] != null) {
          baseDate = new Date(m[1] + " UTC");
        } else {
          const d = new Date();
          baseDate = new Date(d.getUTCFullYear() + "/" + (d.getUTCMonth() + 1) + "/" + d.getUTCDate());
        }

        const offsetParts = m[4].substr(1).split(":");
        let offsetInMinutes = parseInt(offsetParts[0], 10) * 60 + parseInt(offsetParts[1] || "0", 10);

        if (m[4][0] === "+") {
          offsetInMinutes *= -1;
        }

        timeInSeconds += offsetInMinutes * 60;
      } else {
        if (m[1] != null) {
          baseDate = new Date(m[1]);
        } else {
          const d = new Date();
          baseDate = new Date(d.getFullYear() + "/" + (d.getMonth() + 1) + "/" + d.getDate());
        }
      }

      date = new Date(baseDate.getTime() + timeInSeconds * 1000); // correct for daylight savings time

      date = new Date(date.getTime() + (date.getTimezoneOffset() - baseDate.getTimezoneOffset()) * 60000);
    } else {
      throw new Error("The string is not a valid Date.");
    }
  }

  return date;
}

function parse(str, detectUTC = false) {
  const date = parseRaw(str);
  const offset = offsetRegex.exec(str); // .NET always parses DateTime as Local if there's offset info (even "Z")
  // Newtonsoft.Json uses UTC if the offset is "Z"

  const kind = offset != null ? detectUTC && offset[0] === "Z" ? 1
  /* UTC */
  : 2
  /* Local */
  : 0
  /* Unspecified */
  ;
  return DateTime(date.getTime(), kind);
}

function tryParse(v) {
  try {
    // if value is null or whitespace, parsing fails
    if (v === null || v.trim() === "") {
      return [false, minValue()];
    }

    return [true, parse(v)];
  } catch (_err) {
    return [false, minValue()];
  }
}

function create(year, month, day, h = 0, m = 0, s = 0, ms = 0, kind) {
  const dateValue = kind === 1
  /* UTC */
  ? Date.UTC(year, month - 1, day, h, m, s, ms) : new Date(year, month - 1, day, h, m, s, ms).getTime();

  if (isNaN(dateValue)) {
    throw new Error("The parameters describe an unrepresentable Date.");
  }

  const date = DateTime(dateValue, kind);

  if (year <= 99) {
    date.setFullYear(year, month - 1, day);
  }

  return date;
}

function now() {
  return DateTime(Date.now(), 2
  /* Local */
  );
}

function utcNow() {
  return DateTime(Date.now(), 1
  /* UTC */
  );
}

function today() {
  return date(now());
}

function isLeapYear(year) {
  return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
}

function daysInMonth(year, month) {
  return month === 2 ? isLeapYear(year) ? 29 : 28 : month >= 8 ? month % 2 === 0 ? 31 : 30 : month % 2 === 0 ? 30 : 31;
}

function toUniversalTime(date) {
  return date.kind === 1
  /* UTC */
  ? date : DateTime(date.getTime(), 1
  /* UTC */
  );
}

function toLocalTime(date) {
  return date.kind === 2
  /* Local */
  ? date : DateTime(date.getTime(), 2
  /* Local */
  );
}

function specifyKind(d, kind) {
  return create(year(d), month(d), day(d), hour(d), minute(d), second(d), millisecond(d), kind);
}

function timeOfDay(d) {
  return hour(d) * 3600000 + minute(d) * 60000 + second(d) * 1000 + millisecond(d);
}

function date(d) {
  return create(year(d), month(d), day(d), 0, 0, 0, 0, d.kind);
}

function day(d) {
  return d.kind === 1
  /* UTC */
  ? d.getUTCDate() : d.getDate();
}

function hour(d) {
  return d.kind === 1
  /* UTC */
  ? d.getUTCHours() : d.getHours();
}

function millisecond(d) {
  return d.kind === 1
  /* UTC */
  ? d.getUTCMilliseconds() : d.getMilliseconds();
}

function minute(d) {
  return d.kind === 1
  /* UTC */
  ? d.getUTCMinutes() : d.getMinutes();
}

function month(d) {
  return (d.kind === 1
  /* UTC */
  ? d.getUTCMonth() : d.getMonth()) + 1;
}

function second(d) {
  return d.kind === 1
  /* UTC */
  ? d.getUTCSeconds() : d.getSeconds();
}

function year(d) {
  return d.kind === 1
  /* UTC */
  ? d.getUTCFullYear() : d.getFullYear();
}

function dayOfWeek(d) {
  return d.kind === 1
  /* UTC */
  ? d.getUTCDay() : d.getDay();
}

function dayOfYear(d) {
  const _year = year(d);

  const _month = month(d);

  let _day = day(d);

  for (let i = 1; i < _month; i++) {
    _day += daysInMonth(_year, i);
  }

  return _day;
}

function add(d, ts) {
  return DateTime(d.getTime() + ts, d.kind);
}

function addDays(d, v) {
  return DateTime(d.getTime() + v * 86400000, d.kind);
}

function addHours(d, v) {
  return DateTime(d.getTime() + v * 3600000, d.kind);
}

function addMinutes(d, v) {
  return DateTime(d.getTime() + v * 60000, d.kind);
}

function addSeconds(d, v) {
  return DateTime(d.getTime() + v * 1000, d.kind);
}

function addMilliseconds(d, v) {
  return DateTime(d.getTime() + v, d.kind);
}

function addYears(d, v) {
  const newMonth = month(d);
  const newYear = year(d) + v;

  const _daysInMonth = daysInMonth(newYear, newMonth);

  const newDay = Math.min(_daysInMonth, day(d));
  return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind);
}

function addMonths(d, v) {
  let newMonth = month(d) + v;
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

  const newYear = year(d) + yearOffset;

  const _daysInMonth = daysInMonth(newYear, newMonth);

  const newDay = Math.min(_daysInMonth, day(d));
  return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind);
}

function subtract(d, that) {
  return typeof that === "number" ? DateTime(d.getTime() - that, d.kind) : d.getTime() - that.getTime();
}

function toLongDateString(d) {
  return d.toDateString();
}

function toShortDateString(d) {
  return d.toLocaleDateString();
}

function toLongTimeString(d) {
  return d.toLocaleTimeString();
}

function toShortTimeString(d) {
  return d.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
}

function equals(d1, d2) {
  return d1.getTime() === d2.getTime();
}

const compare = _Util.compareDates;
exports.compare = compare;
const compareTo = _Util.compareDates;
exports.compareTo = compareTo;

function op_Addition(x, y) {
  return add(x, y);
}

function op_Subtraction(x, y) {
  return subtract(x, y);
}

function isDaylightSavingTime(x) {
  const jan = new Date(x.getFullYear(), 0, 1);
  const jul = new Date(x.getFullYear(), 6, 1);
  return isDST(jan.getTimezoneOffset(), jul.getTimezoneOffset(), x.getTimezoneOffset());
}

function isDST(janOffset, julOffset, tOffset) {
  return Math.min(janOffset, julOffset) === tOffset;
}