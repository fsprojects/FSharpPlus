module FSharpPlus

[<RequireQualifiedAccess>]
module DateTime =
    open System
    // Returns the DateTime resulting from adding the given TimeSpan to this DateTime.
    let add(value: TimeSpan, date: DateTime ) = date.Add(value)

    // Returns the DateTime resulting from adding a fractional number of
    // days to this DateTime. The result is computed by rounding the
    // fractional number of days given by value to the nearest
    // millisecond, and adding that interval to this DateTime. The
    // value argument is permitted to be negative.
    let addDays(value: double, date: DateTime ) = date.AddDays (value)

    // Returns the DateTime resulting from adding a fractional number of
    // hours to this DateTime. The result is computed by rounding the
    // fractional number of hours given by value to the nearest
    // millisecond, and adding that interval to this DateTime. The
    // value argument is permitted to be negative.
    let addHours(value: double, date: DateTime ) = date.AddHours (value)

    // Returns the DateTime resulting from the given number of
    // milliseconds to this DateTime. The result is computed by rounding
    // the number of milliseconds given by value to the nearest integer,
    // and adding that interval to this DateTime. The value
    // argument is permitted to be negative.
    let addMilliseconds(value: double, date: DateTime ) = date.AddMilliseconds (value)

    // Returns the DateTime resulting from adding a fractional number of
    // minutes to this DateTime. The result is computed by rounding the
    // fractional number of minutes given by value to the nearest
    // millisecond, and adding that interval to this DateTime. The
    // value argument is permitted to be negative.
    let addMinutes(value: double, date: DateTime ) = date.AddMinutes (value)

    // Returns the DateTime resulting from adding the given number of
    // months to this DateTime. The result is computed by incrementing
    // (or decrementing) the year and month parts of this DateTime by
    // months months, and, if required, adjusting the day part of the
    // resulting date downwards to the last day of the resulting month in the
    // resulting year. The time-of-day part of the result is the same as the
    // time-of-day part of this DateTime.
    //
    // In more precise terms, considering this DateTime to be of the
    // form y / m / d + t, where y is the
    // year, m is the month, d is the day, and t is the
    // time-of-day, the result is y1 / m1 / d1 + t,
    // where y1 and m1 are computed by adding months months
    // to y and m, and d1 is the largest value less than
    // or equal to d that denotes a valid day in month m1 of year
    // y1.
    let addMonths(months: int, date: DateTime ) = date.AddMonths (months)

    // Returns the DateTime resulting from adding a fractional number of
    // seconds to this DateTime. The result is computed by rounding the
    // fractional number of seconds given by value to the nearest
    // millisecond, and adding that interval to this DateTime. The
    // value argument is permitted to be negative.
    let addSeconds(value: double, date: DateTime ) = date.AddSeconds (value)

    // Returns the DateTime resulting from adding the given number of
    // 100-nanosecond ticks to this DateTime. The value argument
    // is permitted to be negative.
    let addTicks(value: int64, date: DateTime ) = date.AddTicks (value)

    // Returns the DateTime resulting from adding the given number of
    // years to this DateTime. The result is computed by incrementing
    // (or decrementing) the year part of this DateTime by value
    // years. If the month and day of this DateTime is 2/29, and if the
    // resulting year is not a leap year, the month and day of the resulting
    // DateTime becomes 2/28. Otherwise, the month, day, and time-of-day
    // parts of the result are the same as those of this DateTime.
    let addYears(value: int, date: DateTime ) = date.AddYears (value)

    // Returns the date part of this DateTime. The resulting value
    // corresponds to this DateTime with the time-of-day part set to
    // zero (midnight).
    let date(date: DateTime) = date.Date

    let kind(date: DateTime) = date.Kind
    let subtract(value: TimeSpan, date: DateTime ) = date.Subtract (value)
    let toLocalTime (date: DateTime) = date.ToLocalTime()
    let toUniversalTime (date: DateTime) = date.ToUniversalTime()

    // Returns the day-of-month part of this DateTime. The returned
    // value is an integer between 1 and 31.
    let day (date: DateTime) = date.Day

    // Returns the day-of-week part of this DateTime. The returned value
    // is an integer between 0 and 6, where 0 indicates Sunday, 1 indicates
    // Monday, 2 indicates Tuesday, 3 indicates Wednesday, 4 indicates
    // Thursday, 5 indicates Friday, and 6 indicates Saturday.
    let DayOfWeek (date: DateTime) = date.DayOfWeek

    // Returns the hour part of this DateTime. The returned value is an
    // integer between 0 and 23.
    let hour (date: DateTime) = date.Hour

    // Returns the second part of this DateTime. The returned value is
    // an integer between 0 and 59.
    let second (date: DateTime) = date.Second

    // Returns the minute part of this DateTime. The returned value is
    // an integer between 0 and 59.
    let minute (date: DateTime) = date.Minute

    // Returns the millisecond part of this DateTime. The returned value
    // is an integer between 0 and 999.
    let millisecond (date: DateTime) = date.Millisecond

    // Returns the month part of this DateTime. The returned value is an
    // integer between 1 and 12.
    let month (date: DateTime) = date.Month

    // Returns the year part of this DateTime. The returned value is an
    // integer between 1 and 9999.
    let year (date: DateTime) = date.Year

    // Returns the tick count for this DateTime. The returned value is
    // the number of 100-nanosecond intervals that have elapsed since 1/1/0001
    // 12:00am.
    let ticks (date: DateTime) = date.Ticks

    // Returns the day-of-year part of this DateTime. The returned value
    // is an integer between 1 and 366.
    let dayOfYear (date: DateTime) = date.DayOfYear

    // Returns the time-of-day part of this DateTime. The returned value
    // is a TimeSpan that indicates the time elapsed since midnight.
    let timeOfDay (date: DateTime) = date.TimeOfDay


[<RequireQualifiedAccess>]
module DateTimeOffset =
    open System
    // Returns the DateTimeOffset resulting from adding the given
    // TimeSpan to this DateTimeOffset.
    let add(value: TimeSpan, date: DateTimeOffset ) = date.Add(value)

    // Returns the DateTimeOffset resulting from adding a fractional number of
    // days to this DateTimeOffset. The result is computed by rounding the
    // fractional number of days given by value to the nearest
    // millisecond, and adding that interval to this DateTimeOffset. The
    // value argument is permitted to be negative.
    let addDays(value: double, date: DateTimeOffset ) = date.AddDays (value)

    // Returns the DateTimeOffset resulting from adding a fractional number of
    // hours to this DateTimeOffset. The result is computed by rounding the
    // fractional number of hours given by value to the nearest
    // millisecond, and adding that interval to this DateTimeOffset. The
    // value argument is permitted to be negative.
    let addHours(value: double, date: DateTimeOffset ) = date.AddHours (value)

    // Returns the DateTimeOffset resulting from the given number of
    // milliseconds to this DateTimeOffset. The result is computed by rounding
    // the number of milliseconds given by value to the nearest integer,
    // and adding that interval to this DateTimeOffset. The value
    // argument is permitted to be negative.
    let addMilliseconds(value: double, date: DateTimeOffset ) = date.AddMilliseconds (value)

    // Returns the DateTimeOffset resulting from adding a fractional number of
    // minutes to this DateTimeOffset. The result is computed by rounding the
    // fractional number of minutes given by value to the nearest
    // millisecond, and adding that interval to this DateTimeOffset. The
    // value argument is permitted to be negative.
    let addMinutes(value: double, date: DateTimeOffset ) = date.AddMinutes (value)

    let addMonths(months: int, date: DateTimeOffset ) = date.AddMonths (months)


    // Returns the DateTimeOffset resulting from adding a fractional number of
    // seconds to this DateTimeOffset. The result is computed by rounding the
    // fractional number of seconds given by value to the nearest
    // millisecond, and adding that interval to this DateTimeOffset. The
    // value argument is permitted to be negative.
    let addSeconds(value: double, date: DateTimeOffset ) = date.AddSeconds (value)

    // Returns the DateTimeOffset resulting from adding the given number of
    // 100-nanosecond ticks to this DateTimeOffset. The value argument
    // is permitted to be negative.
    let addTicks(value: int64, date: DateTimeOffset ) = date.AddTicks (value)

    // Returns the DateTimeOffset resulting from adding the given number of
    // years to this DateTimeOffset. The result is computed by incrementing
    // (or decrementing) the year part of this DateTimeOffset by value
    // years. If the month and day of this DateTimeOffset is 2/29, and if the
    // resulting year is not a leap year, the month and day of the resulting
    // DateTimeOffset becomes 2/28. Otherwise, the month, day, and time-of-day
    // parts of the result are the same as those of this DateTimeOffset.
    let addYears(value: int, date: DateTimeOffset ) = date.AddYears (value)

    // Returns the date part of this DateTimeOffset. The resulting value
    // corresponds to this DateTimeOffset with the time-of-day part set to
    // zero (midnight).
    let date(date: DateTimeOffset) = date.Date

    let offset(date: DateTimeOffset) = date.Offset
    let subtract(value: TimeSpan, date: DateTimeOffset ) = date.Subtract (value)
    let toLocalTime (date: DateTimeOffset) = date.ToLocalTime()
    let toUniversalTime (date: DateTimeOffset) = date.ToUniversalTime()

    // Returns the day-of-month part of this DateTimeOffset. The returned
    // value is an integer between 1 and 31.
    let day (date: DateTimeOffset) = date.Day

    // Returns the day-of-week part of this DateTimeOffset. The returned value
    // is an integer between 0 and 6, where 0 indicates Sunday, 1 indicates
    // Monday, 2 indicates Tuesday, 3 indicates Wednesday, 4 indicates
    // Thursday, 5 indicates Friday, and 6 indicates Saturday.
    let DayOfWeek (date: DateTimeOffset) = date.DayOfWeek

    // Returns the hour part of this DateTimeOffset. The returned value is an
    // integer between 0 and 23.
    let hour (date: DateTimeOffset) = date.Hour

    // Returns the second part of this DateTimeOffset. The returned value is
    // an integer between 0 and 59.
    let second (date: DateTimeOffset) = date.Second

    // Returns the minute part of this DateTimeOffset. The returned value is
    // an integer between 0 and 59.
    let minute (date: DateTimeOffset) = date.Minute

    // Returns the millisecond part of this DateTimeOffset. The returned value
    // is an integer between 0 and 999.
    let millisecond (date: DateTimeOffset) = date.Millisecond

    // Returns the month part of this DateTimeOffset. The returned value is an
    // integer between 1 and 12.
    let month (date: DateTimeOffset) = date.Month

    // Returns the year part of this DateTimeOffset. The returned value is an
    // integer between 1 and 9999.
    let year (date: DateTimeOffset) = date.Year

    // Returns the tick count for this DateTimeOffset. The returned value is
    // the number of 100-nanosecond intervals that have elapsed since 1/1/0001
    // 12:00am.
    let ticks (date: DateTimeOffset) = date.Ticks

    // Returns the day-of-year part of this DateTimeOffset. The returned value
    // is an integer between 1 and 366.
    let dayOfYear (date: DateTimeOffset) = date.DayOfYear

    // Returns the time-of-day part of this DateTimeOffset. The returned value
    // is a TimeSpan that indicates the time elapsed since midnight.
    let timeOfDay (date: DateTimeOffset) = date.TimeOfDay
