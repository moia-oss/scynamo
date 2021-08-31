package scynamo.wrapper

import java.time.format.DateTimeFormatter

/** This collection of formatters ensures consistent encoding and decoding of Java time date types. */
private[scynamo] object DateTimeFormatters {

  /** This is a custom formatter for `YearMonth` because "Years outside the range 0000 to 9999 must be prefixed by the plus or minus
    * symbol." but the plus symbol is not added by the default `.toString`.
    */
  final val yearMonth     = DateTimeFormatter.ofPattern("uuuu-MM")
  final def localDate     = DateTimeFormatter.ISO_LOCAL_DATE
  final def localDateTime = DateTimeFormatter.ISO_LOCAL_DATE_TIME
  final def zonedDateTime = DateTimeFormatter.ISO_ZONED_DATE_TIME
}
