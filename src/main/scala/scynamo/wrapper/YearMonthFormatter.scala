package scynamo.wrapper

import java.time.format.DateTimeFormatter

/** This is a custom formatter for `YearMonth` because
  * "Years outside the range 0000 to 9999 must be prefixed by the plus or minus symbol."
  * but the plus symbol is not added by the default `.toString`.
  */
private[scynamo] object YearMonthFormatter {
  final val yearMonthFormatter = DateTimeFormatter.ofPattern("uuuu-MM")
}
