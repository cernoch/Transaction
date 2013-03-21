package cernoch.sm.secret.transaction

import java.util.Date
import java.util.logging._
import java.text.{SimpleDateFormat, DateFormat}

class MinLogFormat extends SimpleFormatter {

	var date = new Date
	val NL = System.getProperty("line.separator", "\n")
	val formatter = new SimpleDateFormat("HH:mm:ss")

	override def format(record: LogRecord): String = {
		date = new Date
		val sb = new StringBuilder()
		printFirstLine(sb, record)
		printSecondLine(sb, record)
		if (record.getThrown != null)
			printThrowable(sb, record, record.getThrown)
		return sb.toString()
	}

	protected def levelName(level: Level): String = {
		if (Level.SEVERE  == level) return "SEV"
		if (Level.WARNING == level) return "WRN"
		if (Level.INFO    == level) return "Inf"
		if (Level.CONFIG  == level) return "cfg"
		if (Level.FINE    == level) return "---"
		if (Level.FINER   == level) return " - "
		if (Level.FINEST  == level) return "   "
		return                             "???"
	}

	protected def printFirstLine(sb: StringBuilder, record: LogRecord) {
		if (Level.SEVERE == record.getLevel) {
			sb.append("=== MESSAGE OF HIGH IMPORTANCE ===").append(NL)
			sb.append(formatter.format(date)).append(NL)
			sb.append(record.getMessage).append(NL)
		}

		val now = formatter.format(new Date(record.getMillis))
		val (h::t) = formatMessage(record).split("\\n").toList

		sb.append("<").append(now).append(" ")
		sb.append(levelName(record.getLevel))
		sb.append("> ").append(h).append(NL)
		for (line <- t) {
			sb.append("             | ")
			sb.append(line).append(NL)
		}
	}

	protected def printSecondLine(sb: StringBuilder, record: LogRecord) {
	}

	protected def printThrowable
	(sb: StringBuilder,
	 record: LogRecord,
	 throwable: Throwable) {

		sb.append("Exception in thread \"")
		sb.append(record.getThreadID.toString).append("\" ")
		sb.append(throwable.getClass.getName).append(": ")
		sb.append(throwable.getMessage).append(NL)

		sb.append("Stack trace:").append(NL)
		var t: Throwable = throwable
		while (t != null) {
			for (ste <- t.getStackTrace) {
				sb.append("\tat ")
				sb.append(ste.getClassName)
				sb.append(".")
				sb.append(ste.getMethodName)
				if (ste.getFileName != null) {
					sb.append("(")
					sb.append(ste.getFileName)
					if (ste.getLineNumber >= 0) {
						sb.append(":")
						sb.append(ste.getLineNumber)
					}
					sb.append(")")
				}
				sb.append(NL)
			}
			t = t.getCause
			if (t != null) {
				sb.append("...caused by...")
				sb.append(NL)
			}
		}
	}
}
