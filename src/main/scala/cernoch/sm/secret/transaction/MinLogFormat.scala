package cernoch.sm.secret.transaction

import java.util.Date
import java.util.logging._
import java.text.{SimpleDateFormat, DateFormat}

class MinLogFormat extends SimpleFormatter {

	var date = new Date
	val NL = System.getProperty("line.separator", "\n")
	val formatter = new SimpleDateFormat("yy/MM/dd-HH:mm:ss")

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
		if (Level.SEVERE  == level) return "SEVERE"
		if (Level.WARNING == level) return "!WARN!"
		if (Level.INFO    == level) return "Notice"
		if (Level.CONFIG  == level) return "config"
		if (Level.FINE    == level) return "------"
		if (Level.FINER   == level) return " ---- "
		if (Level.FINEST  == level) return "  --  "
		return                             "      "
	}

	protected def printFirstLine(sb: StringBuilder, record: LogRecord) {
		if (Level.SEVERE == record.getLevel) {
			sb.append("=== MESSAGE OF HIGH IMPORTANCE ===").append(NL)
			sb.append(formatter.format(date)).append(NL)
			sb.append(record.getMessage)
			sb.append(NL)
		}
		sb.append("<")
		sb.append(formatter.format(new Date(record.getMillis)))
		sb.append(" ")
		sb.append(levelName(record.getLevel))
		sb.append("> ")
		sb.append(formatMessage(record))
		sb.append(NL)
	}

	protected def printSecondLine(sb: StringBuilder, record: LogRecord) {
	}

	protected def printThrowable(sb: StringBuilder, record: LogRecord, throwable: Throwable) {
		sb.append("Class: ").append(throwable.getClass.getName).append(NL)
		sb.append(throwable.getMessage).append(NL)
		sb.append("Class: ").append(throwable.getClass.getName).append(NL)
		sb.append("Trace: ").append(NL)
		var t: Throwable = throwable
		while (t != null) {
			for (ste <- t.getStackTrace) {
				sb.append("      ")
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
