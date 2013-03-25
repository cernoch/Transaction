package cernoch.sm.transaction

class ParamException(msg: String, cause: Throwable)
	extends RuntimeException(msg, cause) {

	def this(cause: Throwable) = this(cause.getMessage, cause)

	def this(outputMessage: String) = this(outputMessage, null)
}
