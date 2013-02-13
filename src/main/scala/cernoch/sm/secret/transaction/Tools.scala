package cernoch.sm.secret.transaction

object Tools {
  def usedMem = {
    System.gc()
    System.runFinalization()
    val usedMem =
      Runtime.getRuntime.totalMemory() -
        Runtime.getRuntime.freeMemory()
    usedMem / 1024 / 1024
  }
}
