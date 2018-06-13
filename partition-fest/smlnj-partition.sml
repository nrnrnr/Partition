structure SMLofNJPartition = struct
    open Partition
    fun runDebug args = BackTrace.monitor (fn () => run args)
end
