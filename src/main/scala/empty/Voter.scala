package empty

import chisel3._
import chisel3.util._

class WordVoter(width: Int, inputs: Int, majorityCount: Int) extends Module {
  require(width > 0)
  require(inputs >= 2)
  require(inputs == 2 || majorityCount >= inputs / 2 + inputs % 1)
  require(inputs == 2 || majorityCount < inputs)

  val io = IO(new Bundle {
    val in = Input(Vec(inputs, UInt(width.W)))
    val out = if (inputs > 2) Some(Output(UInt(width.W))) else None
    val err = Output(Bool())
  })

  // Generate possible sets of input-numbers with equal signals leading to a majority
  val majoritySets = (for (set <- (0 until inputs).toSet.subsets if set.size == majorityCount) yield set).toSet
  
  // Group majoritySets by minimum input-number
  val votingSets = (
    for (i <- 0 until inputs - 1) yield (
      for (set <- majoritySets if set.min == i) yield set.toVector
    )
  ).filter{case set => set.nonEmpty}

  // Generate Output-Signal
  if (io.out.isDefined) {
    // Generate conditions for each input-number to be voted
    val votingConditions =
      for (cond <- votingSets) yield (
        for (comb <- cond) yield (
          for (i <- 0 until comb.size - 1) yield
            io.in(comb(i)) === io.in(comb(i + 1))
        ).reduce(_ && _)
      ).reduce(_ || _)
    
    // Multiplex Input-Signal
    io.out.get := MuxCase(io.in(0), (votingConditions.drop(1).zipWithIndex).map{case (bool, index) => (bool -> io.in(index + 1))})
  }

  // Generate Error-Signal
  io.err := ! (
    // Trivial case for two inputs
    if (inputs == 2) {
      io.in(0) === io.in(1)
    }

    // Majority of number of inputs with same signal required
    else if (majorityCount > inputs / 2) {
      (
        for (comb <- majoritySets) yield {
          val combVec = comb.toVector
          for (i <- 0 until comb.size - 1) yield
            io.in(combVec(i)) === io.in(combVec(i + 1))
        }.reduce(_ && _)
      ).reduce(_ || _)
    }

    // Even allow half of inputs to have same signal if rest differs
    else {
      (
        for (comb <- majoritySets) yield {
          val combVec = comb.toVector
          val counterComb = (0 until inputs).toSet.diff(comb).toVector
          
          // Half of input signals have to match
          ((
            for (i <- 0 until comb.size - 1) yield
              io.in(combVec(i)) === io.in(combVec(i + 1))
          ).reduce(_ && _)
          && (
            // Other half of input signals must differ
            ! (
                for (i <- 0 until counterComb.size - 1) yield
                  io.in(counterComb(i)) === io.in(counterComb(i + 1))
              ).reduce(_ && _)
            // But can be equal to the voted input signal
            || (
              io.in(combVec.min) === io.in(counterComb.min)
            )
          ))
        }
      ).reduce(_ || _)
    }
  )
}

class MultiVoter(width: Int, inputs: Int) extends Module {
  require(width > 0)
  require(inputs >= 2)

  val io = IO(new Bundle {
    val in = Input(Vec(inputs, UInt(width.W)))
    val sel = Input(UInt(log2Ceil(inputs).W))
    val out = Output(UInt(width.W))
    val err = Output(Bool())
  })

  val outOpts = Wire(Vec(inputs - 2, UInt(width.W)))
  val errOpts = Wire(Vec(inputs - 1, Bool()))

  // Select Output- and Error-Signal by Select-Input
  io.out := MuxLookup(io.sel, io.in(0), outOpts.zipWithIndex.map{ case (wire, index) =>
    ((index + 2).U) -> wire
  })
  io.err := MuxLookup(io.sel, false.B, errOpts.zipWithIndex.map{ case (wire, index) =>
    ((index + 1).U) -> wire
  })

  // Instantiate Voters for 2 up to "inputs" number of inputs
  val voterUnits = for (subInputs <- 2 to inputs) {
    val voter = Module(new WordVoter(width, subInputs, subInputs / 2 + subInputs % 2))
    
    // Connect inputs
    for (i <- 0 until subInputs) {
      voter.io.in(i) := io.in(i)
    }

    // Connect Output-Signal
    if (subInputs > 2) {
      outOpts(subInputs - 3) := voter.io.out.get
    }

    // Connect Error-Signal
    errOpts(subInputs - 2) := voter.io.err
  }
}

object VoterMain extends App {
  emitVerilog(new MultiVoter(width = 8, inputs = 8), Array("--target-dir", "generated"))
}