package empty

import chisel3._
import chisel3.util._

class WordVoter(width: Int, inputs: Int, majorityCount: Int) extends Module {
  require(width > 0)
  require(inputs > 1)
  require(majorityCount >= inputs / 2)
  require(majorityCount < inputs)

  val io = IO(new Bundle {
    val in = Input(Vec(inputs, UInt(width.W)))
    val out = if (inputs > 2) Some(Output(UInt(width.W))) else None
    val err = Output(Bool())
  })

  // Generate possible sets of input-numbers with equal signals leading to a majority
  val majoritySets = (for (set <- (0 until inputs).toSet.subsets if set.size == majorityCount) yield set).toSet
  
  // Group majoritySets by minimum input-number
  val votingSets = for (i <- 0 until inputs - 1) yield (for (set <- majoritySets if set.min == i) yield set.toVector)

  // TODO: Implement Error-Signal
  io.err := false.B

  // Generate conditions for each input-number to be voted
  if (io.out.isDefined) {
    val votingConditions =
      for (cond <- votingSets) yield (
        if (cond.isEmpty) Set(false.B) else
        for (comb <- cond) yield (
          for (j <- 0 until comb.size - 1) yield
            (comb(j), comb(j + 1))
        ).map{case (x1, x2) => io.in(x1) === io.in(x2)}.reduce(_ && _)
      ).reduce(_ || _)
    
    // Multiplex Input-Signal
    io.out.get := MuxCase(io.in(0), (votingConditions.drop(1).zipWithIndex).map{case (bool, index) => (bool -> io.in(index + 1))})
  }
}

class MultiVoter(width: Int, inputs: Int) extends Module {
  require(width > 0)
  require(inputs > 1)

  val io = IO(new Bundle {
    val in = Input(Vec(inputs, UInt(width.W)))
    val sel = Input(UInt(log2Ceil(inputs).W))
    val out = Output(UInt(width.W))
    val err = Output(Bool())
  })

  val outOpts = Wire(Vec(inputs - 2, UInt(width.W)))
  val errOpts = Wire(Vec(inputs - 1, Bool()))

  // Select Output- and Error-Signal by sel-Input
  io.out := MuxLookup(io.sel, io.in(0), outOpts.zipWithIndex.map{ case (wire, index) =>
    ((index + 2).U) -> wire
  })
  io.err := MuxLookup(io.sel, 0.B, errOpts.zipWithIndex.map{ case (wire, index) =>
    ((index + 1).U) -> wire
  })

  // Instantiate Voters for 2 up to "inputs" number of inputs
  val voterUnits = for (subInputs <- 2 to inputs) {
    val voter = Module(new WordVoter(width, subInputs, subInputs / 2 + subInputs % 2))
    
    // Connect inputs
    for (i <- 0 until subInputs) {
      voter.io.in(i) := io.in(i)
    }

    // Connect Error-Signal
    errOpts(subInputs - 2) := voter.io.err

    // Connect Output-Signal
    if (subInputs > 2) {
      outOpts(subInputs - 3) := voter.io.out.get
    }
  }
}

object VoterMain extends App {
  emitVerilog(new MultiVoter(width = 8, inputs = 6), Array("--target-dir", "generated"))
}