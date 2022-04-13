package empty

import chisel3._
import chisel3.util._

import scala.collection.immutable.ListMap
import chisel3.experimental.{DataMirror}
import chisel3.{ActualDirection}

class DirectedRecord(record: Record, direction: ActualDirection) extends Record {
  val elements = 
    for ((name, data) <- record.elements
      // Only iterate over Data if it is Record or if direction matches
      if ((data.isInstanceOf[Record] || DataMirror.directionOf(data.asInstanceOf[Data]) == direction)
        && {
          // Avoid zero width Aggregates/Data
          if (data.isInstanceOf[Record]) {
            (new DirectedRecord(data.asInstanceOf[Record], direction)).getWidth > 0
          }
          else {
            data.getWidth > 0
          }
        }))
      yield {
        if (data.isInstanceOf[Record]) {
          (name -> new DirectedRecord(data.asInstanceOf[Record], direction))
        }
        else {
          (name -> data.asInstanceOf[Data].cloneType)
        }
      }

  override def cloneType: this.type = (new DirectedRecord(record, direction)).asInstanceOf[this.type]
}

trait RecordHelperFunctions {
  def connectRecordElements(src: ListMap[String, Any], dest: ListMap[String, Any], direction: Option[ActualDirection] = None) : Unit = {
    for ((name, data) <- dest) {
      // Only connect if element also exists in source and direction matches if specified
      if (src.contains(name) && (data.isInstanceOf[Record] || direction.isEmpty || DataMirror.directionOf(data.asInstanceOf[Data]) == direction.get)) {
        if (data.isInstanceOf[Record]) {
          connectRecordElements(src(name).asInstanceOf[Record].elements, data.asInstanceOf[Record].elements, direction)
        }
        else {
          dest(name).asInstanceOf[Data] := src(name).asInstanceOf[Data]
        }
      }
    }
  }

  def recordElementsEqual(a: ListMap[String, Any], b: ListMap[String, Any]) : Bool = {
    (
      for ((name, data) <- a) yield {
        if (data.isInstanceOf[Record]) {
          recordElementsEqual(data.asInstanceOf[Record].elements, b(name).asInstanceOf[Record].elements)
        }
        else {
          data.asInstanceOf[Data].asUInt === b(name).asInstanceOf[Data].asUInt
        }
      }
    ).reduce(_ && _)
  }

  def muxCaseRecordElements(out: ListMap[String, Any], default: ListMap[String, Any], mapping: Seq[(Bool, ListMap[String, Any])]) : Unit = {
    for ((name, data) <- out) yield {
      if (data.isInstanceOf[Record]) {
        muxCaseRecordElements(data.asInstanceOf[Record].elements, default(name).asInstanceOf[Record].elements, mapping.map{case (bool, elements) => (bool, elements(name).asInstanceOf[Record].elements)})
      }
      else {
        data.asInstanceOf[Data] := MuxCase(default(name).asInstanceOf[Data], mapping.map{case (bool, elements) => (bool -> elements(name).asInstanceOf[Data])})
      }
    }
  }

  def muxLookupRecordElements(out: ListMap[String, Any], key: UInt, default: ListMap[String, Any], mapping: Seq[(UInt, ListMap[String, Any])]) : Unit = {
    for ((name, data) <- out) yield {
      if (data.isInstanceOf[Record]) {
        muxLookupRecordElements(data.asInstanceOf[Record].elements, key, default(name).asInstanceOf[Record].elements, mapping.map{case (bool, elements) => (bool, elements(name).asInstanceOf[Record].elements)})
      }
      else {
        data.asInstanceOf[Data] := MuxLookup(key, default(name).asInstanceOf[Data], mapping.map{case (uint, elements) => (uint -> elements(name).asInstanceOf[Data])})
      }
    }
  }
}

class RecordWordVoter(getRecord: () => Record, inputs: Int, majorityCount: Int) extends Module with RecordHelperFunctions {
  require(inputs >= 2)
  if (inputs > 2) {
    require(majorityCount >= inputs / 2 + inputs % 1)
    require(majorityCount < inputs)
  }

  val io = IO(new Bundle {
    val in = Input(Vec(inputs, getRecord()))
    val out = if (inputs > 2) Some(Output(getRecord())) else None
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
            recordElementsEqual(io.in(comb(i)).elements, io.in(comb(i + 1)).elements)
        ).reduce(_ && _)
      ).reduce(_ || _)
    
    // Multiplex Input-Signal
    muxCaseRecordElements(io.out.get.elements, io.in(0).elements, (votingConditions.drop(1).zipWithIndex).map{case (bool, index) => (bool -> io.in(index + 1).elements)})
  }

  // Generate Error-Signal
  io.err := ! (
    // Trivial case for two inputs
    if (inputs == 2) {
      recordElementsEqual(io.in(0).elements, io.in(1).elements)
    }

    // Majority of number of inputs with same signal required
    else if (majorityCount > inputs / 2) {
      (
        for (comb <- majoritySets) yield {
          val combVec = comb.toVector
          for (i <- 0 until comb.size - 1) yield
            recordElementsEqual(io.in(combVec(i)).elements, io.in(combVec(i + 1)).elements)
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
              recordElementsEqual(io.in(combVec(i)).elements, io.in(combVec(i + 1)).elements)
          ).reduce(_ && _)
          && (
            // Other half of input signals must differ
            ! (
                for (i <- 0 until counterComb.size - 1) yield
                  recordElementsEqual(io.in(counterComb(i)).elements, io.in(counterComb(i + 1)).elements)
              ).reduce(_ && _)
            // But can be equal to the voted input signal
            || (
              recordElementsEqual(io.in(combVec.min).elements, io.in(counterComb.min).elements)
            )
          ))
        }
      ).reduce(_ || _)
    }
  )
}

class RecordMultiVoter(getRecord: () => Record, inputs: Int) extends Module with RecordHelperFunctions {
  require(inputs >= 2)

  val io = IO(new Bundle {
    val in = Input(Vec(inputs, getRecord()))
    val sel = Input(UInt(log2Ceil(inputs).W))
    val out = Output(getRecord())
    val err = Output(Bool())
  })

  val outOpts = Wire(Vec(inputs - 2, getRecord()))
  val errOpts = Wire(Vec(inputs - 1, Bool()))

  // Select Output- and Error-Signal by Select-Input
  muxLookupRecordElements(io.out.elements, io.sel, io.in(0).elements, outOpts.zipWithIndex.map{case (wire, index) => ((index + 2).U) -> wire.elements})
  io.err := MuxLookup(io.sel, false.B, errOpts.zipWithIndex.map{ case (wire, index) =>
    ((index + 1).U) -> wire
  })

  // Instantiate Voters for 2 up to "inputs" number of inputs
  val voterUnits = for (subInputs <- 2 to inputs) {
    val voter = Module(new RecordWordVoter(getRecord, subInputs, subInputs / 2 + subInputs % 2))
    
    // Connect inputs
    for (i <- 0 until subInputs) {
      //voter.io.in(i) := io.in(i)
      connectRecordElements(io.in(i).elements, voter.io.in(i).elements)
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
  //emitVerilog(new MultiVoter(width = 8, inputs = 8), Array("--target-dir", "generated"))
}