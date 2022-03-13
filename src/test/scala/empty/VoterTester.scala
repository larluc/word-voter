package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.util.log2Ceil

class VoterTester extends AnyFlatSpec with ChiselScalatestTester {
  def getMaxValues(list : Iterable[Int]) : Iterable[Int] = {
    val identityMap = list.groupBy(identity).mapValues(_.size)
    val maxCount = identityMap.maxBy(_._2)._2
    for (i <- identityMap if i._2 == maxCount) yield i._1
  }

  def getMaxValueCount(list : Iterable[Int]) = {
    val identityMap = list.groupBy(identity).mapValues(_.size)
    identityMap.maxBy(_._2)._2
  }

  def toBinary(i: Int, digits: Int) = {
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
  }

  // Call function "fire" with all possible combinations of "inputs" numbers from 0 until values
  def generateCombinations(inputs:Int, values:Int, fire: (Vector[Int]) => Unit, comb: Vector[Int] = Vector[Int]()) : Unit = {
    if (inputs > 0) {
      for (i <- 0 until values) {
          generateCombinations(inputs - 1, values, fire, comb :+ i)
      }
    }
    else {
      fire(comb)
    }
  }

  "Voter" should "work" in {
    val inputs = 6
    val numbers = 4

    test(new MultiVoter(log2Ceil(numbers), inputs)) { dut =>
      def fire(comb : Vector[Int]) = {
        // Assign combination to inputs
        for (i <- 0 until inputs) {
          dut.io.in(i).poke(comb(i).U)
        }

        // Test for all possible Select-Signals
        for (subInputs <- 1 to inputs) {
          // Get subgroup of combination
          val subComb = comb.take(subInputs)

          // Assign Select-Signal
          dut.io.sel.poke((subInputs - 1).U)

          // Print hardware and software model results
          val maxValues = getMaxValues(subComb)
          println(subComb.map(x => toBinary(x, log2Ceil(numbers)))
            + ", max:" + maxValues.map(x => toBinary(x, log2Ceil(numbers)))
            + " => " + toBinary(dut.io.out.peek().litValue.toInt, log2Ceil(numbers))
            + (if (dut.io.err.peek().litValue == 1) " : Error" else " : OK"))
          
          // Check for correct output
          if (getMaxValueCount(subComb) >= subInputs / 2 + subInputs % 2) {
            // At least half of input signals match
            if (maxValues.size > 1) {
              // Tie of two groups of input signals, expect error
              dut.io.err.expect(true.B)
            }
            else {
              // Majority of equal input signals, expect no error
              dut.io.err.expect(false.B)
            }
          }
          else {
            // No majority of equal input signals, expect error
            dut.io.err.expect(true.B)
          }
        }
      }

      // Call function fire with all possible input combinations
      generateCombinations(inputs, numbers, fire)
    }
  }
}