/*
 * Scala implementation of Brainfuck
 * Ref: Brainfuck Interpreter in 40 lines of Scala -- Veröffentlicht
 * http://peter-braun.org/2012/07/brainfuck-interpreter-in-40-lines-of-scala/
 * Split the tricky loop in the original implementation
 */

package icu.harx

import scala.io.StdIn

object brainfuck:
  def apply(bits: Int)(code: Array[Char]): Array[Int] =
    apply { Array.fill(bits)(0) }(code)

  def apply(data: Array[Int])(code: Array[Char]): Array[Int] =
    var counter = 0
    var pc      = 0 // program counter

    /** jump over codes inside [ ]
      * @param _pc
      *   pc @ '[' When data(counter) == 0
      * @return
      *   pc @ pairing ']'
      */
    def jump_over(_pc: Int): Int =
      var pc       = _pc + 1
      var unpaired = 1

      while unpaired != 0 do
        code(pc) match
          case '[' => unpaired += 1
          case ']' => unpaired -= 1
          case _   =>
        pc += 1

      pc

    /** jump back from ']' to pairing '['
      * @param _pc
      *   pc @ ']' When data(counter) != 0
      * @return
      *   pc @ pairing '['
      */
    def jump_back(_pc: Int): Int =
      var pc       = _pc - 1
      var unpaired = 1

      while unpaired != 0 do
        code(pc) match
          case '[' => unpaired -= 1
          case ']' => unpaired += 1
          case _   =>
        pc -= 1

      pc

    while pc < code.length do
      code(pc) match
        // addressing
        case '>' => counter += 1
        case '<' => counter -= 1
        // read & write
        case '+' => data(counter) += 1
        case '-' => data(counter) -= 1
        // io
        case ',' => data(counter) = StdIn.readChar()
        case '.' => printf("%c", data(counter))
        // jump
        case '[' => if data(counter) == 0 then pc = jump_over(pc)
        case ']' => if data(counter) != 0 then pc = jump_back(pc)
        // error
        case _ => throw MatchError(s"Unknown Instruction @ $pc")
      // next instruction
      pc += 1

    data
