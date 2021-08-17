/*
 * Brainfuck 的 Scala 实现
 * 作者: houyuanjie@github.com
 * 参考了 Brainfuck Interpreter in 40 lines of Scala -- Veröffentlicht
 * http://peter-braun.org/2012/07/brainfuck-interpreter-in-40-lines-of-scala/
 * 拆分了原方案中技巧性的循环
 */
package icu.harx

import scala.io.StdIn

@main def run =
  val code =
    "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
      .toCharArray()

  val bf = brainfuck(8)(code)

  println(s"\ndata: ${bf.foldLeft("")((s, n) => s + s"{$n}")}")
end run

object brainfuck:
  def apply(bits: Int)(code: Array[Char]): Array[Int] =
    apply { Array.fill(bits)(0) }(code)

  def apply(data: Array[Int])(code: Array[Char]): Array[Int] =
    var counter = 0
    var pc      = 0

    def jump_over(_pc: Int) =
      var pc       = _pc + 1
      var unpaired = 1

      while unpaired != 0 do
        code(pc) match
          case '[' => unpaired += 1
          case ']' => unpaired -= 1
          case _   =>
        pc += 1

      pc
    end jump_over

    def jump_back(_pc: Int) =
      var pc       = _pc - 1
      var unpaired = 1

      while unpaired != 0 do
        code(pc) match
          case '[' => unpaired -= 1
          case ']' => unpaired += 1
          case _   =>
        pc -= 1

      pc
    end jump_back

    while pc < code.length do
      code(pc) match
        case '>' => counter += 1
        case '<' => counter -= 1

        case '+' => data(counter) += 1
        case '-' => data(counter) -= 1

        case ',' => data(counter) = StdIn.readChar()
        case '.' => printf("%c", data(counter))

        case '[' => if data(counter) == 0 then pc = jump_over(pc)
        case ']' => if data(counter) != 0 then pc = jump_back(pc)

        case _ => throw MatchError(s"Unknown Instruction @ $pc")
      end match
      pc += 1
    end while

    data
  end apply
end brainfuck
