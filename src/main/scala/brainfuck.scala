/*
 * Brainfuck 的 Scala 实现
 * 作者: houyuanjie@github.com
 * 参考了 Brainfuck Interpreter in 40 lines of Scala -- Veröffentlicht
 * http://peter-braun.org/2012/07/brainfuck-interpreter-in-40-lines-of-scala/
 * 使用栈操作代替了原方案中技巧性的循环
 */
package icu.harx

import scala.collection.mutable
import scala.io.StdIn

@main def run =
  val code =
    "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
      .toCharArray()

  val bf = brainfuck(8)(code)

  println(s"\n数据: ${bf.foldLeft("")((s, n) => s + s"{$n}")}")
end run

object brainfuck:
  def apply( /* 数据纸带长度 */ bits: Int)( /* 程序纸带 */ code: Array[Char]) =
    // 1. 声明和定义
    val data    = Array.fill(bits)(0) // 数据纸带
    var counter = 0                   // data[counter] 数据指针
    var pc      = 0                   // code[pc] 程序指针(程序计数器)

    // 跳过, 此时 code(pc) == '[' && data(counter) == 0 跳至对应 ']'
    def jump_over(_pc: Int) =
      var pc    = _pc + 1
      val stack = mutable.Stack(_pc)

      while !stack.isEmpty do
        code(pc) match
          case '[' => stack.push(pc)
          case ']' => stack.pop()
          case _   =>
        pc += 1

      pc
    end jump_over

    // 跳回, 此时 code(pc) == ']' && data(counter) == 0 跳至对应 '['
    def jump_back(_pc: Int) =
      var pc    = _pc - 1
      val stack = mutable.Stack(_pc)

      while !stack.isEmpty do
        code(pc) match
          case '[' => stack.pop()
          case ']' => stack.push(pc)
          case _   =>
        pc -= 1

      pc
    end jump_back

    // 2. 运行逻辑
    while pc < code.length do
      // 解释指令并执行
      code(pc) match
        // 前后滚动数据纸带
        case '>' => counter += 1
        case '<' => counter -= 1
        // 增减当前位置数据
        case '+' => data(counter) += 1
        case '-' => data(counter) -= 1
        // 读写当前位置
        case ',' => data(counter) = StdIn.readChar()
        case '.' => printf("%c", data(counter))
        // 跳转当前程序纸带
        case '[' => if data(counter) == 0 then pc = jump_over(pc)
        case ']' => if data(counter) != 0 then pc = jump_back(pc)
        // 未知指令
        case _ => throw new UnknownError(s"遇到未知指令 @ $pc")
      // 更新 pc, 指向下一指令
      pc += 1

    // 3. 返回值
    data
  end apply
end brainfuck
