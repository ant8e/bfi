/* 
   Copyright 2011 Antoine Comte

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */
package net.antoinecomte.bfi

import scala.util.parsing.combinator.RegexParsers
import java.io.OutputStream
import java.io.PrintStream
import java.io.Reader

object bfParser extends RegexParsers {
  def ignore = """[^<>+-\.,\[\]]""".r
  def inc = "+" ^^ { s => IncOp() }
  def dec = "-" ^^ { s => DecOp() }
  def next = ">" ^^ { s => NextOp() }
  def prev = "<" ^^ { s => PrevOp() }
  def out = "." ^^ { s => OutOp() }
  def in = "," ^^ { s => InOp() }
  def op = (ignore *) ~> (inc | next | prev | dec | out) <~ (ignore *)
  def loop = "[" ~> (op *) <~ "]" ^^ { l => Loop(l) }
  def prog = (op | loop) *
}

trait Operation
case class IncOp extends Operation
case class DecOp extends Operation
case class NextOp extends Operation
case class PrevOp extends Operation
case class OutOp extends Operation
case class InOp extends Operation
case class Loop(val operations: Seq[Operation]) extends Operation

class BFInterpreter(val stdin: Reader = Console.in, val stdout: PrintStream = Console.out) {
  private final val memory = scala.collection.mutable.ArrayBuffer.fill[Byte](30000) { 0 }
  private final var pointer = 0

  def run(program: Seq[Operation]): Unit = {
    for (op <- program)
      op match {
        case IncOp() => memory(pointer) = (memory(pointer) + 1).toByte
        case DecOp() => memory(pointer) = (memory(pointer) - 1).toByte
        case NextOp() => pointer += 1
        case PrevOp() => if (pointer > 0) pointer -= 1 else pointer = 0
        case Loop(l) => do { run(l) } while (memory(pointer) != 0)
        case OutOp() => stdout.print(memory(pointer).toChar)
        case InOp() => memory(pointer) = stdin.read().toByte
      }
  }
  def run(program: String) {
    import bfParser._
    bfParser.parseAll(bfParser.prog, program) match {
      case Success(p, _) => run(p)
      case NoSuccess(msg, _) => throw new IllegalArgumentException("Could not parse program: " + msg)
    }
  }
}

object Main extends App {
  val p = """
+++++ +++++             initialize counter (cell #0) to 10
[                       use loop to set the next four cells to 70/100/30/10
    > +++++ ++              add  7 to cell #1
    > +++++ +++++           add 10 to cell #2 
    > +++                   add  3 to cell #3
    > +                     add  1 to cell #4
    <<<< -                  decrement counter (cell #0)
]                   
> ++ .                  print 'H'
> + .                   print 'e'
+++++ ++ .              print 'l'
.                       print 'l'
+++ .                   print 'o'
> ++ .                  print ' '
<< +++++ +++++ +++++ .  print 'W'
> .                     print 'o'
+++ .                   print 'r'
----- - .               print 'l'
----- --- .             print 'd'
> + .                   print '!'
> .                     print '\n
"""
  new BFInterpreter().run(p)

}