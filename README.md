# Scalup
A Scala (G)Lua parser

## Usage

Parsing a single lua file and printing a pretty printerd as well as a minifed version of it back out.
The minified version uses the Compactor before printing the code which will substitute local variable names
for shorter ones (i.e someVariable -> a)
```scala 
import scala.io.Source
import com.scalup._

val fileContents = Source.fromFile("some_file.lua").getLines.mkString
val blockOpt: Option[Block] = LuaParser.parseString(fileContents)

blockOpt.map {
  case Some(block) =>
    // Compact and minify the code
    val compactor = new Compactor()
    val minifiedPrinter = new Printer(Printer.MinifyPrinterConfig)
    val compactedBlock = compactor.compactBlock(block)
    println(s"Compacted code: \n ${minifiedPrinter.printBlock(block)}")
    
    // Pretty print the code
    val prettyPrinter = new Printer()
    println(s"Pretty Printed code: \n ${prettyPrinter.printBlock(block)}")
  
  case None =>
    println("Failed to parse lua file. See console for errors.")
}
```

## Visitors
Walking the tree can be achieved using one of the provided visitor patterns or by implementing your own.

The two provided visitors are:
```scala
class ReducerVisitor[P, R](returnTypeReducer: (R, R) => R, emptyReturnValue: R) extends AbstractVisitor[P, R]
class TreeBuildVisitor[P] extends AbstractVisitor[P, LuaAST]
```
In both cases P represents the type of your "passthrough" variable that is passed on to each visitor call.
R represents your return type from each visitor call.

Example implementations of both of these classes can be found in `src/main/scala/com/scalup/visitors`

