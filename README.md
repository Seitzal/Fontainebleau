# Fontainebleau
Fontainebleau is a mini library for purely functional random forest classifiers in scala.
Note: I wrote this as an exercise, and it is not competitive in terms of performance and accuracy to more established libraries.

# Minimal example
```scala
import eu.seitzal.fontainebleau._

object example {
  def main(args : Array[String]) {

    implicit val structure = Vector("color", "size", "shape", "label")

    val data = Vector(
      Vector("yellow", 4, "round", "apple"),
      Vector("green", 5, "long", "cucumber"),
      Vector("green", 10, "round", "watermelon"),
      Vector("yellow", 3.5, "round", "lemon"),
      Vector("red", 3, "round", "apple")
    )

    val forest = grow_random_forest(data, 100, 2)

    val strange_fruit = Vector("yellow", "round", 3, "?")

    println(forest.classify(strange_fruit))
  }
}

```
This code should output something like this:
```scala
List((watermelon,1), (lemon,39), (cucumber,15), (apple,45))
```
The first half of each tuple represents a class,
and the second represents the number of trees that voted for it.
