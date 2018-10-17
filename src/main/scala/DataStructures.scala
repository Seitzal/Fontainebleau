package eu.seitzal.fontainebleau

import scala.collection.parallel.immutable.ParVector

/**
 *  A yes-or-no question that can be asked about an observation.
 */
trait Question {
  def apply(item : Item) : Boolean
}

/** 
 *  Question whether a property equals some value.
 *  Can be asked at any scale degree, but will generally be less useful
 *  than [[OrdinalQuestion]] at scale degrees higher than nominal.
 */
class NominalQuestion(variable : Int, value : String)
    (implicit structure : Vector[String]) extends Question {
  def apply(item : Item) = {
    if (item.length <= variable) false
    else item(variable) match {
      case s : String => s == value
      case _          => false
    }
  }

  override def toString = 
    "" + structure(variable) + " == " + value + "?"
}

/**
 *  Question whether a property is greather than or equal to some value.
 *  The scale degree of the property must be at least ordinal.
 */
class OrdinalQuestion(variable : Int, value : Double)
    (implicit structure : Vector[String]) extends Question {
  def apply(item : Item) = {
    if (item.length <= variable) false
    else item(variable) match {
      case n : Int    => n >= value
      case x : Double => x >= value
      case _          => false
    }
  }

  override def toString =
    "" + structure(variable) + " >= " + value + "?"
}

/** 
 *  Base trait for any decision tree node or leaf
 */
trait Tree {

  /**
   *  Attempts to classify an item. Returns a list of possible classifications
   *  along with their respective probability, as judged by the decision tree.
   */
  def classify(item : Item) : List[(String, Double)]
}

/** 
 *  A tree that has no branches of its own, and represents a state in which
 *  no more certainty can be gained by asking additional questions.
 */
class Leaf(data : Vector[Item], depth : Int)
    (implicit structure : Vector[String]) extends Tree {

  // Predictions for a leaf are only resolved when they are needed,
  // which may improve runtime for outcomes that are rarely occuring in the data
  private lazy val i = label_col
  private lazy val size = data.length
  private lazy val options = for (cl <- unique_classes(data).toList) yield 
      (cl, data.filter(item => item(i).toString == cl).length.toDouble / size)

  def classify(item : Item) = options

  override def toString = whitespace(depth) + "Leaf: " + options
}

/**
 *  A decision tree node, which splits the data using a given question.
 */
class Node(val depth: Int, val question : Question, val left : Tree,
    val right : Tree) (implicit structure : Vector[String]) extends Tree {

  override def toString =
    whitespace(depth) + "Node: " + question + "\n" + left + "\n" + right

  def classify(item : Item) =
    if (question(item)) left.classify(item)
    else right.classify(item)
}

/** 
 *  A random forest classifier, which consists of a number of decision trees 
 *  trained on bootstrapped subsets of the same dataset.
 */ 
class RandomForest(trees : ParVector[Tree])
    (implicit structure : Vector[String]) {

  val size = trees.length

  def classify(item : Item) : List[(String, Int)] = {
    val responses = trees.map(
      tree => tree.classify(item).sortBy(tuple => tuple._2).last._1
    )
    def count(responses : List[String],
        votes : Map[String, Int]) : Map[String, Int] =
      if (responses.isEmpty) votes
      else if (votes.contains(responses.head))
        count(responses.tail, 
          votes.updated(responses.head, votes(responses.head) + 1))
      else
        count (responses.tail, votes.updated(responses.head, 1))
    count(responses.toList, Map[String, Int]()).toList.sorted.reverse
  }
}
