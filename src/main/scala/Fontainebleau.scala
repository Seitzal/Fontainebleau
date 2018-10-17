package eu.seitzal

import scala.math._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import eu.seitzal.fontainebleau.tasks._

/**
 *  Contains functions for training and using decision tree and random forest
 *  classifiers.
 */
package object fontainebleau {

  /** 
   *  Vector of values representing an observation. The types and order of
   *  these values are described by an implicit structure vector.
   */
  type Item = Vector[Any]

  /**
   *  Partitions a dataset for a given question, returning two smaller datasets
   *  that contain all items for which the question is true or false, respectively
   */
  private def partition(data : Vector[Item], question : Question) =
    (data.filter(item => question(item)), data.filterNot(item => question(item)))

  /**
   *  Returns the column number of the class labels in the implicit structure
   */
  private[fontainebleau] def label_col(implicit structure : Vector[String]) : Int =
    structure.indexOf("label") match {
      case -1           => throw new Error("Training data must contain labels")
      case index        => index
    }

  /**
   *  Returns the unique classes present in a dataset, without duplicates
   */
  private[fontainebleau] def unique_classes(data : Vector[Item], classes : Set[String] = Set())
      (implicit structure : Vector[String]) : Set[String] = {
    if (data.isEmpty) classes
    else unique_classes(data.tail, classes + data.head(label_col).toString)
  }

  /**
   *  Calculates the gini impurity of a dataset
   */
  private def gini_impurity(data : Vector[Item])
      (implicit structure : Vector[String]) : Double = {

    val classes = unique_classes(data).toList
    val num_classes = classes.length
    val i = label_col

    val squared_class_frequencies = for (cl <- classes) yield
      pow(data.filter(item => item(i) == cl).length.toDouble / data.length, 2)

    1.0 - squared_class_frequencies.foldLeft(0.0)(_ + _)
  }

  /**
   *  Calculates the reduction in gini impurity gained by splitting a dataset
   *  in a specific way
   */
  private def information_gain(
      left : Vector[Item], right : Vector[Item], 
      current_size : Int, current_impurity : Double)
      (implicit structure : Vector[String]): Double = {

    current_impurity -
    ((left.length.toDouble / current_size) * gini_impurity(left) +
    (right.length.toDouble / current_size) * gini_impurity(right))
  }

  /** 
   *  Builds a bootstrapped dataset from a given training dataset.
   *  The bootstrapped set contains the same number of observations as the
   *  original dataset, but may contain duplicate observations.
   */
  private def bootstrapped_dataset(data : Vector[Item]) : Vector[Item] = {
    val n = data.length
    val ran = new Random()
    def entry_numbers = for (i <- 0 until n) yield ran.nextInt(n)
    entry_numbers.map(x => data(x)).toVector
  }

  /** 
   *  Finds all possible questions for a given dataset, considering only a
   *  randomly selected set of variables
   */
  private def bootstrapped_possible_questions(data : Vector[Item], n_vars : Int)
      (implicit structure : Vector[String]) : List[Question] = {
    val n = structure.length
    val lc = label_col
    val ran = new Random()
    def choose_variables(chosen : List[Int]) : List[Int] =
      if (chosen.length == n_vars) chosen
      else {
        val random_result = ran.nextInt(n)
        if (random_result == lc || chosen.contains(random_result))
          choose_variables(chosen)
        else
          choose_variables(random_result :: chosen)
      }
    val questions = for (
        variable <- choose_variables(Nil);
        value <- data.map(item => item(variable))
      ) yield value match {
        case n : Int    => new OrdinalQuestion(variable, n.toDouble)
        case x : Double => new OrdinalQuestion(variable, x)
        case s : String => new NominalQuestion(variable, s)
        case _          => throw new Error("Data must be strings or numbers")
      }
    questions.toList
  }

  /** 
   *  Finds all possible questions to split a dataset
   */
  private def possible_questions(data : Vector[Item])
      (implicit structure : Vector[String]) : List[Question] = {
    val lc = label_col
    val questions = for (
        variable <- 0 until structure.length
        if variable != lc;
        value <- data.map(item => item(variable))
      ) yield value match {
        case n : Int    => new OrdinalQuestion(variable, n.toDouble)
        case x : Double => new OrdinalQuestion(variable, x)
        case s : String => new NominalQuestion(variable, s)
        case _          => throw new Error("Data must be strings or numbers")
      }

    questions.toList
  }

  /**
   *  Finds the question which can split a dataset for the highest information gain
   */
  private def best_question(data : Vector[Item], bootstrap : Boolean = false, 
      n_vars : Int = 0)
      (implicit structure : Vector[String]) : (Question, Double) = {

    val current_impurity = gini_impurity(data)
    val current_size = data.length

    def iter(questions : List[Question], best : Question,
        best_gain : Double) : (Question, Double) =

      if (questions.isEmpty) (best, best_gain)

      else {
        val split = partition(data, questions.head)
        val true_branch = split._1
        val false_branch = split._2
        val gain = information_gain(true_branch, false_branch,
          current_size, current_impurity)

        if (gain > best_gain)
          iter(questions.tail, questions.head, gain)
        else
          iter(questions.tail, best, best_gain)
      }

    if (bootstrap)
      iter(bootstrapped_possible_questions(data, n_vars), null, 0.0)
    else iter(possible_questions(data), null, 0.0)
  }

  /**
   *  Builds a new tree node for the given data
   *  @param depth The recursion depth of the call, required to visualize
   *               the tree in the console
   *  @param bootstrap If true, only a random subset of variables are 
   *                   considered for the split question
   *  @param n_vars If bootstrap is set to true, this specifices the number
   *                of variables to consider 
   */
  private def build_node(data : Vector[Item], depth : Int, 
      bootstrap : Boolean = false, n_vars : Int = 0)
      (implicit structure : Vector[String]) : Tree = {

    val tuple = best_question(data, bootstrap, n_vars)
    val question = tuple._1
    val gain = tuple._2

    if(gain == 0) new Leaf(data, depth)
    else {
      val split = partition(data, question)
      val (node_left, node_right) = parallel(
        build_node(split._1, depth + 1, bootstrap, n_vars),
        build_node(split._2, depth + 1, bootstrap, n_vars))
      new Node(
        depth,
        question,
        node_left,
        node_right)
    }
  }

  /**
   *  Builds a decision tree classifier from the given training data
   *  @param bootstrap If true, only a random subset of variables are 
   *                   considered for the split question at each node
   *  @param n_vars If bootstrap is set to true, this specifices the number
   *                of variables to consider at each node
   *  @param structure An implicit vector containing the names of all variables
   *                   in the dataset. The dependent variable must always be 
   *                   named "label".
   */
  def build_tree(data : Vector[Item], bootstrap : Boolean = false,
      n_vars : Int = 0)(implicit structure : Vector[String]) : Tree =
    if (bootstrap)
      build_node(bootstrapped_dataset(data), 0, true, n_vars)
    else
      build_node(data, 0)

  /** 
   *  Trains a random forest from the given training data, with a specified
   *  number of decision trees.
   *  @param n_trees How many trees to build. A larger number of trees may
   *                 make the model more resistant to overfitting, but will
   *                 increase the time required for training.
   *  @param n_vars The number of variables to consider at each tree node.
   *                A good starting value is usually the square root of the
   *                total number of independent variables.
   *  @param structure An implicit vector containing the names of all variables
   *                   in the dataset. The dependent variable must always be 
   *                   named "label".
   */
  def grow_random_forest(data : Vector[Item], n_trees : Int, n_vars : Int)
      (implicit structure : Vector[String]) : RandomForest = {
    val saplings = (0 until n_trees).toVector.par
    val trees = saplings.map(_ => build_tree(data, true, n_vars))
    new RandomForest(trees)
  }

  /**
   *  Helper method for indentation of nodes
   */
  private[fontainebleau] def whitespace(n : Int) : String =
    (for (i <- 0 until n) yield " ").mkString

}