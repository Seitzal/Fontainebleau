package eu.seitzal.fontainebleau.test

import org.scalatest._
import eu.seitzal.fontainebleau._
import eu.seitzal.funcsv.FunCSV

class FontainebleauTest extends FunSuite {
  test("Heart disease dataset") {

    // Import and pre-process the dataset
    println("Loading data...")

    val data = FunCSV.decodeFile("testdata/heartdisease.csv")

    implicit val structure = data.head.toVector

    val data_vectorised = data.tail.map(row => row.toVector).toVector

    val data_clean =
      for (row <- data_vectorised
          if row(11) != "NA" && row(12) != "NA") yield Vector(
        row(0).toInt,
        row(1),
        row(2),
        row(3).toInt,
        row(4).toInt,
        row(5),
        row(6),
        row(7).toInt,
        row(8),
        row(9).toDouble,
        row(10),
        row(11),
        row(12),
        (row(13) == "0").toString
      )

    val training_data = data_clean.take(150)
    val test_data = data_clean.drop(150)

    println("Size of training dataset: " + training_data.length)
    println("Size of test dataset: " + training_data.length)

    // Train the model
    val n_trees = 200
    val n_vars = 4
    println("Training the model:\n" +
      n_trees + " trees with " +
      n_vars + " random variables per node on " +
      training_data.length + " observations")
    val forest = grow_random_forest(training_data, 200, 3)

    // Test the model
    println("Testing the model on " + test_data.size + " observations")
    var correct = 0
    var incorrect = 0
    for (observation <- test_data) {
      val prediction = forest.classify(observation)
      if (prediction.sortBy(tuple => tuple._2).reverse.head._1 ==
          observation(13)) {
        correct = correct + 1
      } else {
        incorrect = incorrect + 1
      }
    }
    println("Model test finished:")
    println(correct + " observations classified correctly")
    println(incorrect + " observations classified incorrectly")
    val success_rate = ((correct.toDouble / test_data.length) * 100).toInt
    println(success_rate + "% success rate")
  }
}