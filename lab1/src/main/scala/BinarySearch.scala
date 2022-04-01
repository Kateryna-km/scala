import scala.io.StdIn.readLine

object BinarySearch extends App {

  def FunctionalBinarySearch(arr: Array[Int], Element_to_Search: Int): Int = {
    def BinarySearch(arr: Array[Int], Element_to_Search: Int, low: Int, high: Int): Int = {
      if (low > high)
        return -1
      val middle = low + (high - low) / 2

      arr match {
        case (arr: Array[Int]) if (arr(middle) == Element_to_Search) => middle

        case (arr: Array[Int]) if (arr(middle) < Element_to_Search) =>
          BinarySearch(arr, Element_to_Search, middle + 1, high)

        case (arr: Array[Int]) if (arr(middle) > Element_to_Search) =>
          BinarySearch(arr, Element_to_Search, low, middle - 1)
      }
    }

    BinarySearch(arr, Element_to_Search, 0, arr.length - 1)
  }

  val inputString1 = readLine()
  val Arr1: Array[Int] = inputString1.split(" ").map(_.toInt)
  val inputString2 = readLine()
  val Arr2: Array[String] = inputString2.split(" ")
  val inputString = readLine()
  val Numb: Int = inputString.toInt
  val index = FunctionalBinarySearch(Arr1, Numb);
  if (index == -1) println("Element not found")
  else println("Element found at Index " + Arr2(index))
}


