package heap

import collection.mutable.ArrayBuffer

class Heap {
  private val heapRepresentation = ArrayBuffer[Int]()
  private val positions = collection.mutable.Map[Int, ArrayBuffer[Int]]()
  private val IndexOfMinValue = 0

  def extractMin(): Int = {
    val min = heapRepresentation(IndexOfMinValue)

    swap(IndexOfMinValue, lastIndex)
    removeLastElement()
    bubbleDown(IndexOfMinValue)

    return min
  }

  def content = heapRepresentation

  def delete(n: Int): Option[Int] = {
    if (positions.getOrElse(n, ArrayBuffer[Int]()).isEmpty) {
      return None

    } else {
      val position = positions(n).head

      swap(position, lastIndex)
      removeLastElement()
      bubbleDown(position)

      return Some(n)
    }
  }

  def insert(n: Int) = {
    append(n)

    var nodeIndex = lastIndex

    def parentIndex = (nodeIndex - 1) / 2
    def smallerThanParent = heapRepresentation(nodeIndex) < heapRepresentation(parentIndex)

    while (parentIndex > IndexOfMinValue && smallerThanParent) {
      swap(nodeIndex, parentIndex)
      nodeIndex = parentIndex
    }

    if (
      parentIndex == IndexOfMinValue &&
      nodeIndex != parentIndex &&
      heapRepresentation(nodeIndex) < heapRepresentation(parentIndex)) {

      swap(nodeIndex, parentIndex)
      nodeIndex = parentIndex
    }
  }

  private def lastIndex: Int = heapRepresentation.length - 1

  private def bubbleDown(index: Int): Unit = {
    var nodeIndex = index

    def leftChildIndex: Int = nodeIndex * 2
    def rightChildIndex: Int = nodeIndex * 2 + 1

    while (true) {
      if (
        leftChildIndex < heapRepresentation.length &&
        heapRepresentation(nodeIndex) > heapRepresentation(leftChildIndex)) {

        swap(leftChildIndex, nodeIndex)
        nodeIndex = leftChildIndex

      } else if (
        rightChildIndex < heapRepresentation.length &&
        heapRepresentation(nodeIndex) > heapRepresentation(rightChildIndex)) {

        swap(rightChildIndex, nodeIndex)
        nodeIndex = rightChildIndex

      } else {
        return
      }
    }
  }

  private def append(n: Int) = {
    val positionOfNewElement = positions.getOrElse(n, ArrayBuffer[Int]())
    heapRepresentation += n

    if (positionOfNewElement.isEmpty) {
      positions(n) = ArrayBuffer[Int](lastIndex)

    } else {
      positionOfNewElement += (lastIndex)
    }
  }

  private def removeLastElement() = {
    val positionOfRemovedElement = positions.getOrElse(heapRepresentation.last, ArrayBuffer[Int]())

    if (positionOfRemovedElement.nonEmpty) {
      positionOfRemovedElement -= (lastIndex)

    } else {
      positions -= heapRepresentation.last
    }

    heapRepresentation.remove(lastIndex)
  }

  private def swap(i: Int, j: Int) = {
    require(i < heapRepresentation.length && i >= IndexOfMinValue)
    require(j < heapRepresentation.length && j >= IndexOfMinValue)

    val temp = heapRepresentation(i)

    positions(heapRepresentation(i)) -= i
    positions(heapRepresentation(j)) -= j
    heapRepresentation(i) = heapRepresentation(j)
    heapRepresentation(j) = temp
    positions(heapRepresentation(i)) += i
    positions(heapRepresentation(j)) += j
  }
}

object Main extends App {
  val heap = new Heap

  for (i <- 5 to 1 by -1) heap.insert(i)

  println(heap.content)
}