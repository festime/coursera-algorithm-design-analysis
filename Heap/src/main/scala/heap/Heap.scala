package heap

import collection.mutable.ArrayBuffer

class Heap {
  // index 0 is for convenience, this value will not be used
  private val heapRepresentation = ArrayBuffer[Int](0)
  private val positions = collection.mutable.Map[Int, ArrayBuffer[Int]]()

  def extractMin(): Int = {
    val min = heapRepresentation(1)

    swap(1, lastIndex)
    removeLastElement()
    bubbleDown(1)

    return min
  }

  def content = heapRepresentation.tail

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

    def parentIndex = nodeIndex / 2
    def smallerThanParent = heapRepresentation(nodeIndex) < heapRepresentation(parentIndex)

    while (parentIndex > 0 && smallerThanParent) {
      swap(nodeIndex, parentIndex)
      nodeIndex = parentIndex
    }
  }

  private def lastIndex: Int = heapRepresentation.length - 1

  private def bubbleDown(n: Int): Unit = {
    var nodeIndex = n

    def leftChildIndex: Int = nodeIndex * 2
    def rightChildIndex: Int = nodeIndex * 2 + 1

    while (true) {
      if (leftChildIndex < heapRepresentation.length && heapRepresentation(nodeIndex) > heapRepresentation(leftChildIndex)) {
        swap(leftChildIndex, nodeIndex)
        nodeIndex = leftChildIndex

      } else if (rightChildIndex < heapRepresentation.length && heapRepresentation(nodeIndex) > heapRepresentation(rightChildIndex)) {
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

    if (positionOfRemovedElement.length > 1) {
      positionOfRemovedElement -= (lastIndex)

    } else {
      positions -= heapRepresentation.last
    }

    heapRepresentation.remove(lastIndex)
  }

  private def swap(i: Int, j: Int) = {
    require(i < heapRepresentation.length && i > 0)
    require(j < heapRepresentation.length && j > 0)

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