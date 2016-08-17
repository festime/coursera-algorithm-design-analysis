package heap

import collection.mutable.ArrayBuffer

class Heap[T <% Ordered[T]] {
  private val heapRepresentation = ArrayBuffer[T]()
  private val positions = collection.mutable.Map[T, ArrayBuffer[Int]]()
  private val IndexOfMinValue = 0

  def extractMin(): T = {
    val min = heapRepresentation(IndexOfMinValue)

    swap(IndexOfMinValue, lastIndex)
    removeLastElement()
    bubbleDown(IndexOfMinValue)

    try {
      assert(heapRepresentation.forall(e => e >= min))
    } catch {
      case e:java.lang.AssertionError => {
        println("======================")
        println("min = " + min)
        heapRepresentation.filter(e => e < min).foreach { println }
        println("======================")
      }
    }
    return min
  }

  def toArrayBuffer = heapRepresentation

  def delete(n: T): Option[T] = {
    if (positions.getOrElse(n, ArrayBuffer[T]()).isEmpty) {
      return None

    } else {
      val position = positions(n).head // potential error here

      swap(position, lastIndex)

      val count = heapRepresentation.count(_ == n)
      val result = removeLastElement()

      if (position > lastIndex) {
        return Some(result)
      }

      // bubble down
      if (rightChildIndex(position) < heapRepresentation.length &&
          (heapRepresentation(position) > heapRepresentation(leftChildIndex(position)) ||
          (heapRepresentation(position) > heapRepresentation(rightChildIndex(position))))) {
        bubbleDown(position)

      } else if (leftChildIndex(position) < heapRepresentation.length &&
               heapRepresentation(position) > heapRepresentation(leftChildIndex(position))) {
        bubbleDown(position)

      } else {
        bubbleUp(position)
      }

      assert(heapRepresentation.count(_ == n) == count - 1)

      return Some(result)
    }
  }

  def insert(n: T) = {
    append(n)
    bubbleUp()
  }

  private def lastIndex: Int = heapRepresentation.length - 1
  private def parentIndex(nodeIndex: Int): Int = (nodeIndex - 1) / 2
  private def leftChildIndex(nodeIndex: Int): Int = (nodeIndex + 1) * 2 - 1
  private def rightChildIndex(nodeIndex: Int): Int = (nodeIndex + 1) * 2

  private def bubbleDown(index: Int): Unit = {
    var nodeIndex = index

    while (true) {
      val rightChildNodeIndex = rightChildIndex(nodeIndex)
      val leftChildNodeIndex = leftChildIndex(nodeIndex)

      if (rightChildNodeIndex < heapRepresentation.length) {
        if (heapRepresentation(nodeIndex) > heapRepresentation(rightChildNodeIndex) &&
            heapRepresentation(rightChildNodeIndex) <= heapRepresentation(leftChildNodeIndex)) {
          swap(rightChildNodeIndex, nodeIndex)
          assert(
            heapRepresentation(nodeIndex) <= heapRepresentation(leftChildNodeIndex) &&
            heapRepresentation(nodeIndex) <= heapRepresentation(rightChildNodeIndex))
          nodeIndex = rightChildNodeIndex

        } else if (heapRepresentation(nodeIndex) > heapRepresentation(leftChildNodeIndex) &&
                   heapRepresentation(leftChildNodeIndex) <= heapRepresentation(rightChildNodeIndex)) {
          swap(leftChildNodeIndex, nodeIndex)
          assert(
            heapRepresentation(nodeIndex) <= heapRepresentation(leftChildNodeIndex) &&
            heapRepresentation(nodeIndex) <= heapRepresentation(rightChildNodeIndex))
          nodeIndex = leftChildNodeIndex

        } else {
          return
        }

      } else if (leftChildNodeIndex < heapRepresentation.length) {
        if (heapRepresentation(nodeIndex) > heapRepresentation(leftChildNodeIndex)) {
          swap(leftChildNodeIndex, nodeIndex)
          assert(heapRepresentation(nodeIndex) <= heapRepresentation(leftChildNodeIndex))
          nodeIndex = leftChildNodeIndex

        } else {
          return
        }

      } else {
        return
      }
    }
  }

  private def bubbleUp(index: Int = lastIndex): Unit = {
    require(
      index < heapRepresentation.length && index >= 0,
      "heapRepresentation.length = " + heapRepresentation.length +
      ", index = " + index +
      ", positions = " + positions)
    var nodeIndex = index

    def smallerThanParent = {
      val parentNodeIndex = parentIndex(nodeIndex)

      if (rightChildIndex(parentNodeIndex) == nodeIndex) {
        if (rightChildIndex(parentNodeIndex) < leftChildIndex(parentNodeIndex)) {
          nodeIndex = nodeIndex - 1

        } else if (rightChildIndex(parentNodeIndex) > leftChildIndex(parentNodeIndex)) {
          nodeIndex = nodeIndex
        }
      }

      heapRepresentation(nodeIndex) < heapRepresentation(parentNodeIndex)
    }

    while (parentIndex(nodeIndex) > IndexOfMinValue && smallerThanParent) {
      val parentNodeIndex = parentIndex(nodeIndex)
      swap(nodeIndex, parentIndex(nodeIndex))

      if (rightChildIndex(parentNodeIndex) == nodeIndex) {
        assert(
          heapRepresentation(parentNodeIndex) <= heapRepresentation(nodeIndex) &&
          heapRepresentation(parentNodeIndex) <= heapRepresentation(nodeIndex - 1))

      } else if (leftChildIndex(parentNodeIndex) == nodeIndex) {
        assert(
          heapRepresentation(parentNodeIndex) <= heapRepresentation(nodeIndex))
      }
      nodeIndex = parentNodeIndex
    }

    if (
      parentIndex(nodeIndex) == IndexOfMinValue &&
      nodeIndex != parentIndex(nodeIndex) &&
      heapRepresentation(nodeIndex) < heapRepresentation(parentIndex(nodeIndex))) {

      swap(nodeIndex, parentIndex(nodeIndex))
      nodeIndex = parentIndex(nodeIndex)
    }
  }

  private def append(n: T) = {
    val positionOfNewElement = positions.getOrElse(n, ArrayBuffer[Int]())
    heapRepresentation += n

    if (positionOfNewElement.isEmpty) {
      positions(n) = ArrayBuffer[Int](lastIndex)

    } else {
      positionOfNewElement += (lastIndex)
    }
  }

  private def removeLastElement(): T = {
    val positionOfRemovedElement = positions.getOrElse(heapRepresentation.last, ArrayBuffer[Int]())
    val lastElement = heapRepresentation.last

    if (positionOfRemovedElement.nonEmpty) {
      positionOfRemovedElement -= (lastIndex)
    }

    heapRepresentation.remove(lastIndex)

    return lastElement
  }

  private def swap(i: Int, j: Int): Unit = {
    require(i < heapRepresentation.length && i >= IndexOfMinValue)
    require(j < heapRepresentation.length && j >= IndexOfMinValue)

    if (i == j) return

    val temp = heapRepresentation(i)

    positions(heapRepresentation(i)) -= i
    positions(heapRepresentation(j)) -= j
    heapRepresentation(i) = heapRepresentation(j)
    heapRepresentation(j) = temp
    positions(heapRepresentation(i)) += i
    positions(heapRepresentation(j)) += j
  }
}
