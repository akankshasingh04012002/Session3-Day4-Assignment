abstract class CrudOperations[T] {
  def create(element: T): Unit

  def read(index: Int): Option[T]

  def update(index: Int, newElement: T): Boolean

  def delete(index: Int): Boolean
}

class ListCrudOperations[T] extends CrudOperations[T] {
  private val elements = collection.mutable.ListBuffer.empty[T]

  def create(element: T): Unit = {
    elements += element
  }

  def read(index: Int): Option[T] = {
    if (index >= 0 && index < elements.length) {
      Some(elements(index))
    } else {
      None
    }
  }

  def update(index: Int, newElement: T): Boolean = {
    read(index) match {
      case Some(_) =>
        elements.update(index, newElement)
        true
      case None => false
    }
  }

  def delete(index: Int): Boolean = {
    read(index) match {
      case Some(_) =>
        elements.remove(index)
        true
      case None => false
    }
  }
}

class SeqCrudOperations[T] extends CrudOperations[T] {
  private var elements = Seq.empty[T]

  def create(element: T): Unit = {
    elements = elements :+ element
  }

  def read(index: Int): Option[T] = {
    if (index >= 0 && index < elements.length) {
      Some(elements(index))
    } else {
      None
    }
  }

  def update(index: Int, newElement: T): Boolean = {
    read(index) match {
      case Some(_) =>
        elements = elements.updated(index, newElement)
        true
      case None => false
    }
  }

  //in this elements.length - 1 takes all the elements of the collection except the last one,so is added to remove duplicacy
  def delete(index: Int): Boolean = {
    read(index) match {
      case Some(_) =>
        var idx = 0
        var deleted = false
        while (idx < elements.length) {
          if (idx == index) {
            deleted = true
          } else if (deleted) {
            elements = elements.updated(idx - 1, elements(idx)).drop(idx)
          }
          idx += 1
        }
        if (deleted) {
          elements = elements.take(elements.length - 1)
        }
        deleted
      case None => false
    }
  }

}
