import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CrudOperationTest extends AnyFlatSpec with Matchers {

  "ListCrudOperations" should "create, read, update and delete items correctly" in {
    val crudObject = new ListCrudOperations[String]

    crudObject.create("Hello")
    crudObject.create("Bye")
    crudObject.read(0) shouldEqual Some("Hello")
    crudObject.read(1) shouldEqual Some("Bye")
    crudObject.read(2) shouldEqual None
    crudObject.update(1, "hey") shouldEqual true
    crudObject.read(1) shouldEqual Some("hey")
    crudObject.delete(0) shouldEqual true
    crudObject.read(0) shouldEqual Some("hey")
    crudObject.delete(1) shouldEqual false
    crudObject.delete(1) shouldEqual false
  }

  "SeqCrudOperations" should "create, read, update and delete items correctly" in {
    val crudSeqObject = new SeqCrudOperations[Int]

    crudSeqObject.create(1)
    crudSeqObject.create(2)
    crudSeqObject.read(0) shouldEqual Some(1)
    crudSeqObject.read(1) shouldEqual Some(2)
    crudSeqObject.read(2) shouldEqual None
    crudSeqObject.update(1, 3) shouldEqual true
    crudSeqObject.read(1) shouldEqual Some(3)
    crudSeqObject.delete(0) shouldEqual true
    crudSeqObject.delete(1) shouldEqual false
  }

}
