package santa.model

import org.scalatest._

class VectorSpec extends FlatSpec with Matchers {
  "Vector" should "collapse sum with reverse vector" in {
    val vector = Vector(Point(0.0, 0.0), Point(1.0, 1.0))
    val reverse = Vector(Point(0.0, 0.0), Point(-1.0, -1.0))
    val empty = Vector(Point(0.0, 0.0), Point(0.0, 0.0))

    vector + reverse should be (empty)
  }

  it should "sum two same vectors" in {
    val vector = Vector(Point(0.0, 0.0), Point(1.0, 1.0))

    vector + vector should be (Vector(Point(0.0, 0.0), Point(2.0, 2.0)))
  }

  it should "sum empty vector" in {
    val vector = Vector(Point(0.0, 0.0), Point(1.0, 1.0))
    val empty = Vector(Point(0.0, 0.0), Point(0.0, 0.0))

    vector + empty should be (vector)
  }

  it should "sum two vector case 1" in {
    val v1 = Vector(Point(0.0, 0.0), Point(1.0,  0.0))
    val v2 = Vector(Point(0.0, 0.0), Point(0.0, -1.0))

    v1 + v2 should be (Vector(Point(0.0, 0.0), Point(1.0, -1.0)))
  }

  it should "sum two vector case 2" in {
    val v1 = Vector(Point(0.0, 0.0), Point(1.0,  0.0))
    val v2 = Vector(Point(0.0, -1.0), Point(0.0, 0.0))

    v1 + v2 should be (Vector(Point(0.0, 0.0), Point(1.0, 1.0)))
  }

  it should "multiply by positive number" in {
    Vector(Point(0.0, 0.0), Point(1.0,  1.0)) * 2.0 should be (Vector(Point(0.0, 0.0), Point(2.0, 2.0)))
  }

  it should "multiply by negative number" in {
    Vector(Point(0.0, 0.0), Point(1.0,  1.0)) * -2.0 should be (Vector(Point(0.0, 0.0), Point(-2.0, -2.0)))
  }

  it should "multiply by 1" in {
    val vector = Vector(Point(0.0, 0.0), Point(1.0,  1.0))
    vector * 1.0 should be (vector)
  }

  it should "multiply by zero" in {
    Vector(Point(0.0, 0.0), Point(1.0,  1.0)) * 0.0 should be (Vector(Point(0.0, 0.0), Point(0.0, 0.0)))
  }
}
