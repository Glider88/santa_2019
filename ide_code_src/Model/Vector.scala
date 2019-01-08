package Model

class Vector(val start: Point, val end: Point) {
  def +(that: Vector): Vector = {
    val x1 = this.end.x - this.start.x
    val y1 = this.end.y - this.start.y
    val x2 = that.end.x - this.start.x
    val y2 = that.end.y - this.start.y

    val p2 = Point(this.start.x + x1 + x2, this.start.y + y1 + y2)
    new Vector(this.start, p2)
  }

  def *(k: Double): Vector = {
    val x1 = this.end.x - this.start.x
    val y1 = this.end.y - this.start.y
    val p2 = Point(this.start.x + x1 * k, this.start.y + y1 * k)
    new Vector(this.start, p2)
  }
}

object Vector {
  def apply(start: Point, end: Point): Vector = {
    new Vector(start, end)
  }
}
