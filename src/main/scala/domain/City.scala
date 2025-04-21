package domain

case class City(id: Int, x: Int, y: Int) {
  def distanceTo(target: City): Int = {
    val xd: Int = x - target.x
    val yd: Int = y - target.y
    math.sqrt(xd * xd + yd * yd).toInt
  }
}
