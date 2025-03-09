package utils

import domain.Context.cities
import domain.Path

import java.awt.{BasicStroke, Color}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object Image {
  private val width: Int = 9000
  private val height: Int = 6000
  private val pointSize = 70

  def draw(path: Path): Unit = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val graphics2D = image.createGraphics
    graphics2D.setPaint(Color.WHITE)
    graphics2D.fillRect(0, 0, width, height)

    graphics2D.setPaint(Color.RED)
    for(city <- cities) {
      graphics2D.fillOval(city.x - pointSize/2, height - city.y - pointSize/2, pointSize, pointSize)
    }

    graphics2D.setPaint(Color.BLUE)
    graphics2D.setStroke(new BasicStroke(10))
    if (path.cities.length > 1) {
      for (i <- 0 until cities.length) {
        val c1 = path.cities(i)
        val c2 = path.cities(i + 1)
        graphics2D.setPaint(Color.BLUE)
        graphics2D.drawLine(c1.x, height - c1.y, c2.x, height - c2.y)
      }
    }

    graphics2D.dispose()
    ImageIO.write(image, "png", new File("image.png"))
  }
}
