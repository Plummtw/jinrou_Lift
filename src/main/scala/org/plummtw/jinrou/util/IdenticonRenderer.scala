package org.plummtw.jinrou.util

import java.awt.{Color,Graphics2D,Polygon,RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage

object IdenticonRenderer {
  private val DEFAULT_PATCH_SIZE = 20
  private val PATCH_CELLS = 4

  private val PATCH_GRIDS = PATCH_CELLS + 1
  private val PATCH_SYMMETRIC : Byte = 1
  private val PATCH_INVERTED  : Byte = 2

  private val patch0 : Array[Byte] = Array( 0, 4, 24, 20, 0)
  private val patch1 : Array[Byte] = Array( 0, 4, 20, 0 )
  private val patch2 : Array[Byte] = Array( 2, 24, 20, 2 )
  private val patch3 : Array[Byte] = Array( 0, 2, 22, 20, 0 )
  private val patch4 : Array[Byte] = Array( 2, 14, 22, 10, 2 )
  private val patch5 : Array[Byte] = Array( 0, 14, 24, 22, 0 )
  private val patch6 : Array[Byte] = Array( 2, 24, 22, 13, 11, 22, 20, 2 )
  private val patch7 : Array[Byte] = Array( 0, 14, 22, 0 )
  private val patch8 : Array[Byte] = Array( 6, 8, 18, 16, 6 )
  private val patch9 : Array[Byte] = Array( 4, 20, 10, 12, 2, 4 )
  private val patch10 : Array[Byte] = Array( 0, 2, 12, 10, 0 )
  private val patch11 : Array[Byte] = Array( 10, 14, 22, 10 )
  private val patch12 : Array[Byte] = Array( 20, 12, 24, 20 )
  private val patch13 : Array[Byte] = Array( 10, 2, 12, 10 )
  private val patch14 : Array[Byte] = Array( 0, 2, 10, 0 )
  private val patch15 : Array[Byte] = Array()

  private val patchTypes : Array[Array[Byte]] = Array(patch0, patch1, patch2,
    patch3, patch4, patch5, patch6, patch7, patch8, patch9, patch10,
    patch11, patch12, patch13, patch14, patch15) // patch15 <- patch0

  private val patchFlags : Array[Byte] = Array(PATCH_SYMMETRIC, 0, 0, 0,
      PATCH_SYMMETRIC, 0, 0, 0, PATCH_SYMMETRIC, 0, 0, 0, 0, 0, 0,
      (PATCH_SYMMETRIC + PATCH_INVERTED).asInstanceOf[Byte])

  private val centerPatchTypes : Array[Int] = Array ( 0, 4, 8, 15 )

  class IdenticonShape (val patchSize : Int, val fore : Color, val back : Color,
                          val stroke : Color) {
    // used to center patch shape at origin because shape rotation works
    // correctly.
    val patchOffset : Int = patchSize / 2
    val patchShapes : Array[Polygon] = new Array[Polygon](patchTypes.length)
    private val scale : Int = patchSize / PATCH_CELLS

    for (i <- 0 until patchTypes.length) {
      patchShapes(i) = new Polygon()
      val patchVertices = patchTypes(i)

      for (j <- 0 until patchVertices.length) {
        val v = patchVertices(j).asInstanceOf[Int];
        val vx = (v % PATCH_GRIDS * scale) - patchOffset
        val vy = (v / PATCH_GRIDS * scale) - patchOffset
        patchShapes(i).addPoint(vx, vy)
      }
    }
  }

  private def getColorDistance(c1 : Color, c2 : Color) : Float = {
    val dx = c1.getRed() - c2.getRed()
    val dy = c1.getGreen() - c2.getGreen()
    val dz = c1.getBlue() - c2.getBlue()
  Math.sqrt(dx * dx + dy * dy + dz * dz).asInstanceOf[Float]
  }

  private def getComplementaryColor(color : Color) : Color = {
    new Color(color.getRGB() ^ 0x00FFFFFF)
    //new Color(color.getRed() ^ 0xFF, color.getGreen() ^ 0xFF, color.getBlue() ^ 0xFF)
  }

  private def drawPatch(g : Graphics2D, shape : IdenticonShape, x : Int, y : Int, patch : Int, turn : Int,
      invert : Boolean) {
    assert(patch >= 0)
    assert(turn >= 0)

    val patch1 = patch % patchTypes.length
    val turn1  = turn % 4
    val invert1 =
      if ((patchFlags(patch1) & PATCH_INVERTED) != 0) !invert
      else invert

    // paint background
    g.setBackground(if (invert1) shape.fore else shape.back)
    g.clearRect(x, y, shape.patchSize, shape.patchSize)

    // offset and rotate coordinate space by patch position (x, y) and
    // 'turn' before rendering patch shape
    val saved = g.getTransform()
    g.translate(x + shape.patchOffset, y + shape.patchOffset)
    g.rotate(Math.toRadians(turn1 * 90))

    // if stroke color was specified, apply stroke
    // stroke color should be specified if fore color is too close to the
    // back color.
    if (shape.stroke != null) {
      g.setColor(shape.stroke)
      g.draw(shape.patchShapes(patch1))
    }

    // render rotated patch using fore color (back color if inverted)
    g.setColor(if (invert1) shape.back else shape.fore)
    g.fill(shape.patchShapes(patch1))

    // restore rotation
    g.setTransform(saved)
  }

  def render(code : Int, size : Int) : BufferedImage = {
    // decode the code into parts
    // bit 0-1: middle patch type
    // bit 2: middle invert
    // bit 3-6: corner patch type
    // bit 7: corner invert
    // bit 8-9: corner turns
    // bit 10-13: side patch type
    // bit 14: side invert
    // bit 15: corner turns
    // bit 16-20: blue color component
    // bit 21-26: green color component
    // bit 27-31: red color component
    val middleType : Int = centerPatchTypes(code & 0x3)
    val middleInvert : Boolean = (((code >> 2) & 0x1) != 0)
    val cornerType : Int = (code >> 3) & 0x0f
    val cornerInvert : Boolean  = (((code >> 7) & 0x1) != 0)
    var cornerTurn : Int  = (code >> 8) & 0x3
    val sideType : Int  = (code >> 10) & 0x0f
    val sideInvert : Boolean  = (((code >> 14) & 0x1) != 0)
    var sideTurn : Int  = (code >> 15) & 0x3
    val blue : Int  = (code >> 16) & 0x01f
    val green : Int  = (code >> 21) & 0x01f
    val red : Int  = (code >> 27) & 0x01f

    // color components are used at top of the range for color difference
    // use white background for now.
    // TODO: support transparency.
    val foreColor = new Color(red << 3, green << 3, blue << 3)
    val backColor = Color.WHITE

    // outline shapes with a noticeable color (complementary will do) if
    // shape color and background color are too similar (measured by color
    // distance).
    val strokeColor =
      if (getColorDistance(foreColor, backColor) < 32.0f)
        getComplementaryColor(foreColor)
      else null

    val shape = new IdenticonShape(DEFAULT_PATCH_SIZE, foreColor, backColor, strokeColor)

    // -------------------------------------------------
    // RENDER AT SOURCE SIZE
    //

    val sourceSize = shape.patchSize * 3;
    val sourceImage = new BufferedImage(sourceSize, sourceSize,
        BufferedImage.TYPE_INT_RGB)
    val g = sourceImage.createGraphics()

    // middle patch
    drawPatch(g, shape, shape.patchSize, shape.patchSize, middleType, 0, middleInvert)

    // side patchs, starting from top and moving clock-wise
    drawPatch(g, shape, shape.patchSize, 0, sideType, sideTurn, sideInvert)
    sideTurn += 1

    drawPatch(g, shape, shape.patchSize * 2, shape.patchSize, sideType, sideTurn, sideInvert)
    sideTurn += 1

    drawPatch(g, shape, shape.patchSize, shape.patchSize * 2, sideType, sideTurn, sideInvert)
    sideTurn += 1

    drawPatch(g, shape, 0, shape.patchSize, sideType, sideTurn, sideInvert)
    sideTurn += 1

    // corner patchs, starting from top left and moving clock-wise
    drawPatch(g, shape, 0, 0, cornerType, cornerTurn, cornerInvert)
    cornerTurn += 1

    drawPatch(g, shape, shape.patchSize * 2, 0, cornerType, cornerTurn, cornerInvert)
    cornerTurn += 1

    drawPatch(g, shape, shape.patchSize * 2, shape.patchSize * 2, cornerType, cornerTurn, cornerInvert)
    cornerTurn += 1

    drawPatch(g, shape, 0, shape.patchSize * 2, cornerType, cornerTurn, cornerInvert)
    cornerTurn += 1

    g.dispose()

    // -------------------------------------------------
    // SCALE TO TARGET SIZE
    //
    // Bicubic algorithm is used for quality scaling

    val targetImage = new BufferedImage(size, size,
        BufferedImage.TYPE_INT_RGB)
    val g2 = targetImage.createGraphics()
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g2.drawImage(sourceImage, 0, 0, size, size, null)
    g2.dispose()

    targetImage
  }

}