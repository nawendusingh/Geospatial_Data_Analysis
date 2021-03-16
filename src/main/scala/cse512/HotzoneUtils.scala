package cse512

object HotzoneUtils {

  def ST_Contains(queryRectangle: String, pointString: String ): Boolean = {
          var r = new Array[String](4)
          r = queryRectangle.split(",")
          var rx1 = r(0).trim.toDouble
          var ry1 = r(1).trim.toDouble
          var rx2 = r(2).trim.toDouble
          var ry2 = r(3).trim.toDouble
            
          var p = new Array[String](2)
          p= pointString.split(",")          
          var px=p(0).trim.toDouble
          var py=p(1).trim.toDouble

          var lx =0.0
          var hx =0.0
          
          if (rx1 < rx2)
          {
            lx = rx1
            hx = rx2
          }
          else
          {
            lx = rx2
            hx = rx1
          }
          
          var ly = math.min(ry1, ry2)
          var hy = math.max(ry1, ry2)
          
          if(py > hy || px < lx || px > hx || py < ly)
            return false
          else
            return true
       
    }
  
}
