import leon.Utils._

object Epsilon1 {

  def rand(): Int = epsilon((x: Int) => true)
  def rand2(x: Int): Int = epsilon((y: Int) => true)

  //this should hold, that is the expected semantic of our epsilon
  def property1(): Boolean = {
    rand() == rand() 
  } holds

  //this should hold, that is the expected semantic of our epsilon
  def property2(x: Int): Boolean = {
    rand2(x) == rand2(x+1) 
  } holds

}
