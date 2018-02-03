import leon.real._
import RealOps._


object Relu {

 def relu(x: Real): Real = {
   require(-10 <= x && x <= 10)
   val ans: Real = x
   if(ans < 0) {
     0
   } else {
     ans
   }
 }

 def call_relu(x: Real): Real = {
   require(-10 <= x && x <= 10)
   relu(x)
 } ensuring (res => res +/- 1e-1)

}