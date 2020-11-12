
package collatz

import chisel3._
import chisel3.iotesters._

class CollatzTest extends GenericTest {
  val factory = () => new Collatz
  behavior of s"CollatzTest"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {

          var x = BigInt(0)

          def collatz_init( inp : BigInt) {
	     x = inp
	  }

          def collatz_step() {
	     val oldx = x
	     if ( (x & BigInt(1)) != 0) {
	       x = x+(x<<1)+1
	     } else {
	       x = (x>>1)
	     }
//	     println( s"\toldx: ${oldx.toString(16)} => ${x.toString(16)}")
	  }

	  val max_trials = 20
	  var trials = 0

	  for { k <- 800 to 800} {      
	      val active = (BigInt(1)<<k)-1
              println( s"Setting active to 2^k - 1: ${active}")
	      poke(c.io.active, active)
	      val start = (BigInt(1)<<(k-1))+1
	      var inp = start
	      while ( inp <= active && trials < max_trials) {
	         inp = BigInt( k-1, rnd) | BigInt(1)<<(k-1)	         
		 println( s"Working on ${inp.toString(16)}")
		 poke(c.io.inp, inp)
		 poke(c.io.ld, 1)
		 step(1)
		 collatz_init(inp)
		 poke(c.io.ld, 0)

		 val max_iter = 100000
		 var iter = 0

		 while ( peek( c.io.isOne) == 0
			 && peek( c.io.ov) == 0
			 && iter < max_iter) {
		    step(1)
		    collatz_step()
		    iter += 1	   
		 }
		 val ov = peek( c.io.ov)
		 val isOne = peek( c.io.isOne)
		 assert( isOne == 0 || x == 1)
		 println( s"isOne in ${iter} iterations isOne: ${isOne} x: ${x} ov: ${ov} trials: ${trials}")
		 inp += 2
		 trials += 1
	      }
	  }
      }
    } should be (true)
  }
}
