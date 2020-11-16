
package collatz

import chisel3._
import chisel3.iotesters._

class AbstractCollatzTester( c : CollatzIfc) extends PeekPokeTester(c) {
  var x = BigInt(0)

  def collatz_init( inp : BigInt) {
    x = inp
  }

  def collatz_step() {
    //val oldx = x
    if ( (x & BigInt(1)) != 0) {
      x = x+(x<<1)+1
    } else {
      x = (x>>1)
    }
    //println( s"\toldx: ${oldx.toString(16)} => ${x.toString(16)}")
  }

  def run_one( active : BigInt, inp : BigInt) {
    poke(c.io.active, active)
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
    println( s"isOne in ${iter} iterations isOne: ${isOne} x: ${x} ov: ${ov}")
  }
}

class CollatzTest( factory : () => CollatzIfc, active : BigInt = 15, inp : BigInt = 9) extends GenericTest {
  behavior of s"CollatzTest"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new AbstractCollatzTester(c) {
        run_one( active, inp)
      }
    } should be (true)
  }
}

class CollatzRandomTest( factory : () => CollatzIfc, lb_k : Int = 800, ub_k : Int = 800, max_trials : Int = 100) extends GenericTest {
  behavior of s"CollatzTest"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new AbstractCollatzTester(c) {
	  for { k <- lb_k to ub_k} {      
	      val active = (BigInt(1)<<k)-1
              println( s"Setting active to 2^k - 1: ${active}")
              for { _ <- 0 until max_trials} {
	         val inp = BigInt( k-1, rnd) | BigInt(1)<<(k-1) | BigInt(1)	         
		 println( s"Working on ${inp.toString(16)}")
                 run_one( active, inp)
	      }
	  }
      }
    } should be (true)
  }
}

class CollatzOnesTwosTest extends CollatzTest( () => new CollatzOnesTwos(16))
class CollatzBestTest extends CollatzTest( () => new CollatzBest(16))

class CollatzOnesTwosRandomTest_800_800_100 extends CollatzRandomTest( () => new CollatzOnesTwos(1024), 800, 800, 100)
class CollatzBestRandomTest_800_800_100 extends CollatzRandomTest( () => new CollatzBest(1024), 800, 800, 100)
class CollatzBestRandomTest_12_12_100 extends CollatzRandomTest( () => new CollatzBest(20), 12, 12, 100)
class CollatzOnesTwosRandomTest_12_12_100 extends CollatzRandomTest( () => new CollatzOnesTwos(20), 12, 12, 100)


class CollatzBestTest_20_12_12_e37 extends CollatzTest( () => new CollatzBest(20), BigInt("fff",16), BigInt("e37",16))
class CollatzOnesTwosTest_20_12_12_e37 extends CollatzTest( () => new CollatzOnesTwos(20), BigInt("fff",16), BigInt("e37",16))
