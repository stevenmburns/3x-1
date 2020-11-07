
package collatz

import chisel3._
import chisel3.iotesters._

class CSA6Wrapper extends Module {
  val io = IO(new Bundle {
     val a = Input(UInt(8.W))
     val b = Input(UInt(8.W))
     val c = Input(UInt(8.W))
     val d = Input(UInt(8.W))
     val e = Input(UInt(8.W))
     val f = Input(UInt(8.W))
     val out0 = Output(UInt(8.W))
     val out1 = Output(UInt(8.W))
     val out2 = Output(UInt(8.W))
  })

  val (out0,out1,out2) = CSA6(io.a,io.b,io.c,io.d,io.e,io.f)
  io.out0 := out0  
  io.out1 := out1
  io.out2 := out2  
}

class CSA6Test extends GenericTest {
  val factory = () => new CSA6Wrapper
  behavior of s"CSA6Test"
  it should "compile and execute without expect violations" in {
    chisel3.iotesters.Driver.execute( factory, optionsManager) { c =>
      new PeekPokeTester(c) {
        poke(c.io.a, BigInt("10101010",2))
        poke(c.io.b, BigInt("11001100",2))
        poke(c.io.c, BigInt("11110000",2))
        poke(c.io.d, 0)
        poke(c.io.e, 0)
        poke(c.io.f, 0)
	step(1)
	expect( c.io.out0, BigInt("10010110",2))
	expect( c.io.out1, BigInt("11101000",2))
	expect( c.io.out2, BigInt("00000000",2))

        poke(c.io.a, 0)
        poke(c.io.b, 0)
        poke(c.io.c, 0)
        poke(c.io.d, BigInt("10101010",2))
        poke(c.io.e, BigInt("11001100",2))
        poke(c.io.f, BigInt("11110000",2))
	step(1)
	expect( c.io.out0, BigInt("10010110",2))
	expect( c.io.out1, BigInt("11101000",2))
	expect( c.io.out2, BigInt("00000000",2))
      }	
    } should be (true)
  }
}

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

	  var trials = 0

	  for { k <- 800 to 800} {      
	      val active = (BigInt(1)<<k)-1
              println( s"Setting active to 2^k - 1: ${active}")
	      poke(c.io.active, active)
	      val start = (BigInt(1)<<(k-1))+1
	      var inp = start
	      while ( inp <= active && trials < 10) {
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
