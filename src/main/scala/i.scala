package collatz

import chisel3._

object HA {
   def apply( a : UInt, b : UInt) : (UInt,UInt) = {
       (a^b, a&b)
   }
}

object CSA {
   def apply( a : UInt, b : UInt, c : UInt) : (UInt,UInt) = {
       (a^b^c, a&b|a&c|b&c)
   }
}

class CollatzIfc(val n : Int) extends Module {
  val io = IO(new Bundle {
     val inp    = Input(UInt(n.W))
     val active = Input(UInt(n.W))
     val ld = Input(Bool())
     val isOne = Output(Bool())
     val ov = Output(Bool())
  })
}

class CollatzOnesTwos(n : Int) extends CollatzIfc(n) {
  val running = RegInit(init=false.B)

  val ps_o = Reg(UInt(n.W))
  val ps_t = Reg(UInt(n.W))
  val ps_z = Reg(UInt(n.W))

  val a0 = ps_o
  val a1 = (ps_t << 1) | 1.U
  val a2 = ps_o << 1
  val a3 = ps_t << 2

  val (s0,c0) = CSA(a0,a1,a2)
  val (ns_o,ns_t) = CSA(a3,s0,c0<<1)

  val ns_z = a3 | s0 | (c0<<1) | ps_z
  //val ns_z = ns_o | ns_t | ps_z

  ps_z := ns_z
  ps_o := ns_o
  ps_t := ns_t

  val isOne2 = !ps_z(1) && ps_o(0)

  io.isOne := ps_t === 0.U && ps_o === 1.U

  assert( isOne2 === io.isOne)

  when ( !ps_o(0)) {
     val a = ps_o >> 1
     val b = ps_t
     val (ns_o,ns_t) = HA(a,b)

     ps_z := a | b | (ps_z>>1)
     ps_o := ns_o
     ps_t := ns_t
  }

  io.ov := ps_z(n-2)

  when (io.ld) {
     ps_z := io.active
     ps_o := io.inp
     ps_t := 0.U
     running := true.B
  }

  val check = false

  if ( check) {
    val x = Reg(UInt(n.W))

    when ( (x & 1.U) === 1.U) {
      x := x + (x << 1) + 1.U
    } .otherwise {
      x := x >> 1
    }

    def scan_ps( ps : UInt) : UInt = 
      VecInit(ps.asBools.scanRight(false.B)( (x,y) => x || y).dropRight(1)).asUInt

    val check_ps_z = scan_ps(ps_o) | scan_ps(ps_t)

    println( s"Width of ps_z ${ps_z.getWidth}, check_ps_z ${check_ps_z.getWidth}")

    when ( running) {
      assert( check_ps_z === ps_z)
      assert( x(0) === ps_o(0))
      assert( x === ps_o + (ps_t << 1))
    }

    printf( "ps_o: %d ps_t: %d ps_z: %x scan_ps_z: %x x: %d\n", ps_o, ps_t, ps_z, check_ps_z, x)

    when (io.ld) {
      ps_z := io.active
      x := io.inp
    }
  }

}

class CollatzBest(n : Int) extends CollatzIfc(n) {
  val running = RegInit(init=false.B)

  val ps_o0 = Reg(UInt(n.W))
  val ps_o1 = Reg(UInt(n.W))
  val ps_z = Reg(UInt(n.W))

  val ps_extra_c = Reg(Bool())

  val a0 = ps_o0
  val a1 = ps_o1
  val a2 = (ps_o0 << 1) | 1.U
  val a3 = (ps_o1 << 1) | ps_extra_c

  val (s0,c0) = CSA(a0,a1,a2)
  val (s1,c1) = CSA(a3,s0,(c0<<1) | ps_extra_c)

  val ns_o0 = s1
  val ns_o1 = (c1 << 1) | ps_extra_c

  val ns_z = ns_o0 | ns_o1 | (ns_o1 >> 1) | ps_z

  ps_z := ns_z
  ps_o0 := ns_o0
  ps_o1 := ns_o1
  ps_extra_c := false.B

  val (s,c) = CSA(ps_o0(0), ps_o1(0), ps_extra_c)

  io.isOne := !ps_z(1) && s(0) && !c(0)

  when ( !s) {
     val ns_o0 = ps_o0 >> 1
     val ns_o1 = ps_o1 >> 1

     ps_z := ns_o0 | ns_o1 | (ps_z>>1)
     ps_o0 := ns_o0
     ps_o1 := ns_o1
     ps_extra_c := c
  }

  io.ov := ps_z(n-1)

  when (io.ld) {
     ps_z := io.active
     ps_o0 := io.inp
     ps_o1 := 0.U
     ps_extra_c := false.B
     running := true.B
  }

  val check = false

  if ( check) {
    val x = Reg(UInt(n.W))

    when ( (x & 1.U) === 1.U) {
      x := x + (x << 1) + 1.U
    } .otherwise {
      x := x >> 1
    }

    def scan_ps( ps : UInt) : UInt = 
      VecInit(ps.asBools.scanRight(false.B)( (x,y) => x || y).dropRight(1)).asUInt

    val check_ps_z = scan_ps(ps_o0) | scan_ps(ps_o1)

    println( s"Width of ps_z ${ps_z.getWidth}, check_ps_z ${check_ps_z.getWidth}")

    when ( running) {
      assert( check_ps_z === ps_z)
      assert( x(0) === s(0))
      assert( x === ps_o0 + ps_o1 + ps_extra_c)
    }

    printf( "s: %x c: %x ps_o0: %d ps_o1: %d ps_z: %x scan_ps_z: %x ps_extra_c: %x x: %d\n", s, c, ps_o0, ps_o1, ps_z, check_ps_z, ps_extra_c, x)

    when (io.ld) {
      ps_z := io.active
      x := io.inp
    }
  }
}
