package collatz

import chisel3._
import chisel3.util._

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

class CollatzAlt extends Module {
  val n = 1024

  val io = IO(new Bundle {
     val inp    = Input(UInt(n.W))
     val active = Input(UInt(n.W))
     val ld = Input(Bool())
     val isOne = Output(Bool())
     val ov = Output(Bool())
  })

  val ps_o = Reg(UInt(n.W))
  val ps_t = Reg(UInt(n.W))
  val ps_z = Reg(UInt(n.W))

  val a0 = ps_o
  val a1 = ps_t << 1 | 1.U
  val a2 = ps_o << 1
  val a3 = ps_t << 2

  val (s0,c0) = CSA(a0,a1,a2)
  val (ns_o,ns_t) = CSA(a3,s0,c0<<1)

  //val ns_z = a3 | s0 | (c0<<1) | ps_z
  val ns_z = ns_o | ns_t | ps_z

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

  io.ov := ps_z(n-1)

  when (io.ld) {
     ps_z := io.active
     ps_o := io.inp
     ps_t := 0.U
  }

}

class Collatz extends Module {
  val n = 1024

  val io = IO(new Bundle {
     val inp    = Input(UInt(n.W))
     val active = Input(UInt(n.W))
     val ld = Input(Bool())
     val isOne = Output(Bool())
     val ov = Output(Bool())
  })

  val ps_o0 = Reg(UInt(n.W))
  val ps_o1 = Reg(UInt(n.W))
  val ps_z = Reg(UInt(n.W))

  val ps_extra_c = Reg(Bool())

  val a0 = ps_o0
  val a1 = ps_o1
  val a2 = ps_o0 << 1 | 1.U
  val a3 = ps_o1 << 1 | ps_extra_c

  val (s0,c0) = CSA(a0,a1,a2)
  val (s1,c1) = CSA(a3,s0,c0<<1)

  val ns_o0 = s1
  val ns_o1 = c1 << 1

  val ns_z = ns_o0 | ns_o1 | ps_z

  ps_z := ns_z
  ps_o0 := ns_o0
  ps_o1 := ns_o1

  val isOne2 = !ps_z(1) && ps_o0(0) && ps_o1(0)

  io.isOne := ps_o0 === 1.U && ps_o1 =/= 1.U || ps_o0 =/= 1.U && ps_o1 === 1.U

  assert( isOne2 === io.isOne)


  val (s,c) = HA(ps_o0(0), ps_o1(0))
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
  }

}
