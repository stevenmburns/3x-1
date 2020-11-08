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

object CSA4 {
   def apply( a0 : UInt, a1 : UInt, a2 : UInt, a3 : UInt) : (UInt,UInt) = {
       val (s0,c0) = CSA(a0,a1,a2)
       CSA(a3,s0,c0<<1)
   }
}

object CSA6 {
   def apply( a0 : UInt, a1 : UInt, a2 : UInt, a3 : UInt, a4 : UInt, a5 : UInt) : (UInt,UInt,UInt) = {

       val (s0,c0) = CSA(a0,a1,a2)
       val (s1,c1) = CSA(a3,a4,a5)
       
       val (out0,c2) = HA(s0,s1)        
       val (out1,out2) = CSA(c0,c1,c2)        

       return (out0,out1,out2)
   }
}

class Collatz6 extends Module {
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
  val ps_f = Reg(UInt(n.W))
  val ps_z = Reg(UInt(n.W))

  val a = ps_o
  val b = ps_t << 1 | 1.U
  val c = ps_f << 2
  val d = ps_o << 1
  val e = ps_t << 2
  val f = ps_f << 3

  val (ns_o,ns_t,ns_f) = CSA6(a,b,c,d,e,f)

  val ns_z = a | b | c | d | e | f | ps_z

  ps_z := ns_z
  ps_o := ns_o
  ps_t := ns_t
  ps_f := ns_f

  val isOne2 = !ps_z(1) && ps_o(0)

  io.isOne := ps_t === 0.U && ps_f === 0.U && ps_o === 1.U

  assert( isOne2 === io.isOne)

  when ( !ps_o(0)) {
     val a = ps_o >> 1
     val b = ps_t
     val c = ps_f << 1
     val (ns_o,ns_t) = CSA(a,b,c)

     ps_z := a | b | c | (ps_z>>1)
     ps_o := ns_o
     ps_t := ns_t
     ps_f := 0.U
  }

  io.ov := ps_z(n-1)

  when (io.ld) {
     ps_z := io.active
     ps_o := io.inp
     ps_t := 0.U
     ps_f := 0.U
  }

}

class Collatz4 extends Module {
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

  val ns_z = a3 | s0 | (c0<<1) | ps_z

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

class Collatz extends Collatz4