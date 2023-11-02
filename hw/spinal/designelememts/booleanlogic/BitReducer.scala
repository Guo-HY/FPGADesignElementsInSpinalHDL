package designelememts.booleanlogic

import spinal.core._
import designelememts._
import spinal.core.formal._
import spinal.core.sim._
import spinal.lib._

object BitReducer {
    // NOTE : nand, nor is NOT associative, xnor, xor is associative
    // TODO : Vec is indexed, can we convert to Seq(Bool)?
    def andReducer(in: Vec[Bool]): Bool = {
        in.reduceBalancedTree(_ & _)
    }

    // for 3 width input it is ~((~(in(0) & in(1))) & in(2))
    def nandReducer(in: Vec[Bool]): Bool = {
        in.reduce((x, y) => ~(x & y))
    }

    def orReducer(in: Vec[Bool]): Bool = {
        in.reduceBalancedTree(_ | _)
    }

    def norReducer(in: Vec[Bool]): Bool = {
        in.reduce((x, y) => ~(x | y))
    }

    def xorReducer(in: Vec[Bool]): Bool = {
        in.reduceBalancedTree(_ ^ _)
    }

    def xnorReducer(in: Vec[Bool]): Bool = {
        in.reduceBalancedTree((x, y) => ~(x ^ y))
    }
}

case class BitReducer(
    val operation: String, 
    val inputCount: Int
) extends Component {
    require(inputCount > 0)

    val io = new Bundle {
        val bitsIn = in Bits(inputCount bits)
        val bitOut = out Bool()
    }

    import BitReducer._
 
    val reducer: Vec[Bool] => Bool = operation match {
        case "AND" => andReducer
        case "NAND" => nandReducer
        case "OR" => orReducer
        case "NOR" => norReducer
        case "XOR" => xorReducer
        case "XNOR" => xnorReducer
    }
    
    io.bitOut := reducer(io.bitsIn.asBools)

}

object BitReducerGen extends App {
    Config.spinal.generateVerilog(BitReducer("XNOR", 3))
}

case class BitReducerDiff(
    val operation: String, 
    val inputCount: Int
) extends Component  {

    val io = new Bundle {
        val bitsIn = in Bits(inputCount bits)
    }

    case class Bit_Reducer(
        val operation: String, 
        val inputCount: Int
    ) extends BlackBox {
        addGeneric("OPERATION", operation)
        addGeneric("INPUT_COUNT", inputCount)

        val io = new Bundle {
            val bits_in = in Bits(inputCount bits)
            val bit_out = out Bool()
        }
        noIoPrefix()
        addRTLPath(s"${sys.env("VSRC_HOME")}/Bit_Reducer.v")
    }

    val ref = Bit_Reducer(operation, inputCount)
    val dut = BitReducer(operation, inputCount)
    ref.io.bits_in := io.bitsIn
    dut.io.bitsIn := io.bitsIn

    assert(ref.io.bit_out === dut.io.bitOut)

}

object BitReducerSim extends App {

    val bitReducerWidth = List(5, 6)
    val reducerTypes = List("AND", "NAND", "OR", "NOR", "XNOR")

    def test(t: String, w:Int): Unit = {
        Config.sim.compile(BitReducerDiff(t, w)).doSim { dut =>
            dut.clockDomain.forkStimulus(period = 10)
            for (i <- 0 to ((1 << w) - 1)) {
                dut.io.bitsIn #= i
                dut.clockDomain.waitRisingEdge()
            }
        }
    }

    bitReducerWidth.foreach(w => 
        reducerTypes.foreach(t => 
            test(t, w)
        )   
    )
    
}


// TODO : Formal verification flow seems not support blackbox

// object BitReducerFormal extends App {

//     FormalConfig.withCover(1000).doVerify(
//         new Component {
//             val dut = FormalDut(v_BitReducer("XNOR", 5))
//             for (i <- 0 to ((1 << 5) - 1)) {
//                 cover(dut.io.bitsIn === B(i, 5 bits))
//             }
//             anyseq(dut.io.bitsIn)
//         }
//     )
// }

