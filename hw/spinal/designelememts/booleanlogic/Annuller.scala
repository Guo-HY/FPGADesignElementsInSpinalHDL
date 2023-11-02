package designelememts.booleanlogic

import spinal.core._
import designelememts._
import spinal.core.formal._
import spinal.core.sim._

object annuller {
    def apply(payload: Bits, annul: Bool): Bits = {
        payload & Cat(List.fill(payload.getWidth)(~annul))
    }
} 


case class AnnullerDiff(wordWidth: Int) extends Component {
    val io = new Bundle {
        val payload = in Bits(wordWidth bits)
        val annul = in Bool()
    }

    case class Annuller(
        val wordWidth: Int,
        val implenentation: String
    ) extends BlackBox {
        addGeneric("WORD_WIDTH", wordWidth)
        addGeneric("IMPLEMENTATION", implenentation)

        val io = new Bundle {
            val annul = in Bool()
            val data_in = in Bits(wordWidth bits)
            val data_out = out Bits(wordWidth bits)
        }        
        noIoPrefix()
        addRTLPath(s"${sys.env("VSRC_HOME")}/Annuller.v")
    }

    val ref = Annuller(wordWidth, "MUX")
    ref.io.annul := io.annul
    ref.io.data_in := io.payload
    
    val dut = annuller(io.payload, io.annul)

    assert(dut === ref.io.data_out)

}

object AnnullerSim extends App {
    val wordWidth = 5
    Config.sim.compile(AnnullerDiff(wordWidth)).doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        (0 to 1).foreach{ annul => 
            (0 to ((1 << wordWidth) - 1)).foreach{ payload => 
                dut.io.annul #= annul.toBoolean
                dut.io.payload #= payload
                dut.clockDomain.waitRisingEdge()
            }
        }
    }    

}