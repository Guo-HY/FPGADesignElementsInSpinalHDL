package designelememts.booleanlogic

import spinal.core._
import designelememts._
import spinal.core.formal._
import spinal.core.sim._
import spinal.lib._

object wordReverser {
    def apply(wordCount: Int, wordWidth: Int, payload: Bits): Bits = {
        assert(wordCount * wordWidth == payload.getWidth)
        payload.subdivideIn(wordCount slices).shuffle(i => wordCount - 1 - i).asBits
    }
}

case class WordReverserDiff(wordCount: Int, wordWidth: Int) extends Component {
    val io = new Bundle {
        val payload = in Bits(wordCount * wordWidth bits)
    }

    case class Word_Reverser(
        val wordCount: Int,
        val wordWidth: Int
    ) extends BlackBox {
        addGeneric("WORD_WIDTH", wordWidth)
        addGeneric("WORD_COUNT", wordCount)

        val io = new Bundle {
            val words_in = in Bits(wordCount * wordWidth bits)
            val words_out = out Bits(wordCount * wordWidth bits)
        }
        noIoPrefix()
        addRTLPath(s"${sys.env("VSRC_HOME")}/Word_Reverser.v")
    }

    val ref = Word_Reverser(wordCount, wordWidth)
    ref.io.words_in := io.payload
    val dut = wordReverser(wordCount, wordWidth, io.payload)

    assert(dut === ref.io.words_out)
}

object WordReverserSim extends App {
    val wordCount = 3
    val wordWidth = 3
    Config.sim.compile(WordReverserDiff(wordCount, wordWidth)).doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        (0 to (1 << (wordCount * wordWidth)) - 1).foreach{ i =>
            dut.io.payload #= i
            dut.clockDomain.waitRisingEdge()
        }
    }  
}