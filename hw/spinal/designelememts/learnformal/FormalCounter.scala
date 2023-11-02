package designelememts.learnformal

import spinal.lib._
import spinal.core._
import spinal.core.formal._

class LimitedCounter extends Component {
    val inc = in Bool()

    val value = Reg(UInt(4 bits)) init 2

    when (inc && value < 10) {
        value := value + 1
    }
}

object FormalCounterTester extends App {

    FormalConfig
    .withBMC(20)
    .withCover(20)
    .withProve(20)
    .withDebug
    .doVerify(new Component{
        val dut = FormalDut(new LimitedCounter())
        val inc = in Bool()
        dut.inc <> inc
        // anyseq(dut.inc)
        assumeInitial(clockDomain.isResetActive)
        for (i <- 2 to 10) {
            cover(dut.value === i)
        }
        cover(pastValidAfterReset && past(inc) && past(dut.value) < 10)
        when (pastValidAfterReset && past(inc) && past(dut.value) < 10) {
            assert(changed(dut.value))
        }

        when (past(inc)) {
            assume(inc === False)
        } .otherwise {
            assume(inc === True)
        }
    })

    

}