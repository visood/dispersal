package evolution.strategy
import scala.math._



object twoPatch{
	import dispersalFunctions._
	import listModifier._
	//the random generator
	implicit val ranGen = new util.Random

	val dispVsEcoChr = DispVsEcoChr(0.0, 0.01, 0.0, 0.01)
	import dispVsEcoChr._
	
	val h0 = Habitat(-1.0,  100, 100.0)
	val h1 = Habitat(1.0, 100, 100.0)
	

	val dspfs = DispersalListModifiers[Agent, StateComponent, Habitat](List(h0, h1))
	import dspfs._

	implicit def evAgentListToNoun(xs: List[Agent]) = Noun(xs)
	implicit def evAgentToNoun(x: Agent) = Noun(List[Agent](x))

	val jack = SimpleAgent(0, h0, DspCharacter(1.0), EcoCharacter(0.0))
	val mack = SimpleAgent(0, h1, DspCharacter(1.0), EcoCharacter(0.0))

	val ixs = List[Agent](jack, mack)
	
	val nxtGen = settlers after dispersing the replicants

	def evolveForGenerations(xs: List[Agent], n: Int): List[Agent] = n match{
		case 0 => xs
		case _ => evolveForGenerations(nxtGen(xs), n - 1)
	}

//analysis
	def recordByGenerations(xs: List[Agent])(n: Int)(f: List[Agent] => Double):  List[(Int, Double)]  = {
		if (n == 0) List[(Int, Double)]() else (n, f(xs)) :: recordByGenerations(xs)(n-1)(f)
	}
		
     
	//def nxtGnr(pop: ListModifier[Agent]): ListModifier[Agent] = settled after dispersing the replicants of pop

}
