package evolution.strategy
import scala.math._


//class DispVsEcoChr(val muDP: Double, val sigmaDP: Double, val muEC: Double, val sigmaEC: Double){

class DispVsEcoChr(val dispersalMutator: Double => Double, val ecoCrtrMutator: Double => Double, val dspPbtFun: Double => Double, val frxnMaxOffSpring: Double => Double,  implicit val randomGenerator: util.Random){
	
	
	//types used
	case class Habitat(ecoType: Double, capacity: Int, maxOffSpring: Double){
		override def toString = ecoType.toString + "\t" + capacity.toString + "\t" + maxOffSpring.toString
	}
	def unknownHabitat = new Error("unknown habitat type")

	implicit object evHabitable extends Habitable[Habitat]{
		def carryingCapacity(h: Habitat) = h match {
			case Habitat(_, c, _) => c
			case _ => throw unknownHabitat
		}
	}

	trait StateComponent
	case class DspCharacter(p: Double) extends StateComponent
	case class EcoCharacter(e: Double) extends StateComponent

	def valueOfStateComponent(s: StateComponent): Double = s match{
		case DspCharacter(p) => p
		case EcoCharacter(e) => e
	}


	trait Agent
	case class SimpleAgent(l: Int, h: Habitat, p: DspCharacter, e: EcoCharacter) extends Agent{
		//the full state
		override def toString = l.toString + "\t" + h.toString + "\t" + valueOfStateComponent(p).toString + "\t" + valueOfStateComponent(e).toString
	}

		//state component encodes the state that is expressed through a function 
		//this implementation is not very general
	def unknownAgent: Error = new Error("unknown agent type")

	implicit object evMigratable extends Migratable[Agent, Habitat]{
		def location(a: Agent) = a match{
			case SimpleAgent(_, h, _, _) => h
			case _ => throw unknownAgent
		}

		def migratedTo(a: Agent, h: Habitat) = a match {
			case SimpleAgent(l, _, p, e) => SimpleAgent(l, h, p, e)
			case _ => throw unknownAgent
		}
	}

	implicit object evDispersable extends Dispersable[Agent, Habitat] {
		def location(a: Agent) = evMigratable.location(a)
		
		def migratedTo(a: Agent, h: Habitat) = evMigratable.migratedTo(a, h)

		implicit val ranGen = randomGenerator
		
		def pbtfn(h: Habitat)(a: Agent)(b: Boolean) = a match {
			case SimpleAgent(_, _, DspCharacter(p), _) => if (b) dspPbtFun(p) else (1.0 - dspPbtFun(p)) //p/(1.0 + p) else 1.0/(1.0 + p)
			case _ => throw unknownAgent
		}

	}


	implicit object evStrategy extends Strategy[Agent, Habitat, Int]{
		def response(h: Habitat)(a: Agent): Int = (h, a) match {
			case (Habitat(et, _, mo), SimpleAgent(_, _, _, EcoCharacter(ec))) => (mo*exp(-1.0 * pow(ec - et, 2.0)/2.0)).toInt
			case _ => throw new Error("uknown agent type")
		}
	}



	//an example implementation of Replicatable
	implicit object evReplicatable extends Replicatable[Agent, StateComponent]{
		def numOffspring(a: Agent): Int = a match{
			//case SimpleAgent(_, Habitat(et, _, mo), _, EcoCharacter(ec)) =>  (mo*exp(-1.0 * pow(ec - et, 2.0)/2.0)).toInt
			case SimpleAgent(_, Habitat(et, _, mo), _, EcoCharacter(ec)) =>  (mo*frxnMaxOffSpring(ec - et)).toInt
			case _ => throw unknownAgent
		}
	
		def replicatedWithState(a: Agent, ss: List[StateComponent]): Agent = (a, ss) match {
			case (_, Nil)	=> a
			case (SimpleAgent(m, h, _, e), (p@DspCharacter(_))::rs) => replicatedWithState(SimpleAgent(m, h, p, e), rs)
			case (SimpleAgent(m, h, p, _), (e@EcoCharacter(_))::rs) => replicatedWithState(SimpleAgent(m, h, p, e), rs)
			case _ => throw new Error("unknown agent type ")
		}
	}

	implicit object evEvolvable extends Evolvable[Agent, StateComponent]{
		def mutated(s: StateComponent): StateComponent = s match {
			//case DspCharacter(p) =>	DspCharacter(p + muDP + sigmaDP*randomGenerator.nextGaussian())
			//case EcoCharacter(e)  =>  EcoCharacter(e + muEC + sigmaEC*randomGenerator.nextGaussian())
			case DspCharacter(p) => DspCharacter(dispersalMutator(p))
			case EcoCharacter(e) => EcoCharacter(ecoCrtrMutator(e))
			case _	=> s
		}

		def states(a: Agent): List[StateComponent] = a match {
			case SimpleAgent(_, _, p, e) => List[StateComponent](p, e)
			case _ => List[StateComponent]()
		}

		def numOffspring(a: Agent): Int = evReplicatable.numOffspring(a)
		def replicatedWithState(a: Agent, ss: List[StateComponent]) = evReplicatable.replicatedWithState(a, ss)
	}


	def populationDispProbs(xs: List[Agent]): List[Double] = xs map { case SimpleAgent(_, _, p, _) => valueOfStateComponent(p)}
	def populationEcoCharac(xs: List[Agent]): List[Double] = xs map { case SimpleAgent(_, _, _, e) => valueOfStateComponent(e)}

	def stateSummary(x: Agent): (Double, Double) = x match {case SimpleAgent(_, _, p, e) => (valueOfStateComponent(p), valueOfStateComponent(e))}
	
	def stateExpression(x: Agent): String = evReplicatable.numOffspring(x).toString + "\t" + evDispersable.pbtfn( evDispersable.location(x))(x)(true)
}

object DispVsEcoChr{
	def apply(muDP: Double, sDP: Double, muEC: Double, sEC: Double)(implicit ranGen: util.Random) =	{
		val dpm = (p: Double) => p + muDP + sDP*ranGen.nextGaussian()
		val epm = (e: Double) => e + muEC + sEC*ranGen.nextGaussian()
		val dspf = (p: Double) => 1.0/(1.0 + p)
		val mof = (e: Double) => exp(-1.0 * pow(e, 2.0)/2.0)
		new DispVsEcoChr(dpm, epm, dspf, mof,  ranGen)
	}
	def apply(fdp: Double => Double, fec: Double => Double, fmof: Double => Double)(implicit ranGen: util.Random) = {
		val dspf = (p: Double) => 1.0/(1.0 + p)
		new DispVsEcoChr(fdp, fec, dspf, fmof, ranGen)
	}
} 
	

	
	
