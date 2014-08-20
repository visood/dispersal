package evolution.strategy
//import scala.util.Random


//lets try to use scala type class pattern to understand what an evolutionary strategy is
//started off assuming that class implementations of strategy will be made by the user.
//now we use the type class pattern for strategies as well.
//we allow the user to implement states of the individual
//that can be anything, and provide functions that will allow the state to be used 
//as a strategy of any kind! An individual will be characterized as a collection of states.

//we have played with the idea of a strategy elsewhere, 
//anything cannot be allowed, so we constrain the second parameter of 
//Strategy to be a state of the individuals.

//2013 June 30, continuing to understand the concepts, here is another attempt

trait Strategy[S, C, O]{
	def response(c: C)(s: S): O //given my state, how will I respond to conditions C?
}

trait MixedStrategy[S, C, O] extends Strategy[S, C, O]{
	def outcomes: Set[O] //what would we do if the outcomes were defined on a continuous space
	def pbtfn(c: C)(s: S)(o: O): Double //need not be normalized
	def norm(c: C)(s: S): Double = (outcomes.toList map pbtfn(c)(s)).sum
	implicit def ranGen: util.Random //how can we make this implicit!
}

trait BinaryDecision[S, C] extends Strategy[S, C, Boolean]

trait StochasticBinaryDecision[S, C] extends MixedStrategy[S, C, Boolean]{
	def outcomes = Set(true, false)
	def response(c: C)(s: S): Boolean = 
		if (ranGen.nextDouble() < pbtfn(c)(s)(true)/norm(c)(s)) true else false
}

trait Habitable[H]{
	def carryingCapacity(h: H): Int
}

trait Locatable[I, H]{
	def location(i: I):  H
}
trait Migratable[I, H] extends Locatable[I, H]{
	def migratedTo(i: I, h: H): I
}


trait Dispersable[I, H] extends Migratable[I, H] with StochasticBinaryDecision[I, H]{
	def dispersedOver(hs: List[H])(i: I): I = {
		if (response( location(i) )(i)) migratedTo(i, stochastic.sampleUniformlyOnceButNot(hs, location(i))(ranGen)) else i
	}
}

//evolvable is an individual with mutatable states
trait Mutatable[S] { //to mutate require a rule that requires some parameters
	def mutated(s: S): S
}

//trait Replicatable[I, S, H]  extends  Strategy[S, H, Int]{

trait Replicatable[I, S] {
	def numOffspring(i: I): Int	
	def replicatedWithState(i: I, ss: List[S]): I //the implementation should be able decide which S
}

trait Evolvable[I, S] extends Mutatable[S] with Replicatable[I, S]{
	def states(i: I): List[S]
	def evolved(i: I): I = replicatedWithState(i, states(i) map mutated)
}
		

//some functions

object dispersalFunctions{
	
	def dispersedOver[I, H](hs: List[H])(xs: List[I])(implicit ev: Dispersable[I, H]): List[I] = 
		xs map (x => ev.dispersedOver(hs)(x))

	def replicatedWithMutations[I, S](xs: List[I])(implicit ev: Evolvable[I, S]): List[I] = 
		xs flatMap (x => Seq.fill(ev.numOffspring(x))(ev.evolved(x)))

	def settlersAmong[I, H](xs: List[I])(implicit evd: Dispersable[I, H], evh: Habitable[H]) : List[I] = {
		val ihs = xs map (x => (x, evd.location(x)))
		val hiMap = ( ihs foldLeft Map[H, List[I]]()){
									case (m, (x, h)) => m + (h -> (x::m.getOrElse(h, List[I]())))
								}
		hiMap.toList flatMap { case (h, xs) => stochastic.sampleWithoutReplacement(xs, evh.carryingCapacity(h))(evd.ranGen)}
	}

}

case class DispersalListModifiers[T, S, H](hs: List[H]){
	 import dispersalFunctions._
	 import listModifier._
	 def replicated(implicit ev: Evolvable[T, S]): ListModifier[T] = Verb[T](replicatedWithMutations[T, S])
   def replication(implicit ev: Evolvable[T, S]): ListModifier[T] = Verb[T](replicatedWithMutations[T, S])
   def replicants(implicit ev: Evolvable[T, S]): ListModifier[T] = Verb[T](replicatedWithMutations[T, S]) 
   def dispersed(implicit ev: Dispersable[T, H]): ListModifier[T] = Verb[T](dispersedOver[T, H](hs))
   def dispersal(implicit ev: Dispersable[T, H]): ListModifier[T] = Verb[T](dispersedOver[T, H](hs))
   def dispersing(implicit ev: Dispersable[T, H]): ListModifier[T] = Verb[T](dispersedOver[T, H](hs))
   def settlers(implicit evd: Dispersable[T, H], evh: Habitable[H]): ListModifier[T] = Verb[T](settlersAmong[T, H])
   def settled(implicit evd: Dispersable[T, H], evh: Habitable[H]): ListModifier[T] = Verb[T](settlersAmong[T, H])
}

object stochastic{
	def sampleUniformlyOnce[T](xs: Seq[T])(implicit ranGen: util.Random): T = xs.length match{
		case 0 => throw new Error("nothing to choose from")
		case l => 		xs( ranGen.nextInt(l))
	}

	def sampleUniformlyOnceButNot[T](xs: Seq[T], y: T)(implicit ranGen: util.Random): T = {
		val x = xs(ranGen.nextInt(xs.length))
		x match {
			case `y` => sampleUniformlyOnceButNot(xs, y)
			case _ => x
		}
	}

	def sampleWithoutReplacement[T](xs: Seq[T], n: Int)(implicit ranGen: util.Random): Seq[T]  = ranGen.shuffle(xs) take n
}
