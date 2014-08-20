package evolution.strategy



object listModifier{
	trait ListModifier[T]{
			def compose(m: ListModifier[T]): ListModifier[T]
			def ==< (m: ListModifier[T]) = compose(m)
			def among(m: ListModifier[T]) = compose(m)
			def of(m: ListModifier[T]) = compose(m)
			def in(m: ListModifier[T]) = compose(m)
			def that(m: ListModifier[T]) = compose(m)
			def after(m: ListModifier[T]) = compose(m)
			def the(m: ListModifier[T]) = compose(m)
			def --(m: ListModifier[T]) = compose(m)
			def <--(m: ListModifier[T]) = compose(m)
			def ~(m: ListModifier[T]) = compose(m)
			def + (m: ListModifier[T]) = compose(m)

			def apply(m: ListModifier[T]): List[T]
		}
		case class Noun[T](xs: List[T]) extends ListModifier[T]{
			def compose(m: ListModifier[T])  = m match {
				case Noun(ys) => Noun(xs ++ ys)
				case Verb(g) => Noun( g(xs) )
			}
			def apply(m: ListModifier[T]) = m match {
				case Noun(ys) => xs ++ ys
				case Verb(g) =>   g(xs)
			}
		}


		case class Verb[T](f: List[T] => List[T]) extends ListModifier[T]{
			def compose(m: ListModifier[T])  = m match{
				case Noun(xs) => Noun(f(xs))
				case Verb(g) => Verb( (xs: List[T]) => f( g(xs) ) )
			}
			def apply(m: ListModifier[T]) = m match {
				case Noun(xs) =>  f(xs)
				case Verb(g) => List[T]()
			}
		}
}

