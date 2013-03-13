package cernoch.sm.logic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import cernoch.scalogic._

@RunWith(classOf[JUnitRunner])
class RefineTest extends Specification {

	val numD = Domain.int("num")

	val catD1 = Domain.cat("cat1")
	val catD2 = Domain.cat("cat1", Set("A", "B", "C"))

	val x = Var(numD)
	val y = Var(catD1)
	val z = Var(catD2)

	val A = Val("A", catD2)
	val B = Val("B", catD2)
	val C = Val("C", catD2)

	"Instantiable variables" should {
		"be only those with a set of values" in {
			Generator.instantiable(Set(x,y,z)) must_==
				Set(z -> A, z -> B, z -> C)
		}
	}

	"All substitutions" should {
		"return all possible mappings" in {
			Generator.allSubsts(List(x,y), List(x,y,z))
				.toSet must_== Set(
					Map(x->x, y->y),
					Map(x->x, y->z))
		}
	}
}