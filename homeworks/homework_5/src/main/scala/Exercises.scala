import monocle.macros.GenLens

object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal

  case class Shelter[+T <: Animal](animals: List[T]) {
    def +[L >: T <: Animal](animal: L): Shelter[L] =
      copy(animals = animal :: (animals : List[L]))

    def ++[L >: T <: Animal](shelter2: Shelter[L]): Shelter[L] =
      copy(animals ::: shelter2.animals)

    def feed[E <: Animal](food: Food[E])(implicit TisE: T <:< E): List[String] =
      animals.map(animal => food.feed(TisE(animal)))

    def getNames: List[String] = animals.map(_.name)
  }

  trait Food[T <: Animal] {
    val name: String
    def feed(t: T): String = s"${t.name} eats ${name}"
  }

  case object Meat extends Food[Animal] {
    override val name: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override val name: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override val name: String = "bread"
  }
}
