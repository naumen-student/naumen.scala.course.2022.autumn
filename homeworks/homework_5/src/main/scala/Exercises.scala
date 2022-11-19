object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal

  case class Shelter[+T <: Animal](animals: List[T]) {
    def +[TT >: T <: Animal](other: TT): Shelter[TT] =
      Shelter(other +: animals)

    def ++[TT >: T <: Animal](other: Shelter[TT]): Shelter[TT] =
      Shelter(animals ++ other.animals)

    def getNames: List[String] = animals.map(_.name)

    def feed[TT >: T <: Animal](food: Food[TT]): List[String] =
      animals.map(food.feed)
  }
  trait Food[T <: Animal]{
    def feed(animal: T): String
  }

  case object Meat extends Food[Animal] {
    override def feed(animal: Animal): String =
      s"${animal.name} eats meat"
  }

  case object Milk extends Food[Cat] {
    override def feed(animal: Cat): String =
      s"${animal.name} eats milk"
  }

  case object Bread extends Food[Dog] {
    override def feed(animal: Dog): String =
      s"${animal.name} eats bread"
  }
}
