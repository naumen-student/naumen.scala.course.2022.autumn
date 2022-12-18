object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal

  case class Shelter[+T <: Animal](animals: List[T]) {

    def +[A >: T <: Animal](animal: A) = Shelter(animals :+ animal)

    def ++[A >: T <: Animal](shelter: Shelter[A]) = Shelter(animals ::: shelter.animals)

    def getNames() = animals.map(_.name)

    def feed(food: Food[T]) = animals.map(food.feed(_))
  }

  trait Food[-T <: Animal]{
    def feed(animal: Animal) = s"${animal.name} eats $toString"

    def toString(): String
  }

  case object Meat extends Food[Animal] {
    override def toString: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override def toString: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override def toString: String = "bread"
  }
}
