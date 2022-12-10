object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal


  case class Shelter[+A <: Animal](animals: List[A]) {
    def +[B >: A <: Animal](animal: B): Shelter[B] = Shelter(animal :: animals)

    def ++[B >: A <: Animal](other: Shelter[B]): Shelter[B] = Shelter(animals ::: other.animals)

    val getNames: List[String] = animals.map(_.name)

    def feed(food: Food[A]): List[String] = animals.map(food.feed)
  }


  trait Food[-A <: Animal] {
    val selfName: String

    def feed(animal: A) = s"${animal.name} eats $selfName"
  }

  case object Meat extends Food[Animal] {
    override val selfName = "meat"
  }

  case object Milk extends Food[Cat] {
    override val selfName = "milk"
  }

  case object Bread extends Food[Dog] {
    override val selfName = "bread"
  }
}
