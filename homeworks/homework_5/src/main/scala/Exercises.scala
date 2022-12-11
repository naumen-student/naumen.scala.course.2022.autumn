object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal


  case class Shelter[+A <: Animal](animals: List[A]) {
    def ++[B >: A <: Animal](other: Shelter[B]): Shelter[B] = Shelter(animals ++ other.animals)

    def +[B >: A <: Animal](animal: B): Shelter[B] = Shelter(animals :+ animal)

    def getNames: List[String] = animals.map(_.name)

    def feed(food: Food[A]): List[String] = animals.map(_.name + " eats " + food.name)
  }


  trait Food[-A <: Animal] {
    def name: String
  }

  case object Meat extends Food[Animal] {
    override def name: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override def name: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override def name: String = "bread"
  }

}
