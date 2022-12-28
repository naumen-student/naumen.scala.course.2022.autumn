object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+T <: Animal](animals: List[T]) {
    def +[B >: T <: Animal](other: B): Shelter[B] = {
      Shelter(animals ++ List(other))
    }

    def ++[B >: T <: Animal](others: Shelter[B]): Shelter[B] = {
      Shelter(others.animals ++ animals)
    }

    def getNames: List[String] = animals.map(_.name)

    def feed(food: Food[T]): List[String] =
      animals.map(animal => animal.name + " eats " + food.toString)
  }



  trait Food [-T <: Animal] {
    override def toString: String = ""
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
