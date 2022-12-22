object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+A <: Animal](animals: List[A]) {
    def +[B >: A <: Animal](other: B): Shelter[B] = {
      Shelter(animals ++ List(other))
    }

    def ++[B >: A <: Animal](others: Shelter[B]): Shelter[B] = {
      Shelter(others.animals ++ animals)
    }

    def getNames: List[String] = animals.map(_.name)

    def feed(food: Food[A]): List[String] = {
      animals.map(animal => animal.name + " eats " + food.toString)
    }
  }



  trait Food[-A <: Animal] {
    override def toString: String = this.getClass.getSimpleName.toLowerCase()
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
