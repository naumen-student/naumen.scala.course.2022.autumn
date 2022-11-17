object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+T <: Animal](animals: List[T]) {
    def +[C >: T <: Animal](animal: C): Shelter[C] = {
      Shelter(animals ++ List(animal))
    }

    def ++[C >: T <: Animal](otherShelter: Shelter[C]): Shelter[C] = {
      Shelter(animals ++ otherShelter.animals)
    }

    def getNames: List[String] = {
      animals.map(_.name)
    }

    def feed(food: Food[T]): List[String] = {
      animals.map(animal => animal.name + " eats " + food.nameToString)
    }
  }


  trait Food[-T <: Animal] {
    val nameToString: String
  }

  case object Meat extends Food[Animal] {
    override val nameToString = "meat"
  }

  case object Milk extends Food[Cat] {
    override val nameToString = "milk"
  }

  case object Bread extends Food[Dog] {
    override val nameToString = "bread"
  }
}
