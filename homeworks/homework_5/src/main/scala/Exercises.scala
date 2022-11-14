object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+T <: Animal](animals: List[T]) {

    val _animals: List[T] = animals
    def +[C >: T <: Animal](animal: C): Shelter[C] = Shelter(_animals :+ animal)

    def ++[C >: T <: Animal](other: Shelter[C]): Shelter[C] = Shelter(_animals ++ other._animals)

    def getNames: List[String] = {
      _animals.map(x => x.name)
    }

    def feed[D <: Animal](food: Food[D]): List[String] = {
      for (animal <- _animals if (animal.isInstanceOf[D]))
        yield animal.name + " eats " + food.name
    }
  }

//  то, что требуется сделать во втором задании для прохождения тестов и то,
//  что написано в md файле - две очень разные вещи
  trait Food[T <: Animal] {
    val name: String
  }

  case object Meat extends Food[Animal]{
    override val name: String = "meat"
  }

  case object Milk extends Food[Cat]{
    override val name: String = "milk"
  }

  case object Bread extends Food[Dog]{
    override val name: String = "bread"
  }
}
