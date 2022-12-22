object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal

  //Task 1
  case class Shelter[+T <: Animal](animals: List[T]) {
    val _animals: List[T] = animals
    def +[A >: T <: Animal](animal: A): Shelter[A] = Shelter(_animals :+ animal)
    def ++[A >: T <: Animal](shelter: Shelter[A]): Shelter[A] = Shelter(_animals ++ shelter.animals)
    def getNames: List[String] = {
      _animals.map(animal => animal.name)
    }
    def feed(food: Food[T]): List[String] = {
      _animals.map(animal => food.feed(animal))
    }
  }

  //Task 2
  trait Food[-T <: Animal] {
    val name: String
    def feed[D <: T](animal: D): String = {
      animal.name + " eats " + name
    }
  }

  case object Meat extends Food[Animal] {
    override val name = "meat"
  }

  case object Milk extends Food[Cat] {
    override val name = "milk"
  }

  case object Bread extends Food[Dog] {
    override val name = "bread"
  }
}
