object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal


  case class Shelter[+T <: Animal](animals: List[T]) {
    def +[T2 >: T <: Animal](animal: T2): Shelter[T2] = Shelter(animals :+ animal)

    def ++[T2 >: T <: Animal](shelter: Shelter[T2]): Shelter[T2] = Shelter(shelter.animals ::: this.animals)

    def getNames: Seq[String] = animals.map(x => x.name)

    def feed[T2 >: T <: Animal](food: Food[T2]): Seq[String] = animals.map(food.feed)
  }


  trait Food[-T <: Animal] {
    def feed[T2 <: T](animal: T2): String = s"${animal.name} eats ${this.getClass.getSimpleName.toLowerCase().dropRight(1)}"
  }

  case object Meat extends Food[Animal]()

  case object Milk extends Food[Cat]()

  case object Bread extends Food[Dog]()
}
