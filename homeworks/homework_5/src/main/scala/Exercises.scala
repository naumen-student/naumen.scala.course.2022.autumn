object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal


  case class Shelter[+B <: Animal](animals: List[B]) {

    private val _list = animals
    def getNames: List[String] = _list.map(_.name)

    def +[A >: B <: Animal](animal: A): Shelter[A] = Shelter(_list :+ animal)

    def ++[A >: B <: Animal](shelter: Shelter[A]): Shelter[A] = Shelter(_list ::: shelter.animals)

    def feed(food: Food[B]): List[String] = _list.map(animal => food.feed(animal))
  }


  trait Food[-A <: Animal] {
    val name: String

    def feed(animal: A): String = animal.name + " eats " + name
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