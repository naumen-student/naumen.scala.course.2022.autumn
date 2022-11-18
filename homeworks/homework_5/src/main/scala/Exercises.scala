object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal


  case class Shelter[+T <: Animal](list: List[T]) {

    private val _list = list
    val getNames: List[String] = _list.map(animal => animal.name)

    def +[A >: T <: Animal](animal: A): Shelter[A] = Shelter(_list :+ animal)

    def ++[A >: T <: Animal](shelter: Shelter[A]): Shelter[A] = Shelter(_list ::: shelter.list)

    def feed(meat: Food[T]): List[String] =
      _list.map(animal => meat.feed(animal))
  }


  trait Food[-T <: Animal] {
    val _name: String

    def feed(animal: T): String =
      animal.name + " eats " + _name
  }

  case object Meat extends Food[Animal] {
    override val _name = "meat"
  }

  case object Milk extends Food[Cat] {
    override val _name = "milk"
  }


  case object Bread extends Food[Dog] {
    override val _name = "bread"
  }
}
