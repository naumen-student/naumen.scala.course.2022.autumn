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

    def feed(food: Food[T]): List[String] = _list.map(animal => animal.name + " eats " + food.name)
  }


  trait Food[-T <: Animal] {
    val name: String
  }

  case object Meat extends Food[Animal] {
    override val name: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override val name: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override val name: String = "bread"
  }
}
