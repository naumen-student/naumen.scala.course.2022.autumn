import scala.collection.immutable

object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+T <: Animal](animals: List[T]) {
    def +[S >: T <: Animal] (animal : S) : Shelter[S] = Shelter(animals :+ animal)

    def ++ [S >: T <: Animal] (shelter: Shelter[S]): Shelter[S] = Shelter(animals ++ shelter.animals)

    def getNames:Seq[String] = animals.map(a => a.name)

    def feed(food: Food[T]): List[String] = animals.map(a => food.feed(a))
  }



  trait Food[-T <: Animal] {
    val foodName: String
    def feed(animal: T) = s"${animal.name} eats $foodName"
  }

  case object Meat extends Food[Animal] {
    override val foodName: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override val foodName: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override val foodName: String = "bread"
  }
}