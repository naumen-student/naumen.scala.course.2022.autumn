object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+T <: Animal](animals : List[T]){
    val _animals: List[T] = animals;

    def +[S >: T <: Animal](animal : S): Shelter[S] ={
      Shelter(_animals :+ animal)
    }

    def ++[S >: T <: Animal](second: Shelter[S]): Shelter[S] = {
      Shelter(_animals ++ second._animals)
    }


    def getNames: List[String] = {
      animals.map(animal=>animal.name)
    }

    def feed(food : Food[T]) : List[String] = {
      _animals.map(animal=>animal.name + " eats " + food.name)
    }


  }



  trait Food[-T <: Animal] {
    val name : String

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
