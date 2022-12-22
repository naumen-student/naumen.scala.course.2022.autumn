object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+Zverushka <: Animal](zverinetc: List[Zverushka]){
    def +[Ktoto >: Zverushka <: Animal](newZver: Ktoto): Shelter[Ktoto] = Shelter(zverinetc.+:(newZver))
    def ++[Ktoto >: Zverushka <: Animal](otherShelter: Shelter[Ktoto]): Shelter[Ktoto] = Shelter(zverinetc ++ otherShelter.zverinetc)
    def getNames: List[String] = zverinetc.map(_.name)
    def feed[Ktoto >: Zverushka <: Animal](food: Food[Ktoto]): List[String] = zverinetc.map(food.feed)
  }



  trait Food[Zverushka <: Animal]{
    val foodName: String
    def feed: Zverushka => String = x => s"${x.name} eats $foodName"
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
