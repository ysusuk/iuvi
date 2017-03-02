Encode
```tut
import com.iuriisusuk.iuvi.ItemEncoder._

case class Person(name: String, age: Int, manager: Boolean)
encode(Person("Bob", 37, true))
```