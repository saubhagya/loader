import com.sau.Application

/**
  * Created by saubhagya on 21/02/17.
  */
object LoaderTest extends App {
  Console.print(getClass.getClassLoader)
  val app = new Application("abc")
  Console.print(app.getClass.getClassLoader)
}
