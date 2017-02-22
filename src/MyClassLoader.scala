import java.io.File

import scala.reflect.internal.util.ScalaClassLoader

/**
  * Created by saubhagya on 21/02/17.
  */

class MyClassLoader extends ClassLoader(Thread.currentThread.getContextClassLoader) {

  import ScalaClassLoader._

  val url = new File("/Users/saubhagya/Development/lib/lib1").toURI.toURL
  val urlClassLoader = new URLClassLoader(Seq(url), null)

  def this(loader: ClassLoader) {
    this()
  }

  override def loadClass(name: String): Class[_] = {
    urlClassLoader.tryToLoadClass(name).getOrElse(super.loadClass(name))
  }

  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    urlClassLoader.tryToLoadClass(name).getOrElse(super.loadClass(name, resolve))
  }
}
