import java.io.File

import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader

/**
  * Created by saubhagya on 21/02/17.
  */

import MyClassLoader._

class MyClassLoader extends ClassLoader(Thread.currentThread.getContextClassLoader) {

  def this(loader: ClassLoader) {
    this()
  }

  override def loadClass(name: String): Class[_] = {
    OldClassLoader.tryToLoadClass(name).getOrElse(super.loadClass(name))
  }

  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    OldClassLoader.tryToLoadClass(name).getOrElse(super.loadClass(name, resolve))
  }

  override def findClass(name: String): Class[_] = super.findClass(name)
}

object OldClassLoader extends URLClassLoader(Seq(oldUrl, appUrl), null)

object NewClassLoader extends URLClassLoader(Seq(newUrl, appUrl), null)

object MyClassLoader {
  val oldUrl = new File("/Users/saubhagya/Development/lib/lib1/loader.jar").toURI.toURL
  val newUrl = new File("/Users/saubhagya/Development/lib/lib2/loader.jar").toURI.toURL
  val appUrl = new File("/Users/saubhagya/Development/loader/out/production/loader").toURI.toURL
}
