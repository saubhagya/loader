package com.test

import java.io.{ByteArrayOutputStream, File}
import java.net._
import java.nio.ByteBuffer
import java.security.cert.Certificate
import java.security._
import java.util.jar.JarFile
import java.lang.ClassLoader
import java.io.Closeable

import sun.misc.{Resource, URLClassPath}

import scala.collection.mutable
import scala.reflect.internal.util.{HasClassPath, ScalaClassLoader}
import scala.reflect.io.AbstractFile
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.lang.ref._
import java.io._
import java.util.{Enumeration, List, ResourceBundle, Set, WeakHashMap}
import java.util.jar.{Attributes, JarFile, Manifest}
import java.util.jar.Attributes.Name
import java.security.CodeSigner
import java.security.PrivilegedAction
import java.security.PrivilegedExceptionAction
import java.security.AccessController
import java.security.AccessControlContext
import java.security.SecureClassLoader
import java.security.CodeSource
import java.security.Permission
import java.security.PermissionCollection

import sun.misc.Resource
import sun.misc.URLClassPath
import sun.net.www.ParseUtil
import sun.net.www.protocol.file.FileURLConnection
import sun.security.util.SecurityConstants

/**
  * Created by saubhagya on 21/02/17.
  */

class MyClassLoader(parent: ClassLoader)
  extends SecureClassLoader(parent)
    with Closeable {
  /* The search path for classes and resources */ private var ucp: URLClassPath = null
  /* The context to be used when loading classes and resources */ private var acc: AccessControlContext = null

  /**
    * Constructs a new MyClassLoader for the given URLs. The URLs will be
    * searched in the order specified for classes and resources after first
    * searching in the specified parent class loader. Any URL that ends with
    * a '/' is assumed to refer to a directory. Otherwise, the URL is assumed
    * to refer to a JAR file which will be downloaded and opened as needed.
    *
    * <p>If there is a security manager, this method first
    * calls the security manager's {@code checkCreateClassLoader} method
    * to ensure creation of a class loader is allowed.
    *
    * @param urls   the URLs from which to load classes and resources
    * @param parent the parent class loader for delegation
    * @exception SecurityException  if a security manager exists and its
    *            { @code checkCreateClassLoader} method doesn't allow
    *                    creation of a class loader.
    * @see SecurityManager#checkCreateClassLoader
    */
  // this is to make the stack depth consistent with 1.1
  val security: SecurityManager = System.getSecurityManager
  var count = 1
  println(s"over here with count $count")
  ucp= if (count % 2 == 0) {
    new URLClassPath(Array(MyClassLoader.oldUrl, MyClassLoader.newAppUrl))
  } else {
    new URLClassPath(Array(MyClassLoader.oldUrl, MyClassLoader.oldAppUrl))
  }
  if (security != null) security.checkCreateClassLoader()
//  ucp = new URLClassPath(Array(MyClassLoader.oldUrl, MyClassLoader.appUrl))
  this.acc = AccessController.getContext

  println(s"Instantiated MyLoader with parent $parent")

  private val closeables: WeakHashMap[Closeable, Void] = new WeakHashMap[Closeable, Void]

  /**
    * Returns an input stream for reading the specified resource.
    * If this loader is closed, then any resources opened by this method
    * will be closed.
    *
    * <p> The search order is described in the documentation for {@link
    * #getResource(String)}.  </p>
    *
    * @param  name
    * The resource name
    * @return An input stream for reading the resource, or <tt>null</tt>
    *         if the resource could not be found
    * @since 1.7
    */
  override def getResourceAsStream(name: String): InputStream = {
    println(s"inside getResourceAsStream for $name")
    val url: URL = getResource(name)
    try {
      if (url == null) return null
      val urlc: URLConnection = url.openConnection
      val is: InputStream = urlc.getInputStream
      if (urlc.isInstanceOf[JarURLConnection]) {
        val juc: JarURLConnection = urlc.asInstanceOf[JarURLConnection]
        val jar: JarFile = juc.getJarFile
        closeables synchronized {
          if (!closeables.containsKey(jar)) closeables.put(jar, null)
        }
      }
      else if (urlc.isInstanceOf[FileURLConnection]) closeables synchronized {
        closeables.put(is, null)
      }
      is
    } catch {
      case e: IOException => {
        null
      }
    }
  }

  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    println(s"over here with count $count")
    count += 1
    ucp= if (count % 2 == 0) {
      new URLClassPath(Array(MyClassLoader.oldUrl, MyClassLoader.newAppUrl))
    } else {
      new URLClassPath(Array(MyClassLoader.oldUrl, MyClassLoader.oldAppUrl))
    }


    println(s"loading $name with $this")
    try {
      val x = if (name.startsWith("java.") || name.startsWith("scala.") || name.startsWith("sun.")) {
        super.loadClass(name, resolve)
      } else {
        findClass(name)
      }
      println(s"loaded $x with ${x.getClassLoader}")
      x
    } catch {
      case ex:Exception => println(s"failed to load $name with exception: $ex")
        null
    }
  }

  /**
    * Closes this MyClassLoader, so that it can no longer be used to load
    * new classes or resources that are defined by this loader.
    * Classes and resources defined by any of this loader's parents in the
    * delegation hierarchy are still accessible. Also, any classes or resources
    * that are already loaded, are still accessible.
    * <p>
    * In the case of jar: and file: URLs, it also closes any files
    * that were opened by it. If another thread is loading a
    * class when the {@code close} method is invoked, then the result of
    * that load is undefined.
    * <p>
    * The method makes a best effort attempt to close all opened files,
    * by catching {@link IOException}s internally. Unchecked exceptions
    * and errors are not caught. Calling close on an already closed
    * loader has no effect.
    * <p>
    *
    * @throws IOException       if closing any file opened by this class loader
    *                           resulted in an IOException. Any such exceptions are caught internally.
    *                           If only one is caught, then it is re-thrown. If more than one exception
    *                           is caught, then the second and following exceptions are added
    *                           as suppressed exceptions of the first one caught, which is then re-thrown.
    * @throws SecurityException if a security manager is set, and it denies
    *                           { @link RuntimePermission}<tt>("closeClassLoader")</tt>
    * @since 1.7
    */
  @throws[IOException]
  def close() {
    val security: SecurityManager = System.getSecurityManager
    if (security != null) security.checkPermission(new RuntimePermission("closeClassLoader"))
    val errors: List[IOException] = ucp.closeLoaders
    // now close any remaining streams.
    closeables synchronized {
      val keys: Set[Closeable] = closeables.keySet
      import scala.collection.JavaConversions._
      for (c <- keys) {
        try {
          c.close()
        }
        catch {
          case ioex: IOException => {
            errors.add(ioex)
          }
        }
      }
      closeables.clear()
    }
    if (errors.isEmpty) return
    val firstex: IOException = errors.remove(0)
    // Suppress any remaining exceptions
    import scala.collection.JavaConversions._
    for (error <- errors) {
      firstex.addSuppressed(error)
    }
    throw firstex
  }

  /**
    * Appends the specified URL to the list of URLs to search for
    * classes and resources.
    * <p>
    * If the URL specified is <code>null</code> or is already in the
    * list of URLs, or if this loader is closed, then invoking this
    * method has no effect.
    *
    * @param url the URL to be added to the search path of URLs
    */
  protected def addURL(url: URL) {
    ucp.addURL(url)
  }

  /**
    * Returns the search path of URLs for loading classes and resources.
    * This includes the original list of URLs specified to the constructor,
    * along with any URLs subsequently appended by the addURL() method.
    *
    * @return the search path of URLs for loading classes and resources.
    */
  def getURLs: Array[URL] = ucp.getURLs

  /**
    * Finds and loads the class with the specified name from the URL search
    * path. Any URLs referring to JAR files are loaded and opened as needed
    * until the class is found.
    *
    * @param name the name of the class
    * @return the resulting class
    * @exception ClassNotFoundException if the class could not be found,
    *            or if the loader is closed.
    */
  @throws[ClassNotFoundException]
  override protected def findClass(name: String): Class[_] = try {
    println(s"inside findClass for $name")
    AccessController.doPrivileged(new PrivilegedExceptionAction[Class[_]]() {
      @throws[ClassNotFoundException]
      def run: Class[_] = {
        val path: String = name.replace('.', '/').concat(".class")
        println(s"path is $path ucp is $ucp")
        val res: Resource = ucp.getResource(path, false)
        if (res != null) try {
          println(s"went to defineClass $name")
          defineClass(name, res)
        } catch {
          case e: IOException => {
            throw new ClassNotFoundException(name, e)
          }
        }
        else throw new ClassNotFoundException(name)
      }
    }, acc)
  }
  catch {
    case pae: PrivilegedActionException => {
      throw pae.getException.asInstanceOf[ClassNotFoundException]
    }
  }

  /*
   * Retrieve the package using the specified package name.
   * If non-null, verify the package using the specified code
   * source and manifest.
   */ private def getAndVerifyPackage(pkgname: String, man: Manifest, url: URL): Package = {
    val pkg: Package = getPackage(pkgname)
    if (pkg != null) {
      // Package found, so check package sealing.
      if (pkg.isSealed) {
        // Verify that code source URL is the same.
        if (!pkg.isSealed(url)) throw new SecurityException("sealing violation: package " + pkgname + " is sealed")
      }
      else {
        // Make sure we are not attempting to seal the package
        // at this code source URL.
        if ((man != null) && isSealed(pkgname, man)) throw new SecurityException("sealing violation: can't seal package " + pkgname + ": already loaded")
      }
    }
    pkg
  }

  /*
   * Defines a Class using the class bytes obtained from the specified
   * Resource. The resulting Class must be resolved before it can be
   * used.
   */ @throws[IOException]
  private def defineClass(name: String, res: Resource): Class[_] = {
    println(s"inside defineClass for $name")
    val t0: Long = System.nanoTime
    val i: Int = name.lastIndexOf('.')
    val url: URL = res.getCodeSourceURL
    if (i != -1) {
      val pkgname: String = name.substring(0, i)
      // Check if package already loaded.
      val man: Manifest = res.getManifest
      if (getAndVerifyPackage(pkgname, man, url) == null)
        try {
          if (man != null) definePackage(pkgname, man, url)
          else definePackage(pkgname, null, null, null, null, null, null, null)
        } catch {
          case iae: IllegalArgumentException => {
            // parallel-capable class loaders: re-verify in case of a
            // race condition
            if (getAndVerifyPackage(pkgname, man, url) == null) {
              // Should never happen
              throw new AssertionError("Cannot find package " + pkgname)
            }
          }
        }
    }
    // Now read the class bytes and define the class
    val bb: ByteBuffer = res.getByteBuffer
    if (bb != null) {
      // Use (direct) ByteBuffer:
      val signers: Array[CodeSigner] = res.getCodeSigners
      val cs: CodeSource = new CodeSource(url, signers)
      sun.misc.PerfCounter.getReadClassBytesTime.addElapsedTimeFrom(t0)
      println(s"using direct bytebuffer")
      defineClass(name, bb, cs)
    }
    else {
      val b: Array[Byte] = res.getBytes
      // must read certificates AFTER reading bytes.
      val signers: Array[CodeSigner] = res.getCodeSigners
      val cs: CodeSource = new CodeSource(url, signers)
      sun.misc.PerfCounter.getReadClassBytesTime.addElapsedTimeFrom(t0)
      println(s"using read bytebuffer")
      defineClass(name, b, 0, b.length, cs)
    }
  }

  /**
    * Defines a new package by name in this ClassLoader. The attributes
    * contained in the specified Manifest will be used to obtain package
    * version and sealing information. For sealed packages, the additional
    * URL specifies the code source URL from which the package was loaded.
    *
    * @param name the package name
    * @param man  the Manifest containing package version and sealing
    *             information
    * @param url  the code source url for the package, or null if none
    * @exception IllegalArgumentException if the package name duplicates
    *            an existing package either in this class loader or one
    *            of its ancestors
    * @return the newly defined Package object
    */
  @throws[IllegalArgumentException]
  protected def definePackage(name: String, man: Manifest, url: URL): Package = {
    println(s"inside definePackage for $name")

    val path: String = name.replace('.', '/').concat("/")
    var specTitle: String = null
    var specVersion: String = null
    var specVendor: String = null
    var implTitle: String = null
    var implVersion: String = null
    var implVendor: String = null
    var `sealed`: String = null
    var sealBase: URL = null
    var attr: Attributes = man.getAttributes(path)
    if (attr != null) {
      specTitle = attr.getValue(Name.SPECIFICATION_TITLE)
      specVersion = attr.getValue(Name.SPECIFICATION_VERSION)
      specVendor = attr.getValue(Name.SPECIFICATION_VENDOR)
      implTitle = attr.getValue(Name.IMPLEMENTATION_TITLE)
      implVersion = attr.getValue(Name.IMPLEMENTATION_VERSION)
      implVendor = attr.getValue(Name.IMPLEMENTATION_VENDOR)
      `sealed` = attr.getValue(Name.SEALED)
    }
    attr = man.getMainAttributes
    if (attr != null) {
      if (specTitle == null) specTitle = attr.getValue(Name.SPECIFICATION_TITLE)
      if (specVersion == null) specVersion = attr.getValue(Name.SPECIFICATION_VERSION)
      if (specVendor == null) specVendor = attr.getValue(Name.SPECIFICATION_VENDOR)
      if (implTitle == null) implTitle = attr.getValue(Name.IMPLEMENTATION_TITLE)
      if (implVersion == null) implVersion = attr.getValue(Name.IMPLEMENTATION_VERSION)
      if (implVendor == null) implVendor = attr.getValue(Name.IMPLEMENTATION_VENDOR)
      if (`sealed` == null) `sealed` = attr.getValue(Name.SEALED)
    }
    if ("true".equalsIgnoreCase(`sealed`)) sealBase = url
    definePackage(name, specTitle, specVersion, specVendor, implTitle, implVersion, implVendor, sealBase)
  }

  /*
   * Returns true if the specified package name is sealed according to the
   * given manifest.
   */ private def isSealed(name: String, man: Manifest): Boolean = {
    val path: String = name.replace('.', '/').concat("/")
    var attr: Attributes = man.getAttributes(path)
    var sealedVar: String = null
    if (attr != null) {
      sealedVar = attr.getValue(Name.SEALED)
    }
    if (sealedVar == null) {
      if (attr != null)
        sealedVar = attr.getValue(Name.SEALED)
    }
    "true".equalsIgnoreCase(sealedVar)
  }

  /**
    * Finds the resource with the specified name on the URL search path.
    *
    * @param name the name of the resource
    * @return a <code>URL</code> for the resource, or <code>null</code>
    *         if the resource could not be found, or if the loader is closed.
    */
  override def findResource(name: String): URL = {
    println(s"inside findResource for $name")

    val url: URL = AccessController.doPrivileged(new PrivilegedAction[URL]() {
      def run: URL = ucp.findResource(name, true)
    }, acc)
    if (url != null) ucp.checkURL(url)
    else null
  }

  /**
    * Returns an Enumeration of URLs representing all of the resources
    * on the URL search path having the specified name.
    *
    * @param name the resource name
    * @exception IOException if an I/O exception occurs
    * @return an <code>Enumeration</code> of <code>URL</code>s
    *         If the loader is closed, the Enumeration will be empty.
    */
  @throws[IOException]
  override def findResources(name: String): Enumeration[URL] = {
    println(s"inside findResources for $name")
    val e: Enumeration[URL] = ucp.findResources(name, true)
    new Enumeration[URL]() {
      private var url: URL = null
      private

      def next: Boolean = {
        if (url != null) return true
        do {
          val u: URL = AccessController.doPrivileged(new PrivilegedAction[URL]() {
            def run: URL = {
              if (!e.hasMoreElements) return null
              e.nextElement
            }
          }, acc)
          if (u == null)
            return (url != null) //todo: break is not supported
          url = ucp.checkURL(u)
        } while (url == null)
        url != null
      }

      def nextElement: URL =
      {
        if (!next) throw new NoSuchElementException
        val u: URL = url
        url = null
        u
      }

      def hasMoreElements: Boolean =
      {
        next
      }
    }
  }

  /**
    * Returns the permissions for the given codesource object.
    * The implementation of this method first calls super.getPermissions
    * and then adds permissions based on the URL of the codesource.
    * <p>
    * If the protocol of this URL is "jar", then the permission granted
    * is based on the permission that is required by the URL of the Jar
    * file.
    * <p>
    * If the protocol is "file" and there is an authority component, then
    * permission to connect to and accept connections from that authority
    * may be granted. If the protocol is "file"
    * and the path specifies a file, then permission to read that
    * file is granted. If protocol is "file" and the path is
    * a directory, permission is granted to read all files
    * and (recursively) all files and subdirectories contained in
    * that directory.
    * <p>
    * If the protocol is not "file", then permission
    * to connect to and accept connections from the URL's host is granted.
    *
    * @param codesource the codesource
    * @return the permissions granted to the codesource
    */
  override protected def getPermissions(codesource: CodeSource): PermissionCollection = {
    val perms: PermissionCollection = super.getPermissions(codesource)
    val url: URL = codesource.getLocation
    var p: Permission = null
    var urlConnection: URLConnection = null
    try {
      urlConnection = url.openConnection
      p = urlConnection.getPermission

    } catch {
      case ioe: IOException => {
        p = null
        urlConnection = null
      }
    }
    if (p.isInstanceOf[FilePermission]) {
      // if the permission has a separator char on the end,
      // it means the codebase is a directory, and we need
      // to add an additional permission to read recursively
      var path: String = p.getName
      if (path.endsWith(File.separator)) {
        path += "-"
        p = new FilePermission(path, SecurityConstants.FILE_READ_ACTION)
      }
    }
    else if ((p == null) && (url.getProtocol == "file")) {
      var path: String = url.getFile.replace('/', File.separatorChar)
      path = ParseUtil.decode(path)
      if (path.endsWith(File.separator)) path += "-"
      p = new FilePermission(path, SecurityConstants.FILE_READ_ACTION)
    }
    else {
      /**
        * Not loading from a 'file:' URL so we want to give the class
        * permission to connect to and accept from the remote host
        * after we've made sure the host is the correct one and is valid.
        */
      var locUrl: URL = url
      if (urlConnection.isInstanceOf[JarURLConnection]) locUrl = urlConnection.asInstanceOf[JarURLConnection].getJarFileURL
      val host: String = locUrl.getHost
      if (host != null && (host.length > 0)) p = new SocketPermission(host, SecurityConstants.SOCKET_CONNECT_ACCEPT_ACTION)
    }
    // make sure the person that created this class loader
    // would have this permission
    if (p != null) {
      val sm: SecurityManager = System.getSecurityManager
      if (sm != null) {
        val fp: Permission = p
        AccessController.doPrivileged(new PrivilegedAction[Void]() {
          @throws[SecurityException]
          def run: Void = {
            sm.checkPermission(fp)
            null
          }
        }, acc)
      }
      perms.add(p)
    }
    perms
  }

  /**
    * Creates a new instance of MyClassLoader for the specified
    * URLs and parent class loader. If a security manager is
    * installed, the <code>loadClass</code> method of the MyClassLoader
    * returned by this method will invoke the
    * <code>SecurityManager.checkPackageAccess</code> method before
    * loading the class.
    *
    * @param urls   the URLs to search for classes and resources
    * @param parent the parent class loader for delegation
    * @return the resulting class loader
    */
  def newInstance(urls: Array[URL], parent: ClassLoader): MyClassLoader = {
    // Need a privileged block to create the class loader
    val ucl: MyClassLoader = AccessController.doPrivileged(new PrivilegedAction[MyClassLoader]() {
      def run: MyClassLoader = new FactoryMyClassLoader(urls, parent)
    })
    ucl
  }

  /**
    * Creates a new instance of MyClassLoader for the specified
    * URLs and default parent class loader. If a security manager is
    * installed, the <code>loadClass</code> method of the MyClassLoader
    * returned by this method will invoke the
    * <code>SecurityManager.checkPackageAccess</code> before
    * loading the class.
    *
    * @param urls the URLs to search for classes and resources
    * @return the resulting class loader
    */
  def newInstance(urls: Array[URL]): MyClassLoader = {
    // Need a privileged block to create the class loader
    val ucl: MyClassLoader = AccessController.doPrivileged(new PrivilegedAction[MyClassLoader]() {
      def run: MyClassLoader = new FactoryMyClassLoader(urls)
    })
    ucl
  }
  ClassLoader.registerAsParallelCapable()
}

final class FactoryMyClassLoader(urls: Array[URL], parent: ClassLoader)
  extends MyClassLoader(parent) {

  def this(urls: Array[URL]) {
    this(urls, Thread.currentThread().getContextClassLoader)
  }

  override final def loadClass(name: String, resolve: Boolean): Class[_] = {
    // First check if we have permission to access the package. This
    // should go away once we've added support for exported packages.
    val sm: SecurityManager = System.getSecurityManager
    if (sm != null) {
      val i: Int = name.lastIndexOf('.')
      if (i != -1) sm.checkPackageAccess(name.substring(0, i))
    }
    super.loadClass(name, resolve)
  }
}

object MyClassLoader {
  val oldUrl = new File("/Users/ManikSingla/Development/Classloader/out/production/Classloader").toURI.toURL
  val oldAppUrl = new File("/Users/ManikSingla/testLoader/oldTestApp.jar").toURI.toURL
  val newAppUrl = new File("//Users/ManikSingla/testLoader/newTestApp.jar").toURI.toURL
}