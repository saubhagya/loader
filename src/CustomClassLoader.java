import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.List;

import java.io.File;

public class CustomClassLoader extends ClassLoader
{
  private ChildClassLoader childClassLoader;
  public CustomClassLoader(ClassLoader loader) throws MalformedURLException
  {
    super(Thread.currentThread().getContextClassLoader());
    URL[] urls = new URL[]{new File("/Users/saubhagya/Development/lib/lib2/loader.jar").toURI().toURL()};
    childClassLoader = new ChildClassLoader( urls, new DetectClass(this.getParent()) );
  }
  @Override
  protected synchronized Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException
  {
    try
    {
      return childClassLoader.findClass(name);
    }
    catch( ClassNotFoundException e )
    {
      return super.loadClass(name, resolve);
    }
  }
  private static class ChildClassLoader extends URLClassLoader
  {
    private DetectClass realParent;
    public ChildClassLoader( URL[] urls, DetectClass realParent )
    {
      super(urls, null);
      this.realParent = realParent;
    }
    @Override
    public Class<?> findClass(String name) throws ClassNotFoundException
    {
      try
      {
        Class<?> loaded = super.findLoadedClass(name);
        if( loaded != null )
          return loaded;
        return super.findClass(name);
      }
      catch( ClassNotFoundException e )
      {
        return realParent.loadClass(name);
      }
    }
  }
  private static class DetectClass extends ClassLoader
  {
    public DetectClass(ClassLoader parent)
    {
      super(parent);
    }
    @Override
    public Class<?> findClass(String name) throws ClassNotFoundException
    {
      return super.findClass(name);
    }
  }
}
