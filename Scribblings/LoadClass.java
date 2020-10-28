import java.lang.ClassLoader;
import java.nio.file.Files;
import java.nio.file.Paths;

public class LoadClass {
    static class RiggedClassLoader extends ClassLoader {
        Class loadClass(String pathname, String name) {
            byte[] bytes;
            try {
                bytes = Files.readAllBytes(Paths.get(pathname));
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            return this.defineClass(name, bytes, 0, bytes.length);
        }
    }
    
    public static void main(String[] args) {
        String pathname = args[0];
        String name     = args[1];
        RiggedClassLoader loader = new RiggedClassLoader();
        Class c = loader.loadClass(pathname, name);
    }
}
