import java.util.Arrays;
public class TestTestFunction {
    public static void main(String[] args) {
        TestFunction f = new TestFunction();
        f.initialize();
        Object[] x = f.call(null, null);
        System.out.println(x);
        System.out.println(Arrays.toString(x));
    }
}
