public class AFunction implements Function {
    private static Object[] constants;
    private Object[] closed;
    
    static void initialize(Environment e) {
        constants = new Object[] {e.lookup("CL", "1+"), e.lookup("CL", "ERROR")};
    }
    
    public Object[] call(Object[] arguments) {
        return new Object[] {1, 2};
    }
}
