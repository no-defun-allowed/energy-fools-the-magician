public class AFunction implements Function {
    private static Object[] constants;
    public Object[] environment;
    
    static void initialize(Environment e, Object[] constants) {
        this.constants = constants;
    }
    
    public Object[] call(Object[] arguments, Object[] dynamicEnvironment) {
        return new Object[] {1, 2};
    }
}
