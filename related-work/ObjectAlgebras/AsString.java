// Object algebra for asString of expressions
class ExprAsString implements ExprAlg<String> {
    public String cons(int value) { return Integer.toString(value); }
    public String add(String first, String second) {
        return "(" + first + " + " + second + ")";
    }
    public String mul(String first, String second) {
        return "(" + first + " * " + second + ")";
    }
}