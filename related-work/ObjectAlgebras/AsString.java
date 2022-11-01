// Object algebra for asString of expressions
class ExprAsString implements ExprAlg<String> {
    public String cons(int i) { return String.valueOf(i); }
    public String add(String e1, String e2) {
        return "(" + e1 + " + " + e2 + ")";
    }
    public String mul(String first, String second) {
        return "(" + e1 + " * " + e2 + ")";
    }
}
