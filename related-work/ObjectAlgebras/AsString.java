// Interface for the asString function
interface AsString {
    String asString();
}

// Object algebra for asString of expressions
class ExprAsString implements ExprAlg<AsString> {
    public AsString cons(int value) { return () -> Integer.toString(value); }
    public AsString add(AsString first, AsString second) {
        return () -> "(" + first.asString() + " + " + second.asString() + ")";
    }
    public AsString mul(AsString first, AsString second) {
        return () -> "(" + first.asString() + " * " + second.asString() + ")";
    }
}