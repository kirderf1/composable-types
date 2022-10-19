// Object algebra interface for negation, extending interface for expressions
interface NegAlg<T> extends ExprAlg<T> {
    T neg (T e);
}

// Object algebra for evaluation of negation, extending the OA for eval of expressions
class NegEval extends ExprEval implements NegAlg<Eval> {
    public Eval neg(Eval e) {return () -> -e.eval();}
}

// Object algebra for asString of negation, extending the OA for asString of expressions
class NegAsString extends ExprAsString implements NegAlg<String> {
    public String neg(String e) {return "(-" + e + ")";}
}