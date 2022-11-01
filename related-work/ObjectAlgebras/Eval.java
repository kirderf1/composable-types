// Interface for the evaluation function
interface Eval {
    int eval();
}

// Object algebra for evaluation of expressions
class ExprEval implements ExprAlg<Eval> {
    public Eval cons(int i) { return () -> i; }
    public Eval add(Eval e1, Eval e2) {
        return () -> first.eval() + second.eval();
    }
    public Eval mul(Eval e1, Eval e2) {
        return () -> first.eval() * second.eval();
    }
}
