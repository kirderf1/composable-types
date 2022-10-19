// Interface for the evaluation function
interface Eval {
    int eval();
}

// Object algebra for evaluation of expressions
class ExprEval implements ExprAlg<Eval> {
    public Eval cons(int value) { return () -> value; }
    public Eval add(Eval first, Eval second) {
        return () -> first.eval() + second.eval();
    }
    public Eval mul(Eval first, Eval second) {
        return () -> first.eval() * second.eval();
    }
}
