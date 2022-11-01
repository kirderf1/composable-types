// Object algebra for desug of expressions
class ExprDesug<T> implements ExprAlg<T> {
    ExprAlg<T> alg;
    public ExprDesug(ExprAlg<T> alg) {
        this.alg = alg;
    }
    public T cons(int i) { return alg.cons(i); }
    public T add(T e1, T e2) {
        return alg.add(e1, e2);
    }
    public T mul(T e1, T e2) {
        return alg.mul(e1, e2);
    }
}

// Object algebra for desug of negation
class NegDesug<T> extends ExprDesug<T> implements NegAlg<T> {
    public NegDesug(ExprAlg<T> alg) {
        super(alg);
    }
    public T neg(T e) { 
        return alg.mul(alg.cons(-1), e); 
    }
}

