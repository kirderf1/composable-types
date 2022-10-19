// Interface for the desug function
interface Desug<T> {
    T desug();
}

// Object algebra for desug of expressions
class ExprDesug<T> implements ExprAlg<T> {
    ExprAlg<T> alg;
    public ExprDesug(ExprAlg<T> alg) {
        this.alg = alg;
    }
    public T cons(int value) { return alg.cons(value); }
    public T add(T first, T second) {
        return alg.add(first, second);
    }
    public T mul(T first, T second) {
        return alg.mul(first, second);
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

