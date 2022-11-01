// Object algebra interface for expressions
interface ExprAlg<T> {
    T cons(int i);
    T add(T e1, T e2);
    T mul(T e1, T e2);
}
