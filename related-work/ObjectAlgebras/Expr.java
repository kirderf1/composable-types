// Object algebra interface for expressions
interface ExprAlg<T> {
    T cons(int value);
    T add(T first, T second);
    T mul(T first, T second);
}
